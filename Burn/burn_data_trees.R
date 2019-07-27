# This is a scratch space for stuff I'll move (if valuable) to burn_data_EDA.Rmd (Where the data sets used here were creatd)

# Summary of project goals as of 6/7/19 (in order to be completed):

# 1. Compare best models derived from Greenhalgh and Sirs variables (separately) to current Greenhalgh and SIRS criteria. The models may be simple cutoffs, logistic regression, and/or decision trees. 
# 2. Same as above but combining Greenhalgh and SIRS variables.
# 3. Consider demographic variables, e.g. age, gender, but still limited to single time point data.
# 4. (If time) Consider derived variables like cumulative days above a threshold. This cant be implemented currently, but could help argue for expanded capabilities with medical records. 

# Summary of next steps as of meeting with Sandy 7/24
# 1. Impute missing data using last value recorded
# 2. Add systolic BP to the GH+SIRS+ vars set because it was used in another study
# 3. Consider multiple different weightings and write up any major differences/include discussion of why we weighted as we did (I-reweighted to approximately even negative and positive cases)
# 4. Include Youden's index, specificity, sensitivity and (maybe) accuracy in write-up [Sandy doesn't have a strict answer for false positive rates are acceptable] 
# 5. Add a couple exploratory trees to investigate what other variables *might* be useful for future, esp. age given our EDA
# 6. Remove filter on time_performed_1 and time_performed_2 but save a round of results before I do
#    Not adding any other filters for now, Sandy confirmed to keep in patients who died 
# last. Apply to PCR data set
# Ask Pamela if we'd have funders to thank


# Notes
# 1. Respiratory rate: We know that distributions of respiratory rate are different for those on vs. not on ventilagor (see TT.vent.plot), but we don't have a variable for ventilation in terms of l/min, which is what the Greenhalgh criteria uses for ventilated patients (Minute ventilation > 12 l/min ventilated). Thus, we can only lag UNventilated patients based on this rate, which could create bias.
# 2. We select the tree that minimizes cross-validation error
# - Needed higher than default xval (cross-validation) when more variables in tree
# 3. Other entries to filter out of the data before modelling, e.g. high mod score? study outcome? (death, withdrawn?) Met study criteria == No
# 4. Sandy argued for using day-before reading instead of techniques for missing data. It's common in the medical communitiy to only note when something changes and otherwise assume it's stayed the same. Special cases:
#   - If they are missing a vital on day 1, let's exclude those data points.
#   - If they are missing multiple days in a row, let's carry the last value forward if it is within 48 hours. (Sandy "balance between something reasonable and dropping records.")

# Questions
# I noticed for platelets, "Thrombocytopenia <100,000/mcl (will not apply until 3 days after initial resuscitation)". How can I incorporate the three-day thing?
                                 

#################################################################################################################################
# ------------------------------------------------------------------------------------------------------------------------------
# 0. Setup ####
# ------------------------------------------------------------------------------------------------------------------------------
#   - packages and wd ----
library(dplyr)
library(plotly)
library(crosstalk)
library(reshape2)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(ROCR)
library(cowplot)
library(stringr)
library(tidyr)
library(readxl)

ndigits = 2
setwd("~/Documents/DSI/Burn/")

#   - load DATA, initialize TT.tree.data ####

TT <- read.csv("TT_jane.csv", stringsAsFactors = F, check.names = F)
TT$per_code_factor = as.factor(TT$PER_CODE)
TT$per_code_factor = reorder(TT$per_code_factor, TT$total_days_since_first_collection) #for plot order later
TT = TT %>% mutate(Blood_test_performed = as.factor(case_when(
  !is.na(V_TIME_PERFORMED_1) & !is.na( V_TIME_PERFORMED_2) ~ "Blood CBC and Chemistry",
  is.na(V_TIME_PERFORMED_1) & !is.na( V_TIME_PERFORMED_2) ~ "Blood CBC",
  !is.na(V_TIME_PERFORMED_1) &  is.na(V_TIME_PERFORMED_2) ~ "Blood Chemistry"))) #both NA auto goes to NA
#vital_vars = names(TT)[grepl(x = names(TT), "V_.*")]
vital_vars = c("V_DATE_COLLECTION", "V_TIME_PERFORMED_1", "V_TIME_PERFORMED_2", "V_TIME_PERFORMED_3",
               "V_HEART_RATE", "V_PB_SYSTOLIC", "V_BP_DIASTOLIC", "V_MEANARTERIAL_PRESSURE", "V_CENTRAL_VENOUS_PRESSURE",
               "V_TEMPERATURE", "V_PAO2", "V_FIO2", "V_PACO2", "V_HCO3",
               "V_RESPIRATORY_RATE", "V_SODIUM", "V_POTASSIUM", "V_BLOOD_UREA_NITROGEN", "V_CREATININE",
               "V_PLATELET_COUNT", "V_WHITE_BC", "V_HEMOGLOBIN", "V_HEMATOCRIT",  "V_GLASCOWCOMA_SCALE", 
               "V_GLUCOSE", "V_MODS_SCORE", "V_PH","V_BILIRUBIN") #for individual plot ordering later

TT_Demo1 = read.csv("TT_Demo1.csv")
TT_per = read.csv("TT_per_jane.csv", stringsAsFactors = F)


#add in V_VENT variable

TT_ABA_2 = read_xlsx("Data/ABA_TT_Daily_Collection_Part_2.xlsx") #I got this later spreadsheet, see email from Sandy 6-7-19
TT = left_join(TT, TT_ABA_2 %>% select(PER_CODE, V_DATE_COLLECTION, V_VENT) %>% 
                 mutate(V_DATE_COLLECTION = as.character(V_DATE_COLLECTION))) %>%
  mutate(V_VENT = ifelse(V_VENT == "No", FALSE, TRUE))
# Node respiratory rates higher on average for ventilated people
TT.vent.plot = TT %>% dplyr::select(V_RESPIRATORY_RATE,V_VENT) %>% ggplot() + geom_histogram(aes(x = V_RESPIRATORY_RATE, group = V_VENT, fill = V_VENT), position = "dodge")

# Create new version of data frame for fitting trees ----
TT.tree.data = TT
Greenhalgh_vars = c("V_TEMPERATURE", "V_HEART_RATE", "V_RESPIRATORY_RATE",  "V_PLATELET_COUNT", "V_GLUCOSE", "V_VENT")
SIRS_vars = c("V_TEMPERATURE", "V_HEART_RATE", "V_RESPIRATORY_RATE",  "V_WHITE_BC", "V_PACO2")
other_vars = c("V_PB_SYSTOLIC")

# Impute missing data for Greenhalgh, SIRS and systolic BP using last recorded metric  ----

impute_missing_from_previous <- function(x) {
  which_missing = which(is.na(x))
  if (length(which_missing) == 0) {
    #pass
  } else {
    x[which_missing] = dplyr::lag(x, default = NA)[which_missing]
    if (is.na(x[1])) {x[]}
    
  }
  return(x)
}

apply(TT.tree.data[,unique(c(Greenhalgh_vars, SIRS_vars, other_vars))], 2, function(x) {mean(is.na(x))})

tmp  = TT.tree.data %>% group_by(per_code_factor) %>% arrange(V_DATE_COLLECTION) %>%
  mutate_at(unique(c(Greenhalgh_vars, SIRS_vars, other_vars)), impute_missing_from_previous)

apply(tmp[,unique(c(Greenhalgh_vars, SIRS_vars, other_vars))], 2, function(x) {mean(is.na(x))})

tmp  = tmp %>% group_by(per_code_factor) %>% arrange(V_DATE_COLLECTION) %>%
  mutate_at(unique(c(Greenhalgh_vars, SIRS_vars, other_vars)), impute_missing_from_previous)

apply(tmp[,unique(c(Greenhalgh_vars, SIRS_vars, other_vars))], 2, function(x) {mean(is.na(x))})

#   - Add the criteria (best I can) to the TT data ####

#     - Add Greenhlagh features (see Greenhalgh et all p. 779 for the ranges used) ----


TT.tree.data = TT.tree.data %>% 
  mutate(
    gh_abnorm_temp = (V_TEMPERATURE > 39 | V_TEMPERATURE < 36.5),
    gh_abnorm_heart_rate = V_HEART_RATE > 110,
    gh_abnorm_resp_rate = V_RESPIRATORY_RATE > 25 & !V_VENT,
    gh_abnorm_plat_count = V_PLATELET_COUNT < 100,
    gh_abnorm_glucose = V_GLUCOSE > 200,
    Greenhalgh = rowSums(cbind(gh_abnorm_temp, gh_abnorm_heart_rate, gh_abnorm_resp_rate,
                               gh_abnorm_plat_count, gh_abnorm_glucose), na.rm = T) >= 3
  )

TT.tree.data %>% select(contains("gh_")) %>% cor(use = "pairwise") %>% round(2)

#     - Add SIRS features (see Greenhalgh et all p. 777 for the ranges used) ----
# Consider bad SIRS criteria - "The definition is so inclusive as to be meaningless."

TT.tree.data = TT.tree.data %>% 
  mutate(
    sirs_abnorm_tmp = (V_TEMPERATURE > 38 | V_TEMPERATURE < 36),
    sirs_abnorm_heart_rate =  V_HEART_RATE > 90,
    # supposed to be maintenance of PaCO2 > 32 - what counts as maintenance?:
    sirs_abnorm_resp_rate =  (!is.na(V_RESPIRATORY_RATE) & V_RESPIRATORY_RATE > 20) | (!is.na(V_PACO2) & V_PACO2 < 32),
    #note this also says "or left shift defined as > 10% of bands, what does that mean?:
    sirs_abnorm_wbc = (V_WHITE_BC < 4 | V_WHITE_BC > 12),
    SIRS = rowSums(cbind(sirs_abnorm_tmp, sirs_abnorm_heart_rate, sirs_abnorm_resp_rate,
                         sirs_abnorm_wbc), na.rm = T) >= 2
  )

TT.tree.data %>% select(contains("sirs_")) %>% cor(use = "pairwise") %>% round(2)

# How much missing data?
round(sapply( TT.tree.data %>% select(contains("abnorm")), function(x) { sum(is.na(x)) / nrow(TT.tree.data) } ), 3)

#     - Compare SIRS and Greenhalgh by Confusion Matrix -- Neither is good. LOTS of false positives ----

#How much missing data?
TT.tree.data %>% select(contains("abnorm_")) %>% colMeans(na.rm = T) %>% round(2)
#How many caes?
table(TT_per$n_blood) #70 patients out of 246 had any blood infection
#How many days satisfying criteria?
table(TT.tree.data$SIRS) 
table(TT.tree.data$Greenhalgh)

# Confusion matrix for different criteria 

# First blood infection day
TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0))  %>% 
  select(Greenhalgh, first_blood_l) %>% table() %>% prop.table(1) %>% round(3)

TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0))  %>% 
  select(SIRS, first_blood_l) %>% table() %>% prop.table(1) %>% round(3)

# Day before infection
TT.tree.data %>% filter(before_blood == TRUE | (blood_onset_tomorrow == TRUE | n_blood == 0))  %>% 
  select(Greenhalgh, blood_onset_tomorrow) %>% table() %>% prop.table(1) %>% round(3)

TT.tree.data %>% filter(before_blood == TRUE | (blood_onset_tomorrow == TRUE | n_blood == 0))  %>% 
  select(SIRS, blood_onset_tomorrow) %>% table() %>% prop.table(1) %>% round(3)

#Check abnorm temperature specifically
round(prop.table(table(TT.tree.data$sirs_abnorm_tmp, TT.tree.data$first_blood_l), 1), 3)
round(prop.table(table(TT.tree.data$gh_abnorm_temp, TT.tree.data$blood_onset_tomorrow), 1), 3)

# Any infection day
round(prop.table(table(TT.tree.data$Greenhalgh, TT.tree.data$Blood), 1), 2)
round(prop.table(table(TT.tree.data$SIRS, TT.tree.data$Blood), 1), 2)


#   - Helper functions ----
#     - cumsum2 -----
cumsum2 <- function(v) {cumsum(replace_na(v, 0))}
#     - tree_etc ----

# input data set as dataframe
# setting day_cutoff (with y_var "days_until_first_blood") will set a "TRUE" response to be that the individual will have a blood infection within [day_cutoff] days. For example, day_cutoff 1 means that day or the next.

tree_etc <- function(dataset, x_vars, y_var, days_before = NULL, minbucket = 10, cp = .01, xval = 200, weights = 99, maxsurrogate = 1, usesurrogate = 2) {
 
  if (is.null(days_before)) {
    dataset$response = dataset[[y_var]]
  } else {
    dataset$response = (dataset[[y_var]] <= days_before) & (dataset[[y_var]] >= 0)
    dataset$response[is.na(dataset$response)] = FALSE
  }

  tree = dataset %>% 
          dplyr::filter(before_blood != FALSE | (response == TRUE | n_blood == 0)) %>% 
          # can only detect blood infection if at least one of these tests was performed 
              # Only 2 entries have Blood infection reported when neither Blood CBC and Chemisty were performed )
          dplyr::filter(!is.na(V_TIME_PERFORMED_1) | !is.na(V_TIME_PERFORMED_2)) %>% 
          select(c(x_vars, "response")) %>%
          rpart(formula = response ~ . ,
                control = rpart.control(minbucket = minbucket, cp = cp, xval = xval, 
                                        usesurrogate = usesurrogate, maxsurrogate = maxsurrogate), 
                weights = weights*(response)+1,
                model = TRUE, y = TRUE) 
  
  cptable = tree$cptable
  plotcp(tree)
  
  cp_min.row = which.min(cptable[,which(colnames(cptable)=="xerror")])
  cp_min = cptable[cp_min.row,"CP"] - 1e-6
  pruned = prune(tree, cp = cp_min)
  
  plotcp(pruned)
  
  #     + Confusion-matrix related metrics ----
  pred <- prediction(predict(pruned, pruned$model), labels = pruned$y)
  tp_pct <- pred@tp[[1]] / (pred@tp[[1]] + pred@fp[[1]])
  
  #     + Roc curve ----
  plot(performance(pred, "tpr", "fpr"))
  abline(a = 0, b = 1, lty = 3)
  pruned_auc = performance(pred, "auc")@y.values[[1]] #auc
  cat("pruned_auc", pruned_auc)
  
  return(
    list(tree = tree,
         pruned_tree = pruned,
         pruned_pred = pred,
         pruned_tp_pct = tp_pct,
         pruned_auc = pruned_auc
    )
  )
  
}

#     - confustion_etc ----
confusion_etc <- function(prediction, quantiles) {
  lapply (quantiles, function(q) {
    tp <- as.numeric(quantile(prediction@predictions[[1]][prediction@labels[[1]]==TRUE|1], 1 - q))
    cm  <- table(prediction@predictions[[1]] >= tp, prediction@labels[[1]], dnn = c("Predicted", "Actual"))
    return(cm)
  })
  
}

#     - (ROC) plot_etc ----
plot_etc = function(fit1, fit2, fit3, main = "") {
  plot(performance(fit1$pruned_pred, "tpr", "fpr"), main = main)
  abline(a = 0, b = 1, lty = 3)
  points( performance(fit2$pruned_pred, "tpr", "fpr")@"x.values"[[1]],  
          performance(fit2$pruned_pred, "tpr", "fpr")@"y.values"[[1]], type = "l", col = "red")
  points( performance(fit3$pruned_pred, "tpr", "fpr")@"x.values"[[1]],  
          performance(fit3$pruned_pred, "tpr", "fpr")@"y.values"[[1]], type = "l", col = "blue")
  legend("bottomright", 
         legend = c(paste("day of; ", "AUC = ", round(fit1$pruned_auc, 2)),
                    paste("day before; ", "AUC = ", round(fit2$pruned_auc, 2)),
                    paste("period up to 2 days before; ", "AUC = ", round(fit3$pruned_auc, 2))),
         col = c(1,2,3), lty = 1)
}

# ------------------------------------------------------------------------------------------------------------------------------
# 1. Decision trees from Greenhalgh and SIRS variables SEPERATE ####
# ------------------------------------------------------------------------------------------------------------------------------
#  - Greenhalgh ----
#   Sometimes cutoffs are pretty similar, especially 39 as a temp cutoff 

xval1  = 5000 # Choose xval large enough so that xval xerror within xstd 

# Fit tree and pruned tree
par(mfrow = c(3,3))
gh_tree_first_blood = tree_etc(TT.tree.data,
                               x_vars = c(Greenhalgh_vars, "Greenhalgh"),
                               y_var = "first_blood_l",
                               xval = xval1, cp = .01, weights = 139) #table(gh_tree_first_blood$tree$model$response, useNA = "a")

gh_tree_day_before= tree_etc(TT.tree.data,
                             x_vars = c(Greenhalgh_vars, "Greenhalgh"),
                             y_var = "blood_onset_tomorrow",
                             xval = xval1, cp = .01, weights = 145) #table(gh_tree_day_before$tree$model$response, useNA = "a")

# gh_tree_multi_day = tree_etc(TT.tree.data,
#                         x_vars = c(Greenhalgh_vars, "Greenhalgh"), 
#                         days_before = 2,
#                         y_var = "days_until_first_blood",
#                         xval = xval1, cp = .01, weights = 47) #table(gh_tree_multi_day$tree$model$response, useNA = "a")

# Show tree plts
par(mfrow = c(1, 3))
gh_tree_first_blood$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)
gh_tree_day_before$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)
# interestingly close to SIRS cutoffs
#gh_tree_multi_day$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)



# Confusion matrices
confusion_etc(gh_tree_first_blood$pruned_pred, seq(0,1, .25))
confusion_etc(gh_tree_day_before$pruned_pred, seq(0,1, .25))
#confusion_etc(gh_tree_multi_day$pruned_pred, seq(0,1, .25))


#  - SIRS ----

# Fit tree and pruned tree
par(mfrow = c(3,3))
sirs_tree_first_blood = tree_etc(TT.tree.data, 
                                 x_vars = c(SIRS_vars, "SIRS"), 
                                 y_var = "first_blood_l",
                                 xval = xval1, cp = .01, weights = 139)

sirs_tree_day_before= tree_etc(TT.tree.data, 
                               x_vars = c(SIRS_vars, "SIRS"), 
                               y_var = "blood_onset_tomorrow",
                               xval = xval1, cp = .01, weights = 145)

# sirs_tree_multi_day = tree_etc(TT.tree.data,
#                              x_vars = c(SIRS_vars, "SIRS"), 
#                              days_before = 2,
#                              y_var = "days_until_first_blood",
#                              xval = xval1, cp = .01, weights = 47)

# Show tree plts
par(mfrow = c(1, 3))
sirs_tree_first_blood$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)
sirs_tree_day_before$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)
#sirs_tree_multi_day$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

# Confusion matrices
confusion_etc(sirs_tree_first_blood$pruned_pred, seq(0,1, .25))
confusion_etc(sirs_tree_day_before$pruned_pred, seq(0,1, .25))
#confusion_etc(sirs_tree_multi_day$pruned_pred, seq(0,1, .25))

# ------------------------------------------------------------------------------------------------------------------------------
# 2. Decision trees from Greenhalgh and Sirs variables COMBINED 
# ------------------------------------------------------------------------------------------------------------------------------

xval2 = 5000

par(mfrow = c(3,3))
ghsirs_tree_first_blood = tree_etc(TT.tree.data, 
                                   x_vars = unique(c(Greenhalgh_vars, SIRS_vars)),
                                   y_var = "first_blood_l", 
                                   xval = xval2, cp = .01, weights = 139) #table(ghsirs_tree_first_blood$tree$model$response, useNA = "a")

ghsirs_tree_first_blood$pruned_tree$cptable
ghsirs_tree_first_blood$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

ghsirs_tree_day_before= tree_etc(TT.tree.data, 
                                 x_vars = unique(c(Greenhalgh_vars, SIRS_vars)),
                                 y_var = "blood_onset_tomorrow", 
                                 xval = xval2, cp = .01, weights = 145)

# Show tree plts
par(mfrow = c(1, 3))
ghsirs_tree_first_blood$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)
ghsirs_tree_day_before$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

# Confusion matrices
confusion_etc(ghsirs_tree_first_blood$pruned_pred, seq(0,1, .25))
confusion_etc(ghsirs_tree_day_before$pruned_pred, seq(0,1, .25))


# ------------------------------------------------------------------------------------------------------------------------------
# 3. Add in demographic vars (age, gender, etc.) ----
# ------------------------------------------------------------------------------------------------------------------------------

xval3 = 5000

par(mfrow = c(3,3))
ghsirsplus_tree_first_blood = tree_etc(TT.tree.data, 
                                   x_vars = c(Greenhalgh_vars, "Greenhalgh", SIRS_vars, "SIRS",
                                              "Age", "Gender", "TBSA"),
                                   y_var = "first_blood_l", 
                                   xval = xval3, cp = .01, weights = 139)

ghsirsplus_tree_day_before= tree_etc(TT.tree.data, 
                                 x_vars = c(Greenhalgh_vars, "Greenhalgh", SIRS_vars, "SIRS",
                                            "Age", "Gender", "TBSA"),
                                 y_var = "blood_onset_tomorrow", 
                                 xval = xval3, cp = .01, weights = 145)
# Show tree plts
par(mfrow = c(1, 3))
ghsirsplus_tree_first_blood$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)
ghsirsplus_tree_day_before$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

# Confusion matrices
confusion_etc(ghsirsplus_tree_first_blood$pruned_pred, seq(0,1, .1))
confusion_etc(ghsirsplus_tree_day_before$pruned_pred, seq(0,1, .1))


# ------------------------------------------------------------------------------------------------------------------------------
# 4. All vars  ----

xval4 = 5000

all.var = c('V_PB_SYSTOLIC', 'V_BP_DIASTOLIC', 'V_MEANARTERIAL_PRESSURE', 'V_HEART_RATE', 'V_RESPIRATORY_RATE', 'V_TEMPERATURE',
            'V_CENTRAL_VENOUS_PRESSURE', 'V_GLASCOWCOMA_SCALE', 'V_WHITE_BC', 'V_HEMOGLOBIN', 'V_HEMATOCRIT', 'V_PLATELET_COUNT',
            'V_SODIUM', 'V_POTASSIUM', 'V_BLOOD_UREA_NITROGEN', 'V_CREATININE', 'V_GLUCOSE', 'V_BILIRUBIN', 'V_PAO2', 'V_FIO2',
            'V_PACO2', 'V_HCO3', 'V_PH', 'V_MODS_SCORE', 'V_VENT',
            'Age', 'Partial Thickness', 'Full Thickness', 'TBSA','Inhalation Injury','Treatment Group', 'Gender', 'Race', 'Ethnicity',
            'Transfusion Count', 'Volume All Transfusions', 'PRBC Trnsf Count', 'Volume PRBC', 'FFP Trnsf Count', 'Volume FFP', 
            'PDR','days_since_first_collection', "APACHE Score", "PDR" )  
            #'Volume Cryopreciptate', "'TBSA Group', 'ICU Days','Vent Days', "'Age Group', 'study outcome abbr.', 
            #''Last Exc-Graft Date', 'Operations Count', 'Platelet Trnsf Count', 'Volume Platelet', 'Cryoprecipitate Count',

par(mfrow = c(3,3))
all_tree_first_blood = tree_etc(TT.tree.data, 
                                       x_vars = all.var,
                                       y_var = "first_blood_l", 
                                       xval = xval4, cp = .01, weights =139)
all_tree_first_blood$pruned_tree$cptable
all_tree_first_blood$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)


all_tree_day_before= tree_etc(TT.tree.data, 
                                     x_vars = all.var,
                                     y_var = "blood_onset_tomorrow", 
                                     xval = xval4, cp = .01, weights = 145)

#TT.tree.data %>% 
#  dplyr::filter(before_blood != FALSE | (first_blood_l == TRUE | n_blood == 0)) %>% 
#  select(c(all.var[1:10], "first_blood_l")) %>%
#  rpart(formula = first_blood_l ~ . ,
#        control = rpart.control(minbucket = 10, cp = .01, xval = xval4, maxsurrogate = 1), 
#        weights = 139*(first_blood_l)+1,
#        model = TRUE, y = TRUE)  ->tmp
# tmp$cptable; prune(tmp, .02) %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

# Show tree plts
par(mfrow = c(1, 3))
all_tree_first_blood$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)
all_tree_day_before$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

# Confusion matrices
confusion_etc(all_tree_first_blood$pruned_pred, seq(0,1, .1))
confusion_etc(all_tree_day_before$pruned_pred, seq(0,1, .1))

# ------------------------------------------------------------------------------------------------------------------------------
# 5. Add in derived vars (cumulative days, etc) ----

# Add cumulative variables 
TT.tree.data = TT.tree.data %>% group_by(per_code_factor) %>% arrange(V_DATE_COLLECTION) %>%
  mutate(
    cum_gh_abnorm_temp = cumsum2(gh_abnorm_temp),
    cum_gh_abnorm_heart_rate = cumsum(gh_abnorm_heart_rate),
    cum_gh_abnorm_resp_rate = cumsum2(gh_abnorm_resp_rate),
    cum_gh_abnorm_plat_count = cumsum2(gh_abnorm_plat_count),
    cum_gh_abnorm_glucose = cumsum2(gh_abnorm_glucose),
    cum_abnorm_Greenhalgh = cumsum2(Greenhalgh)
  ) %>%
  ungroup()

TT.tree.data = TT.tree.data %>% group_by(per_code_factor) %>% arrange(V_DATE_COLLECTION) %>%
  mutate(
    cum_sirs_abnorm_temp = cumsum2(sirs_abnorm_tmp),
    cum_sirs_abnorm_heart_rate = cumsum(sirs_abnorm_heart_rate),
    cum_sirs_abnorm_resp_rate = cumsum2(sirs_abnorm_resp_rate),
    cum_sirs_abnorm_plat_count = cumsum2(sirs_abnorm_wbc),
    cum_abnorm_SIRS = cumsum2(SIRS)
  ) %>%
  ungroup()


xval5 = 5000
cumvars = names(TT.tree.data)[grep(names(TT.tree.data), pattern = "cum_")]

par(mfrow = c(3,3))
ghsirspluscum_tree_first_blood = tree_etc(TT.tree.data, 
                                       x_vars = c(Greenhalgh_vars, "Greenhalgh", SIRS_vars, "SIRS",
                                                  "Age", "Gender", "TBSA", cumvars),
                                       y_var = "first_blood_l", 
                                       xval = xval5, cp = .01, weights = 139)

ghsirspluscum_tree_day_before= tree_etc(TT.tree.data, 
                                     x_vars = c(Greenhalgh_vars, "Greenhalgh", SIRS_vars, "SIRS",
                                                "Age", "Gender", "TBSA", cumvars),
                                     y_var = "blood_onset_tomorrow", 
                                     xval = xval5, cp = .01, weights = 145)

# Show tree plts
par(mfrow = c(1, 3))
ghsirspluscum_tree_first_blood$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)
ghsirspluscum_tree_day_before$pruned_tree %>% rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

# Confusion matrices
confusion_etc(ghsirspluscum_tree_first_blood$pruned_pred, seq(0,1, .1))
confusion_etc(ghsirspluscum_tree_day_before$pruned_pred, seq(0,1, .1))


# -------------------------------------------------------- Overall summaries -----------------------------------------------------

# look at all auc values
do.call("rbind", sapply(ls(), function(x) {
  x = get(x)
  if (class(x) == "list" && class(x[[1]])=="rpart") {
    return(x$pruned_auc)
  }
}))

##################################################################################################################################
# # --------------------------------------------------      OTHER      -----------------------------------------------------------
# # Sub-populations -------------------------------------------
# 
# TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
#   select(vital_vars[-c(1:4)], "first_blood_l", "Gender", "Age", "TBSA", contains("cum")) %>%
#   filter(V_MODS_SCORE < 16) %>%
#   rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 15),
#         weights = 5*first_blood_l+1) %>%
#   rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)
# 
# TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
#   select(vital_vars[-c(1:4)], "first_blood_l", "Gender", "Age", "TBSA", contains("cum")) %>%
#   filter(V_MODS_SCORE < 16, Gender == "Female") %>%
#   rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 10),
#         weights = 2*first_blood_l+1) %>%
#   rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)
# 
# TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
#   select(vital_vars[-c(1:4)], "first_blood_l", "Gender", "Age", "TBSA", contains("cum")) %>%
#   filter(V_MODS_SCORE < 16, "study outcome abbr." != "Death") %>%
#   rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 10), weights = 5*first_blood_l+1) %>%
#   rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)
# 
# TT.tree.data  %>%
#   filter(Gender == "Female") %>%
#   select(vital_vars[-c(1:4)], "Blood", "Gender", "Age", "TBSA") %>%
#   rpart(formula = Blood ~ ., data = ., control = rpart.control(minbucket = 5)) %>% 
#   rpart.plot(., roundint = FALSE)
# 
# TT.tree.data  %>%
#   filter(TBSA >50, V_MODS_SCORE < 16) %>%
#   select(vital_vars[-c(1:4)], "Blood", "Gender", "Age", "TBSA") %>%
#   rpart(formula = Blood ~ ., data = ., control = rpart.control(minbucket = 10)) %>% 
#   rpart.plot(., roundint = FALSE)
# 
# 
# 
# # SIRS With training and testing set 
# #         +  day of ----
# TT.split = sample(1:nrow(TT.tree.data), size = nrow(TT.tree.data)/2)
# TT.train = TT.tree.data[TT.split,]
# TT.test = TT.tree.data[-TT.split,]
# 
# sirs_tree_first_blood_train = TT.train %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
#   select(SIRS_vars, "SIRS", "first_blood_l") %>%
#   rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 10, cp = .005, xval = 100), 
#         weights = 1*(first_blood_l)+1, model = TRUE, y = TRUE) 
# 
# rpart.plot(sirs_tree_first_blood_train, roundint = F, extra = 1)
# plotcp(sirs_tree_first_blood_train)
# 
# pred <- prediction(predict(sirs_tree_first_blood_train, sirs_tree_first_blood_train$model), labels = sirs_tree_first_blood_train$y)
# plot(performance(pred, "tpr", "fpr"))
# table(pred@predictions[[1]] > .005, sirs_tree_first_blood_train$y)
# 
# pred <- prediction(predict(sirs_tree_first_blood_train, TT.test), labels = TT.test$first_blood_l)
# plot(performance(pred, "tpr", "fpr"), add = T, col = "red", lty = 2)
# abline(a = 0, b = 1, lty = 3)
# table(pred@predictions[[1]] > .005, TT.test$first_blood_l)
# 
# #         +  day before ----
# 
# TT.split = sample(1:nrow(TT.tree.data), size = nrow(TT.tree.data)/2)
# TT.train = TT.tree.data[TT.split,]
# TT.test = TT.tree.data[-TT.split,]
# 
# sirs_tree_db_blood_train = TT.train %>% filter(before_blood == TRUE | (blood_onset_tomorrow == TRUE | n_blood == 0)) %>%
#   select(SIRS_vars, "SIRS", "blood_onset_tomorrow") %>%
#   rpart(formula = blood_onset_tomorrow ~ ., data = ., control = rpart.control(minbucket = 10, cp = .005, xval = 100), 
#         weights = 1*(blood_onset_tomorrow)+1, model = TRUE, y = TRUE) 
# 
# rpart.plot(sirs_tree_db_blood_train, roundint = F, extra = 1)
# plotcp(sirs_tree_db_blood_train)
# 
# pred <- prediction(predict(sirs_tree_db_blood_train, sirs_tree_db_blood_train$model), labels = sirs_tree_db_blood_train$y)
# plot(performance(pred, "tpr", "fpr"))
# table(pred@predictions[[1]] > .03, sirs_tree_db_blood_train$y)
# 
# pred <- prediction(predict(sirs_tree_db_blood_train, TT.test), labels = TT.test$blood_onset_tomorrow)
# plot(performance(pred, "tpr", "fpr"), add = T, col = "red", lty = 2)
# abline(a = 0, b = 1, lty = 3)
# table(pred@predictions[[1]] > .02, TT.test$blood_onset_tomorrow)
# 
# 
# #  All days compared to blood infection days ----
# TT.tree.data %>%
#   select("V_TEMPERATURE", "V_HEART_RATE", "V_RESPIRATORY_RATE", "V_RESPIRATORY_RATE", "V_WHITE_BC", "SIRS", "Blood") %>%
#   rpart(formula = Blood ~ ., data = ., control = rpart.control(minbucket = 10), 
#         weights = 1*(Blood)+1) %>%
#   rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)  
# # ------------------------------------------------------------------------------------------------------------------------------
# # Investigate heart rate by age ####################################################################################
# hr_age_day_of = filter(TT.tree.data, first_blood_l) %>% ggplot(aes(x = Age, y = V_HEART_RATE)) + geom_point() +
#   geom_smooth(method = "lm", se = FALSE, size = .2, linetype = 2) + 
#   ylim(0, 200)+
#   ggtitle("Heart rate vs. Age on day of first blood infection") + theme_bw()
# 
# hr_age_day_before = filter(TT.tree.data, as.logical(blood_onset_tomorrow)) %>% ggplot(aes(x = Age, y = V_HEART_RATE)) + geom_point() +
#   geom_smooth(method = "lm", se = FALSE, size = .2, linetype = 2) + 
#   ylim(0, 200) +
#   ggtitle("Heart rate vs. Age on day before first blood infection") + theme_bw()
#   
# hr_age_overall = TT.tree.data %>% ggplot(aes(x = Age, y = V_HEART_RATE)) + #geom_point(alpha = .2) +
#   geom_smooth(method = "lm", se = FALSE, size = .2, linetype = 2) + 
#   geom_hex(alpha = .3) +
#   ylim(0, 200) +
#   ggtitle("Heart rate vs. Age on all days") + theme_bw()
#   
# ggsave(plot_grid(hr_age_day_of, hr_age_day_before, hr_age_overall, nrow = 1), 
#        filename = "V_HEART_RATE_vs_Age.png", device = "png", height = 4, width = 16)
# 
# filter(TT.tree.data, first_blood_l) %>% lm(V_HEART_RATE ~ Age, data = .)
# filter(TT.tree.data, as.logical(blood_onset_tomorrow)) %>% lm(V_HEART_RATE ~ Age, data = .)
# TT.tree.data %>% lm(V_HEART_RATE ~ Age, data = .)
# 
# # Investigate respiratory rate cutoff ####################################################################################
# resp_day_of = filter(TT.tree.data, before_blood | first_blood_l) %>% select(V_RESPIRATORY_RATE, first_blood_l) %>%
#              mutate(resp_rate_nearest_5 = round(V_RESPIRATORY_RATE*2, -1)/2) %>%
#              group_by(resp_rate_nearest_5) %>%
#              summarize(first_inf_rate = mean(first_blood_l), n_obs = n()) %>% 
#              ggplot(aes(x = resp_rate_nearest_5, y = first_inf_rate, size = n_obs)) + 
#              geom_vline(xintercept = 25, color = "gray", linetype = 2) +
#              geom_point(alpha = .5, color = "red") + 
#              theme_bw() + 
#              theme(plot.title = element_text(size = 10)) +
#              ggtitle("Infection rate vs. V_RESPIRATORY_RATE for days up to and including day of first blood infection")
#              
# resp_day_before = filter(TT.tree.data, before_blood | first_blood_l) %>% select(V_RESPIRATORY_RATE, blood_onset_tomorrow) %>%
#              mutate(resp_rate_nearest_5 = round(V_RESPIRATORY_RATE*2, -1)/2) %>%
#              group_by(resp_rate_nearest_5) %>%
#              summarize(tomorrow_first_inf_rate = mean(blood_onset_tomorrow), n_obs = n()) %>% 
#              ggplot(aes(x = resp_rate_nearest_5, y = tomorrow_first_inf_rate, size = n_obs)) + 
#              geom_vline(xintercept = 25, color = "gray", linetype = 2) +
#              geom_point(alpha = .5, color = "red") +
#              theme_bw() + 
#              theme(plot.title = element_text(size = 10)) +
#              ggtitle("Infection rate vs. V_RESPIRATORY_RATE for days up to and including day before first blood infection")
# 
# # Compare that to temperature
# temp_day_of = filter(TT.tree.data, before_blood | first_blood_l) %>% select(V_TEMPERATURE, first_blood_l) %>%
#              mutate(temp_nearest_half_deg = round(V_TEMPERATURE*2)/2) %>%
#              group_by(temp_nearest_half_deg) %>%
#              summarize(first_inf_rate = mean(first_blood_l), n_obs = n()) %>% 
#              ggplot(aes(x = temp_nearest_half_deg, y = first_inf_rate, size = n_obs)) + 
#              geom_vline(xintercept = c(36.5,39), color = "gray", linetype = 2) +
#              geom_point(alpha = .5) + 
#              theme_bw() + 
#              theme(plot.title = element_text(size = 10)) +
#              ggtitle("Infection rate vs. V_TEMPERATURE for days up to and including day of first blood infection")
#              
# 
# temp_day_before = filter(TT.tree.data, before_blood) %>% select(V_TEMPERATURE, blood_onset_tomorrow) %>%
#              mutate(temp_nearest_half_deg = round(V_TEMPERATURE*2)/2) %>%
#              group_by(temp_nearest_half_deg) %>%
#              summarize(tomorrow_first_inf_rate = mean(blood_onset_tomorrow), n_obs = n()) %>% 
#              ggplot(aes(x = temp_nearest_half_deg, y = tomorrow_first_inf_rate, size = n_obs)) + 
#              geom_vline(xintercept = c(36.5,39), color = "gray", linetype = 2) +
#              geom_point(alpha = .5) +
#              theme_bw() + 
#              theme(plot.title = element_text(size = 10)) +
#              ggtitle("Infection rate vs. V_TEMPERATURE for days up to and including day before first blood infection")
# 
# ggsave(plot_grid(resp_day_of, resp_day_before, temp_day_of, temp_day_before, nrow = 2),
#        filename = "RESP_and_TEMP.png", device = "png", height = 8, width = 16)
# 
# # ------------------------------------------------------------------------------------------------------------------------------
# # Pairs plot (have to filter down the variables to make this useful)----
# 
# library(GGally)
# 
# data1 = filter(TT, PER_CODE == person_to_look_at) %>% 
#   select(c(vital_vars[c(5,6,10,16,17,20,21,25)], "Blood", "Any", "Urine", "Pneumonia", "Wound", "study outcome abbr.")) %>%
#   select(-c("V_DATE_COLLECTION", "V_TIME_PERFORMED_1", "V_TIME_PERFORMED_2", "V_TIME_PERFORMED_3",  "study outcome abbr.")) #%>%
#   #select(-one_of("V_ER_DEF_DATE_01","V_TIME_PERFORMED")) %>%
#   #melt(id=c("Blood", "Any", "Urine", "Pneumonia", "Wound"))
# 
# ggpairs(data1, aes(color = Blood), cardinality_threshold = 24)
# 
# # ------------------------------------------------------------------------------------------------------------------------------
# # All of one variable in plotly ----
# 
# # Where you do things affects performance!
# # This work but is very slow. Can we make it faster with more pre-computation?
# 
# var1 = "V_MODS_SCORE"
# 
# TT.shared = SharedData$new(
#   data = TT %>%
#     select(c(vital_vars, "Blood_test_performed", "Blood", "Any", "Urine", "Pneumonia", "Wound", "study outcome abbr.",
#              "per_code_factor", "days_since_first_collection")) %>%
#     #select(-one_of("V_ER_DEF_DATE_01","V_TIME_PERFORMED")) %>%
#     melt(id=c("V_DATE_COLLECTION", "V_TIME_PERFORMED_1", "V_TIME_PERFORMED_2", "V_TIME_PERFORMED_3",
#               "Blood_test_performed", "Blood", "Any", "Urine", "Pneumonia", "Wound", "study outcome abbr.",
#               "per_code_factor", "days_since_first_collection")) %>%
#     filter(variable == var1),
#    ~per_code_factor
# )  
# 
# p <- plot_ly(TT.shared, x = ~days_since_first_collection, y=~value) %>%
#   group_by(per_code_factor) %>%
#   add_lines(text = ~paste(per_code_factor,value), hoverinfo = "text", size = .3, sizes = c(.3,.3)) %>%
#   highlight(on = "plotly_click", persistent = FALSE, selectize = TRUE)
# 
# p %>% add_fun(function(p) {
#   p %>% filter(Blood) %>%
#     add_markers(x = ~days_since_first_collection, y=~value, color = "black") %>%
#     highlight(color = "red", opacityDim = .1) %>% rangeslider()
# })
# 
