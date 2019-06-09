# This is a scratch space for stuff I'll move (if valuable) to burn_data_EDA.Rmd (Where the data sets used here were creatd)

# Summary of project goals as of 6/7/19 (in order to be completed):

# 1. Compare best models derived from Greenhalgh and Sirs variables (separately) to current Greenhalgh and SIRS criteria. The models may be simple cutoffs, logistic regression, and/or decision trees. 
# 2. Same as above but combining Greenhalgh and SIRS variables.
# 3. Consider demographic variables, e.g. age, gender and interactions, but still limited to single time point data.
# 4. (If time) Consider derived variables like cumulative days above a threshold. This cant be implemented currently, but could help argue for expanded capabilities with medical records. 

# ------------------------------------------------------------------------------------------------------------------------------
# 0. Setup ####

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

ndigits = 2
cumsum2 <- function(v) {cumsum(replace_na(v, 0))}


#   - load DATA ####

TT <- read.csv("TT_jane.csv", stringsAsFactors = F, check.names = F)[,-1]
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
                          
TT_Demo1 = read.csv("TT_Demo1.csv")[,-1]


#add in V_VENT variable
TT_ABA_2 = spreadsheets[["ABA_TT_Daily_Collection_Part_2.xlsx"]][[1]] #I got this later spreadsheet, see email from Sandy 6-7-19
TT = left_join(TT, TT_ABA_2 %>% select(PER_CODE, V_DATE_COLLECTION, V_VENT) %>% 
                 mutate(V_DATE_COLLECTION = as.character(V_DATE_COLLECTION))) %>%
                 mutate(V_VENT = ifelse(V_VENT == "No", FALSE, TRUE))
# Node respiratory rates higher on average for ventilated people
TT %>% select(V_RESPIRATORY_RATE,V_VENT) %>% ggplot() + geom_histogram(aes(x = V_RESPIRATORY_RATE, group = V_VENT, fill = V_VENT), position = "dodge")

# ------------------------------------------------------------------------------------------------------------------------------
# 1. Compare best models derived from Greenhalgh and Sirs variables ####
#   - Add the criteria (best I can) to the TT data ####

#   - Add Greenhlagh features (see Greenhalgh et all p. 779 for the ranges used) ----
Greenhalgh_vars = c("V_TEMPERATURE", "V_HEART_RATE", "V_RESPIRATORY_RATE",  "V_PLATELET_COUNT", "V_GLUCOSE")

TT = TT %>% 
       mutate(
        gh_abnorm_temp = (V_TEMPERATURE > 39 | V_TEMPERATURE < 36.5),
        gh_abnorm_heart_rate = V_HEART_RATE > 110,
        gh_abnorm_resp_rate = V_RESPIRATORY_RATE > 25,
        gh_abnorm_plat_count = V_PLATELET_COUNT < 100,
        gh_abnorm_glucose = V_GLUCOSE > 200,
        Greenhalgh = rowSums(cbind(gh_abnorm_temp, gh_abnorm_heart_rate, gh_abnorm_resp_rate,
                                   gh_abnorm_plat_count, gh_abnorm_glucose), na.rm = T) >= 3
       )


#   - Add SIRS features (see Greenhalgh et all p. 777 for the ranges used) ----
# Consider bad SIRS criteria - "The definition is so inclusive as to be meaningless."

SIRS_vars = c("V_TEMPERATURE", "V_HEART_RATE", "V_RESPIRATORY_RATE",  "V_WHITE_BC", "V_PACO2")

TT = TT %>% 
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

# How much missing data?
round(sapply( TT %>% select(contains("abnorm")), function(x) { sum(is.na(x)) / nrow(TT) } ), 3)

# Table Compare SIRS and Greenhalgh ----
TT.tree.data %>% select(contains("abnorm_")) %>% select(-contains("cum")) %>% colMeans(na.rm = T) %>% round(2)
table(TT.tree.data$SIRS)
table(TT.tree.data$Greenhalgh)

# First infection day
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
round(prop.table(table(TT.tree.data$abnorm_temp, TT.tree.data$first_blood_l), 1), 3)
round(prop.table(table(TT.tree.data$abnorm_temp, TT.tree.data$blood_onset_tomorrow), 1), 3)

# Any infection day
round(prop.table(table(TT.tree.data$Greenhalgh, TT.tree.data$Blood), 1), 2)

round(prop.table(table(TT.tree.data$SIRS, TT.tree.data$Blood), 1), 2)

####################################################################################################################
# Decision trees, ROC curves ###################################################################################
####################################################################################################################
# Setup ####

TT.tree.data = TT %>% group_by(per_code_factor) %>% arrange(V_DATE_COLLECTION) %>%
    mutate(
         cum_gh_abnorm_temp = cumsum2(abnorm_temp),
         cum_gh_abnorm_heart_rate = cumsum(abnorm_heart_rate),
         cum_gh_abnorm_resp_rate = cumsum2(abnorm_resp_rate),
         cum_gh_abnorm_plat_count = cumsum2(abnorm_plat_count),
         cum_gh_abnorm_glucose = cumsum2(abnorm_glucose),
         cum_abnorm_Greenhalgh = cumsum2(Greenhalgh)
         ) %>%
  ungroup()

TT.tree.data = TT %>% group_by(per_code_factor) %>% arrange(V_DATE_COLLECTION) %>%
  mutate(
    cum_sirs_abnorm_temp = cumsum2(sirs_abnorm_tmp),
    cum_sirs_abnorm_heart_rate = cumsum(sirs_abnorm_heart_rate),
    cum_sirs_abnorm_resp_rate = cumsum2(sirs_abnorm_resp_rate),
    cum_sirs_abnorm_plat_count = cumsum2(sirs_abnorm_wbc),
    cum_abnorm_SIRS = cumsum2(SIRS)
  ) %>%
  ungroup()

# Decision tree with Greenhalgh variables. Sometimes cutoffs are pretty similar, especially 39 as a temp cutoff ----

TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
        select(Greenhalgh_vars, "V_WHITE_BC", #<- bc not in but in SIRS so I kept it
               "Greenhalgh", "first_blood_l") %>%
        rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 100), 
              weights = 20*(first_blood_l)+1) %>% 
        rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

TT.tree.data %>% filter(before_blood == TRUE | (blood_onset_tomorrow == TRUE | n_blood == 0)) %>%
        select(Greenhalgh_vars, "V_WHITE_BC", #<- bc not in but in SIRS so I kept it
                "Greenhalgh", "blood_onset_tomorrow") %>%
        rpart(formula = blood_onset_tomorrow ~ ., data = ., control = rpart.control(minbucket = 100), 
              weights = 20*(blood_onset_tomorrow)+1) %>% 
        rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

#   + With training and testing set ----
TT.split = sample(1:nrow(TT.tree.data), size = nrow(TT.tree.data)/2)
TT.train = TT.tree.data[TT.split,]
TT.test = TT.tree.data[-TT.split,]

TT.tree.train = TT.train %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
        select(Greenhalgh_vars, "V_WHITE_BC","Greenhalgh", "first_blood_l") %>%
        rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 100), weights = 10*(first_blood_l)+1) 
rpart.plot(TT.tree.train, roundint = F, extra = 1)

pred <- prediction(predict(TT.tree.train, TT.train), labels = TT.train$first_blood_l)
plot(performance(pred, "tpr", "fpr"))
pred <- prediction(predict(TT.tree.train, TT.test), labels = TT.test$first_blood_l)
plot(performance(pred, "tpr", "fpr"), add = T, col = "red", lty = 2)

# Decision tree with SIRS variables. ----

#Only days of and before first blood infection
TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
        select(SIRS_vars, "SIRS", "first_blood_l") %>%
        rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 100), 
              weights = 20*(first_blood_l)+1) %>% 
        rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

#Only days before first blood infection - response is day before infection
TT.tree.data %>% filter(before_blood == TRUE | (blood_onset_tomorrow == TRUE | n_blood == 0)) %>%
        select(SIRS_vars, "SIRS", "blood_onset_tomorrow") %>%
        rpart(formula = blood_onset_tomorrow ~ ., data = ., control = rpart.control(minbucket = 100), 
              weights = 20*(blood_onset_tomorrow)+1) %>% 
        rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)


#All days compared to blood infection days
TT.tree.data %>%
        select("V_TEMPERATURE", "V_HEART_RATE", "V_RESPIRATORY_RATE", "V_RESPIRATORY_RATE", "V_WHITE_BC", "SIRS", "Blood") %>%
        rpart(formula = Blood ~ ., data = ., control = rpart.control(minbucket = 10), 
              weights = 1*(Blood)+1) %>%
        rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)  


 
# Decision tree with all variables (test error worse) ----
#   Day of first blood infection vs. not, filtering out days after ----

#   + Whole population ----

TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
        select(vital_vars[-c(1:4)], "first_blood_l", "Gender", "Age", "TBSA", contains("cum"), "days_since_first_collection") %>%
        rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 20), 
              weights = 5*(first_blood_l)+1, parms = "information") %>%
        rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)


TT.tree = TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
        select(vital_vars[-c(1:4)], "first_blood_l", "Gender", "Age", "TBSA", contains("cum"), "days_since_first_collection") %>%
        rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 20), 
              weights = 5*(first_blood_l)+1, parms = "information")

rpart.plot(TT.tree, roundint = F)

#       Roc curve ----
pred <- prediction(predict(TT.tree), labels = TT.tree$y)
plot(performance(pred, "tpr", "fpr"))
abline()

#   + With training and testing set ----
# Note range of outcomes due to random split
TT.split = sample(1:nrow(TT.tree.data), size = nrow(TT.tree.data)/2)
TT.train = TT.tree.data[TT.split,]
TT.test = TT.tree.data[-TT.split,]

TT.tree.train = TT.train %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
        select(vital_vars[-c(1:4)], "first_blood_l", "Gender", "Age", "TBSA", contains("cum"), "days_since_first_collection") %>%
        rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 200), 
              weights = 10*(first_blood_l)+1, parms = "information")

rpart.plot(TT.tree.train)

pred <- prediction(predict(TT.tree.train, TT.train), labels = TT.train$first_blood_l)
plot(performance(pred, "tpr", "fpr"))
pred <- prediction(predict(TT.tree.train, TT.test), labels = TT.test$first_blood_l)
plot(performance(pred, "tpr", "fpr"), add = T, col = "red", lty = 2)

#   + Sub-populations ----

TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
        select(vital_vars[-c(1:4)], "first_blood_l", "Gender", "Age", "TBSA", contains("cum")) %>%
        filter(V_MODS_SCORE < 16) %>%
        rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 15),
              weights = 5*first_blood_l+1) %>%
        rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
        select(vital_vars[-c(1:4)], "first_blood_l", "Gender", "Age", "TBSA", contains("cum")) %>%
        filter(V_MODS_SCORE < 16, Gender == "Female") %>%
        rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 10),
              weights = 2*first_blood_l+1) %>%
        rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

TT.tree.data %>% filter(before_blood == TRUE | (first_blood_l == TRUE | n_blood == 0)) %>%
        select(vital_vars[-c(1:4)], "first_blood_l", "Gender", "Age", "TBSA", contains("cum")) %>%
        filter(V_MODS_SCORE < 16, "study outcome abbr." != "Death") %>%
        rpart(formula = first_blood_l ~ ., data = ., control = rpart.control(minbucket = 10), weights = 5*first_blood_l+1) %>%
        rpart.plot(., roundint = FALSE, digits = ndigits, extra = 1)

#       Day of blood infection vs. not (less valuable, includes days after first infection) ----
#   + Whole population ----

TT.tree.data  %>%
  select(vital_vars[-c(1:4)], "Blood", "Gender", "Age", "TBSA", contains("cum")) %>%
  rpart(formula = Blood ~ ., data = ., control = rpart.control(minbucket = 5)) %>% 
  rpart.plot()

#   + Sub-populations ----

TT.tree.data  %>%
  filter(Gender == "Female") %>%
  select(vital_vars[-c(1:4)], "Blood", "Gender", "Age", "TBSA") %>%
  rpart(formula = Blood ~ ., data = ., control = rpart.control(minbucket = 5)) %>% 
  rpart.plot(., roundint = FALSE)

TT.tree.data  %>%
  filter(TBSA >50, V_MODS_SCORE < 16) %>%
  select(vital_vars[-c(1:4)], "Blood", "Gender", "Age", "TBSA") %>%
  rpart(formula = Blood ~ ., data = ., control = rpart.control(minbucket = 10)) %>% 
  rpart.plot(., roundint = FALSE)


####################################################################################################################
# Investigate heart rate by age ####################################################################################
hr_age_day_of = filter(TT.tree.data, first_blood_l) %>% ggplot(aes(x = Age, y = V_HEART_RATE)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = .2, linetype = 2) + 
  ylim(0, 200)+
  ggtitle("Heart rate vs. Age on day of first blood infection") + theme_bw()

hr_age_day_before = filter(TT.tree.data, as.logical(blood_onset_tomorrow)) %>% ggplot(aes(x = Age, y = V_HEART_RATE)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = .2, linetype = 2) + 
  ylim(0, 200) +
  ggtitle("Heart rate vs. Age on day before first blood infection") + theme_bw()
  
hr_age_overall = TT.tree.data %>% ggplot(aes(x = Age, y = V_HEART_RATE)) + #geom_point(alpha = .2) +
  geom_smooth(method = "lm", se = FALSE, size = .2, linetype = 2) + 
  geom_hex(alpha = .3) +
  ylim(0, 200) +
  ggtitle("Heart rate vs. Age on all days") + theme_bw()
  
ggsave(plot_grid(hr_age_day_of, hr_age_day_before, hr_age_overall, nrow = 1), 
       filename = "V_HEART_RATE_vs_Age.png", device = "png", height = 4, width = 16)

filter(TT.tree.data, first_blood_l) %>% lm(V_HEART_RATE ~ Age, data = .)
filter(TT.tree.data, as.logical(blood_onset_tomorrow)) %>% lm(V_HEART_RATE ~ Age, data = .)
TT.tree.data %>% lm(V_HEART_RATE ~ Age, data = .)

# Investigate respiratory rate cutoff ####################################################################################
resp_day_of = filter(TT.tree.data, before_blood | first_blood_l) %>% select(V_RESPIRATORY_RATE, first_blood_l) %>%
             mutate(resp_rate_nearest_5 = round(V_RESPIRATORY_RATE*2, -1)/2) %>%
             group_by(resp_rate_nearest_5) %>%
             summarize(first_inf_rate = mean(first_blood_l), n_obs = n()) %>% 
             ggplot(aes(x = resp_rate_nearest_5, y = first_inf_rate, size = n_obs)) + 
             geom_vline(xintercept = 25, color = "gray", linetype = 2) +
             geom_point(alpha = .5, color = "red") + 
             theme_bw() + 
             theme(plot.title = element_text(size = 10)) +
             ggtitle("Infection rate vs. V_RESPIRATORY_RATE for days up to and including day of first blood infection")
             
resp_day_before = filter(TT.tree.data, before_blood | first_blood_l) %>% select(V_RESPIRATORY_RATE, blood_onset_tomorrow) %>%
             mutate(resp_rate_nearest_5 = round(V_RESPIRATORY_RATE*2, -1)/2) %>%
             group_by(resp_rate_nearest_5) %>%
             summarize(tomorrow_first_inf_rate = mean(blood_onset_tomorrow), n_obs = n()) %>% 
             ggplot(aes(x = resp_rate_nearest_5, y = tomorrow_first_inf_rate, size = n_obs)) + 
             geom_vline(xintercept = 25, color = "gray", linetype = 2) +
             geom_point(alpha = .5, color = "red") +
             theme_bw() + 
             theme(plot.title = element_text(size = 10)) +
             ggtitle("Infection rate vs. V_RESPIRATORY_RATE for days up to and including day before first blood infection")

# Compare that to temperature
temp_day_of = filter(TT.tree.data, before_blood | first_blood_l) %>% select(V_TEMPERATURE, first_blood_l) %>%
             mutate(temp_nearest_half_deg = round(V_TEMPERATURE*2)/2) %>%
             group_by(temp_nearest_half_deg) %>%
             summarize(first_inf_rate = mean(first_blood_l), n_obs = n()) %>% 
             ggplot(aes(x = temp_nearest_half_deg, y = first_inf_rate, size = n_obs)) + 
             geom_vline(xintercept = c(36.5,39), color = "gray", linetype = 2) +
             geom_point(alpha = .5) + 
             theme_bw() + 
             theme(plot.title = element_text(size = 10)) +
             ggtitle("Infection rate vs. V_TEMPERATURE for days up to and including day of first blood infection")
             

temp_day_before = filter(TT.tree.data, before_blood) %>% select(V_TEMPERATURE, blood_onset_tomorrow) %>%
             mutate(temp_nearest_half_deg = round(V_TEMPERATURE*2)/2) %>%
             group_by(temp_nearest_half_deg) %>%
             summarize(tomorrow_first_inf_rate = mean(blood_onset_tomorrow), n_obs = n()) %>% 
             ggplot(aes(x = temp_nearest_half_deg, y = tomorrow_first_inf_rate, size = n_obs)) + 
             geom_vline(xintercept = c(36.5,39), color = "gray", linetype = 2) +
             geom_point(alpha = .5) +
             theme_bw() + 
             theme(plot.title = element_text(size = 10)) +
             ggtitle("Infection rate vs. V_TEMPERATURE for days up to and including day before first blood infection")

ggsave(plot_grid(resp_day_of, resp_day_before, temp_day_of, temp_day_before, nrow = 2),
       filename = "RESP_and_TEMP.png", device = "png", height = 8, width = 16)

####################################################################################################################
# Pairs plot (have to filter down the variables to make this useful)----

library(GGally)

data1 = filter(TT, PER_CODE == person_to_look_at) %>% 
  select(c(vital_vars[c(5,6,10,16,17,20,21,25)], "Blood", "Any", "Urine", "Pneumonia", "Wound", "study outcome abbr.")) %>%
  select(-c("V_DATE_COLLECTION", "V_TIME_PERFORMED_1", "V_TIME_PERFORMED_2", "V_TIME_PERFORMED_3",  "study outcome abbr.")) #%>%
  #select(-one_of("V_ER_DEF_DATE_01","V_TIME_PERFORMED")) %>%
  #melt(id=c("Blood", "Any", "Urine", "Pneumonia", "Wound"))

ggpairs(data1, aes(color = Blood), cardinality_threshold = 24)

####################################################################################################################
# All of one variable in plotly ----

# Where you do things affects performance!
# This work but is very slow. Can we make it faster with more pre-computation?

var1 = "V_MODS_SCORE"

TT.shared = SharedData$new(
  data = TT %>%
    select(c(vital_vars, "Blood_test_performed", "Blood", "Any", "Urine", "Pneumonia", "Wound", "study outcome abbr.",
             "per_code_factor", "days_since_first_collection")) %>%
    #select(-one_of("V_ER_DEF_DATE_01","V_TIME_PERFORMED")) %>%
    melt(id=c("V_DATE_COLLECTION", "V_TIME_PERFORMED_1", "V_TIME_PERFORMED_2", "V_TIME_PERFORMED_3",
              "Blood_test_performed", "Blood", "Any", "Urine", "Pneumonia", "Wound", "study outcome abbr.",
              "per_code_factor", "days_since_first_collection")) %>%
    filter(variable == var1),
   ~per_code_factor
)  

p <- plot_ly(TT.shared, x = ~days_since_first_collection, y=~value) %>%
  group_by(per_code_factor) %>%
  add_lines(text = ~paste(per_code_factor,value), hoverinfo = "text", size = .3, sizes = c(.3,.3)) %>%
  highlight(on = "plotly_click", persistent = FALSE, selectize = TRUE)

p %>% add_fun(function(p) {
  p %>% filter(Blood) %>%
    add_markers(x = ~days_since_first_collection, y=~value, color = "black") %>%
    highlight(color = "red", opacityDim = .1) %>% rangeslider()
})

