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


#     - cumsum2 -----
cumsum2 <- function(v) {cumsum(replace_na(v, 0))}
#     - tree_etc ----

# input data set as dataframe
# setting day_cutoff (with y_var "days_until_first_blood") will set a "TRUE" response to be that the individual will have a blood infection within [day_cutoff] days. For example, day_cutoff 1 means that day or the next.
# rel.freq = how frequently positive cases should occur relative to negative -> weights for positive cases adjusted accordingly. If null, no weights.

tree_etc <- function(dataset, x_vars, y_var, days_before = NULL, minbucket = 10, cp = .01, xval = 200,
                     rel.freq = 1, maxsurrogate = 2, usesurrogate = 2) {
  
  if (is.null(days_before)) {
    dataset$response = dataset[[y_var]]
  } else {
    dataset$response = (dataset[[y_var]] <= days_before) & (dataset[[y_var]] >= 0)
    dataset$response[is.na(dataset$response)] = FALSE
  }
  
  tree.data = dataset %>% 
    dplyr::filter(before_blood != FALSE | (response == TRUE | n_blood == 0)) %>% 
    # can only detect blood infection if at least one of these tests was performed  <- Sandy said not to use this filter
    # Only 2 entries have Blood infection reported when neither Blood CBC and Chemisty were performed )
    #dplyr::filter(!is.na(V_TIME_PERFORMED_1) | !is.na(V_TIME_PERFORMED_2)) %>%
    select(c(x_vars, "response"))
  
  weight = ifelse(is.null(rel.freq), 0, rel.freq/(1-mean(!tree.data$response)))
  
  tree = rpart(tree.data,
               formula = response ~ . ,
               control = rpart.control(minbucket = minbucket, cp = cp, xval = xval, 
                                       usesurrogate = usesurrogate, maxsurrogate = maxsurrogate), 
               weights = weight*(response)+1,
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
  cat("pruned_auc", pruned_auc, "\n")
  
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
          performance(fit3$pruned_pred, "tpr", "fpr")@"y.values"[[1]], type = "l", col = "red")
  legend("bottomright", 
         legend = c(paste("day of; ", "AUC = ", round(fit1$pruned_auc, 2)),
                    paste("day before; ", "AUC = ", round(fit2$pruned_auc, 2)),
                    paste("day of or before; ", "AUC = ", round(fit3$pruned_auc, 2))),
         col = c(1,2,3), lty = 1)
}

#     - tree_options ----
tree_options = expand.grid( maxsurrogate = c(0, 2, 5),
                            rel.freq= c(1, .75, .5, .25, NULL),
                            response = c("first_blood_l", "blood_onset_tomorrow", "first_or_tomorrow"),
                            xval = 5000,
                            stringsAsFactors = FALSE
)