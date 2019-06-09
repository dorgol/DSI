# EDA of burn data -> moved to burn_data_EDA.Rmd
# Jane Carlen

#A Randomized Clinical Trial of Restrictive vs. Traditional Blood 
#Transfusion Practices in Burn Patients

# To compare the outcome of a restrictive blood transfusion policy
#(i.e. maintaining a hemoglobin level 7-8g/dL) vs. a traditional transfusion policy (maintaining hemoglobin 10-11 g/dL) 
# on the 
#    - incidence of blood stream infection
#    - hospital length of stay
#    - duration of mechanical ventilation, 
#    - mortality
#    - wound healing after major burn injury in adults.

# where is treatment type?

# 0. Setup ####
library(readxl)
library(dplyr)
library(ggplot2)

setwd("~/Documents/DSI/Burn/Data")

files = list.files(pattern = "xlsx")

sheets = sapply(files, excel_sheets)

spreadsheets = lapply(files, function(x) {
  sheets = sapply(x, excel_sheets)
  lapply(sheets, read_xlsx, path = x)
})
names(spreadsheets) = files

# 1. Exploratory ####

## Visualize Infections ####
Infection = spreadsheets[["Infection.xlsx"]][[1]]
# STUDY_NUMER same for all entries
# PER_CODE == PATPROT_PATSTDID # both patient ID
Infection = Infection %>% arrange(PER_CODE, V_DATE_COLLECTION)
Infection = Infection %>% mutate(nstudy = as.numeric(as.factor(PER_CODE)),
                                 nstudylabel = ifelse(duplicated(nstudy), NA, nstudy),
                                 sizeOnset = 1 + as.numeric(Onset=="Yes"),
                                 sizeAny = 1 + as.numeric(Any=="Yes"),
                                 Onset = as.factor(Onset),
                                 index = 1:nrow(Infection)) %>%arrange(PER_CODE, desc(V_DATE_COLLECTION))
  
ggplot(Infection, aes(x = index, y = V_DATE_COLLECTION, shape = Any, color = Onset)) +
  geom_point(aes(size = sizeAny)) +
  scale_size(range = c(1,3), guide = FALSE) +
  scale_shape_manual(values = c("Yes"=16, "No"=4)) +
  geom_text(aes(label=nstudylabel), color = "black", hjust=0, vjust=1, size = 3)

Infection_per = Infection %>% group_by(PER_CODE) %>%
  summarize(n_any = sum(Any=="Yes"),
            n_onset = sum(Onset=="Yes"),
            n_collect = n(),
            n_any_per = ifelse(n_collect ==0, 0, n_any/n_collect),
            n_onset_per = ifelse(n_collect ==0, 0, n_onset/n_collect))

## Characteristics ####

ABA = spreadsheets[["ABA_TT_Daily_Collection_Part_1.xlsx"]][[1]]
PCR = spreadsheets[["Copy of PCR DATA SET 03-19-2018_SEPSIS.xlsx"]]
# It seems like the control vs. treatment var is V_RANDOM_GROUP
# based on "PCR_SEPSIS_DICTIONARY.xlsx" is the data dictionary. 


Results = left_join(PCR, Infection_per, by = "PER_CODE")
ggplot(Results, aes(x = V_HEMOGLOBIN, y = n_any)) +
  geom_point()





# 2. Questions

#How is this possible?
sum(Infection$Onset=="Yes" & Infection$Any=="No") #=47
sum(Infection$Onset=="No" & Infection$Any=="Yes") #=55
