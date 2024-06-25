###########################################################################
#                                                                         #
# Author: Fernando Guntoro
# Project: CHD in the UKB
# Update: 19/06/2024
#                                                                         #
###########################################################################


###########################################################################
#                                                                         #
# Set up R environment ---------------------------------------------------#
#                                                                         #
###########################################################################

library(dplyr)
library(stringi)
#source("./functions.R")
#source("../outcome_definition/Scripts/functions.R")

# Create relevant directory
ifelse(dir.exists("../Data/"), FALSE, dir.create("../Data/"))

# data_path = "~/CHD/extraction_and_recoding/outputs/ukb_recoded.rds"
# withdraw_path = "/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_673609/withdraw69328_136_20231013.txt"
# covar_dict_path = "~/CHD/column_dictionary.csv"

clean_data <- function(data_path, withdraw_path, covar_dict_path) {
  
  ###########################################################################
  #                                                                         #
  # Load & basic data cleaning ---------------------------------------------#
  #                                                                         #
  ###########################################################################
  
  print(paste0("Loading data from: ", data_path))
  
  ukb <- readRDS(data_path)
  
  # Remove participants withdrawn
  withdrawn <- read.delim(withdraw_path, header = F)
  ukb <- subset(ukb, !(rownames(ukb) %in% withdrawn$V1))
  
  # Keep only the first time point
  ukb <-  ukb[grep(".*\\.0\\.0.*", colnames(ukb), )]
  colnames(ukb) <- sub(pattern = "\\.0\\.0", replacement = "", colnames(ukb))
  
  # get variable dictionary for columns and selection
  col_dict <- read.csv(covar_dict_path)
  name <- c(subset(col_dict, Include == 1)$CodingName)
  # name_figure <- c(subset(col_dict, Include == 1 & !(Type %in% c("Metabolic","Biochemistry","CancerRegistry")))$FigureName)
  
  print(paste0("Number of variables: ", length(name)))
  
  # colnames(data) <- name_figure
  
  # add townsend index from prior basket
  # tmp <- readRDS("~/covid19_metabolomics/Scripts/extraction_and_recoding/outputs/ukb_extracted.rds")
  # tmp$townsend_ind <- tmp$townsend_ind.0.0
  # ukb <- merge(ukb, tmp[c("townsend_ind")], by = "row.names")
  # colnames(ukb)[1] <- "eid"
  # rm(tmp)
  
  ukb$eid <- rownames(ukb)
  ukb <- ukb %>%
    dplyr::select(any_of(c("eid", name)))
  
  if (!all(name %in% colnames(ukb))) {
    print(paste0("Variables not found: ", name[!name %in% colnames(ukb)]))
  }
  
  
  ###########################################################################
  #                                                                         #
  # Refactoring variables --------------------------------------------------#
  #                                                                         #
  ###########################################################################
  ## TODO need to loop over variable vector
  
  
  print("Refactoring variables...")
  data <- ukb
  
  # Ethnicity
  table(data$ethnicity, useNA = "ifany")
  data$ethnicity_detailed <- data$ethnicity
  data$ethnicity <- ifelse(data$ethnicity %in% c("White", "British", "Irish", "Any other white background"), "White",
                           ifelse(data$ethnicity %in% c("Indian", "Pakistani", "Bangladeshi") | (data$ethnicity %in% c("Asian or Asian British", "Any other Asian background") & data$country_birth %in% c("Bhutan", "Maldives", "Nepal", "Sri Lanka", "India", "Pakistan", "Bangladesh")), "SouthAsian",
                                  NA))
  data$ethnicity <- relevel(as.factor(data$ethnicity), "White")
  table(data$ethnicity, useNA = "ifany")
  
  # Household income
  # table(data$household_income, useNA = "ifany")
  # data$household_income_detailed <- data$household_income
  # data$household_income <- ifelse(data$household_income %in% c("Less than 18,000"), "LowerIncome",
  #                                 ifelse(data$household_income %in% c("18,000 to 30,999", "31,000 to 51,999"), "MiddleIncome",
  #                                        ifelse(data$household_income %in% c("18,000 to 30,999", "31,000 to 51,999", "52,000 to 100,000", "Greater than 100,000"), "HigherIncome",
  #                                               NA)))
  # data$household_income <- relevel(as.factor(data$household_income), "LowerIncome")
  # table(data$household_income, useNA = "ifany")
  
  # Education
  # table(data$education_status, useNA = "ifany")
  # data$education_status_detailed <- data$education_status
  # data$education_status <- ifelse(data$education_status %in% c("College or University degree"), "University",
  #                                 ifelse(data$education_status %in% c("A levels/AS levels or equivalent"), "ALevel",
  #                                        ifelse(data$education_status %in% c("O levels/GCSEs or equivalent", "CSEs or equivalent"), "OLevel",
  #                                               ifelse(data$education_status %in% c("Prefer not to answer", "None of the above", NA), NA, "Other"))))
  # data$education_status <- relevel(as.factor(data$education_status), "University")
  # table(data$education_status, useNA = "ifany")
  
  # Employment
  # table(data$employment_status, useNA = "ifany")
  # data$employment_status <- ifelse(data$employment_status %in% c("In paid employment or self-employed"), "Employed",
  #                                  ifelse(data$employment_status %in% c("Retired"), "Retired",
  #                                         ifelse(data$employment_status %in% c("Prefer not to answer", NA), NA, "Unemployed")))
  # data$employment_status <- relevel(as.factor(data$employment_status), "Unemployed")
  # table(data$employment_status, useNA = "ifany")
  
  
  # Smoking status
  table(data$smoking_status, useNA = "ifany")
  data$smoking_status_detailed <- data$smoking_status
  data$smoking_status <- ifelse(data$smoking_status %in% c("Current"), "CurrentSmoker",
                                ifelse(data$smoking_status %in% c("Previous"), "FormerSmoker",
                                       ifelse(data$smoking_status %in% c("Never"), "NeverSmoker", NA)))
  data$smoking_status <- relevel(as.factor(data$smoking_status), "NeverSmoker")
  table(data$smoking_status, useNA = "ifany")
  
  
  # Alcohol status
  # table(data$alcohol_status, useNA = "ifany")
  # data$alcohol_status_detailed <- data$alcohol_status
  # data$alcohol_status <- ifelse(data$alcohol_status %in% c("Current"), "CurrentDrinker",
  #                               ifelse(data$alcohol_status %in% c("Previous"), "FormertDrinker",
  #                                      ifelse(data$alcohol_status %in% c("Never"), "NeverDrinker", NA)))
  # data$alcohol_status <- relevel(as.factor(data$alcohol_status), "NeverDrinker")
  # table(data$alcohol_status, useNA = "ifany")
  
  # BP Systolic & Diastolic
  data <- data %>%
    mutate(bp_sys = if_else(!is.na(bp_sys) & !is.na(bp_systolic_auto), (bp_sys + bp_systolic_auto)/2,
                            if_else(!is.na(bp_sys), bp_sys,
                                    if_else(!is.na(bp_systolic_auto), bp_systolic_auto, NA))),
           bp_dias = if_else(!is.na(bp_dias) & !is.na(bp_diastolic_auto), (bp_dias + bp_diastolic_auto)/2,
                           if_else(!is.na(bp_dias), bp_dias,
                                   if_else(!is.na(bp_diastolic_auto), bp_diastolic_auto, NA))))
  data$bp_diastolic_auto <- data$bp_systolic_auto <- NULL
  
  ### TODO fix coding, i.e. when combining numerical and encoding values
  # Sedentary lifestyle
  ukb_raw <- readRDS(paste0("~/CHD/extraction_and_recoding/outputs/ukb_extracted.rds"))
  
  data <- data %>%
    mutate(time_spent_tv = if_else(!is.na(ukb_raw$time_spent_tv.0.0), 
                                   if_else(ukb_raw$time_spent_tv.0.0 == -10, 0, 
                                           if_else(ukb_raw$time_spent_tv.0.0 %in% c(-1,-3), NA, ukb_raw$time_spent_tv.0.0)), NA),
           time_spent_computer = if_else(!is.na(ukb_raw$time_spent_computer.0.0), 
                                   if_else(ukb_raw$time_spent_computer.0.0 == -10, 0, 
                                           if_else(ukb_raw$time_spent_computer.0.0 %in% c(-1,-3), NA, ukb_raw$time_spent_computer.0.0)), NA),
           time_spent_driving = if_else(!is.na(ukb_raw$time_spent_driving.0.0), 
                                   if_else(ukb_raw$time_spent_driving.0.0 == -10, 0, 
                                           if_else(ukb_raw$time_spent_driving.0.0 %in% c(-1,-3), NA, ukb_raw$time_spent_driving.0.0)), NA),
           sedentary_lifestyle = if_else(rowSums(pick(c(time_spent_tv, time_spent_computer, time_spent_driving)), na.rm = T) > 8, 1, 0))
  data$time_spent_tv <- data$time_spent_computer <- data$time_spent_driving <- NULL
  
  # Waist-hip ratio
  data <- data %>%
    mutate(waist_hip_ratio = waist_circ / hip_circ)
  data$waist_circ <- data$hip_circ <- NULL
  
  
  # Index of Multiple Deprivation (IMD)
  data <- data %>%
    mutate(imd_england = ntile(imd_england, 10),
           imd_scotland = ntile(imd_scotland, 10),
           imd_wales = ntile(imd_wales, 10),
           imd = as.factor(if_else(!is.na(imd_england), imd_england,
                         if_else(!is.na(imd_scotland), imd_scotland,
                                 if_else(!is.na(imd_wales), imd_wales, NA)))))
  data$imd_england <- data$imd_scotland <- data$imd_wales <- NULL
  
  # Dealing with dates
  data$date_recr <- as.Date(data$date_recr)
  data$date_of_death <- as.Date(data$date_of_death)
  
  # Rename age
  data$age <- data$age_recr_continuous
  data$age_recr_continuous <- NULL
  
  return(data)
}

one_hot_encode <- function(data, covars) {
  data <- data[,c("eid", covars)]
  data$eid <- as.numeric(data$eid)
  data <- as.data.frame(model.matrix(~., model.frame(~ ., data, na.action=na.pass),
                                    contrasts.arg = lapply(data[,sapply(data, is.factor)], contrasts, contrasts=FALSE)))
  col_keep_idx <- vapply(data, function(x) length(unique(x)) > 1, logical(1L))
  col_remove_name <- colnames(data)[-col_keep_idx]
  print(paste0("Columns removed (all same values): ", col_remove_name))
  
  data <- data[,col_keep_idx]
  
  return(data)
}
