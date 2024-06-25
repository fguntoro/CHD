rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./01-data_cleaning.R")
source("./02-outcome_definition.R")
source("./03-model_definition.R")
source("./04-matching.R")
source("./05-stats.R")

# col_dict <- read.csv("../extraction_and_recoding/column_dictionary.csv")

# function for data cleaning
data_clean <- clean_data(data_path = "~/CHD/extraction_and_recoding/outputs/ukb_recoded.rds",
                         withdraw_path = "/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_673609/withdraw69328_136_20231013.txt",
                         covar_dict_path = "~/CHD/column_dictionary.csv")
saveRDS(data_clean, "~/CHD/Data/ukb_sub.rds")

# function for outcome definition
# data_clean <- readRDS("~/CHD/Data/ukb_sub.rds")
data_merged <- define_outcome_cvd(data_clean)
data_merged <- define_outcome_covar(data_merged)
saveRDS(data_merged, "~/CHD/Data/ukb_merged.rds")

# function for inclusion/exclusion
# data_merged <- readRDS("~/CHD/Data/ukb_merged.rds") #fix
data_filtered <- filter_exclusion(data_merged)
saveRDS(data_filtered, "~/CHD/Data/ukb_filtered.rds")

# function for ohe
# data_filtered <- readRDS("~/CHD/Data/ukb_filtered.rds")
# covars <- c("ethnicity", "sex", "age_recr_continuous", "smoking_status", "sedentary_lifestyle", "bmi", "hdl_cholesterol","triglycerides","assessment_centre","imd", "waist_hip_ratio", "Diabetes", "Hypertension", "Hypercholesterolaemia", "time", "event")
# data_ohe <- one_hot_encode(data_filtered, covars)

# function for matching
# data_filtered <- readRDS("~/CHD/Data/ukb_filtered.rds")
matching(data_filtered)

# function for stats
#covars_full <- c("ethnicity", "sex", "age_recr_continuous", "smoking_status", "sedentary_lifestyle", "bmi", "hdl_cholesterol","triglycerides","assessment_centre","imd", "Diabetes", "Hypercholesterolaemia", "Hypertension")
covars_adjusted <- c("ethnicity", "sex", "age", "smoking_status", "sedentary_lifestyle", "bmi", "hdl_cholesterol","triglycerides","imd", "Diabetes", "Hypercholesterolaemia", "Hypertension")
covars_adjusted1 <- c("ethnicity", "sex", "age")
#covars_matching <- covars_adjusted[!covars_adjusted %in% c("assessment_centre")]

stat_coxph("../Data/matched_nearest_ratio1.rds", covars_adjusted, "mod2")
stat_coxph("../Data/matched_nearest_ratio5.rds", covars_adjusted, "mod2")
stat_coxph("../Data/matched_nearest_ratio10.rds", covars_adjusted, "mod2")
stat_coxph("../Data/matched_quick_ratio1.rds", covars_adjusted, "mod2")
stat_coxph("../Data/matched_null_ratio1.rds", covars_adjusted, "mod2")

stat_coxph("../Data/matched_nearest_ratio1.rds", covars_adjusted1, "mod1")
stat_coxph("../Data/matched_nearest_ratio5.rds", covars_adjusted1, "mod1" )
stat_coxph("../Data/matched_nearest_ratio10.rds", covars_adjusted1, "mod1")
stat_coxph("../Data/matched_quick_ratio1.rds", covars_adjusted1, "mod1")
stat_coxph("../Data/matched_null_ratio1.rds", covars_adjusted1, "mod1")
