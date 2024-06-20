rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./01-data_cleaning.R")
source("./02-outcome_definition.R")
source("./03-model_definition.R")
source("./04-matching.R")
source("./05-stats.R")

col_dict <- read.csv("../extraction_and_recoding/column_dictionary.csv")
# covid_data <- readRDS("../covid_data.rds")

withdraw_path <- "~/../projects/chadeau_ukbb_folder/live/data/project_data/UKB_677583/withdraw69328_330_20240527.txt"

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
# data_clean <- readRDS("~/CHD/Data/ukb_sub.rds") #fix
data_filtered <- filter_exclusion(data_merged)
saveRDS(data_filtered, "~/CHD/Data/ukb_filtered.rds")

# function for model definition
data_filtered <- readRDS("~/CHD/Data/ukb_filtered.rds")

# arg "covariate"/ "metab" / "biochem" allow combination
# return dat

# function for matching
# arg matching scenario
# return dat

# function for stats
# arg stats
# return results

multi_data <- readRDS("../../outcome_definition/Outputs/output_hes_multi.rds")
