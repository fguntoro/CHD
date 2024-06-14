setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./01-data_cleaning.R")
source("./02-outcome_definition.R")
source("./03-model_definition.R")
source("./04-matching.R")
source("./05-stats.R")

ukb <- readRDS(paste0("../data/ukb_merged_data.rds"))
col_dict <- read.csv("../extraction_and_recoding/column_dictionary.csv")
covid_data <- readRDS("../covid_data.rds")

withdraw_path <- "~/../projects/chadeau_ukbb_folder/live/data/project_data/UKB_677583/withdraw69328_330_20240527.txt"

# function for data cleaning
# arg dat, withdraw, variables to recode
# return dat

# function for outcome definition
# arg dat, "prior cvd"/"no prior"
# return dat

# function for inclusion/exclusion
# inclusion/exclusion criteria
# arg dat
# return dat

# function for model definition
# arg "covariate"/ "metab" / "biochem" allow combination
# return dat

# function for matching
# arg matching scenario
# return dat

# function for stats
# arg stats
# return results

multi_data <- readRDS("../../outcome_definition/Outputs/output_hes_multi.rds")
