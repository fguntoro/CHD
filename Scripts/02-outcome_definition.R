library(dplyr)
library(rlang)
# function for outcome definition
# arg dat, "prior cvd"/"no prior"
# return dat

# function for inclusion/exclusion
# inclusion/exclusion criteria
# arg dat
# return dat



# icd10 <- subset(code_dict, Group == "Hypertension" & Type == "ICD10")$ID
# x <- unique(grep(paste(icd10, collapse = "|"), data_merged$cause_of_death_primary))
# tmp2 <- data_merged[x,]

define_outcome_cvd <- function(data, definition=NULL) {
  
  outcome <- readRDS("~/CHD/outcome_definition/Outputs/out1/output_final.rds")
  code_dict <- read.xlsx("~/CHD/outcome_definition/Definitions/code_dict.xlsx")
  
  eid_case <- subset(outcome, case == 1)$eid
  eid_incident <- subset(outcome, incident_case == 1)$eid
  eid_prevalent <- subset(outcome, prevalent_case == 1)$eid
  
  data_merged <- merge(data, outcome[,-2], by = "eid")
  
  icd10_ascvd <- subset(code_dict, Group == "ASCVD" & Type == "ICD10")$ID
  case_idx <- unique(grep(paste(icd10_ascvd, collapse = "|"), data_merged$cause_of_death_primary))
  
  # Defining outcome with primary cause of death
  data_merged[case_idx,] <- data_merged[case_idx,] %>%
    mutate(case = 1,
           date_diagnosis = if_else(!is.na(date_diagnosis), date_diagnosis, date_death),
           prevalent_case = if_else(date_diagnosis <= date_recr, 1, 0),
           incident_case = if_else(date_diagnosis > date_recr, 1, 0),
           time_to_diagnosis = if_else(incident_case == 1 & (!is.na(date_diagnosis) & !is.na(date_recr)), as.numeric(date_diagnosis - date_recr), NA))
  
  # Survival analysis (time, event)
  data_merged <- data_merged %>%
    mutate(event = incident_case + 1,# censoring status 1 = censored, 2 = event
           time = if_else(event == 2, 
                          as.numeric(date_diagnosis - date_recr),
                          if_else(is.na(date_death), as.numeric(as.Date("2022-10-31") - date_recr), as.numeric(date_death - date_recr))))
  
  return(data_merged)
}

define_outcome_covar <- function(data) {
  code_dict <- read.xlsx("~/CHD/outcome_definition/Definitions/code_dict.xlsx")
  
  # health prevalent
  covar_idxs <- unique(subset(code_dict, Group != "ASCVD")$Index)
  
  for (i in covar_idxs) {
    covar <- unique(code_dict$Group)[i]
    print(paste0("Imputing ", covar))
    
    outcome <- readRDS(paste0("~/CHD/outcome_definition/Outputs/out",i, "/output_final.rds"))
    
    eid_case <- subset(outcome, case == 1)$eid
    eid_incident <- subset(outcome, incident_case == 1)$eid
    eid_prevalent <- subset(outcome, prevalent_case == 1)$eid
    
    tmp <- data %>%
      mutate(!!covar := if_else(eid %in% eid_prevalent, 1, 0))
    
    print(paste0("Individuals with prevalent ", covar, ": ", sum(tmp[[covar]])))
    
    if (covar == "Diabetes") {
      tmp <- tmp %>%
        mutate(!!covar := if_else(.data[[covar]] == 1, 1,
                                  if_else(hba1c > 48, 1, 0, missing = 0)))
    } else if (covar == "Hypertension") {
      tmp <- tmp %>%
        mutate(!!covar := if_else(.data[[covar]] == 1, 1,
                                  if_else(bp_sys > 140 | bp_dias > 90, 1, 0, missing = 0)))
    } else if (covar == "Hypercholesterolaemia") {
      tmp <- tmp %>%
        mutate(!!covar := if_else(.data[[covar]] == 1, 1,
                                  if_else(ldl_direct > 4.1, 1, 0, missing = 0)))
    }
    
    print(paste0("Total individuals with prevalent ", covar, " (extra def): ", sum(tmp[[covar]])))
    
  }
  
  return(tmp)
  
}
  
filter_exclusion <- function(data) {
  # Filters
  nrow_tmp <- print(nrow(data))
  
  data <- data %>%
    filter(ethnicity %in% c("White", "SouthAsian"))
  print(paste0("Excluding non-white, non-southasian individuals: ", nrow_tmp - nrow(data)))
  nrow_tmp <- nrow(data)
  print(nrow_tmp)
  
  data <- data %>%
    filter(prevalent_case != 1)
  print(paste0("Excluding prevalent CVD case: ", nrow_tmp - nrow(data)))
  print(nrow_tmp - nrow(data))
  nrow_tmp <- nrow(data)
  print(nrow_tmp)
  
  return(data)
}



# 
# define_outcome_cvd <- function(data, definition) {
#   
#   startdate <- as.Date("2020-01-01")
#   enddate <- as.Date("2021-01-01")
#   
#   data$outcome = data$case
#   
#   if(definition == "noprior") {
#     data <- data %>%
#       mutate(outcome = if_else(!is.na(date_diagnosis) & !is.na(specdate),
#                                ifelse(date_diagnosis < specdate, 0, 1), outcome))
#       
#   } else if (definition == "priorcvd") {
#     cvd_data_multi <- readRDS("../../outcome_definition/Outputs/output_hes_multi.rds")
#     cvd_data_multi <- merge(cvd_data_multi, data[, c("Row.names", "specdate")], by.x = "eid", by.y = "Row.names", all.x = T)
#     
#     # outcome 0 no cvd, but must have prior cvd (before 2020)
#     # outcome 1 cvd, but must have prior cvd
#     # maybe we just say that we don't include those who had cvd after 2020, pre covid infection
#     
#     cvd_data_multi_covid <- subset(cvd_data_multi, !is.na(specdate))
#     cvd_data_multi_nocovid <- subset(cvd_data_multi, is.na(specdate))
#     
#     eid_cvd_post2020_nocovid <- cvd_data_multi_nocovid %>%
#       filter(date >= as.Date("2020-01-01"))
#     eid_cvd_post2020_nocovid <- unique(eid_cvd_post2020_nocovid$eid)
#     
#     eid_nocvd_post2020_nocovid <- cvd_data_multi_nocovid %>%
#       filter(!(eid %in% eid_cvd_post2020_nocovid))
#     eid_nocvd_post2020_nocovid <- unique(eid_nocvd_post2020_nocovid$eid)
#     
#     eid_cvd_post2020_covid <- cvd_data_multi_covid %>%
#       filter(date >= specdate)
#     eid_cvd_post2020_covid <- unique(eid_cvd_post2020_covid$eid)
#     
#     eid_nocvd_post2020_covid <- cvd_data_multi_covid %>%
#       filter(!(eid %in% eid_cvd_post2020_covid))
#     eid_nocvd_post2020_covid <- unique(eid_nocvd_post2020_covid$eid)
#     
#     ## RISK number here is not right. probably matching error?!!
#     cvd_data_multi <- cvd_data_multi %>%
#       filter(date <= as.Date("2020-01-01") | date <= specdate)
#     priorcvd_eid <- unique(cvd_data_multi$eid)
#     
#     cvd_postcovid_eid <- cvd_data_multi_tmp %>%
#       filter(date >=specdate)
#     
#     data <- data %>%
#       filter(Row.names %in% priorcvd_eid)
#     
#     data <- data %>%
#       mutate(outcome = if_else(!is.na(specdate),
#                                ifelse(date_diagnosis < specdate, 0, 1), NA))
#     
#   }
#   return(data)
# }
# 
# define_outcome_covid <- function(data, covid_data_path) {
#   covid_data <- readRDS(covid_data_path)
#   
#   data <- merge(ukb, covid_data, by.x = "row.names", by.y = "eid", all.x = T)
#   
#   return(data)
# }
# 
# population_filter <- function(data) {
#   print(paste0("Total number of UKB participants: ",nrow(data)))
#   
#   data <- subset(data, is.na(date_of_death) | !(date_of_death <= as.Date("2020-01-01")))
#   print(paste0("Total number (excluded died before 2020-01-01): ",nrow(data)))
#   
#   data <- subset(data, is.na(date_of_death) | !(date_of_death >= as.Date("2020-12-01")))
#   print(paste0("Total number (excluded died after 2020-12-01): ",nrow(data)))
#   
#   data <- subset(data, !(outcome == 1 & is.na(date_diagnosis)))
#   print(paste0("Total number (excluded CVD diagnosis date unknown): ",nrow(data)))
#   
#   data <- subset(data, !(result == 1 & is.na(specdate)))
#   print(paste0("Total number (excluded COVID-19 test date unknown): ",nrow(data)))
#   
#   #data <- subset(data, is.na(date_diagnosis) | date_diagnosis >= as.Date("2020-01-01") )
#   #print(paste0("Total number (excluded CVD before 2020): ",nrow(data)))
#   
#   ##### Participants COVID-19 -ve
#   data_nocovid <- subset(data, is.na(specdate))
#   print(paste0("Total number without COVID-19 +ve record: ",nrow(data_nocovid)))
#   
#   data_nocovid <- subset(data_nocovid, is.na(date_of_death_covid))
#   print(paste0("Total number without COVID-19 +ve record (excluded covid primary cause of death): ",nrow(data_nocovid)))
#   
#   ##### Participants COVID-19 +ve
#   data_covid <- subset(data, !is.na(specdate))
#   print(paste0("Total number with COVID-19 +ve record: ",nrow(data_covid)))
#   
#   #data_covid <- subset(data_covid, !(outcome == 1 & date_diagnosis < specdate))
#   #print(paste0("Total number without COVID-19 +ve record (excluded CVD before COVID): ",nrow(data_covid)))
#   
#   data <- rbind(data_covid, data_nocovid)
#   
#   table(data$outcome, data$result) #2648    126
#   
#   return(data)
# }
# 
