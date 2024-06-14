data <- 
define_outcome_cvd <- function(data, definition) {
  
  
  startdate <- as.Date("2020-01-01")
  enddate <- as.Date("2021-01-01")
  
  data$outcome = data$case
  
  if(definition == "noprior") {
    data <- data %>%
      mutate(outcome = if_else(!is.na(date_diagnosis) & !is.na(specdate),
                               ifelse(date_diagnosis < specdate, 0, 1), outcome))
      
  } else if (definition == "priorcvd") {
    cvd_data_multi <- readRDS("../../outcome_definition/Outputs/output_hes_multi.rds")
    cvd_data_multi <- merge(cvd_data_multi, data[, c("Row.names", "specdate")], by.x = "eid", by.y = "Row.names", all.x = T)
    
    # outcome 0 no cvd, but must have prior cvd (before 2020)
    # outcome 1 cvd, but must have prior cvd
    # maybe we just say that we don't include those who had cvd after 2020, pre covid infection
    
    cvd_data_multi_covid <- subset(cvd_data_multi, !is.na(specdate))
    cvd_data_multi_nocovid <- subset(cvd_data_multi, is.na(specdate))
    
    eid_cvd_post2020_nocovid <- cvd_data_multi_nocovid %>%
      filter(date >= as.Date("2020-01-01"))
    eid_cvd_post2020_nocovid <- unique(eid_cvd_post2020_nocovid$eid)
    
    eid_nocvd_post2020_nocovid <- cvd_data_multi_nocovid %>%
      filter(!(eid %in% eid_cvd_post2020_nocovid))
    eid_nocvd_post2020_nocovid <- unique(eid_nocvd_post2020_nocovid$eid)
    
    eid_cvd_post2020_covid <- cvd_data_multi_covid %>%
      filter(date >= specdate)
    eid_cvd_post2020_covid <- unique(eid_cvd_post2020_covid$eid)
    
    eid_nocvd_post2020_covid <- cvd_data_multi_covid %>%
      filter(!(eid %in% eid_cvd_post2020_covid))
    eid_nocvd_post2020_covid <- unique(eid_nocvd_post2020_covid$eid)
    
    ## RISK number here is not right. probably matching error?!!
    cvd_data_multi <- cvd_data_multi %>%
      filter(date <= as.Date("2020-01-01") | date <= specdate)
    priorcvd_eid <- unique(cvd_data_multi$eid)
    
    cvd_postcovid_eid <- cvd_data_multi_tmp %>%
      filter(date >=specdate)
    
    data <- data %>%
      filter(Row.names %in% priorcvd_eid)
    
    data <- data %>%
      mutate(outcome = if_else(!is.na(specdate),
                               ifelse(date_diagnosis < specdate, 0, 1), NA))
    
  }
  return(data)
}

define_outcome_covid <- function(data, covid_data_path) {
  covid_data <- readRDS(covid_data_path)
  
  data <- merge(ukb, covid_data, by.x = "row.names", by.y = "eid", all.x = T)
  
  return(data)
}

population_filter <- function(data) {
  print(paste0("Total number of UKB participants: ",nrow(data)))
  
  data <- subset(data, is.na(date_of_death) | !(date_of_death <= as.Date("2020-01-01")))
  print(paste0("Total number (excluded died before 2020-01-01): ",nrow(data)))
  
  data <- subset(data, is.na(date_of_death) | !(date_of_death >= as.Date("2020-12-01")))
  print(paste0("Total number (excluded died after 2020-12-01): ",nrow(data)))
  
  data <- subset(data, !(outcome == 1 & is.na(date_diagnosis)))
  print(paste0("Total number (excluded CVD diagnosis date unknown): ",nrow(data)))
  
  data <- subset(data, !(result == 1 & is.na(specdate)))
  print(paste0("Total number (excluded COVID-19 test date unknown): ",nrow(data)))
  
  #data <- subset(data, is.na(date_diagnosis) | date_diagnosis >= as.Date("2020-01-01") )
  #print(paste0("Total number (excluded CVD before 2020): ",nrow(data)))
  
  ##### Participants COVID-19 -ve
  data_nocovid <- subset(data, is.na(specdate))
  print(paste0("Total number without COVID-19 +ve record: ",nrow(data_nocovid)))
  
  data_nocovid <- subset(data_nocovid, is.na(date_of_death_covid))
  print(paste0("Total number without COVID-19 +ve record (excluded covid primary cause of death): ",nrow(data_nocovid)))
  
  ##### Participants COVID-19 +ve
  data_covid <- subset(data, !is.na(specdate))
  print(paste0("Total number with COVID-19 +ve record: ",nrow(data_covid)))
  
  #data_covid <- subset(data_covid, !(outcome == 1 & date_diagnosis < specdate))
  #print(paste0("Total number without COVID-19 +ve record (excluded CVD before COVID): ",nrow(data_covid)))
  
  data <- rbind(data_covid, data_nocovid)
  
  table(data$outcome, data$result) #2648    126
  
  return(data)
}

