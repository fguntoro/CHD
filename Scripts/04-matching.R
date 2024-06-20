data <- data_filtered
data_M <- subset(data, sex == "Male")
data_F <- subset(data, sex == "Female")

method <- "nearest"
ratios = c(1, 10)
sexes <- c("M", "F")
formula <- as.formula("ethnicity ~ age_recr_continuous + smoking_status + sedentary_lifestyle + bmi + hdl_cholesterol + triglycerides + townsend_ind")

for (sex in sexes) {
  print(paste0("Running ", method, " matching for data ", sex, "..."))
  tmp_dat <- get(paste0("data_", sex))
  
  tmp_dat <- tmp_dat %>%
    dplyr::select(all_of(c("eid", "time","ethnicity", "event", "sex", "age_recr_continuous", "smoking_status", "sedentary_lifestyle", "bmi", "hdl_cholesterol","triglycerides","assessment_centre","townsend_ind"))) %>%
    na.omit()
  
  for (ratio in ratios) {
    print(paste0("Using ratio: ", ratio))
    m.out <- matchit(formula = formula, data = tmp_dat, method = method, distance = "glm", caliper = 0.2, ratio = ratio)
    # m.out.summ <- summary(m.out)
    # m.dat <- match.data(m.out)
    # strat_coxph(m.dat)
    
    file.out <- paste0("~/CHD/Data/matched_", method, "_", sex, "_ratio", ratio, ".rds")
    print(paste0("Saving matching output to: ", file.out))
    saveRDS(m.out, file.out)
  }
}


### SMD



matching <- function(data, matching_spec) {
  
  return(data)
}