data <- data_filtered




data_merged_M <- subset(data, sex == "Male")
data_merged_F <- subset(data, sex == "Female")

strat_coxph <- function(data, weights = NULL) {
  form0 <- as.formula("Surv(time, event) ~ ethnicity + age_recr_continuous + smoking_status + sedentary_lifestyle + bmi + hdl_cholesterol + triglycerides + assessment_centre + townsend_ind")
  
  res.cox <- coxph(form0, data = data, weights = weights)
  print(summary(res.cox))
}

strat_coxph(data_merged_M)
strat_coxph(data_merged_F)


m.out <- readRDS("~/CHD/Data/matched_nearest_M_ratio1.rds")
m.dat <- match.data(m.out)
strat_coxph(m.dat, weights = "weights")

m.out <- readRDS("~/CHD/Data/matched_nearest_F_ratio1.rds")
m.dat <- match.data(m.out)
strat_coxph(m.dat, weights = "weights")

m.out <- readRDS("~/CHD/Data/matched_nearest_M_ratio1.rds")
m.dat <- match.data(m.out)
strat_coxph(m.dat, weights = "weights")

m.out <- readRDS("~/CHD/Data/matched_nearest_F_ratio1.rds")
m.dat <- match.data(m.out)
strat_coxph(m.dat, weights = "weights")

stats_analysis <- function(data, stats_model) {
  
  return(data)
}
