library(filenamer)
library(MatchIt)
library(survival)

stat_coxph <- function(match.out.path, covars = NULL, suffix = NULL, weights = NULL) {
  dirpath <- "~/CHD/Results/"
  file.out <- insert(basename(match.out.path), ext="rds", replace=T)
  file.out <- insert(file.out, tag=suffix)
  
  match.out <- readRDS(match.out.path)
  data <- match.data(match.out$m.out)
  
  if(is.null(covars)) {
    form0 <- paste0("Surv(time, event) ~ . - subclass - weights - eid")
  } else {
    form0 <- paste0("Surv(time, event) ~ ", paste0(covars, collapse = "+"))
  }
  
  if (is.null(match.out$m.out$call$method)) {
    subclass <- NULL
  }
  
  res.cox <- coxph(as.formula(form0), data = data, weights = weights, robust = T, cluster = subclass)
  summ <- summary(res.cox)
  print(summ)
  saveRDS(summ, paste0(dirpath, insert(file.out, tag="full")))
  
  # Stratify male
  res.cox <- coxph(as.formula(paste0(form0, "- sex")), data = data, weights = weights, robust = T, cluster = subclass, subset = (sex == "Male"))
  summ <- summary(res.cox)
  print(summ)
  saveRDS(summ, paste0(dirpath, insert(file.out, tag="m")))
  
  # Stratify female
  res.cox <- coxph(as.formula(paste0(form0, "- sex")), data = data, weights = weights, robust = T, cluster = subclass, subset = (sex == "Female"))
  summ <- summary(res.cox)
  print(summ)
  saveRDS(summ, paste0(dirpath, insert(file.out, tag="f")))
  
  # return(tab)
}

# 
# 
# data_merged_M <- subset(data, sexMale == 1)
# data_merged_F <- subset(data, sexMale == 0)
# 
# stat_coxph(data_merged_M, covars)
# stat_coxph(data_merged_F, covars)
# 
# 
# m.out <- readRDS("~/CHD/Data/matched_nearest_M_ratio1.rds")
# m.dat <- match.data(m.out)
# stat_coxph(m.dat)
# 
# m.out <- readRDS("~/CHD/Data/matched_nearest_F_ratio1.rds")
# m.dat <- match.data(m.out)
# stat_coxph(m.dat)
# 
# m.out <- readRDS("~/CHD/Data/matched_nearest_M_ratio10.rds")
# m.dat <- match.data(m.out)
# stat_coxph(m.dat, weights = "weights")
# 
# m.out <- readRDS("~/CHD/Data/matched_nearest_F_ratio10.rds")
# m.dat <- match.data(m.out)
# stat_coxph(m.dat, weights = "weights")
# 
# stats_analysis <- function(data, stats_model) {
#   
#   return(data)
# }
