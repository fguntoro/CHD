library(tableone)
library(dplyr)
library(caret)
# function for model definition
# arg "covariate"/ "metab" / "biochem" allow combination
# return dat

define_model <- function(data, models) {
  
  return(data)
}

## Summaries

data <- data_filtered

table(data$case, data$ethnicity)
table(data$case_MI, data$ethnicity)
table(data$case_CR, data$ethnicity)
table(data$case_IS, data$ethnicity)

covars <- c("age_recr_continuous", "sex", "imd", "Diabetes", "Hypertension", "Hypercholesterolaemia", "smoking_status", "bmi", "waist_hip_ratio", "hdl_cholesterol", "triglycerides")
data <- data[c("ethnicity", covars)]
dummy <- dummyVars(~., data = data)
df <- data.frame(predict(dummy, newdata=data))

contVars = c("age_recr_continuous", "bmi", "waist_hip_ratio", "hdl_cholesterol", "triglycerides")
catVars <- df
catVars[contVars] <- NULL
contVars <- df[contVars]

catVars[sapply(catVars, is.numeric)] <- lapply(catVars[sapply(catVars, is.numeric)], factor, levels = c(0,1))
final_df_full <- cbind(contVars, catVars)
tableone <- CreateTableOne(strata = c("ethnicity.SouthAsian"), data = final_df_full, testExact = "fishers.test", smd=T)
table <- print(tableone, smd = T, noSpaces = T, missing = T)[,-4]

colnames(table) <- c("European", "South Asian", "p-value", "SMD")
rownames(table) <- c("N", "Age (mean (SD))", "BMI (mean (SD))","Waist-Hip ratio", "HDL-cholesterol (mean(SD))", "Triglycerides (mean(SD))","Ethnicity = South Asian (%)", "Sex = Female (%)",
                     paste0("imd.", 1:10), "Diabetes", "Hypertension", "Hypercholesterolaemia", 
                     "Smoking = Never (%)", "Smoking = Current (%)", "Smoking = Former (%)")
write.csv(table, "./Results/table_demographics_full.csv")

summ <- data %>%
  group_by(ethnicity) %>%
  summarise(n = n(),
            ASCVD = sum(case, na.rm = T),
            ASCVD_perc = sum(case, na.rm = T)/n*100,
            MI = sum(case_MI, na.rm = T),
            MI_perc = sum(case_MI, na.rm = T)/n*100,
            CR = sum(case_CR, na.rm = T),
            CR_perc = sum(case_CR, na.rm = T)/n*100,
            IS = sum(case_IS, na.rm = T),
            IS_perc = sum(case_IS, na.rm = T)/n*100)
write.csv(summ, "~/CHD/Results/summary_ethnicity.csv", row.names = F)

summ <- data %>%
  group_by(ethnicity) %>%
  summarise(n = n(),
            ASCVD = sum(case, na.rm = T),
            ASCVD_perc = sum(case, na.rm = T)/n*100,
            
            Male = sum(sex == "Male" & case == 1, na.rm = T),
            Male_total = sum(sex == "Male", na.rm = T),
            Male_perc = sum(sex == "Male" & case == 1, na.rm = T)/Male_total*100,
            
            Female = sum(sex == "Female" & case == 1, na.rm = T),
            Female_total = sum(sex == "Female", na.rm = T),
            Female_perc = sum(sex == "Female" & case == 1, na.rm = T)/Female_total*100,
            
            Age55Under = sum(age<55 & case == 1, na.rm = T),
            Age55Under_total = sum(age<55, na.rm = T),
            Age55Under_perc = sum(age<55 & case == 1, na.rm = T)/Age55Under_total*100,
            
            Age55andAbove = sum(age>=55 & case == 1, na.rm = T),
            Age55andAbove_total = sum(age>=55, na.rm = T),
            Age55andAbove_perc = sum(age>=55 & case == 1, na.rm = T)/Age55andAbove_total*100,)
write.csv(summ, "~/CHD/Results/summary_demographics.csv", row.names = F)

summ <- data %>%
  mutate(ethnicity_refactored = if_else(ethnicity_detailed %in% c("White", "British", "Irish", "Any other white background"), "White", 
                                        if_else(ethnicity_detailed %in% c("Any other Asian background", "Asian or Asian British"),"Other South Asian", ethnicity_detailed))) %>%
  group_by(ethnicity_refactored) %>%
  summarise(n = n(),
            ASCVD = sum(case, na.rm = T),
            ASCVD_perc = sum(case, na.rm = T)/n*100,
            MI = sum(case_MI, na.rm = T),
            MI_perc = sum(case_MI, na.rm = T)/n*100,
            CR = sum(case_CR, na.rm = T),
            CR_perc = sum(case_CR, na.rm = T)/n*100,
            IS = sum(case_IS, na.rm = T),
            IS_perc = sum(case_IS, na.rm = T)/n*100)
write.csv(summ, "~/CHD/Results/summary_ethnicity_detailed.csv", row.names = F)
