source("~/Matching/Matching-Applied/Scripts/match_functions.R")

# library(plyr)
library(caret)

matching <- function(data) {
  
  # Create relevant directory
  ifelse(dir.exists("../Results/"), FALSE, dir.create("../Results/"))
  
  columns <- c("eid","ethnicity", "sex", "age", "smoking_status", "sedentary_lifestyle", "bmi", "hdl_cholesterol","triglycerides","assessment_centre","imd", "waist_hip_ratio", "Diabetes", "Hypertension", "Hypercholesterolaemia", "time", "event")
  
  data <- data[,columns] %>%
    na.omit()
  
  covars <- colnames(data)[!colnames(data) %in% c("eid", "time", "event")]
  covars <- covars[-grep("assessment_centre|imd", covars)]
  
  dlist <- list(method = c("nearest", "quick", "null"),
                ratio = c(1,5,10),
                match.fm = paste0(covars[1], " ~ ", paste0(covars[-1], collapse = "+")))
  
  map_full <- do.call(expand.grid, dlist) %>%
    filter(!(method == "quick" & ratio != 1)) %>%
    filter(!(method == "null" & ratio != 1))
  map_full <- map_full %>% distinct()
  
  out_combined <- data.frame()
  
  for (i in 1:nrow(map_full)) {
    map_idx = i
    map <- map_full[map_idx,]
    
    seeds=1
    out <- lapply(seeds, function(seed) {
      set.seed(seed)
      
      ############################
      # RUN #
      ############################
      
      match.out <- MatchingFun(formula=map$match.fm, data=data, estimand="ATT", method=map$method, distance="glm", ratio=map$ratio, caliper=0.3, replace = F, discard = "both", full_covar=T, exact = "sex")
      
      file.out <- paste0("~/CHD/Data/matched_", map$method, "_ratio", map$ratio, ".rds")
      print(paste0("Saving matching output to: ", file.out))
      saveRDS(match.out, file.out)
      
      m.dat <- match.data(match.out$m.out)
      m.dat <- m.dat[c("ethnicity", covars)]
      dummy <- dummyVars(~., data = m.dat)
      df <- data.frame(predict(dummy, newdata=m.dat))
      
      contVars = c("age", "bmi", "waist_hip_ratio", "hdl_cholesterol", "triglycerides")
      catVars <- df
      catVars[contVars] <- NULL
      contVars <- df[contVars]
      
      catVars[sapply(catVars, is.numeric)] <- lapply(catVars[sapply(catVars, is.numeric)], factor, levels = c(0,1))
      final_df_full <- cbind(contVars, catVars)
      tableone <- CreateTableOne(strata = c("ethnicity.SouthAsian"), data = final_df_full, testExact = "fishers.test", smd=T)
      table <- print(tableone, smd = T, noSpaces = T, missing = T)[,-4]
      
      colnames(table) <- c("European", "South Asian", "p-value", "SMD")
      rownames(table) <- c("N", "Age (mean (SD))", "BMI (mean (SD))","Waist-Hip ratio", "HDL-cholesterol (mean(SD))", "Triglycerides (mean(SD))","Ethnicity = White (%)", "Ethnicity = South Asian (%)", "Sex = Female (%)", "Sex = Male (%)", paste0("imd.", 1:10), "Diabetes", "Hypertension", "Hypercholesterolaemia", 
                           "Smoking = Never (%)", "Smoking = Current (%)", "Smoking = Former (%)")
      write.csv(table, paste0("~/CHD/Results/matched_", map$method, "_ratio", map$ratio, "_table_demo.csv"))
      
      res <- data.frame(as.list(c(map, match.out$res)))
      res <- with(map, data.frame(map_idx=map_idx, seed=seed,
                                  res))
      return(res)
    })
    
    out <- do.call(rbind.data.frame, out)
    
    out_combined <- rbind.fill(out_combined, out)
  }
  
  write.csv(out_combined, "~/CHD/Results/res_combined.csv", row.names = F)
}




### SMD visualization
# match.out <- readRDS("../Data/matched_nearest_ratio1.rds")
# 
# data <- match.out$m.dat
# 
# res <- summary(match.out$res)
# 
# res <- read.csv("~/CHD/Results/res_combined.csv")
# p.dat <- res %>%
#   mutate(method = factor(method, levels =rev(c("null", "quick", "nearest"))),
#          group = paste0(method, ratio)) %>%
#   dplyr::select(c("group","method", "ratio", ends_with(".SMD"))) %>%
#   rename_all(list(~stringr::str_replace_all(., "\\.SMD", "")))
# 
# p.dat <- reshape2::melt(p.dat, id.vars = "group")
# # p.dat <- merge(p.dat, label_dict, by.x="variable", by.y ="varname", all.x = T)
# 
# # add dummy values to method null
# # p.dat$value[which(p.dat$method == "null")] = 1:943/1000
# 
# ggplot(p.dat, aes(x = value, y = variable)) +
#   geom_vline(xintercept = 0.1, linetype="dotted", colour ="grey") +
#   geom_line(aes(group =group, colour= group), orientation = "y", alpha = 0.2) +
#   geom_point(aes(group=variable, colour = group), data = subset(p.dat, method != "null"),colour="white") +
#   geom_point(aes(group=variable, colour = group), shape= 3, size = 4, data = subset(p.dat, method == "null")) +
#   geom_point(aes(group=variable, colour = group), shape = 21, size = 2, data = subset(p.dat, method != "null"), alpha = 0.5) +
#   theme_bw() +
#   scale_colour_tableau() +
#   new_scale_color() +
#   ylab("") +
#   xlab("SMD") +
#   geom_text(aes(label=distance.label), size = 1.3, hjust= 0.5, vjust = 0.5,data = subset(p.dat, method != "null")) +
#   scale_discrete_identity(
#     aesthetics = "label",
#     name = "Distance",
#     breaks = p.dat$distance.label,
#     labels = p.dat$distance,
#     guide = "legend"
#   ) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.text.x = element_text(face = "bold", hjust=0),
#         strip.background = element_rect(fill="lightgrey"),
#         legend.title = element_text(size = 6),
#         legend.text = element_text(size = 4),
#         legend.key.size = unit(0.2, "cm"),
#         legend.spacing.y = unit(0.1, 'cm'),
#         legend.margin = margin(10, 0, 0, 0)) 
