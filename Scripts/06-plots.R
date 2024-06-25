library(ggplot2)
library(dplyr)
library(reshape2)
library(ggthemes)
library(ggnewscale)
library(grid)
library(GGally)
library(grid)
library(pBrackets)
library(cowplot)
library(cobalt)

res <- read.csv("R:/home/CHD/Results/res_combined.csv")
p.dat <- res %>%
  mutate(method = factor(method, levels =rev(c("null", "quick", "nearest"))),
         group = paste0(method, ratio)) %>%
  dplyr::select(c("group","method", "ratio", ends_with(".SMD"))) %>%
  rename_all(list(~stringr::str_replace_all(., "\\.SMD", "")))

p.dat <- reshape2::melt(p.dat, id.vars = c("group","method", "ratio"))
# p.dat <- merge(p.dat, label_dict, by.x="variable", by.y ="varname", all.x = T)

# add dummy values to method null
# p.dat$value[which(p.dat$method == "null")] = 1:943/1000


ggplot(p.dat, aes(x = value, y = variable)) +
  geom_vline(xintercept = 0.1, linetype="dotted", colour ="grey") +
  geom_line(aes(group =group, colour= group), orientation = "y", alpha = 0.2) +
  geom_point(aes(group=variable, colour = group), data = subset(p.dat, method != "null"),colour="white") +
  geom_point(aes(group=variable, colour = group), shape= 3, size = 4, data = subset(p.dat, method == "null")) +
  geom_point(aes(group=variable, colour = group), shape = 21, size = 2, data = subset(p.dat, method != "null"), alpha = 0.5) +
  theme_bw() +
  scale_colour_tableau() +
  new_scale_color() +
  ylab("") +
  xlab("SMD")

  geom_text(aes(label=distance.label), size = 1.3, hjust= 0.5, vjust = 0.5,data = subset(p.dat, method != "null")) +
  scale_discrete_identity(
    aesthetics = "label",
    name = "Distance",
    breaks = p.dat$distance.label,
    labels = p.dat$distance,
    guide = "legend"
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(face = "bold", hjust=0),
        strip.background = element_rect(fill="lightgrey"),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 4),
        legend.key.size = unit(0.2, "cm"),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.margin = margin(10, 0, 0, 0))


### Use COBALT
match.out <- readRDS("R:/home/CHD/Data/matched_quick_ratio1.rds")
m.out <- match.out$m.out
bal.tab(m.out, un = T, weights = m.out$weights)
sum(is.na(m.out$subclass))

bal.plot(m.out, var.name = "distance", which = "both")

stat_coxph <- function(data, covars, weights = NULL) {
  form0 <- as.formula(paste0("Surv(time, event) ~ ", paste0(covars, collapse = "+")))
  
  res.cox <- coxph(form0, data = data, weights = weights, robust = T, cluster = subclass)
  print(summary(res.cox))
}

data <- data_filtered

data <- match.out$m.dat

covars <- colnames(data)[!colnames(data) %in% c("eid", "time", "event", "subclass", "weights", "sexMale")]
covars <- covars[-grep("assessment_centre|imd", covars)]
