### Present Bias in Quasi-Hyperbolic Discounting: A Meta-Analysis ###


# Clear environment
rm(list=ls())

# Load packages
library(readxl)
library(ggplot2)
library(ggpubr)
library(DescTools)
library(lmtest)
library(metafor)
library(sandwich)
library(dplyr)
library(tidyr)
library(xtable)
library(stargazer)
library("data.table")
library(MetaStudies)
library(phacking)
library(plm)
library(ivreg)
library(MAIVE)
library(puniform)
library(corrplot)
library(BMS)
library(car)
library(fwildclusterboot)
library(foreign)
library(LowRankQP)
library(ggtext)
library(stringr)
library(reshape2)


# Import dataset 
setwd("~/Documents/IES/Bachelor's Thesis/Present Bias in Quasi-hyperbolic Discounting")
df<-read_excel("beta_data.xlsx")


#------------------------------
# Overview of Data
#------------------------------

# Set options to avoid scientific notation and limit to 4 decimal places
options(scipen = 999, digits = 3)
summary(df)

# Define individual
df$individual <- abs(df$aggregate-1)

# Table of study characteristics
table_study_characteristics <- function(df){
  study.data <- df %>%
  group_by(study_id) %>%
  summarise(
    n_estimates  = n(),
    #reporting of estimates
    aggregate    = any(aggregate == 1),
    individual   = any(aggregate == 0),
    appendix     = any(appendix == 1),
    
    #treatment
    treatment    = any(treatment == 1),
    
    #data type
    experiment   = any(experiment == 1),
    survey       = any(survey == 1),
    longitudinal = any(longitudinal == 1),
    
    #study setting
    lab          = any(lab == 1),
    field        = any(field == 1),
    online       = any(online == 1),
    school_work  = any(school_work == 1),
    
    #geography
    africa       = any(africa == 1),
    north_america= any(north_america == 1),
    south_america= any(south_america == 1),
    europe       = any(europe == 1),
    asia         = any(asia == 1),
    australia    = any(australia == 1),
    
    #sample
    subsample    = any(subsample == 1),
    students     = any(uni_students == 1),
    general_pop  = any(general_pop == 1),
    adolescents  = any(adolescents == 1),
    clinical     = any(clinical_pop == 1),
    
    #reward domain
    real_reward  = any(real == 1),
    money_reward = any(money == 1),
    effort_reward= any(real_effort == 1),
    food         = any(food == 1),
    health       = any(health == 1),
    uncertainty  = any(deal_uncertainty == 1),
    transaction  = any(deal_transaction_cost == 1),
    
    #elicitation method
    mpl          = any(mpl == 1),
    matching     = any(matching == 1),
    ctb          = any(ctb == 1),
    wage         = any(wage == 1),
    fed          = any(FED == 1, na.rm = T),
    
    #reward timing
    end          = any(end == 1),
    day          = any(day == 1),
    diff_day     = any(diff_day == 1),
    
    #utility control
    util_control = any(utility_control == 1),
    risk_prefer  = any(risk_prefer == 1),
    intertemp_sub= any(intertemp_sub == 1),
    CRRA         = any(CRRA == 1),
    
    #estimation method
    switch_point = any(switch_point == 1),
    ml           = any(ml == 1),
    ols          = any(ols == 1),
    nls          = any(nls == 1),
    tobit        = any(tobit == 1),
    joint        = any(joint_estimation == 1),
    
    #background consumption
    fixed        = any(fixed == 1),
    estimated    = any(estimated == 1),
    
    #discipline
    economics    = any(economics == 1),
    neuroscience = any(neuroscience == 1),
    psychology   = any(psychology == 1),
    
    #publication
    published    = any(published == 1),
    not_published    = any(published == 0),
    published_top5= any(pub_top_five == 1),
    
    .groups = "drop"
    )

N <- nrow(study.data)

sstat.name <- c(
  # Reporting of estimates
  "Aggregate-level",
  "Individual-level",
  "Estimates reported in appendix",
  
  # Treatment
  "Treatment",
  
  # Data type
  "Experimental data",
  "Survey data",
  "Longitudinal data",
  
  # Study setting
  "Laboratory",
  "Field",
  "Online",
  "School or workplace",
  
  # Geography
  "Africa",
  "North America",
  "South America",
  "Europe",
  "Asia",
  "Australia",
  
  # Sample
  "Subsample analysis",
  "University students",
  "General population",
  "Adolescents",
  "Clinical population",
  
  # Reward domain
  "Real incentives",
  "Monetary",
  "Real Effort",
  "Food",
  "Health",
  "Reward uncertainty accounted for",
  "Transaction costs accounted for",
  
  # Elicitation method
  "Multiple price list",
  "Matching task",
  "Convex time budget",
  "Wage-based elicitation",
  "Front-end delay",
  
  # Reward timing
  "Immediate (end of experiment)",
  "Same day",
  "Different day",
  
  # Utility controls
  "Controls for utility",
  "Risk preferences",
  "Intertemporal substitution",
  "CRRA",
  
  # Estimation method
  "From switching points",
  "Maximum likelihood",
  "OLS",
  "NLS",
  "Tobit",
  "Joint estimation",
  
  # Background consumption
  "Fixed background consumption",
  "Estimated background consumption",
  
  # Discipline
  "Economics",
  "Neuroscience",
  "Psychology",
  
  #Publication
  "Published",
  "Not Published",
  "Published Top Five"
)

sstat.freq <- c(
  # Reporting of estimates
  sum(study.data$aggregate),
  sum(study.data$individual),
  sum(study.data$appendix),
  
  # Treatment
  sum(study.data$treatment),
  
  # Data type
  sum(study.data$experiment),
  sum(study.data$survey),
  sum(study.data$longitudinal),
  
  # Study setting
  sum(study.data$lab),
  sum(study.data$field),
  sum(study.data$online),
  sum(study.data$school_work),
  
  # Geography
  sum(study.data$africa),
  sum(study.data$north_america),
  sum(study.data$south_america),
  sum(study.data$europe),
  sum(study.data$asia),
  sum(study.data$australia),
  
  # Sample
  sum(study.data$subsample),
  sum(study.data$students),
  sum(study.data$general_pop),
  sum(study.data$adolescents),
  sum(study.data$clinical),
  
  # Reward domain
  sum(study.data$real_reward),
  sum(study.data$money_reward),
  sum(study.data$effort_reward),
  sum(study.data$food),
  sum(study.data$health),
  sum(study.data$uncertainty),
  sum(study.data$transaction),
  
  # Elicitation method
  sum(study.data$mpl),
  sum(study.data$matching),
  sum(study.data$ctb),
  sum(study.data$wage),
  sum(study.data$fed),
  
  # Reward timing
  sum(study.data$end),
  sum(study.data$day),
  sum(study.data$diff_day),
  
  # Utility controls
  sum(study.data$util_control),
  sum(study.data$risk_prefer),
  sum(study.data$intertemp_sub),
  sum(study.data$CRRA),
  
  # Estimation method
  sum(study.data$switch_point),
  sum(study.data$ml),
  sum(study.data$ols),
  sum(study.data$nls),
  sum(study.data$tobit),
  sum(study.data$joint),
  
  # Background consumption
  sum(study.data$fixed),
  sum(study.data$estimated),
  
  # Discipline
  sum(study.data$economics),
  sum(study.data$neuroscience),
  sum(study.data$psychology),
  
  # Publication
  sum(study.data$published),
  sum(study.data$not_published),
  sum(study.data$published_top5)
)

sstat.perc <- formatC(100 * sstat.freq / N, format = "f", digits = 1)


table.1 <- data.frame(
  Variable   = sstat.name,
  Frequency  = sstat.freq,
  Proportion = sstat.perc
)

print(table.1)
}
table_study_characteristics(df)


summary_table <- df %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    n    = sum(!is.na(value)),
    .groups = "drop"
  )

print(summary_table, n=nrow(summary_table))


#------------------------------------
# Winsorization and Data Preparation
#------------------------------------

# Set Winsorization level
win_level <- 0.05 # <- set winsorization level

# Winsorize data
df$win_beta_estimate <- Winsorize(df$beta_estimate, val=quantile(df$beta_estimate, probs=c(win_level,1 - win_level)))
df$win_beta_se <- Winsorize(df$beta_se, val=quantile(df$beta_se, probs=c(win_level,1 - win_level)))

# Omit winsorized observations (for plots only)
lower <- quantile(df$beta_estimate, win_level, na.rm = TRUE)
upper <- quantile(df$beta_estimate, 1 - win_level, na.rm = TRUE)
lower_se <- quantile(df$beta_se, win_level, na.rm = TRUE)
upper_se <- quantile(df$beta_se, 1 - win_level, na.rm = TRUE)

df_plot <- df[df$beta_estimate >= lower & df$beta_estimate <= upper & df$beta_se >= lower_se & df$beta_se <= upper_se, ]

summary(df$beta_estimate)
summary(df$win_beta_estimate)
summary(df$beta_se)
summary(df$win_beta_se)


# Which estimates are winsorized?
winsorized_rows <- df[
  df$beta_estimate != df$win_beta_estimate |
    df$beta_se != df$win_beta_se,
  c("study_name",
    "beta_estimate", "win_beta_estimate",
    "beta_se", "win_beta_se")
]

print(winsorized_rows, n = nrow(winsorized_rows))


# Which study design features correlate with effect size or SE?
num_df <- df[sapply(df, is.numeric)]
target_vars <- c("win_beta_estimate", "win_beta_se")

corr_subset <- cor(
  num_df[, target_vars],
  num_df,
  use = "pairwise.complete.obs")

corr_subset

col= colorRampPalette(c("#FF474C", "white", "#00008B"))
corrplot.mixed(corr_subset, lower = "number", upper = "circle", lower.col=col(200), upper.col=col(200), tl.pos = c("lt"), diag = c("u"), tl.col="black", tl.srt=45, tl.cex=0.5, number.cex = 0.3, cl.cex=0.8, cl.ratio=0.1)


#------------------------------
# Histograms of Beta Estimates
#------------------------------

ggplot(df, aes(x = beta_estimate)) +
  geom_histogram(
    binwidth = 0.01,
    fill = "#2e00fa",
    color = "#2e00fa",
    alpha = 0.5
  ) +
  xlab("Beta Estimate") +
  ylab("Frequency") +
  geom_vline(xintercept = mean(df$win_beta_estimate), color = "red") +
  geom_vline(xintercept = median(df$win_beta_estimate), linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_blank()
  ) + xlim(0.5, 1.5)




# Plot histograms for different subsets
plots_all <- function(){

# Function for saving 
save_plot <- function(plot, filename) {
  ggsave(
    filename = filename,
    plot = plot,
    width = 5,
    height = 4,
    units = "in"
  )
}


# Aggregation

df$individual <- abs(df$aggregate - 1)
df_plot$individual <- abs(df_plot$aggregate - 1)

df_plot_agg <- df_plot %>% mutate(treatment = case_when( aggregate == 1 ~ "aggregate", individual == 1 ~ "individual")) %>% filter(!is.na(treatment))

df_plot_agg <- df_plot_agg %>%
  mutate(treatment = factor(treatment,
                            levels = c("aggregate", "individual"),
                            labels = c("Aggregate", "Individual")))

denisty_aggregation <- ggdensity(
  df_plot_agg,
  x       = "beta_estimate",
  color   = "treatment", 
  linetype = "treatment",  
  fill    = NA,             
  xlab    = "Beta Estimate",
  ylab    = "Density",
  palette = c("#2e00fa", "#ca0086")
) +
  theme(
    legend.position = c(0.2,0.8),
    legend.text = element_text(size = 14)
  ) +
  labs(color = NULL, fill = NULL, linetype = NULL)

denisty_aggregation



save_plot(denisty_aggregation, "density_aggregation.pdf")

histogram_aggregation <- gghistogram(df_plot_agg,
            x       = "beta_estimate",
            bins    = 100,
            color   = "treatment", 
            fill    = "treatment",
            xlab    = "Beta Estimate",
            ylab    = "Density",
            palette = c("#2e00fa", "#ca0086")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) + labs(color = NULL, fill = NULL) # coordinates in plot area (x, y)

histogram_aggregation

save_plot(histogram_aggregation, "histogram_aggregation.pdf")

# Treatment
df_plot_treat <- df_plot %>% mutate(treatment = case_when( treatment == 1 ~ "treatment", treatment == 0 ~ "no_treatment")) %>% filter(!is.na(treatment))

df_plot_treat <- df_plot_treat %>%
  mutate(treatment = factor(treatment,
                            levels = c("no_treatment", "treatment"),
                            labels = c("No Treatment", "Treatment")))

denisty_treatment <- ggdensity(df_plot_treat,
          x       = "beta_estimate",
          color   = "treatment", 
          linetype = "treatment",  
          fill    = NA,     
          xlab    = "Beta Estimate",
          ylab    = "Density",
          palette = c("#2e00fa", "#ca0086")
) + theme(legend.position = c(0.25,0.8), legend.text = element_text(size = 14)) + labs(color = NULL, fill = NULL, linetype = NULL) # coordinates in plot area (x, y)

denisty_treatment

save_plot(denisty_treatment, "density_treatment.pdf")

histogram_treatment <- gghistogram(df_plot_treat,
            x       = "beta_estimate",
            bins    = 100,
            color   = "treatment", 
            fill    = "treatment",
            xlab    = "Beta Estimate",
            ylab    = "Density",
            palette = c("#2e00fa", "#ca0086")
) + theme(legend.position = c(0.25,0.8), legend.text = element_text(size = 14)) + labs(color = NULL, fill = NULL) # coordinates in plot area (x, y)

save_plot(histogram_treatment, "histogram_treatment.pdf")

# Incentives
df_plot_incentives <- df_plot %>% mutate(treatment = case_when(real == 1 ~ "real", real == 0 ~ "hypothetical")) %>% filter(!is.na(treatment))

df_plot_incentives <- df_plot_incentives %>%
  mutate(treatment = factor(treatment,
                            levels = c("real", "hypothetical"),
                            labels = c("Real", "Hypothetical")))

denisty_incentives <- ggdensity(df_plot_incentives,
          x       = "beta_estimate",
          color   = "treatment", 
          fill    = "treatment",
          xlab    = "Beta Estimate",
          ylab    = "Density",
          palette = c("#2e00fa", "#ca0086")
) + theme(legend.position = c(0.25,0.8), legend.text = element_text(size = 14)) + labs(color = NULL, fill = NULL) # coordinates in plot area (x, y)

save_plot(denisty_incentives, "density_incentives.pdf")

histogram_incentives <- gghistogram(df_plot_incentives,
            x       = "beta_estimate",
            bins    = 100,
            color   = "treatment", 
            fill    = "treatment",
            xlab    = "Beta Estimate",
            ylab    = "Density",
            palette = c("#2e00fa", "#ca0086")
) + theme(legend.position = c(0.25,0.8), legend.text = element_text(size = 14)) + labs(color = NULL, fill = NULL) # coordinates in plot area (x, y)

save_plot(histogram_incentives, "histogram_incentives.pdf")

# Reward Domain 

df_plot_reward <- df_plot %>% mutate(treatment = case_when( money == 1 ~ "money", real_effort == 1 ~ "real_effort")) %>% filter(!is.na(treatment))

df_plot_reward <- df_plot_reward %>%
  mutate(treatment = factor(treatment,
                            levels = c("money", "real_effort"),
                            labels = c("Money", "Real Effort")))

denisty_reward <- ggdensity(df_plot_reward,
          x       = "beta_estimate",
          color   = "treatment", 
          fill    = "treatment",
          xlab    = "Beta Estimate",
          ylab    = "Density",
          palette = c("#2e00fa", "#ca0086")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) + labs(color = NULL, fill = NULL) # coordinates in plot area (x, y)

save_plot(denisty_reward, "density_reward.pdf")

histogram_reward <- gghistogram(df_plot_reward,
          x       = "beta_estimate",
          bins    = 100,
          color   = "treatment", 
          fill    = "treatment",
          xlab    = "Beta Estimate",
          ylab    = "Density",
          palette = c("#2e00fa", "#ca0086")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) + labs(color = NULL, fill = NULL) # coordinates in plot area (x, y)

save_plot(histogram_reward, "histogram_reward.pdf")


# Data type
df_plot_datatype <- df_plot %>% mutate(datatype = case_when(experiment   == 1 ~ "experiment", survey       == 1 ~ "survey",)) %>% filter(!is.na(datatype))

df_plot_datatype <- df_plot_datatype %>%
  mutate(datatype = factor(datatype,
                            levels = c("experiment", "survey"),
                            labels = c("Experiment", "Survey")))

denisty_datatype <- ggdensity(df_plot_datatype,
          x       = "beta_estimate",
          color   = "datatype", 
          fill    = "datatype",
          xlab    = "Beta Estimate",
          ylab    = "Density",
          palette = c("#2e00fa", "#ca0086")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) + labs(color = NULL, fill = NULL) # coordinates in plot area (x, y)

save_plot(denisty_datatype, "density_datatype.pdf")

histogram_datatype <- gghistogram(df_plot_datatype,
            x       = "beta_estimate",
            bins    = 100,
            color   = "datatype", 
            fill    = "datatype",
            xlab    = "Beta Estimate",
            ylab    = "Density",
            palette = c("#2e00fa", "#ca0086")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) + labs(color = NULL, fill = NULL) # coordinates in plot area (x, y)

save_plot(histogram_datatype, "histogram_datatype.pdf")

# Study Setting
df_plot_setting <- df_plot %>%
  mutate(category = case_when(
    lab         == 1 ~ "lab",
    field       == 1 ~ "field",
    online      == 1 ~ "online",
    school_work == 1 ~ "school_work"
  )) %>%
  filter(!is.na(category)) %>%
  mutate(setting = factor(category,   # Use 'category' here
                          levels = c("lab", "field",  "online", "school_work"),
                          labels = c("Lab", "Field", "Online", "School/Work")))

# Density plot
denisty_setting <- ggdensity(df_plot_setting,
          x       = "beta_estimate",
          color   = "setting", 
          linetype = "setting",  
          fill    = NA,   
          alpha = 0.1,
          xlab    = "Beta Estimate",
          ylab    = "Density",
          palette = c("#2e00fa", "#a000bc", "black", "#e40058")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) + labs(color = NULL, fill = NULL, linetype = NULL)


save_plot(denisty_setting, "density_setting.pdf")

# Histogram
histogram_setting <- gghistogram(df_plot_setting,
            x       = "beta_estimate",
            bins    = 100,
            color   = "setting", 
            fill    = "setting",
            xlab    = "Beta Estimate",
            ylab    = "Density",
            palette = c("#2e00fa", "#a000bc", "black", "#e40058")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) +
  labs(color = NULL, fill = NULL)

save_plot(histogram_setting, "histogram_setting.pdf")

# Geography
df_plot_geo <- df_plot %>%
  mutate(geo = case_when(
    north_america == 1 ~ "North America",
    europe        == 1 ~ "Europe",
    asia          == 1 ~ "Asia"
  )) %>%
  filter(!is.na(geo))

# Density plot
denisty_geo <- ggdensity(df_plot_geo,
          x       = "beta_estimate",
          color   = "geo", 
          linetype = "geo",  
          fill    = NA,
          alpha = 0.1,
          xlab    = "Beta Estimate",
          ylab    = "Density",
          palette = c("#2e00fa", "#a000bc", "#e40058")
) + theme(legend.position = c(0.25,0.8), legend.text = element_text(size = 14)) +
  labs(color = NULL, fill = NULL, linetype=NULL)

denisty_geo

save_plot(denisty_geo, "density_geo.pdf")

# Histogram
histogram_geo <- gghistogram(df_plot_geo,
            x       = "beta_estimate",
            bins    = 100,
            color   = "geo", 
            fill    = "geo",
            xlab    = "Beta Estimate",
            ylab    = "Density",
            palette = c("#2e00fa", "#a000bc",  "#e40058")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) +
  labs(color = NULL, fill = NULL)

save_plot(histogram_geo, "histogram_geo.pdf")

# Elicitation method
df_plot_elicitation <- df_plot %>%
  mutate(category = case_when(
    wage     == 1 ~ "WfW",
    mpl      == 1 ~ "MPL",
    matching == 1 ~ "Matching",
    ctb      == 1 ~ "CTB"
  )) %>%
  filter(!is.na(category))


# Density plot
denisty_elicitation <- ggdensity(df_plot_elicitation,
          x       = "beta_estimate",
          color   = "category", 
          linetype = "category",  
          fill    = NA,
          alpha = 0.1,
          xlab    = "Beta Estimate",
          ylab    = "Density",
          palette = c("#2e00fa", "#a000bc", "black", "#e40058")
) + theme(legend.position = c(0.19,0.8), legend.text = element_text(size = 14)) +
  labs(color = NULL, fill = NULL, linetype = NULL)

denisty_elicitation
save_plot(denisty_elicitation, "density_elicitation.pdf")

# Histogram
histogram_elicitation <- gghistogram(df_plot_elicitation,
            x       = "beta_estimate",
            bins    = 100,
            color   = "category", 
            fill    = "category",
            xlab    = "Beta Estimate",
            ylab    = "Density",
            palette = c("#2e00fa", "#a000bc", "black", "#e40058")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) +
  labs(color = NULL, fill = NULL)

save_plot(histogram_elicitation, "histogram_elicitation.pdf")

# Estimation method
df_plot_estimation <- df_plot %>%
  mutate(category = case_when(
    switch_point     == 1 ~ "Switch Point",
    ml               == 1 ~ "ML",
    nls              == 1 ~ "NLS",
    tobit            == 1 ~ "Tobit"
  )) %>%
  filter(!is.na(category))


# Density plot
denisty_estimation <- ggdensity(df_plot_estimation,
          x       = "beta_estimate",
          color   = "category", 
          linetype = "category",  
          fill    = NA,
          alpha = 0.1,
          xlab    = "Beta Estimate",
          ylab    = "Density",
          palette = c("#2e00fa", "#a000bc", "black", "#e40058")
) + theme(legend.position = c(0.25,0.8), legend.text = element_text(size = 14)) +
  labs(color = NULL, fill = NULL, linetype = NULL)

denisty_estimation

save_plot(denisty_estimation, "density_estimation.pdf")

# Histogram
histogram_estimation <- gghistogram(df_plot_estimation,
            x       = "beta_estimate",
            bins    = 100,
            color   = "category", 
            fill    = "category",
            xlab    = "Beta Estimate",
            ylab    = "Density",
            palette = c("#2e00fa", "#a000bc", "black", "#e40058")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) +
  labs(color = NULL, fill = NULL)

save_plot(histogram_estimation, "histogram_estimation.pdf")

# Subject Pool
df_plot_subject <- df_plot %>%
  mutate(category = case_when(
    general_pop               == 1 ~ "General Population",
    adolescents              == 1 ~ "Adolescents",
    uni_students     == 1 ~ "University Students"
  )) %>%
  filter(!is.na(category))

# Density plot
denisty_sample <- ggdensity(df_plot_subject,
          x       = "beta_estimate",
          color   = "category", 
          linetype = "category",  
          fill    = NA,
          alpha = 0.1,
          xlab    = "Beta Estimate",
          ylab    = "Density",
          palette = c("#2e00fa", "#a000bc", "#e40058")
) + theme(legend.position = c(0.3,0.8), legend.text = element_text(size = 14)) +
  labs(color = NULL, fill = NULL, linetype=NULL)

denisty_sample

save_plot(denisty_sample, "density_sample.pdf")

# Histogram
histogram_sample <- gghistogram(df_plot_subject,
            x       = "beta_estimate",
            bins    = 100,
            color   = "category", 
            fill    = "category",
            xlab    = "Beta Estimate",
            ylab    = "Density",
            palette = c("#2e00fa", "#a000bc", "#e40058")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) +
  labs(color = NULL, fill = NULL)

save_plot(histogram_sample, "histogram_sample.pdf")

# Payment Method
df_plot_payment <- df_plot %>%
  mutate(category = case_when(
    soon_cash     == 1 ~ "Cash",
    soon_check               == 1 ~ "Check",
    soon_bank              == 1 ~ "Bank Transfer",
    soon_paypal   == 1 ~ "Digital Transfer"
  )) %>%
  filter(!is.na(category))

# Density plot
denisty_payment <- ggdensity(df_plot_payment,
          x       = "beta_estimate",
          color   = "category", 
          linetype = "category",  
          fill    = NA,
          alpha = 0.1,
          xlab    = "Beta Estimate",
          ylab    = "Density",
          palette = c("#2e00fa", "#a000bc","black", "#e40058")
) + theme(legend.position = c(0.25,0.8), legend.text = element_text(size = 14)) +
  labs(color = NULL, fill = NULL, linetype = NULL)

denisty_payment

save_plot(denisty_payment, "density_payment.pdf")

# Histogram
histogram_payment <- gghistogram(df_plot_payment,
            x       = "beta_estimate",
            bins    = 100,
            color   = "category", 
            fill    = "category",
            xlab    = "Beta Estimate",
            ylab    = "Density",
            palette = c("#2e00fa", "#a000bc","black", "#e40058")
) + theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 14)) +
  labs(color = NULL, fill = NULL)

save_plot(histogram_payment, "histogram_payment.pdf")

}

# Save plots 
#plots_all()


# Plot estimates in time (years of publication)
ggplot(df_plot, aes(x = pub_year, y = beta_estimate)) + 
  geom_point(aes(size = 1/beta_se), color="blue")

ggplot(df, aes(x = pub_year, y = win_beta_estimate)) +
  geom_point(aes(size = 1/win_beta_se), alpha = 0.6, color="blue") +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  scale_size_continuous(name = "Precision (1/SE)") +
  coord_cartesian(
    xlim = c(2008, 2025),
    ylim = c(0.4, 1.7)
  )

ggplot(df_plot, aes(x = pub_year, y = beta_estimate)) +
  geom_point(aes(size = 1/beta_se), alpha = 0.6, color="blue") +
  geom_smooth(
    method = "lm",
    aes(weight = 1/beta_se),
    se = TRUE,
    color = "red"
  ) +
  scale_size_continuous(name = "Precision (1/SE)") +
  theme_minimal()



#------------------------------
# Forest Plot
#------------------------------

df_forest <- df#[df$no_estimates>1,]

# Fix name
df_forest$study_name <- gsub("Bartoš et al\\.", "Bartos et al.", df_forest$study_name)

# Order alphabetically 
df_forest$study_name <- factor(df_forest$study_name, levels = sort(unique(df_forest$study_name), decreasing = TRUE))

#Create the forest plot across studies
ggplot(df_forest, aes(x = beta_estimate, y = study_name)) +
  geom_boxplot(fill = "#2e00fa", alpha=0.4, color="#2e00fa") +  
  ylab("Study") +
  xlab("Beta Estimate") +
  geom_vline(xintercept = mean(df$win_beta_estimate), linetype = "solid", color = "red") +
  geom_vline(xintercept = median(df$win_beta_estimate), linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0.25, 2)) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.2),
    axis.line.y = element_line(color = "black", linewidth = 0.2),
    axis.text.x = element_text(hjust = 1)
  ) +
  guides(fill = FALSE)




#------------------------------
# Meta-Analytic Average
#------------------------------

# Unweighted and weighted means for subsets
means <- function(){

# Define function for unweighted and weighted mean and confidence interval
mean_ci <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x  <- x[ok]
  w  <- w[ok]
  
  # Unweighted
  m  <- mean(x)
  se <- sd(x) / sqrt(length(x))
  
  # Weighted
  wm <- sum(w * x) / sum(w)
  var_w <- sum(w * (x - wm)^2) / (sum(w) - (sum(w^2)/sum(w)))
  se_w  <- sqrt(var_w)
  
  c(
    mean       = m,
    ci_lower   = m - 1.96 * se,
    ci_upper   = m + 1.96 * se,
    w_mean     = wm,
    w_ci_lower = wm - 1.96 * (se_w/sqrt(sum(w))),
    w_ci_upper = wm + 1.96 * (se_w/sqrt(sum(w))),
    N_obs      = length(x)
  )
}

# Define inv_no_estimates for weights
df <- df %>%
  group_by(study_id) %>%
  mutate(no_estimates = n()) %>%
  ungroup()
#View(df)
df$inv_no_estimates <- 1 / df$no_estimates

# Create result matrix for table 
ci_mat <- rbind(
  mean_ci(df$win_beta_estimate, df$inv_no_estimates),
  
  mean_ci(df$win_beta_estimate[df$aggregate == 1],
          df$inv_no_estimates[df$aggregate == 1]),
  mean_ci(df$win_beta_estimate[df$aggregate == 0],
          df$inv_no_estimates[df$aggregate == 0]),
  
  mean_ci(df$win_beta_estimate[df$treatment == 1],
          df$inv_no_estimates[df$treatment == 1]),
  mean_ci(df$win_beta_estimate[df$treatment == 0],
          df$inv_no_estimates[df$treatment == 0]),
  
  mean_ci(df$win_beta_estimate[df$money == 1],
          df$inv_no_estimates[df$money == 1]),
  mean_ci(df$win_beta_estimate[df$real_effort == 1],
          df$inv_no_estimates[df$real_effort == 1]),
  mean_ci(df$win_beta_estimate[df$food == 1],
          df$inv_no_estimates[df$food == 1]),
  mean_ci(df$win_beta_estimate[df$health == 1],
          df$inv_no_estimates[df$health == 1]),
  mean_ci(df$win_beta_estimate[df$environmental == 1],
          df$inv_no_estimates[df$environmental == 1]),
  
  mean_ci(df$win_beta_estimate[df$real == 1],
          df$inv_no_estimates[df$real == 1]),
  mean_ci(df$win_beta_estimate[df$real == 0],
          df$inv_no_estimates[df$real == 0]),
  
  mean_ci(df$win_beta_estimate[df$experiment == 1],
          df$inv_no_estimates[df$experiment == 1]),
  mean_ci(df$win_beta_estimate[df$survey == 1],
          df$inv_no_estimates[df$survey == 1]),
  
  mean_ci(df$win_beta_estimate[df$lab == 1],
          df$inv_no_estimates[df$lab == 1]),
  mean_ci(df$win_beta_estimate[df$field == 1],
          df$inv_no_estimates[df$field == 1]),
  mean_ci(df$win_beta_estimate[df$school_work == 1],
          df$inv_no_estimates[df$school_work == 1]),
  mean_ci(df$win_beta_estimate[df$online == 1],
          df$inv_no_estimates[df$online == 1]),
  
  mean_ci(df$win_beta_estimate[df$africa == 1],
          df$inv_no_estimates[df$africa == 1]),
  mean_ci(df$win_beta_estimate[df$north_america == 1],
          df$inv_no_estimates[df$north_america == 1]),
  mean_ci(df$win_beta_estimate[df$south_america == 1],
          df$inv_no_estimates[df$south_america == 1]),
  mean_ci(df$win_beta_estimate[df$europe == 1],
          df$inv_no_estimates[df$europe == 1]),
  mean_ci(df$win_beta_estimate[df$asia == 1],
          df$inv_no_estimates[df$asia == 1]),
  mean_ci(df$win_beta_estimate[df$australia == 1],
          df$inv_no_estimates[df$australia == 1]),
  
  mean_ci(df$win_beta_estimate[df$uni_students == 1],
          df$inv_no_estimates[df$uni_students == 1]),
  mean_ci(df$win_beta_estimate[df$general_pop == 1],
          df$inv_no_estimates[df$general_pop == 1]),
  mean_ci(df$win_beta_estimate[df$adolescents == 1],
          df$inv_no_estimates[df$adolescents == 1]),
  mean_ci(df$win_beta_estimate[df$clinical_pop == 1],
          df$inv_no_estimates[df$clinical_pop == 1]),
  
  mean_ci(df$win_beta_estimate[df$mpl == 1],
          df$inv_no_estimates[df$mpl == 1]),
  mean_ci(df$win_beta_estimate[df$matching == 1],
          df$inv_no_estimates[df$matching == 1]),
  mean_ci(df$win_beta_estimate[df$ctb == 1],
          df$inv_no_estimates[df$ctb == 1]),
  mean_ci(df$win_beta_estimate[df$wage == 1],
          df$inv_no_estimates[df$wage == 1]),
  mean_ci(df$win_beta_estimate[df$elicit_other == 1],
          df$inv_no_estimates[df$elicit_other == 1]),
  
  mean_ci(df$win_beta_estimate[df$switch_point == 1],
          df$inv_no_estimates[df$switch_point == 1]),
  mean_ci(df$win_beta_estimate[df$ml == 1],
          df$inv_no_estimates[df$ml == 1]),
  mean_ci(df$win_beta_estimate[df$ols == 1],
          df$inv_no_estimates[df$ols == 1]),
  mean_ci(df$win_beta_estimate[df$nls == 1],
          df$inv_no_estimates[df$nls == 1]),
  mean_ci(df$win_beta_estimate[df$tobit == 1],
          df$inv_no_estimates[df$tobit == 1]),
  mean_ci(df$win_beta_estimate[df$estimate_other == 1],
          df$inv_no_estimates[df$estimate_other == 1]),
  
  mean_ci(df$win_beta_estimate[df$end == 1],
          df$inv_no_estimates[df$end == 1]),
  mean_ci(df$win_beta_estimate[df$day == 1],
          df$inv_no_estimates[df$day == 1]),
  mean_ci(df$win_beta_estimate[df$diff_day == 1],
          df$inv_no_estimates[df$diff_day == 1]),
  
  mean_ci(df$win_beta_estimate[df$payment_same == 1],
          df$inv_no_estimates[df$payment_same == 1]),
  mean_ci(df$win_beta_estimate[df$payment_same == 0],
          df$inv_no_estimates[df$payment_same == 0]),
  
  mean_ci(df$win_beta_estimate[df$soon_cash == 1],
          df$inv_no_estimates[df$soon_cash == 1]),
  mean_ci(df$win_beta_estimate[df$soon_check == 1],
          df$inv_no_estimates[df$soon_check == 1]),
  mean_ci(df$win_beta_estimate[df$soon_bank == 1],
          df$inv_no_estimates[df$soon_bank == 1]),
  mean_ci(df$win_beta_estimate[df$soon_voucher == 1],
          df$inv_no_estimates[df$soon_voucher == 1]),
  mean_ci(df$win_beta_estimate[df$soon_paypal == 1],
          df$inv_no_estimates[df$soon_paypal == 1]),
  mean_ci(df$win_beta_estimate[df$soon_postal == 1],
          df$inv_no_estimates[df$soon_postal == 1]),
  
  mean_ci(df$win_beta_estimate[df$economics == 1],
          df$inv_no_estimates[df$economics == 1]),
  mean_ci(df$win_beta_estimate[df$neuroscience == 1],
          df$inv_no_estimates[df$neuroscience == 1]),
  mean_ci(df$win_beta_estimate[df$psychology == 1],
          df$inv_no_estimates[df$psychology == 1]),
  
  mean_ci(df$win_beta_estimate[df$published == 1],
          df$inv_no_estimates[df$published == 1]),
  mean_ci(df$win_beta_estimate[df$published == 0],
          df$inv_no_estimates[df$published == 0]),
  mean_ci(df$win_beta_estimate[df$pub_top_five == 1],
          df$inv_no_estimates[df$pub_top_five == 1]),
  
  mean_ci(df$win_beta_estimate[df$FED == 1],
          df$inv_no_estimates[df$FED == 1]),
  mean_ci(df$win_beta_estimate[df$FED == 0],
          df$inv_no_estimates[df$FED == 0]),
  
  mean_ci(df$win_beta_estimate[df$utility_control == 1],
          df$inv_no_estimates[df$utility_control == 1]),
  mean_ci(df$win_beta_estimate[df$utility_control == 0],
          df$inv_no_estimates[df$utility_control == 0]),
  mean_ci(df$win_beta_estimate[df$deal_uncertainty == 1],
          df$inv_no_estimates[df$deal_uncertainty == 1]),
  mean_ci(df$win_beta_estimate[df$deal_uncertainty == 0],
          df$inv_no_estimates[df$deal_uncertainty == 0]),
  mean_ci(df$win_beta_estimate[df$deal_transaction_cost == 1],
          df$inv_no_estimates[df$deal_transaction_cost == 1]),
  mean_ci(df$win_beta_estimate[df$deal_transaction_cost == 0],
          df$inv_no_estimates[df$deal_transaction_cost == 0])

)


table.2 <- data.frame(
  category   = c(
    "All",
    "Aggregate", "Individual", "Treatment", "Neutral Treatment",
    "Money", "Real effort", "Food", "Health", "Environmental",
    "Real", "Hypothetical",
    "Experiment", "Survey",
    "Lab", "Field", "School/Workplace", "Online",
    "Africa", "North America", "South America", "Europe", "Asia", "Australia",
    "Students", "General population", "Adolescents", "Clinical",
    "MPL", "Matching", "CTB", "Wage", "Elicitation Other",
    "Switch point", "ML", "OLS", "NLS", "Tobit", "Estimation Other",
    "End of Experiment", "End of Day", "Different Day",
    "Same payment method", "Different payment method",
    "Cash", "Check", "Bank", "Voucher", "Paypal", "Postal",
    "Economics", "Neuroscience", "Psychology", 
    "Published", "Not published", "Published top 5",
    "FED", "No FED",
    "Control for Utility", "No Control for Utility",
    "Control for uncertainty",  "No Control for uncertainty",
    "Control for Transaction Costs",   "No Control for Transaction Costs"
  ),
  Mean       = ci_mat[, "mean"],
  CI_lower  = ci_mat[, "ci_lower"],
  CI_upper  = ci_mat[, "ci_upper"],
  Weighted_Mean    = ci_mat[, "w_mean"],
  CI_lower_w = ci_mat[, "w_ci_lower"],
  CI_upper_w = ci_mat[, "w_ci_upper"],
  N_obs     = ci_mat[, "N_obs"]
)

print(table.2)

#print(xtable(table.2),file = "table_2.tex",type = "latex",include.rownames = FALSE,comment = FALSE)
}

means()


#------------------------------
# Publication Bias
#------------------------------


#------------------------------
# Funnel Plot
#------------------------------

#Function to create funnel plot
create_funnel_plot <- function(df_plot, x_column, y_column, variable, main_title) {
  # Load necessary packages
  if (!requireNamespace("stats", quietly = TRUE)) {
    install.packages("stats")
  }
  
  library(stats)
  
  # Calculate Precision
  df_plot$Precision <- 1 / df_plot$beta_se
  
  # Calculate mean and median
  mean_x <- mean(df$win_beta_estimate, na.rm = TRUE)
  median_x <- median(df$win_beta_estimate, na.rm = TRUE)
  
  # Create funnel plot
  plot(df_plot[[x_column]], df_plot[[y_column]],
       pch = 21,
       bg = adjustcolor("#2e00fa", alpha.f = 0.1),  
       col = "#2e00fa",                             
       cex = 0.6,
       xlim = range(0.5,1.5), ylim = range(0,500),
       ylab = "Precision of Beta Estimate (1/SE)", 
       xlab = variable,
       bty = "l")
  
  
  abline(v = mean_x, lty = 1, lwd = 1.5, col = "red")
  abline(v = median_x, lty = 2, lwd = 1.5, col = "red")
}


# Use the function to create the funnel plot for all betas
create_funnel_plot(df_plot, "beta_estimate", "Precision", "Beta Estimates", "Funnel Plot")
create_funnel_plot(df, "beta_estimate", "Precision", "Beta Estimates", "Funnel Plot")


# Use the function to create funnel plots for different subsets
create_funnel_plot(df[df$money == 1,], "beta_estimate", "Precision", "Beta Estimates", "Funnel Plot for Money Reward")
create_funnel_plot(df[df$real_effort == 1,], "beta_estimate", "Precision", "Beta Estimates", "Funnel Plot for Real Effort")
create_funnel_plot(df[df$real == 1,], "beta_estimate", "Precision", "Beta Estimates", "Funnel Plot for Real Incentives")
create_funnel_plot(df[df$real == 0,], "beta_estimate", "Precision", "Beta Estimates", "Funnel Plot for Non-Real Incentives")
create_funnel_plot(df[df$treatment == 1,], "beta_estimate", "Precision", "Beta Estimates", "Funnel Plot for Treatment")
create_funnel_plot(df[df$treatment == 0,], "beta_estimate", "Precision", "Beta Estimates", "Funnel Plot for Neutral Treament")
create_funnel_plot(df[df$aggregate == 1,], "beta_estimate", "Precision", "Beta Estimates", "Funnel Plot for Aggregate")
create_funnel_plot(df[df$aggregate == 0,], "beta_estimate", "Precision", "Beta Estimates", "Funnel Plot for Individual")


#------------------------------
# Linear Techniques
#------------------------------

LT <- function(win_beta_estimate, win_beta_se, df, df_plot, label){

# OLS
cat("\n============================================================\n")
cat("OLS Results:", label, "\n")
  
OLS_model <- lm(win_beta_estimate ~ win_beta_se, data = df)
clustered_OLS <- coeftest(OLS_model, vcov = vcovCL(OLS_model, type = "HC0", cluster = df$study_id))
print(clustered_OLS)

# Get correct p-value for H0: intercept = 1
t_OLS <- (clustered_OLS["(Intercept)", "Estimate"] - 1)/clustered_OLS["(Intercept)", "Std. Error"]
p_OLS <- 2 * pt(abs(t_OLS), df = Inf, lower.tail = FALSE)

OLS.boot_st.error<-boottest(OLS_model,clustid = "study_id", param = c("win_beta_se"), B=9999)
OLS.boot_intercept<-boottest(OLS_model,clustid = "study_id", param = c("(Intercept)"), B=9999)
summary(OLS.boot_st.error)
summary(OLS.boot_intercept)

# FE
cat("\n============================================================\n")
cat("FE Results:", label, "\n")

FE <- plm(win_beta_estimate ~ win_beta_se, data = df, model = "within", index = "study_id")
clustered_FE <- coeftest(FE, vcov = vcov(FE, type = "fixed", cluster = c(df$study_id))) 
print(clustered_FE)
FE_intercept <- within_intercept(FE)
t_FE <- (FE_intercept[1] - 1)/attr(FE_intercept, "se")
p_FE <- 2*pt(abs(t_FE), df = Inf, lower.tail = FALSE)

# BE
cat("\n============================================================\n")
cat("BE Results:", label, "\n")

BE = plm(win_beta_estimate ~ win_beta_se, data = df, model = "between", index = "study_id")
summary(BE)
clustered_BE = coeftest(BE, vcov = vcov(BE, type = "HC0", cluster = c(df$study_id))) 
print(clustered_BE)

t_BE <- (clustered_BE["(Intercept)", "Estimate"] - 1)/clustered_BE["(Intercept)", "Std. Error"]
p_BE <- 2 * pt(abs(t_BE), df = Inf, lower.tail = FALSE)

# RE
cat("\n============================================================\n")
cat("RE Results:", label, "\n")

RE = plm(win_beta_estimate ~ win_beta_se, data = df, model = "random", index = "study_id")
summary(RE)
clustered_RE = coeftest(RE, vcov = vcov(RE, type = "HC0", cluster = c(df$study_id))) 
print(clustered_RE)

t_RE <- (clustered_RE["(Intercept)", "Estimate"] - 1)/clustered_RE["(Intercept)", "Std. Error"]
p_RE <- 2 * pt(abs(t_RE), df = Inf, lower.tail = FALSE)


# Calculate precision, inv_no_estimates, and invsqrtsamplesize
df$precision_win <- 1 / df$win_beta_se
df$invsqrtsamplesize <- 1 / sqrt(df$sample_size)

df <- df %>%
  group_by(study_id) %>%
  mutate(no_estimates = n()) %>%
  ungroup()
#View(df)
df$inv_no_estimates <- 1 / df$no_estimates

# OLS Weighted by inv number of estimates per study, clustered
cat("\n============================================================\n")
cat("OLS weighted by inv num of estimates per study Results:", label, "\n")

OLS_study_win <- lm(formula = win_beta_estimate ~ win_beta_se, data = df, weight = (df$inv_no_estimates))
OLS_study_c_win <- coeftest(OLS_study_win, vcov = vcovCL(OLS_study_win, type = "HC0", cluster = c(df$study_id))) 
print(OLS_study_c_win) 

t_OLS_study <- (OLS_study_c_win["(Intercept)", "Estimate"] - 1)/OLS_study_c_win["(Intercept)", "Std. Error"]
p_OLS_study <- 2 * pt(abs(t_OLS_study), df = Inf, lower.tail = FALSE)

# Wild Bootstrap
weight_study_boot_st.error<-boottest(OLS_study_win,clustid = "study_id", param = c("win_beta_se"), B=9999)
weight_study_boot_intercept<-boottest(OLS_study_win,clustid = "study_id", param = c("(Intercept)"), B=9999)
summary(weight_study_boot_st.error)
summary(weight_study_boot_intercept)

# Weighted by precision
cat("\n============================================================\n")
cat("OLS weighted by precision Results:", label, "\n")
OLS_precision_win <- lm(formula = win_beta_estimate ~ win_beta_se, data = df, weight = c(df$precision_win))
OLS_precision_c_win <- coeftest(OLS_precision_win, vcov = vcovCL(OLS_precision_win, type = "HC0", cluster = c(df$study_id))) 
print(OLS_precision_c_win) #OLS weighted by precision, clustered

t_OLS_precision <- (OLS_precision_c_win["(Intercept)", "Estimate"] - 1)/OLS_precision_c_win["(Intercept)", "Std. Error"]
p_OLS_precision <- 2 * pt(abs(t_OLS_precision), df = Inf, lower.tail = FALSE)

# Wild Bootstrap
weight_precision_boot_st.error<-boottest(OLS_precision_win,clustid = "study_id", param = c("win_beta_se"), B=9999)
weight_precision_boot_intercept<-boottest(OLS_precision_win,clustid = "study_id", param = c("(Intercept)"), B=9999)
summary(weight_precision_boot_st.error)
summary(weight_precision_boot_intercept)


# Create table with linear test results
table.3 <- data.frame(
  Technique = rep(c(
    "OLS",
    " ",
    "FE",
    " ",
    "BE",
    " ",
    "RE",
    " ",
    "OLS weighted by 1/num of estimates",
    " ",
    "OLS weighted by precision",
    " "
  ), each = 1),
  
  Term = rep(c("Effect beyond bias", "Publication bias"), times = 6),
  
  Estimate = c(
    clustered_OLS["(Intercept)", "Estimate"],
    clustered_OLS["win_beta_se", "Estimate"],
    
    within_intercept(FE)[1],
    clustered_FE["win_beta_se", "Estimate"],
    
    clustered_BE["(Intercept)", "Estimate"],
    clustered_BE["win_beta_se", "Estimate"],
    
    clustered_RE["(Intercept)", "Estimate"],
    clustered_RE["win_beta_se", "Estimate"],
    
    OLS_study_c_win["(Intercept)", "Estimate"],
    OLS_study_c_win["win_beta_se", "Estimate"],
    
    OLS_precision_c_win["(Intercept)", "Estimate"],
    OLS_precision_c_win["win_beta_se", "Estimate"]
  ),
  
  Std_Error = c(
    clustered_OLS["(Intercept)", "Std. Error"],
    clustered_OLS["win_beta_se", "Std. Error"],
    
    attr(FE_intercept, "se"),
    clustered_FE["win_beta_se", "Std. Error"],
    
    clustered_BE["(Intercept)", "Std. Error"],
    clustered_BE["win_beta_se", "Std. Error"],
    
    clustered_RE["(Intercept)", "Std. Error"],
    clustered_RE["win_beta_se", "Std. Error"],
    
    OLS_study_c_win["(Intercept)", "Std. Error"],
    OLS_study_c_win["win_beta_se", "Std. Error"],
    
    OLS_precision_c_win["(Intercept)", "Std. Error"],
    OLS_precision_c_win["win_beta_se", "Std. Error"]
  ),
  
  p_value = c(
    p_OLS,
    clustered_OLS["win_beta_se", "Pr(>|t|)"],
    
    p_FE,
    clustered_FE["win_beta_se", "Pr(>|t|)"],
    
    p_BE,
    clustered_BE["win_beta_se", "Pr(>|t|)"],
    
    p_RE,
    clustered_RE["win_beta_se", "Pr(>|t|)"],
    
    p_OLS_study,
    OLS_study_c_win["win_beta_se", "Pr(>|t|)"],
    
    p_OLS_precision,
    OLS_precision_c_win["win_beta_se", "Pr(>|t|)"]
  )
)

# Print <0.001 for very small p-values
table.3$p_value <- ifelse(
  table.3$p_value < 0.001,
  "<0.001",
  formatC(table.3$p_value, format = "f", digits = 3)
)

cat("\n============================================================\n")
cat("Linear Test Results for:", label, "\n")
cat("Subset unweighted mean:", mean(df$win_beta_estimate), "\n")
cat("Observations used:", nrow(df), "\n")
cat("Winsorization level:", win_level, "\n")
cat("p-values for H0: beta = 1 and H0: beta_se = 0 \n")

print(table.3)
print("BS CIs:")

print("OLS intercept:")
print(OLS.boot_intercept$conf_int)

print("OLS SE:")
print(OLS.boot_st.error$conf_int)

print("Study:")
print(weight_study_boot_intercept$conf_int)

print("Study SE:")
print(weight_study_boot_st.error$conf_int)

print("Precision:")
print(weight_precision_boot_intercept$conf_int)

print("Precision SE:")
print(weight_precision_boot_st.error$conf_int)

create_funnel_plot(df_plot, "beta_estimate", "Precision", "Beta Estimates", "Funnel Plot")


}

# Linear tests for all estimates
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df, df_plot = df_plot, label = "All Estimates")

# Linear tests for subsets:

# Money x Real Effort 
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$money == 1,],  df_plot = df_plot[df_plot$money == 1,], label = "Money")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$real_effort == 1,], df_plot = df_plot[df_plot$real_effort == 1,], label = "Real Effort")

# Preferred x Appendix
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$preffered_estimate == 1,],  df_plot = df_plot[df_plot$preffered_estimate == 1,], label = "Preferred")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$appendix == 1,],  df_plot = df_plot[df_plot$appendix == 1,], label = "Appendix")

# Treatment x Non-treatment
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$treatment == 1,],  df_plot = df_plot[df_plot$treatment == 1,], label = "Treatment")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$treatment == 0,],  df_plot = df_plot[df_plot$treatment == 0,], label = "Non-Treatment")

# FED x no FED
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$FED == 1 & !is.na(df$FED), ],  df_plot = df_plot[df_plot$FED == 1 & !is.na(df_plot$FED), ], label = "FED")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$FED == 0 & !is.na(df$FED), ],  df_plot = df_plot[df_plot$FED == 0 & !is.na(df_plot$FED), ], label = "FED")

# Utility control
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$utility_control == 1, ],  df_plot = df_plot[df_plot$utility_control == 1, ], label = "Utility Control")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$utility_control == 0, ],  df_plot = df_plot[df_plot$utility_control == 0, ], label = "No Utility Control")

# Transaction costs
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$deal_transaction_cost == 1, ],  df_plot = df_plot[df_plot$deal_transaction_cost == 1, ], label = "Transaction cost control")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$deal_transaction_cost == 0, ],  df_plot = df_plot[df_plot$deal_transaction_cost == 0, ], label = "No Transaction cost control")

# Uncertainty
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$deal_uncertainty == 1, ],  df_plot = df_plot[df_plot$deal_uncertainty == 1, ], label = "Uncertainty control")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$deal_uncertainty == 0, ],  df_plot = df_plot[df_plot$deal_uncertainty == 0, ], label = "No uncertainty control")

# Confounding factors all
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$utility_control == 1 & df$deal_transaction_cost == 1 & df$deal_uncertainty == 1,], df_plot[df_plot$utility_control == 1 & df_plot$deal_transaction_cost == 1 & df_plot$deal_uncertainty == 1,],label = "All Confounding controlled")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$utility_control == 0 & df$deal_transaction_cost == 0 & df$deal_uncertainty == 0,], df_plot[df_plot$utility_control == 0 & df_plot$deal_transaction_cost == 0 & df_plot$deal_uncertainty == 0,],label = "No Confounding controlled")

LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$utility_control == 1 & df$deal_transaction_cost == 1 & df$deal_uncertainty == 1 & df$money == 1,], df_plot[df_plot$utility_control == 1 & df_plot$deal_transaction_cost == 1 & df_plot$deal_uncertainty == 1 & df_plot$money == 1,],label = "Confounding controlled Money")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$utility_control == 1 & df$deal_transaction_cost == 1 & df$deal_uncertainty == 1 & df$real_effort == 1,], df_plot[df_plot$utility_control == 1 & df_plot$deal_transaction_cost == 1 & df_plot$deal_uncertainty == 1 & df_plot$real_effort == 1,],label = "Confounding controlled Real Effort")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$utility_control == 1 & df$deal_transaction_cost == 1 & df$deal_uncertainty == 1 & df$lab == 1,], df_plot[df_plot$utility_control == 1 & df_plot$deal_transaction_cost == 1 & df_plot$deal_uncertainty == 1 & df_plot$lab == 1,],label = "Confounding controlled Lab")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$utility_control == 1 & df$deal_transaction_cost == 1 & df$deal_uncertainty == 1 & df$field == 1,], df_plot[df_plot$utility_control == 1 & df_plot$deal_transaction_cost == 1 & df_plot$deal_uncertainty == 1 & df_plot$field == 1,],label = "Confounding controlled Field")

# Aggregate x Individual
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$aggregate == 1,], df_plot = df_plot[df_plot$aggregate== 1,], label = "Aggregate")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$aggregate == 0,], df_plot = df_plot[df_plot$aggregate == 0,], label = "Individual")

# Aggregate + Money x Aggregate + Real Effort
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$money == 1 & df$aggregate == 1,], df_plot[df_plot$money == 1 & df_plot$aggregate == 1,],label = "Aggregate Money")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$real_effort == 1 & df$aggregate == 1,],df_plot[df_plot$real_effort == 1 & df_plot$aggregate == 1,], label = "Aggregate Real Effort")

# Money + Lab x Money + Field
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$money == 1 & df$lab == 1,], df_plot[df_plot$money == 1 & df_plot$lab == 1,],label = "Money Lab")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$money == 1 & df$field == 1,],df_plot[df_plot$money == 1 & df_plot$field == 1,], label = "Money Field")

# Lab x Field x Online
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$lab == 1,], df_plot = df_plot[df_plot$lab == 1,], label = "Lab")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$field == 1,], df_plot = df_plot[df_plot$field == 1,], label = "Field")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$school_work == 1,], df_plot = df_plot[df_plot$school_work == 1,], label = "School/Workplace")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$online == 1,], df_plot = df_plot[df_plot$online == 1,], label = "Online")

# Money + Lab + CTB
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$money == 1 & df$lab == 1 & df$ctb == 1,], df_plot[df_plot$money == 1 & df_plot$lab == 1 & df_plot$ctb == 1,],label = "Money Lab CTB")

# Money + Field + MPL
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$money == 1 & df$field == 1 & df$mpl == 1,],df_plot[df_plot$money == 1 & df_plot$field == 1 & df_plot$mpl == 1,], label = "Money Field MPL")

#Students x General population
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$uni_students == 1,], df_plot = df_plot[df_plot$uni_students == 1,], label = "Students")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$general_pop == 1,], df_plot = df_plot[df_plot$general_pop == 1,], label = "General Population")

# Students + Money x Students + Real Effort
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$uni_students == 1 & df$money == 1,], df_plot = df_plot[df_plot$uni_students == 1& df_plot$money == 1,], label = "Students Money")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$uni_students == 1 & df$real_effort == 1,], df_plot = df_plot[df_plot$uni_students == 1& df_plot$real_effort == 1,], label = "Students Real Effort")

# Students + Lab x Students + Field
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$uni_students == 1 & df$lab == 1,], df_plot = df_plot[df_plot$uni_students == 1& df_plot$lab == 1,], label = "Students Lab")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$uni_students == 1 & df$field == 1,], df_plot = df_plot[df_plot$uni_students == 1& df_plot$field == 1,], label = "Students Field")

# Students + CTB x Students + MPL x Students + Wage
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$uni_students == 1 & df$ctb == 1,], df_plot[df_plot$uni_students == 1 & df_plot$ctb == 1,],label = "Students CTB")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$uni_students == 1 & df$mpl == 1,], df_plot[df_plot$uni_students == 1 & df_plot$mpl == 1,],label = "Students MPL")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$uni_students == 1 & df$wage == 1,], df_plot[df_plot$uni_students == 1 & df_plot$wage == 1,],label = "Students Wage")


# CTB x MPL x Work for wage
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$ctb == 1,], df_plot = df_plot[df_plot$ctb == 1,], label = "CTB")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$mpl == 1,], df_plot = df_plot[df_plot$mpl == 1,], label = "MPL")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$wage == 1,], df_plot = df_plot[df_plot$wage == 1,], label = "Work for Wage")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$matching == 1,], df_plot = df_plot[df_plot$matching == 1,], label = "Matching task")


# Real Effort + CTB
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$real_effort == 1 & df$ctb == 1,], df_plot[df_plot$real_effort == 1 & df_plot$ctb == 1,],label = "Real Effort CTB")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$real_effort == 1 & df$wage == 1,], df_plot[df_plot$real_effort == 1 & df_plot$wage == 1,],label = "Real Effort Wage")


# Money + CTB x Money + MPL
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$money == 1 & df$ctb == 1,], df_plot[df_plot$money == 1 & df_plot$ctb == 1,],label = "Money CTB")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$money == 1 & df$mpl == 1,], df_plot[df_plot$money == 1 & df_plot$mpl == 1,],label = "Money MPL")


# Asia x Europe x North America
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$asia == 1,], df_plot = df_plot[df_plot$asia == 1,], label = "Asia")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$europe == 1,], df_plot = df_plot[df_plot$europe == 1,], label = "Europe")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$north_america == 1,], df_plot = df_plot[df_plot$north_america == 1,], label = "North America")

# North America + Money x North America + Real Effort
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$money == 1 & df$north_america == 1,], df_plot[df_plot$money == 1 & df_plot$north_america == 1,],label = "North America Money")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$real_effort == 1 & df$north_america == 1,],df_plot[df_plot$real_effort == 1 & df_plot$north_america == 1,], label = "North America Real Effort")

# North America + Lab x North America + Field
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$lab == 1 & df$north_america == 1,], df_plot[df_plot$lab == 1 & df_plot$north_america == 1,],label = "North America Lab")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$field == 1 & df$north_america == 1,],df_plot[df_plot$field == 1 & df_plot$north_america == 1,], label = "North America Field")

# ML x NLS x Tobit x Switch Point
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$ml == 1,], df_plot = df_plot[df_plot$ml == 1,], label = "ML")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$nls == 1,], df_plot = df_plot[df_plot$nls == 1,], label = "NLS")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$tobit == 1,], df_plot = df_plot[df_plot$tobit == 1,], label = "Tobit")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$switch_point == 1,], df_plot = df_plot[df_plot$switch_point == 1,], label = "Switch Point")

# Published
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$published == 1,], df_plot = df_plot[df_plot$published == 1,], label = "Published")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$pub_top_five == 1,], df_plot = df_plot[df_plot$pub_top_five == 1,], label = "Published Top 5")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$published == 0,], df_plot = df_plot[df_plot$published == 0,], label = "Not Published")

# Experiment x Survey
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$experiment == 1,], df_plot = df_plot[df_plot$experiment== 1,], label = "Experiment")
LT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$survey == 1,], df_plot = df_plot[df_plot$survey== 1,], label = "Survey")


# Beta = alpha + gamma_1 * SE(Beta) + gamma_2 * "Interaction_var" + gamma_3 * SE(Beta) * "Interaction_var"
LT_interaction <- function(win_beta_estimate, win_beta_se, interaction, df, df_plot, label){
# OLS
cat("\n============================================================\n")
cat("OLS Results:", label, "\n")

OLS_model <- lm(win_beta_estimate ~ win_beta_se*interaction, data = df)
clustered_OLS <- coeftest(OLS_model, vcov = vcovCL(OLS_model, type = "HC0", cluster = df$study_id))
print(clustered_OLS)

# Get correct p-value for H0: intercept = 1
t_OLS <- (clustered_OLS["(Intercept)", "Estimate"] - 1)/clustered_OLS["(Intercept)", "Std. Error"]
p_OLS <- 2 * pt(abs(t_OLS), df = Inf, lower.tail = FALSE)
print(p_OLS)


# FE
cat("\n============================================================\n")
cat("FE Results:", label, "\n")

FE <- plm(win_beta_estimate ~ win_beta_se + interaction + win_beta_se:interaction, data = df, model = "within", index = "study_id")
clustered_FE <- coeftest(FE, vcov = vcov(FE, type = "fixed", cluster = c(df$study_id))) 
print(clustered_FE)
FE_intercept <- within_intercept(FE)
t_FE <- (FE_intercept[1] - 1)/attr(FE_intercept, "se")
p_FE <- 2*pt(abs(t_FE), df = Inf, lower.tail = FALSE)
print(p_FE)

# BE
cat("\n============================================================\n")
cat("BE Results:", label, "\n")

BE = plm(win_beta_estimate ~ win_beta_se + interaction + win_beta_se:interaction, data = df, model = "between", index = "study_id")
summary(BE)
clustered_BE = coeftest(BE, vcov = vcov(BE, type = "HC0", cluster = c(df$study_id))) 
print(clustered_BE)

t_BE <- (clustered_BE["(Intercept)", "Estimate"] - 1)/clustered_BE["(Intercept)", "Std. Error"]
p_BE <- 2 * pt(abs(t_BE), df = Inf, lower.tail = FALSE)
print(p_BE)

# RE
cat("\n============================================================\n")
cat("RE Results:", label, "\n")

RE = plm(win_beta_estimate ~ win_beta_se + interaction + win_beta_se:interaction, data = df, model = "random", index = "study_id")
summary(RE)
clustered_RE = coeftest(RE, vcov = vcov(RE, type = "HC0", cluster = c(df$study_id))) 
print(clustered_RE)

t_RE <- (clustered_RE["(Intercept)", "Estimate"] - 1)/clustered_RE["(Intercept)", "Std. Error"]
p_RE <- 2 * pt(abs(t_RE), df = Inf, lower.tail = FALSE)
print(p_RE)

# Calculate precision, inv_no_estimates, and invsqrtsamplesize
df$precision_win <- 1 / df$win_beta_se
df$invsqrtsamplesize <- 1 / sqrt(df$sample_size)

df <- df %>%
  group_by(study_id) %>%
  mutate(no_estimates = n()) %>%
  ungroup()
#View(df)
df$inv_no_estimates <- 1 / df$no_estimates

# OLS Weighted by inv number of estimates per study, clustered
cat("\n============================================================\n")
cat("OLS weighted by inv num of estimates per study Results:", label, "\n")

OLS_study_win <- lm(formula = win_beta_estimate ~ win_beta_se + interaction + win_beta_se:interaction, data = df, weight = (df$inv_no_estimates))
OLS_study_c_win <- coeftest(OLS_study_win, vcov = vcovCL(OLS_study_win, type = "HC0", cluster = c(df$study_id))) 
print(OLS_study_c_win) 

t_OLS_study <- (OLS_study_c_win["(Intercept)", "Estimate"] - 1)/OLS_study_c_win["(Intercept)", "Std. Error"]
p_OLS_study <- 2 * pt(abs(t_OLS_study), df = Inf, lower.tail = FALSE)
print(p_OLS_study)

# Weighted by precision
cat("\n============================================================\n")
cat("OLS weighted by precision Results:", label, "\n")
OLS_precision_win <- lm(formula = win_beta_estimate ~ win_beta_se + interaction + win_beta_se:interaction, data = df, weight = c(df$precision_win))
OLS_precision_c_win <- coeftest(OLS_precision_win, vcov = vcovCL(OLS_precision_win, type = "HC0", cluster = c(df$study_id))) 
print(OLS_precision_c_win) #OLS weighted by precision, clustered

t_OLS_precision <- (OLS_precision_c_win["(Intercept)", "Estimate"] - 1)/OLS_precision_c_win["(Intercept)", "Std. Error"]
p_OLS_precision <- 2 * pt(abs(t_OLS_precision), df = Inf, lower.tail = FALSE)
print(p_OLS_precision)
}

# SExField 
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$field, df=df, df_plot = df_plot, label = "All Estimates")

# SExReal_EFfort
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$real_effort, df=df, df_plot = df_plot, label = "All Estimates")
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$money, df=df, df_plot = df_plot, label = "All Estimates")

# SExIndividual
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$individual, df=df, df_plot = df_plot, label = "All Estimates")

# SExStudents
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$uni_students, df=df, df_plot = df_plot, label = "All Estimates")
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$general_pop, df=df, df_plot = df_plot, label = "All Estimates")

# SExUtil_control
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$utility_control, df=df, df_plot = df_plot, label = "All Estimates")
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$deal_uncertainty, df=df, df_plot = df_plot, label = "All Estimates")
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$deal_transaction_cost, df=df, df_plot = df_plot, label = "All Estimates")

# SExNorth_America
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$north_america, df=df, df_plot = df_plot, label = "All Estimates")

# SExTobit
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$tobit, df=df, df_plot = df_plot, label = "All Estimates")
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$nls, df=df, df_plot = df_plot, label = "All Estimates")
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$ml, df=df, df_plot = df_plot, label = "All Estimates")
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$switch_point, df=df, df_plot = df_plot, label = "All Estimates")

# SE x published
LT_interaction(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, interaction = df$published, df=df, df_plot = df_plot, label = "All Estimates")



#------------------------------
# Non-Linear Techniques
#------------------------------

NLT <- function(win_beta_estimate, win_beta_se, df, df_plot, label){

  
# Recenter Beta as Degree of Present Bias
df$DPB <- 1 - df$win_beta_estimate
df_plot$DPB <- 1 - df_plot$beta_estimate

# Plot histogram of DPB
ggplot(df_plot, aes(x = DPB)) +
  geom_histogram(data = df_plot,
                 bins = 75, fill = "blue", color = "blue", alpha=0.5) +
  xlab("Degree of Present Bias Estimate") +
  ylab("Frequency") +
  geom_vline(xintercept = mean(df$DPB), linetype = "solid", color = "red") +
  geom_vline(xintercept = median(df$DPB), linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))


#------------------------------
# WAAP - can use Beta
#------------------------------

cat("\n============================================================\n")
cat("WAAP Results:", label, "\n")


df$Precision <- 1 / df$win_beta_se

WLS_FE_avg <- sum(df$win_beta_estimate/df$win_beta_se)/sum(1/df$win_beta_se) #sum of weighted effects divided by the sum of weights
WAAP_bound <- abs(WLS_FE_avg - 1)/2.8 # Calculate effect with respect to 1!
WAAP_reg <- lm(formula = win_beta_estimate ~ -Precision, data = df[df$win_beta_se<WAAP_bound,])
WAAP_reg_cluster <- coeftest(WAAP_reg, vcov = vcovHC(WAAP_reg, type = "HC0", cluster = c(df$study_id)))
print(WAAP_reg_cluster)

# Get correct p-value for H0: intercept = 1
t_WAAP <- (WAAP_reg_cluster["(Intercept)", "Estimate"] - 1)/WAAP_reg_cluster["(Intercept)", "Std. Error"]
p_WAAP <- 2 * pt(abs(t_WAAP), df = Inf, lower.tail = FALSE)
p_WAAP


#------------------------------
# Top 10 - can use Beta
#------------------------------

cat("\n============================================================\n")
cat("Top 10 Results:", label, "\n")

T10_bound <- quantile(df$Precision, probs = 0.9)

T10_reg <- lm(formula = win_beta_estimate ~ -Precision, data = df[df$Precision > T10_bound,])
T10_reg_cluster <- coeftest(T10_reg, vcov = vcovHC(T10_reg, type = "HC0", cluster = c(df$study_id)))
print(T10_reg_cluster)

# Get correct p-value for H0: intercept = 1
t_Top10 <- (T10_reg_cluster["(Intercept)", "Estimate"] - 1)/T10_reg_cluster["(Intercept)", "Std. Error"]
p_Top10 <- 2 * pt(abs(t_Top10), df = Inf, lower.tail = FALSE)


#------------------------------
# Stem 
#------------------------------

cat("\n============================================================\n")
cat("STEM results:", label, "\n")

source("stem_method.R") #github.com/Chishio318/stem-based_method

# For all estimates 
est_stem <- stem(df$beta_estimate, df$beta_se, param)
print(est_stem$estimates)
#View(est_stem$estimates)
funnels_stem <- stem_funnel(df$beta_estimate, df$beta_se, est_stem$estimates) #For more detail see link above

t_stem <- (est_stem$estimates[1, "estimate"] - 1)/ est_stem$estimates[1, "se"]
p_stem <- 2 * pt(abs(t_stem), df = Inf, lower.tail = FALSE)
p_stem

# For all estimates omitting outliers 
est_stem_omit <- stem(df_plot$beta_estimate, df_plot$beta_se, param)
print(est_stem_omit$estimates)
#View(est_stem$estimates)
funnels_stem_omit <- stem_funnel(df_plot$beta_estimate, df_plot$beta_se, est_stem_omit$estimates) #For more detail see link above

t_stem_omit <- (est_stem_omit$estimates[1, "estimate"] - 1)/ est_stem_omit$estimates[1, "se"]
p_stem_omit <- 2 * pt(abs(t_stem_omit), df = Inf, lower.tail = FALSE)
p_stem_omit





# Median per study
df_study_medians_win <- df%>%
  group_by(study_id) %>%
  summarise(
    median_win_beta_estimate = median(beta_estimate, na.rm = TRUE),
    median_win_beta_se       = median(beta_se, na.rm = TRUE),
    n_estimates              = n(),
    .groups = "drop"
  )

n_distinct(df$study_id)
nrow(df_study_medians_win)

# Try study medians - omitted
df_study_medians <- df %>%
  group_by(study_id) %>%
  summarise(
    median_beta_estimate = median(beta_estimate, na.rm = TRUE),
    median_beta_se       = median(beta_se, na.rm = TRUE),
    n_estimates          = n(),
    .groups = "drop"
  )

nrow(df_study_medians)

est_stem_median <- stem(df_study_medians$median_beta_estimate, df_study_medians$median_beta_se, param)
print(est_stem_median$estimates)
# View(est_stem_median$estimates)
funnels_stem_median <- stem_funnel(df_study_medians$median_beta_estimate, df_study_medians$median_beta_se, est_stem_median$estimates)

t_stem_median <- (est_stem_median$estimates[1, "estimate"] - 1)/ est_stem_median$estimates[1, "se"]
p_stem_median <- 2 * pt(abs(t_stem_median), df = Inf, lower.tail = FALSE)


#------------------------------
# Endogenous Kink 
#------------------------------

cat("\n============================================================\n")
cat("Endo Kink Results:", label, "\n")


### Endogenous Kink - Bom & Rachinger (2019)
### Source: https://onlinelibrary.wiley.com/doi/abs/10.1002/jrsm.1352
### Rewritten from STATA code to R - results may be unstable

#' Runs the Endogenous Kink method by Bom & Rachinger (2019) to estimate the mean effect size and the random effects variance component.

#' @param data [data.frame] A data frame with two columns, the first column being the effect data and the second column being the standard error data.
#' @param verbose [bool] A boolean indicating whether to print the results to the console. Default is TRUE.
#' @return A list with the following elements:
#' "b0_ek": the estimated mean effect size by the method
#' "sd0_ek": the standard error of the estimated mean effect size
#' "b1_ek": the estimated publication bias coefficient by the Endo-Kink method, if applicable
#' "sd1_ek": the standard error of the estimated publication bias coefficient, if applicable

# Data prep
data <- df[, c("win_beta_estimate", "win_beta_se")]


runEndoKink <- function(data, verbose = T){
  # Input validation
  stopifnot(
    is.data.frame(data), # Only data frame
    is.logical(verbose), # Only boolean
    ncol(data) == 2, # Effect data, Standard error data
    sapply(data, is.numeric) # Only numeric input
  )
  # Rename source data
  colnames(data) <- c("bs", "sebs")
  
  # Create new variables
  data$ones <- 1
  M <- nrow(data)
  sebs_min <- min(data$sebs)
  sebs_max <- max(data$sebs)
  data$sebs2 <- data$sebs^2
  data$wis <- data$ones / data$sebs2
  data$bs_sebs <- data$bs / data$sebs
  data$ones_sebs <- data$ones / data$sebs
  data$bswis <- data$bs * data$wis
  wis_sum <- sum(data$wis) # Redundant
  
  # FAT-PET
  fat_pet <- lm(bs_sebs ~ 0 + ones_sebs + ones, data = data) # No constant
  # Auxiliary
  fat_pet_est <- coeftest(fat_pet)["ones_sebs", "Estimate"] # Fat pet ones_sebs estimate
  fat_pet_se <- coeftest(fat_pet)["ones_sebs", "Std. Error"] # Fat pet ones_sebs standard error
  # End auxiliary
  pet <- fat_pet_est
  t1_linreg <- fat_pet_est / fat_pet_se
  b_lin <-  fat_pet_est
  Q1_lin <- sum(resid(fat_pet)^2)
  abs_t1_linreg <- abs(t1_linreg) 
  
  # PEESE
  peese_model <- lm(bs_sebs ~ 0 + ones_sebs + sebs, data = data) # No constant
  # Auxiliary
  peese_est <- coeftest(peese_model)["ones_sebs", "Estimate"]
  # End auxiliary
  peese <- peese_est
  b_sq <- peese_est
  Q1_sq <- sum(resid(peese_model)^2) # Sum of squared residuals
  
  # FAT-PET-PEESE
  if (abs_t1_linreg > qt(0.975, M-2)) {
    combreg <- b_sq
    Q1 <- Q1_sq
  } else {
    combreg <- b_lin
    Q1 <- Q1_lin
  }
  
  # Estimation of random effects variance component
  df_m <- df.residual(peese_model) # DoF from the last regression (peese-model)
  sigh2hat <- max(0, M * ((Q1 / (M - df_m - 1)) - 1) / wis_sum)
  sighhat <- sqrt(sigh2hat)
  
  # Cutoff value for EK
  if (combreg > 1.96 * sighhat) {
    a1 <- (combreg - 1.96 * sighhat) * (combreg + 1.96 * sighhat) / (2 * 1.96 * combreg)
  } else {
    a1 <- 0
  }
  
  # Rename variables - messy source code, kept to the original
  names(data)[names(data) == "bs"] <- "bs_original"
  names(data)[names(data) == "bs_sebs"] <- "bs"
  names(data)[names(data) == "ones_sebs"] <- "constant"
  names(data)[names(data) == "ones"] <- "pub_bias"
  
  # Regressions and coefficient extraction in various scenarios
  if (a1 > sebs_min & a1 < sebs_max) {
    data$sebs_a1 <- ifelse(data$sebs > a1, data$sebs - a1, 0)
    data$pubbias <- data$sebs_a1 / data$sebs
    ek_regression <- lm(bs ~  0 + constant + pubbias, data = data)
    b0_ek <- coef(ek_regression)[1]
    b1_ek <- coef(ek_regression)[2]
    sd0_ek <- summary(ek_regression)$coefficients[1, 2]
    sd1_ek <- summary(ek_regression)$coefficients[2, 2]
  } else if (a1 < sebs_min) {
    ek_regression <- lm(bs ~ 0 + constant + pub_bias, data = data)
    b0_ek <- coef(ek_regression)[1]
    b1_ek <- coef(ek_regression)[2]
    sd0_ek <- summary(ek_regression)$coefficients[1, 2]
    sd1_ek <- summary(ek_regression)$coefficients[2, 2]
  } else if (a1 > sebs_max) {
    ek_regression <- lm(bs ~ 0 + constant, data = data)
    b0_ek <- coef(ek_regression)[1]
    sd0_ek <- summary(ek_regression)$coefficients[1, 2]
    b1_ek <- NA
    sd1_ek <- NA
  }
  # Print results to console if desired
  if (verbose){
    cat("EK's mean effect estimate (alpha1) and standard error:")
    cat(b0_ek) # Mean effect estimate
    cat(" and ")
    cat(sd0_ek) # Mean effect standard error
    cat(". EK's publication bias estimate (delta) and standard error:")
    cat(b1_ek) # Pub bias estimate
    cat(" and ")
    cat(sd1_ek) # Pub bias standard error
  }
  # Return the four coefficients
  return (c(b0_ek, sd0_ek, b1_ek, sd1_ek))
}

temp <- runEndoKink(data)

t_EK <- (temp[1] - 1)/  temp[2]
p_EK <- 2 * pt(abs(t_EK), df = Inf, lower.tail = FALSE)

t_pb_EK <-  temp[3]/temp[4]
p_pb_EK <- 2 * pt(abs(t_pb_EK), df = Inf, lower.tail = FALSE)


#------------------------------
# Andrews and Kasy Selection Model (from beauty.R) - uses DPB instead of Beta
#------------------------------

cat("\n============================================================\n")
cat("AK Selection Results:", label, "\n")

run_ak_subsample <- function(data,
                             y_var,
                             se_var,
                             subset_idx = NULL,
                             label      = "",
                             B_boot     = 500) {
  
  if (is.null(subset_idx)) {
    subset_idx <- rep(TRUE, nrow(data))
  }
  
  ok   <- subset_idx & !is.na(data[[y_var]]) & !is.na(data[[se_var]])
  dsub <- data[ok, ]
  
  cat("\n============================================================\n")
  cat("Andrews & Kasy metastudies model for:", label, "\n")
  cat("Observations used:", nrow(dsub), "\n")
  
  if (nrow(dsub) == 0) {
    cat("No observations in this subsample.\n")
    return(invisible(NULL))
  }
  
  premium_sample <- data.frame(
    X     = dsub[[y_var]],
    sigma = dsub[[se_var]]
  )
  
  ms <- metastudies_estimation(
    X         = premium_sample$X,
    sigma     = premium_sample$sigma,
    model     = "t",
    cutoffs   = c(-1.96, 0, 1.96),
    symmetric = FALSE
  )
  
  cat("\nEstimation table (ms$est_tab):\n")
  print(ms$est_tab)
  
  cat("\nCorrelations between X and sigma within metastudies:\n")
  print(metastudy_X_sigma_cors(ms))
  
  cat("\nBootstrap specification tests (B =", B_boot, "):\n")
  res_boot <- bootstrap_specification_tests(
    premium_sample$X,
    premium_sample$sigma,
    B = B_boot
  )
  print(res_boot)
  
  invisible(list(ms = ms, bootstrap = res_boot))
}

# ---- 4.1 Andrews & Kasy calls (same subsamples as STEM) ----

ak_full <- run_ak_subsample(
  data       = df,
  y_var      = "DPB",
  se_var     = "win_beta_se",
  subset_idx = NULL,
  label      = "Full sample"
)

t_AK <- ak_full$ms$est_tab["estimate", "μ"] / ak_full$ms$est_tab["standard error", "μ"]
p_AK <- 2 * pt(abs(t_AK), df = Inf, lower.tail = FALSE)

t_pb_AK <- ak_full$ms$est_tab["estimate", "τ"] / ak_full$ms$est_tab["standard error", "τ"]
p_pb_AK <- 2 * pt(abs(t_pb_AK), df = Inf, lower.tail = FALSE)


#------------------------------
# RTMA (from beauty.R)
#------------------------------

# Function for saving 
save_plot_rtma <- function(plot, filename) {
  ggsave(
    filename = filename,
    plot = plot,
    width = 6,
    height = 5,
    units = "in"
  )
}

cat("\n============================================================\n")
cat("RTMA Results:", label, "\n")

# Helper to run RTMA on a given (sub)sample and (y, se) variables
run_rtma <- function(data, y_var, se_var,
                     subset_idx = NULL,
                     label = "") {
  
  if (is.null(subset_idx)) {
    subset_idx <- rep(TRUE, nrow(data))
  }
  
  ok <- subset_idx & !is.na(data[[y_var]]) & !is.na(data[[se_var]])
  d  <- data[ok, ]

  
  if (nrow(d) == 0) {
    cat("No observations in this subsample. Skipping.\n")
    return(invisible(NULL))
  }
  
  yi  <- d[[y_var]]
  sei <- d[[se_var]]
  vi  <- sei^2
  
  # Z-score density plot
  p <- z_density(
    yi, vi,
    sei          = sei,
    alpha_select = 0.05,
    crit_color   = "red"
  ) +
    coord_cartesian(xlim = c(NA, 20)) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )
  
  print(p)
  save_plot_rtma(p, "_z.pdf")
  
  
  
  
  
  # Nonaffirmative (insignificant) count
  z_scores       <- yi / sei
  nonaffirmative <- abs(z_scores) < 1.96
  num_insig      <- sum(nonaffirmative)
  total          <- length(z_scores)
  prop_insig     <- round(100 * num_insig / total, 1)
  
  cat(sprintf("Nonaffirmative (insignificant) estimates: %d of %d (%.1f%%)\n",
              num_insig, total, prop_insig))
  
  # RTMA via phacking_meta()
  fit <- phacking_meta(
    yi             = yi,
    vi             = vi,
    favor_positive = TRUE,      # DPB expected to be positive
    alpha_select   = 0.05,
    ci_level       = 0.95,
    stan_control   = list(adapt_delta = 0.98, max_treedepth = 20),
    parallelize    = TRUE
  )
  
  print(summary(fit))
  invisible(fit)
}


# 0) Full sample
rtma_full <- run_rtma(
  data   = df,
  y_var  = "DPB",
  se_var = "win_beta_se",
  label  = label
)

t_RTMA <- as.numeric(rtma_full$stats[1,4]) / as.numeric(rtma_full$stats[1,5])
p_RTMA <- 2 * pt(abs(t_RTMA), df = Inf, lower.tail = FALSE)

t_pb_RTMA <- as.numeric(rtma_full$stats[1,5]) / as.numeric(rtma_full$stats[2,5])
p_pb_RTMA <- 2 * pt(abs(t_pb_RTMA), df = Inf, lower.tail = FALSE)


p <- rtma_qqplot(rtma_full)

p$layers[[2]]$aes_params$colour <- "blue"
p$layers[[2]]$aes_params$pch <- 21


p <- p + 
  ggplot2::theme_minimal() %+replace%
  ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(color = "black"),
    axis.line.y = ggplot2::element_line(color = "black"),
    panel.border = ggplot2::element_blank(),
  )

print(p)
save_plot_rtma(p, "_qq.pdf")





table.4 <- data.frame(
  Technique = rep(c(
    "WAAP",
    " ",
    "Top 10",
    " ",
    "STEM (all estimates)",
    " ",
    " ",
    "STEM (study medians)",
    " ",
    " ",
    "Endo Kink",
    " ",
    "AK Selection (DPB)",
    " ",
    " ",
    "RTMA (DPB)",
    " ",
    " "
  ), each = 1),
  
  Term = c(rep(c("Effect Beyond Bias", "Publication Bias"), times = 2),rep(c("Effect Beyond Bias", "Publication Bias", "% info used, num in stem"), times = 2), rep(c("Effect Beyond Bias", "Publication Bias"), times = 1), rep(c("Effect Beyond Bias", "Publication Bias", "Effect Beyond Bias (Beta)"), times = 2)),
  
  Estimate = c(
    # ----------------
    # WAAP
    # ----------------
    WAAP_reg_cluster["(Intercept)", "Estimate"],
    " ",
    
    # ----------------
    # Top 10
    # ----------------
   T10_reg_cluster["(Intercept)", "Estimate"],
    " ",
    
    # ----------------
    # STEM (all est)
    # ----------------
    est_stem$estimates[1, "estimate"],
    " ",
    est_stem$estimates[7],
    
    # ----------------
    # STEM (median)
    # ----------------
    est_stem_median$estimates[1, "estimate"],
    " ",
    est_stem_median$estimates[7],
    
    # ----------------
    # Endogenous Kink
    # ----------------
    temp[1],   # b0_ek
    temp[3],
    
    # ----------------
    # Andrews & Kasy
    # ----------------
    ak_full$ms$est_tab["estimate", "μ"],
    ak_full$ms$est_tab["estimate", "τ"],
    1 - ak_full$ms$est_tab["estimate", "μ"],
    
    # ----------------
    # RTMA
    # ----------------
    as.numeric(rtma_full$stats[1,4]),
    as.numeric(rtma_full$stats[2,4]),
    1 - as.numeric(rtma_full$stats[1,4])
  ),
  
  Std_Error = c(
    # WAAP
    WAAP_reg_cluster["(Intercept)", "Std. Error"],
    " ",
    
    # Top 10
   T10_reg_cluster["(Intercept)", "Std. Error"],
   " ",
    
    # STEM (all est)
    est_stem$estimates[1, "se"],
    " ",
    est_stem$estimates[4],
    
    # STEM (median)
    est_stem_median$estimates[1, "se"],
    " ",
    est_stem_median$estimates[4],
    
    # Endo Kink
    temp[2],   # sd0_ek
    temp[4],   # sd1_ek
    
    # AK Selection
    ak_full$ms$est_tab["standard error", "μ"],
    ak_full$ms$est_tab["standard error", "τ"],
    " ",
    
    # RTMA 
    as.numeric(rtma_full$stats[1,5]),
    as.numeric(rtma_full$stats[2,5]),
    " "
  ),
  
  p_value = c(
    # WAAP
    p_WAAP,
    " ",
    
    # Top 10
    p_Top10,
    " ",
    
    # STEM (all est)
    p_stem,
    " ",
    " ",
    
    # STEM (median)
    p_stem_median,
    " ",
    " ",
    
    # Endo Kink
    p_EK,   
    p_pb_EK,
    
    # AK Selection
    p_AK,
    p_pb_AK,
    " ",
    
    # RTMA 
    p_RTMA,
    p_pb_RTMA,
    " ")
  
  )


cat("\n============================================================\n")
cat("Non-Linear Test Results for:", label, "\n")
cat("Observations used:", nrow(df), "\n")
cat("Winsorization level:", win_level, "\n")

table.4_print <- table.4

format_num_col <- function(x, digits = 3) {
  num <- suppressWarnings(as.numeric(x))
  out <- ifelse(
    is.na(num),
    x,  # keep blanks / non-numeric entries
    formatC(num, digits = digits, format = "f")
  )
  out
}

table.4_print$Estimate  <- format_num_col(table.4_print$Estimate, 3)
table.4_print$Std_Error <- format_num_col(table.4_print$Std_Error, 3)
table.4_print$p_value <- format_num_col(table.4_print$p_value, 3)

print(table.4_print, row.names = FALSE)
}

# Non-Linear tests for all estimates
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df, df_plot = df_plot, label = "All Estimates")

# Non-Linear tests for subsets
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$money == 1,], df_plot = df_plot[df_plot$money == 1,], label = "Money")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$real_effort == 1,],df_plot = df_plot[df_plot$real_effort == 1,], label = "Real Effort")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$aggregate == 1,],df_plot = df_plot[df_plot$aggregate == 1,], label = "Aggregate")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$aggregate == 0,],df_plot = df_plot[df_plot$aggregate == 0,], label = "Individual")

NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$aggregate == 1 & df$money == 1,],df_plot = df_plot[df_plot$aggregate == 1 & df_plot$money == 1,], label = "Aggregate Money")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$aggregate == 1 & df$real_effort == 1,],df_plot = df_plot[df_plot$aggregate == 1 & df_plot$real_effort == 1,], label = "Aggregate Real Effort")

NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$aggregate == 0 & df$money == 1,],df_plot = df_plot[df_plot$aggregate == 0 & df_plot$money == 1,], label = "Individual Money")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$aggregate == 0 & df$real_effort == 1,],df_plot = df_plot[df_plot$aggregate == 0 & df_plot$real_effort == 1,], label = "Individual Real Effort")

NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$source == "Cheung et al." & df$money==1,], df_plot = df_plot[df_plot$source == "Cheung et al." & df_plot$money==1,], label = "Cheung et al. only")

NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$published == 1,],df_plot = df_plot[df_plot$published == 1,], label = "Published")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$published == 0,],df_plot = df_plot[df_plot$published == 0,], label = "Not Published")

NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$ctb == 1,],df_plot = df_plot[df_plot$ctb == 1,], label = "CTB")

NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$field == 1,],df_plot = df_plot[df_plot$field == 1,], label = "Field")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$lab == 1,],df_plot = df_plot[df_plot$lab == 1,], label = "Lab")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$online == 1,],df_plot = df_plot[df_plot$online == 1,], label = "Online")

NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$ml == 1,],df_plot = df_plot[df_plot$ml == 1,], label = "ML")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$nls == 1,],df_plot = df_plot[df_plot$nls == 1,], label = "NLS")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$tobit == 1,],df_plot = df_plot[df_plot$tobit == 1,], label = "Tobit")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$switch_point == 1,],df_plot = df_plot[df_plot$switch_point == 1,], label = "Switch Point")

NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se,df=df[df$utility_control == 1 & df$deal_transaction_cost == 1 & df$deal_uncertainty == 1,], df_plot[df_plot$utility_control == 1 & df_plot$deal_transaction_cost == 1 & df_plot$deal_uncertainty == 1,], label = "Confounding control")

NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$preffered_estimate == 1,],df_plot = df_plot[df_plot$preffered_estimate == 1,], label = "Preferred")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$appendix == 1,],df_plot = df_plot[df_plot$preffered_estimate == 1,], label = "Preferred")

NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$asia == 1,],df_plot = df_plot[df_plot$asia == 1,], label = "Asia")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$europe == 1,],df_plot = df_plot[df_plot$europe == 1,], label = "Europe")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$north_america == 1,],df_plot = df_plot[df_plot$north_america == 1,], label = "North America")

NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$experiment == 1,],df_plot = df_plot[df_plot$experiment == 1,], label = "Experiment")
NLT(win_beta_estimate = win_beta_estimate, win_beta_se = win_beta_se, df=df[df$survey == 1,], df_plot = df_plot[df_plot$survey == 1,], label = "Survey")



#------------------------------
# Techniques Allowing for Endogeneity
#------------------------------

ggplot(data = df, aes(x = sample_size)) +
  geom_histogram(color = "blue", fill = "blue", alpha=0.3, binwidth = 100 )


#------------------------------
# preliminary IV tests
#------------------------------

# 1/sqrt(N)
instrument1 <- 1/sqrt(df$sample_size) #instrument to be used as weight
IV_reg1 <- ivreg(formula = win_beta_estimate ~ win_beta_se | instrument1, data = df)
IV_reg1_clust <- summary(IV_reg1, vcov. = function(x) vcovHC(x, cluster = df$study_id), diagnostics = TRUE)
IV_reg1_clust

t_IV1 <- (IV_reg1_clust$coefficients["(Intercept)", "Estimate"] - 1)/IV_reg1_clust$coefficients["(Intercept)", "Std. Error"]
p_IV1 <- 2 * pt(abs(t_IV1), df = Inf, lower.tail = FALSE)
p_IV1

# 1/N
instrument2 <- 1/(df$sample_size)
IV_reg2 <- ivreg(formula = win_beta_estimate ~ win_beta_se | instrument2, data = df)
IV_reg2_clust <- summary(IV_reg2, vcov. = function(x) vcovHC(x, cluster = df$study_id), diagnostics = TRUE)
IV_reg2_clust

t_IV2 <- (IV_reg2_clust$coefficients["(Intercept)", "Estimate"] - 1)/IV_reg2_clust$coefficients["(Intercept)", "Std. Error"]
p_IV2 <- 2 * pt(abs(t_IV2), df = Inf, lower.tail = FALSE)
p_IV2

# 1/N^2
instrument3 <- 1/(df$sample_size^2)
IV_reg3 <- ivreg(formula = win_beta_estimate ~ win_beta_se | instrument3, data = df)
IV_reg3_clust <- summary(IV_reg3, vcov. = function(x) vcovHC(x, cluster = df$study_id), diagnostics = TRUE)
IV_reg3_clust

t_IV3 <- (IV_reg3_clust$coefficients["(Intercept)", "Estimate"] - 1)/IV_reg3_clust$coefficients["(Intercept)", "Std. Error"]
p_IV3 <- 2 * pt(abs(t_IV3), df = Inf, lower.tail = FALSE)
p_IV3

# 1/log(N)
instrument4 <- log(df$sample_size)
IV_reg4 <- ivreg(formula = win_beta_estimate ~ win_beta_se | instrument4, data = df)
IV_reg4_clust <- summary(IV_reg4, vcov. = function(x) vcovHC(x, cluster = df$study_id), diagnostics = TRUE)
IV_reg4_clust

t_IV4 <- (IV_reg4_clust$coefficients["(Intercept)", "Estimate"] - 1)/IV_reg4_clust$coefficients["(Intercept)", "Std. Error"]
p_IV4 <- 2 * pt(abs(t_IV4), df = Inf, lower.tail = FALSE)
p_IV4


#------------------------------
# p-uniform*
#------------------------------
run_puni <- function(df){

df$DPB <- 1 - df$win_beta_estimate
df_plot$DPB <- 1 - df_plot$beta_estimate

df_study_medians_win <- df%>%
  group_by(study_id) %>%
  summarise(
    median_win_beta_estimate = median(win_beta_estimate, na.rm = TRUE),
    median_win_beta_se       = median(win_beta_se, na.rm = TRUE),
    median_win_DPB           = median(DPB, na.rm = TRUE),
    n_estimates              = n(),
    .groups = "drop"
  )

puni <- puni_star(yi = df_study_medians_win$median_win_DPB, vi = df_study_medians_win$median_win_beta_se^2, side = "right", method = "ML", alpha = 0.05)
puni
}

run_puni(df)
run_puni(df[df$money==1,])
run_puni(df[df$real_effort==1,])
run_puni(df[df$lab==1,])
run_puni(df[df$field==1,])
run_puni(df[df$online==1,])
run_puni(df[df$ml==1,])
run_puni(df[df$nls==1,])
run_puni(df[df$tobit==1,])
run_puni(df[df$switch_point==1,])
run_puni(df[df$published==1,])
run_puni(df[df$published==0,])
run_puni(df[df$preffered_estimate==1,])
run_puni(df[df$appendix==1,])
run_puni(df[df$asia==1,])
run_puni(df[df$europe==1,])
run_puni(df[df$north_america==1,])
run_puni(df[df$experiment==1,])
run_puni(df[df$survey==1,])


#------------------------------
# MAIVE
#------------------------------

maive_subset <- function(df){
dat <- df %>%
  select(
    bs       = win_beta_estimate,
    sebs     = win_beta_se,
    Ns       = sample_size,
    study_id = study_id
)


# standard MAIVE
maive1_noboot <- maive1 <- maive(dat = dat, method = 3, weight = 0, instrument = 1, studylevel = 2, SE = 3, AR = 1, first_stage = 0L)
maive1_noboot$boot_result <- NULL
print(maive1_noboot)
}

maive_subset(df)
maive_subset(df[df$money==1,])
maive_subset(df[df$field==1,])
maive_subset(df[df$real_effort==1,])
maive_subset(df[df$lab==1,])
maive_subset(df[df$online==1,])
maive_subset(df[df$ml==1,])
maive_subset(df[df$nls==1,])

maive_subset(df[df$tobit==1,])
maive_subset(df[df$north_america==1,])
maive_subset(df[df$switch_point==1,])
maive_subset(df[df$asia==1,])
maive_subset(df[df$published==1,])
maive_subset(df[df$published==0,])
maive_subset(df[df$preffered_estimate==1,])
maive_subset(df[df$appendix==1,])
maive_subset(df[df$asia==1,])
maive_subset(df[df$europe==1,])
maive_subset(df[df$experiment==1,])
maive_subset(df[df$survey==1,])




#------------------------------
# Hisogram of t-statistics 
#------------------------------

# Calculate t-statistics
df$t_statistic <- (1-df$beta_estimate) / df$beta_se

binwidth <- 0.2

t_hist <- ggplot(df, aes(x = t_statistic)) +
  geom_histogram(
    aes(
      y = after_stat(density),
      fill = after_stat(
        ifelse(
          abs(x - 0) < binwidth |
            abs(x - 1.96) < binwidth |
            abs(x + 1.96) < binwidth,
          "highlight", "normal"
        )
      )
    ),
    color = "black",
    binwidth = binwidth,
    boundary = 0
  ) +
  geom_density(color = "black", linewidth = 0.5, adjust = 0.5) +
  geom_vline(xintercept = 0, color = "red", linewidth = 0.7, linetype = "dashed") +
  geom_vline(xintercept = c(-1.96, 1.96),
             color = "red", linewidth = 0.7) +
  scale_fill_manual(values = c("normal" = "grey90", "highlight" = "red")) +
  labs(x = "t-statistic of Degree of Present Bias estimates", y = "Density") +
  scale_x_continuous(
    breaks = c(-4, -1.96, 0, 1.96, 4, 8, 12, 16, 20),
    labels = c(
      "-4",
      "<span style='color:red'>-1.96</span>",
      "0",
      "<span style='color:red'>1.96</span>\n",
      "4", "8", "12", "16", "20"
    ),
    limits = c(-5, 15)
  )+
  theme(
    axis.text.x = element_markdown(color = "black"),
    axis.text.y = element_text(colour = "black"),
    axis.title.x = element_text(size = 14),  #
    axis.title.y = element_text(size = 14),  
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.background = element_rect(fill = "white"),
    legend.position = "none"
  )


t_hist


#------------------------------
# Significancy funnel - from AK
#------------------------------

#for drawing
critval=1.96

metastudies_plot <- function(X, sigma) {
  n <- length(X)
  significant <- (abs(X / sigma) > critval)
  nooutlier <- (sigma < 30 * mean(sigma)) & (abs(X) < 30 * mean(abs(X)))
  dat <- data.frame(X, sigma, as.factor(significant & nooutlier))
  names(dat) <- c("xvar", "yvar", "significant")
  rangeX <- 1.1 * max(max(abs(X)), max(abs(sigma[nooutlier])) * critval)
  
  dat <- dat[order(dat$significant), ]
  
  ggplot(dat, aes(x = xvar, y = yvar)) +
    xlab("Degree of Present Bias Estimate") +
    ylab("Standard Error") +
    geom_abline(intercept = 0, slope = 1 / critval, color = "grey") +
    geom_abline(intercept = 0, slope = -1 / critval, color = "grey") +
    geom_point(pch = 21, aes(colour = significant, fill = significant),
               alpha = min(.8, max(40 / n, .3))) +
    scale_fill_manual(values = c("grey", "blue")) +
    scale_colour_manual(values = c("grey50", "blue")) +
    scale_x_continuous(expand = c(0, 0), limits = c(-0.5, 0.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 0.25)) +
    theme(
      legend.position = c(0.02, 0.02),      # bottom-left corner
      legend.justification = c(0, 0),       # anchor legend to bottom-left
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),          # remove all grid lines
      panel.border = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title.x = element_text(size = 14),  #
      axis.title.y = element_text(size = 14)  
    )
}

metastudies_plot((1 - df$beta_estimate), df$beta_se)


#------------------------------
# Caliper Test around significance levels - from Baydadaeva
#------------------------------

# Define significant levels based on t-statistic
df$significant_level <- 0
df$significant_level[df$t_statistic > 1.96] <- 1
df$significant_level[df$t_statistic < -1.96] <- 1

# Function to perform Caliper tests for specific bounds
caliper_test_bounds <- function(df, lower_bound, upper_bound) {
  # Filter df by predefined bounds
  df <- filter(df, df$t_statistic < upper_bound & df$t_statistic > lower_bound)
  print(nrow(df))
  # Perform linear regression
  lm_model <- lm(formula = significant_level ~ 1, data = df)
  
  # Clustered standard errors
  coeftest_result <- coeftest(lm_model, vcov = vcovHC(lm_model, type = "const", cluster = df$study_id))
  
  return(coeftest_result)
}

# Perform Caliper tests for 1.96 bound
Cal_1 <- caliper_test_bounds(df, 1.91, 2.01)
Cal_2 <- caliper_test_bounds(df, 1.86, 2.06)
Cal_3 <- caliper_test_bounds(df, 1.81, 2.11)

# Print results
print(Cal_1)
print(Cal_2)
print(Cal_3)


# Perform Caliper tests for -1.96 bound
Cal_1_neg <- caliper_test_bounds(df, -2.01, -1.91)
Cal_2_neg <- caliper_test_bounds(df, -2.06, -1.86)
Cal_3_neg <- caliper_test_bounds(df, -2.11, -1.81)

# Print results
print(Cal_1_neg)
print(Cal_2_neg)
print(Cal_3_neg)


#------------------------------
# Caliper Tests for different ranges of Beta (as in discrate)
#------------------------------

# Function to perform Caliper tests for specific estimate bounds
caliper_test_est_bounds <- function(df, lower_bound, upper_bound) {
  # Filter df by predefined bounds
  df <- filter(df, df$win_beta_estimate < upper_bound & df$win_beta_estimate > lower_bound)
  cat("\n============================================================\n")
  cat("Number of observation in caliper:",nrow(df))
  
  # Perform linear regression
  caliper_OLS_model <- lm(win_beta_estimate ~ win_beta_se, data = df)
  caliper_clustered_OLS <- coeftest(caliper_OLS_model, vcov = vcovHC(caliper_OLS_model, type = "HC0", cluster = df$study_id))
  
  cat("\n============================================================\n")
  cat("Caliper OLS Results:", "\n")
  print(caliper_clustered_OLS)
  
  # Weighted by precision
  df$precision_win <- 1 / df$win_beta_se
  caliper_OLS_precision <- lm(formula = win_beta_estimate ~ win_beta_se, data = df, weight = c(df$precision_win))
  caliper_clustered_OLS_precision<- coeftest(caliper_OLS_precision, vcov = vcovHC(caliper_OLS_precision, type = "HC0", cluster = c(df$study_id))) 
  cat("\n============================================================\n")
  cat("Caliper OLS weighted by precision Results:", "\n")
  print(caliper_clustered_OLS_precision)
}


# OLS and Precision weights around beta=1
caliper_test_est_bounds(df, 0.8, 1.2)
caliper_test_est_bounds(df, 0.9, 1.1)
caliper_test_est_bounds(df, 0.95, 1.05)


# OLS and Precision weights for beta safely < 1
caliper_test_est_bounds(df, 0.55, 0.95)
caliper_test_est_bounds(df, 0.6, 0.9)
caliper_test_est_bounds(df, 0.7, 0.8)



#------------------------------
# Heterogeneity 
#------------------------------


#------------------------------
# BMA 
#------------------------------

df_BMA <- df
df_BMA = transform(df_BMA,ID = as.numeric(factor(study_id)))
df_BMA$ID = as.factor(df_BMA$ID)

# Selecting only usable variables
df_BMA = subset(df_BMA, select = c(win_beta_estimate, win_beta_se, preffered_estimate, appendix, aggregate, individual, treatment, sample_size, uni_students, general_pop, adolescents, clinical_pop, subsample, female, age_mean, lab, field, online, school_work, africa, north_america, south_america, europe, asia, australia, experiment, survey, longitudinal, real, money, real_effort, food, health, environmental, soon_cash, soon_check, soon_bank, soon_voucher, soon_paypal, soon_postal, late_cash, late_check, late_bank, late_voucher, late_paypal, late_postal, payment_same, mpl, matching, ctb, wage, elicit_other, num_sooner, num_delay, num_frames, FED, end, day, diff_day, deal_uncertainty, deal_transaction_cost, utility_control, CRRA, switch_point, ml, ols, nls, tobit, estimate_other, fixed, estimated, economics, neuroscience, psychology, joint_estimation, published, pub_top_five, pub_year, log_citat_per_year, impact_factor))
summary(df_BMA)

# Dropping baseline variables
df_BMA = subset(df_BMA, select = -c(aggregate, general_pop, lab, europe, experiment, money, soon_cash, late_cash, ctb, end, ml, economics)) 
summary(df_BMA)

# Deal with missing observations: impute 
# For float or dummy -> median

df_BMA$age_mean[is.na(df_BMA$age_mean)] <- median(df_BMA$age_mean, na.rm = TRUE)
df_BMA$age_sd[is.na(df_BMA$age_sd)] <- median(df_BMA$age_sd, na.rm = TRUE)
df_BMA$num_sooner[is.na(df_BMA$num_sooner)] <- median(df_BMA$num_sooner, na.rm = TRUE)
df_BMA$num_delay[is.na(df_BMA$num_delay)] <- median(df_BMA$num_delay, na.rm = TRUE)
df_BMA$num_frames[is.na(df_BMA$num_frames)] <- median(df_BMA$num_frames, na.rm = TRUE)
df_BMA$FED[is.na(df_BMA$FED)] <- median(df_BMA$FED, na.rm = TRUE)
df_BMA$impact_factor[is.na(df_BMA$impact_factor)] <- median(df_BMA$impact_factor, na.rm = TRUE)

# For ratios -> mean
df_BMA$female[is.na(df_BMA$female)] <- median(df_BMA$female, na.rm = TRUE)


# Check 
summary(df_BMA)
means_df <- data.frame(
  variable = names(df_BMA),
  mean = sapply(df_BMA, function(x)
    if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
  )
)

subset(means_df, mean < 0.03 | mean > 0.97)

# Soon and late payment methods have high correlation -> use only soon
df_BMA = subset(df_BMA, select = -c(late_check, late_bank, late_voucher, late_paypal, late_postal)) 

# Pool dummy variables with mean <0.03 as "other", or omit them
df_BMA = subset(df_BMA, select = -c(clinical_pop)) #only 0.6% of observations
df_BMA$continent_other  <- pmax(df_BMA$australia, df_BMA$south_america)
df_BMA$reward_other     <- pmax(df_BMA$food, df_BMA$health, df_BMA$environmental)
df_BMA$discipline_other <- pmax(df_BMA$psychology, df_BMA$neuroscience)
df_BMA$delivery_other   <- pmax(df_BMA$soon_postal, df_BMA$soon_voucher)
df_BMA$estimation_other   <- pmax(df_BMA$estimate_other, df_BMA$ols)
df_BMA$elicitation_other   <- pmax(df_BMA$elicit_other, df_BMA$matching)

df_BMA = subset(df_BMA, select = -c(australia, south_america, food, health, psychology, neuroscience, soon_postal, soon_voucher, environmental, elicit_other, estimate_other, matching, ols))


######## Correlations and VIFs ###########
col= colorRampPalette(c("#FF474C", "white", "#00008B"))
M = cor(df_BMA[,-1])
corrplot.mixed(M, lower = "number", upper = "circle", lower.col=col(200), upper.col=col(200), tl.pos = c("lt"), diag = c("u"), tl.col="black", tl.srt=45, tl.cex=0.5, number.cex = 0.3, cl.cex=0.8, cl.ratio=0.1)

model = lm(win_beta_estimate ~., data = df_BMA)
summary(model)
sort(vif(model))

# Omit variables with VIF > 10
df_BMA = subset(df_BMA, select = -c(mpl)) 
df_BMA = subset(df_BMA, select = -c(num_frames)) 
df_BMA = subset(df_BMA, select = -c(pub_top_five)) 

# Check
model = lm(win_beta_estimate ~., data = df_BMA)
summary(model)
M = cor(df_BMA[,-1])
corrplot.mixed(M, lower = "number", upper = "circle", lower.col=col(200), upper.col=col(200), tl.pos = c("lt"), diag = c("u"), tl.col="black", tl.srt=45, tl.cex=0.5, number.cex = 0.3, cl.cex=0.8, cl.ratio=0.1)
sort(vif(model))

# SE interactions
df_BMA$SE_field <- df_BMA$win_beta_se*df_BMA$field
df_BMA$SE_switch_point <- df_BMA$win_beta_se*df_BMA$switch_point
df_BMA$SE_real_effort <- df_BMA$win_beta_se*df_BMA$real_effort

# Assign readable names
{
colnames(df_BMA)[colnames(df_BMA) == "win_beta_estimate"] <- "Present Bias Estimate"
colnames(df_BMA)[colnames(df_BMA) == "win_beta_se"] <- "SE"
colnames(df_BMA)[colnames(df_BMA) == "preffered_estimate"] <- "Preferred Estimate"
colnames(df_BMA)[colnames(df_BMA) == "appendix"] <- "Appendix"
colnames(df_BMA)[colnames(df_BMA) == "individual"] <- "Individual"
colnames(df_BMA)[colnames(df_BMA) == "treatment"] <- "Treatment"
colnames(df_BMA)[colnames(df_BMA) == "sample_size"] <- "Sample Size"
colnames(df_BMA)[colnames(df_BMA) == "uni_students"] <- "University Students"
colnames(df_BMA)[colnames(df_BMA) == "adolescents"] <- "Adolescents"
colnames(df_BMA)[colnames(df_BMA) == "subsample"] <- "Subsample"
colnames(df_BMA)[colnames(df_BMA) == "female"] <- "Female"
colnames(df_BMA)[colnames(df_BMA) == "age_mean"] <- "Age Mean"
colnames(df_BMA)[colnames(df_BMA) == "field"] <- "Field"
colnames(df_BMA)[colnames(df_BMA) == "online"] <- "Online"
colnames(df_BMA)[colnames(df_BMA) == "school_work"] <- "School/Work"
colnames(df_BMA)[colnames(df_BMA) == "africa"] <- "Africa"
colnames(df_BMA)[colnames(df_BMA) == "north_america"] <- "North America"
colnames(df_BMA)[colnames(df_BMA) == "asia"] <- "Asia"
colnames(df_BMA)[colnames(df_BMA) == "survey"] <- "Survey"
colnames(df_BMA)[colnames(df_BMA) == "longitudinal"] <- "Longitudinal"
colnames(df_BMA)[colnames(df_BMA) == "real"] <- "Real"
colnames(df_BMA)[colnames(df_BMA) == "real_effort"] <- "Real Effort"
colnames(df_BMA)[colnames(df_BMA) == "soon_check"] <- "Soon Check"
colnames(df_BMA)[colnames(df_BMA) == "soon_bank"] <- "Soon Bank"
colnames(df_BMA)[colnames(df_BMA) == "soon_paypal"] <- "Soon PayPal"
colnames(df_BMA)[colnames(df_BMA) == "payment_same"] <- "Payment Same"
colnames(df_BMA)[colnames(df_BMA) == "matching"] <- "Matching"
colnames(df_BMA)[colnames(df_BMA) == "wage"] <- "Work for Wage"
colnames(df_BMA)[colnames(df_BMA) == "num_sooner"] <- "Num Sooner"
colnames(df_BMA)[colnames(df_BMA) == "num_delay"] <- "Num Delay"
colnames(df_BMA)[colnames(df_BMA) == "FED"] <- "FED"
colnames(df_BMA)[colnames(df_BMA) == "day"] <- "Day"
colnames(df_BMA)[colnames(df_BMA) == "diff_day"] <- "Diff Day"
colnames(df_BMA)[colnames(df_BMA) == "deal_uncertainty"] <- "Deal Uncertainty"
colnames(df_BMA)[colnames(df_BMA) == "deal_transaction_cost"] <- "Deal Transaction Cost"
colnames(df_BMA)[colnames(df_BMA) == "utility_control"] <- "Utility Control"
colnames(df_BMA)[colnames(df_BMA) == "risk_prefer"] <- "Risk Prefer"
colnames(df_BMA)[colnames(df_BMA) == "CRRA"] <- "CRRA"
colnames(df_BMA)[colnames(df_BMA) == "switch_point"] <- "Switch Point"
colnames(df_BMA)[colnames(df_BMA) == "nls"] <- "NLS"
colnames(df_BMA)[colnames(df_BMA) == "ols"] <- "OLS"
colnames(df_BMA)[colnames(df_BMA) == "tobit"] <- "Tobit"
colnames(df_BMA)[colnames(df_BMA) == "fixed"] <- "Fixed"
colnames(df_BMA)[colnames(df_BMA) == "estimated"] <- "Estimated"
colnames(df_BMA)[colnames(df_BMA) == "joint_estimation"] <- "Joint Estimation"
colnames(df_BMA)[colnames(df_BMA) == "published"] <- "Published"
colnames(df_BMA)[colnames(df_BMA) == "pub_year"] <- "Pub Year"
colnames(df_BMA)[colnames(df_BMA) == "log_citat_per_year"] <- "Log Citations per Year"
colnames(df_BMA)[colnames(df_BMA) == "impact_factor"] <- "Impact Factor"
colnames(df_BMA)[colnames(df_BMA) == "continent_other"] <- "Continent Other"
colnames(df_BMA)[colnames(df_BMA) == "reward_other"] <- "Reward Other"
colnames(df_BMA)[colnames(df_BMA) == "discipline_other"] <- "Discipline Other"
colnames(df_BMA)[colnames(df_BMA) == "delivery_other"] <- "Delivery Other"
colnames(df_BMA)[colnames(df_BMA) == "SE_real_effort"] <- "SE x Real Effort"
colnames(df_BMA)[colnames(df_BMA) == "SE_tobit"] <- "SE x Tobit"
colnames(df_BMA)[colnames(df_BMA) == "SE_field"] <- "SE x Field"
colnames(df_BMA)[colnames(df_BMA) == "SE_switch_point"] <- "SE x Switch Point"
colnames(df_BMA)[colnames(df_BMA) == "estimation_other"] <- "Estimation Other"
colnames(df_BMA)[colnames(df_BMA) == "elicitation_other"] <- "Elicitation Other"

}

mean(df_BMA$`SE x Real Effort`)
sd(df_BMA$`SE x Real Effort`)
mean(df_BMA$`SE x Field`)
sd(df_BMA$`SE x Field`)
mean(df_BMA$`SE x Switch Point`)
sd(df_BMA$`SE x Switch Point`)

col= colorRampPalette(c("red", "white", "#2e00fa"))
M = cor(df_BMA[,-1])
corrplot.mixed(M, lower = "number", upper = "circle", lower.col=col(200), upper.col=col(200), tl.pos = c("lt"), diag = c("u"), tl.col="black", tl.srt=45, tl.cex=0.5, number.cex = 0.3, cl.cex=0.8, cl.ratio=0.1)
M

summary(df_BMA)
#### BMA #### 

# unit information g-prior, uniform model prior

#get plot using lesser number of iterations
BMA_uniform = bms(df_BMA, burn=1e5, iter=20000, g="UIP", mprior="uniform", nmodel=50000, mcmc="bd", user.int=FALSE) 
cairo_pdf("BMA_uniform.pdf", width = 9, height = 9)
image(
  BMA_uniform,
  yprop2pip = FALSE,
  order.by.pip = TRUE,
  do.par = TRUE,
  do.grid = TRUE,
  do.axis = TRUE,
  xlab = "",
  main = "",
  col = c("red","#2e00fa"),
  cex.axis = 0.7,
  covariates = 1:ncol(df_BMA)
)
dev.off()

# numerical results
BMA_uniform = bms(df_BMA, burn=1e5, iter=3000000, g="UIP", mprior="uniform", nmodel=50000, mcmc="bd", user.int=FALSE) 
print(BMA_uniform) 
coef(BMA_uniform, order.by.pip=FALSE, exact=TRUE, include.constant=TRUE) 
summary(BMA_uniform) 
plot(BMA_uniform) 
print(BMA_uniform$topmod[1]) 


# unit information g-prior, dilution model prior

#get plot using lesser number of iterations
BMA_dilut = bms(df_BMA, burn=1e5, iter=20000, g="UIP", mprior="dilut", nmodel=50000, mcmc="bd", user.int=FALSE) 
cairo_pdf("BMA_dilut.pdf", width = 9, height = 9)
image(
  BMA_dilut,
  yprop2pip = FALSE,
  order.by.pip = TRUE,
  do.par = TRUE,
  do.grid = TRUE,
  do.axis = TRUE,
  xlab = "",
  main = "",
  col = c("red","#2e00fa"),
  cex.axis = 0.7,
  covariates = 1:ncol(df_BMA)
)
dev.off()

#numerical results
BMA_dilut = bms(df_BMA, burn=1e5,iter=3e6, g="UIP", mprior="dilut", nmodel=50000, mcmc="bd", user.int=FALSE) 
print(BMA_dilut) 
coef(BMA_dilut,order.by.pip= F, exact=T, include.constant=T) 
summary(BMA_dilut) 
plot(BMA_dilut) 
print(BMA_dilut$topmod[1]) 


# BRIC g-prior, random model prior

#get plot using lesser number of iterations
BMA_BRIC = bms(df_BMA, burn=1e5, iter=20000, g="BRIC", mprior="random", nmodel=50000, mcmc="bd", user.int=FALSE) 
cairo_pdf("BMA_BRIC.pdf", width = 9, height = 9)
image(
  BMA_BRIC,
  yprop2pip = FALSE,
  order.by.pip = TRUE,
  do.par = TRUE,
  do.grid = TRUE,
  do.axis = TRUE,
  xlab = "",
  main = "",
  col = c("red","#2e00fa"),
  cex.axis = 0.7,
  covariates = 1:ncol(df_BMA)
)
dev.off()

BMA_BRIC = bms(df_BMA, burn=1e5,iter=3e6, g="BRIC", mprior="random", nmodel=50000, mcmc="bd", user.int=FALSE) 
print(BMA_BRIC) 
coef(BMA_BRIC,order.by.pip= F, exact=T, include.constant=T) 
summary(BMA_BRIC) 
plot(BMA_BRIC) 
print(BMA_BRIC$topmod[1]) 


# Hannan-Quinn g-prior, random model prior

#get plot using lesser number of iterations
BMA_HQ = bms(df_BMA, burn=1e5, iter=20000, g="HQ", mprior="random", nmodel=50000, mcmc="bd", user.int=FALSE) 
cairo_pdf("BMA_HQ.pdf", width = 9, height = 9)
image(
  BMA_HQ,
  yprop2pip = FALSE,
  order.by.pip = TRUE,
  do.par = TRUE,
  do.grid = TRUE,
  do.axis = TRUE,
  xlab = "",
  main = "",
  col = c("red","#2e00fa"),
  cex.axis = 0.7,
  covariates = 1:ncol(df_BMA)
)
dev.off()

BMA_HQ = bms(df_BMA, burn=1e5,iter=3e6, g="HQ", mprior="random", nmodel=50000, mcmc="bd", user.int=FALSE) 
print(BMA_HQ) 
coef(BMA_HQ,order.by.pip= F, exact=T, include.constant=T) 
summary(BMA_HQ) 
plot(BMA_HQ) 
print(BMA_HQ$topmod[1])

plotComp("UIP and Dilut"=BMA_dilut,"UIP and Uniform"=BMA_uniform,"BRIC and Random"=BMA_BRIC,"HQ and Random"=BMA_HQ, add.grid=F)


#------------------------------
# FMA
#------------------------------

# Order variables by PIP from BMA results
bma_ordered <- coef(BMA_dilut,order.by.pip= T, exact=T)
df_FMA <- df_BMA[, c("Present Bias Estimate", rownames(bma_ordered))]

# Add study_id for cluster_robust SEs
df_FMA$study_id <- df$study_id 

# Run FMA using cluster-robust SEs
run_FMA <- function(df_FMA){
  
  
  # Read cluster IDs
  cluster_id <- df_FMA$study_id
  df_FMA$study_id <- NULL
  
  
  # Prepare independent variables
  x.data <- df_FMA[,-1] # Takes all rows, takes all columns except the first, which is the dependent variable
  const_ <- rep(1, nrow(df_FMA)) # Creates a vector of 1s, explcitly adds a constant instead of adding the intercept in lm()
  x.data <- cbind(const_, x.data) # Binds the constant to the explanatory variables, ie now we have X = (1,X1, X2, ...)
  x <- sapply(1:ncol(x.data), function(i) x.data[, i] / max(x.data[, i])) # Loops over each column and divides by its maximum value, normalizes each explanatory variable to lie in [0,1] to prevent large units dominating in Mallows Criterion
  scale.vector <- as.matrix(sapply(1:ncol(x.data), function(i) max(x.data[, i]))) # Stores the scaling factors used in previous line, to later recover original units
  Y <- as.matrix(df_FMA[, 1]) # Extracts the dependent variable and converts it to a matrix 
  output.colnames <- colnames(x.data) #Stores regressor names
  
  
  
  # Full model fit
  full.fit <- lm(Y ~ x - 1) # Estimates the full (largest) model with all regressors
  beta.full <- as.matrix(coef(full.fit)) # Extracts estimated coefficients and converts them to a matrix
  M <- k <- ncol(x) # Sets k to total number of regressors, and subsequently sets M to number of candidate models - models are nested, M = k
  n <- nrow(x) # Sets number of observations
  beta <- matrix(0, k, M) # Creates a k x M matrix of zeros, to store all coefficient estimates
  e <- matrix(0, n, M) # Creates a n x M matrix of zeros, to store all residuals
  K_vector <- matrix(1:M) # Creates a vector K = (1,2,...,M)
  var.matrix <- matrix(0, k, M) # Creates a k x M matrix of zeros, to store variance estimates for coefficients
  bias.sq <- matrix(0, k, M) # Creates a k x M matrix of zeros, to store squared bias (ie (beta_jm - beta_jfull)^2) for each coefficient and model 
  
  
  
  # Cluster-robust vcov (fully safe version)
  cluster_vcov <- function(X, e, cluster) { # Creates a function to compute cluster-robust covariance matrix for coefficients
    cluster <- as.factor(cluster)
    n <- nrow(X)
    k <- ncol(X)
    G <- length(unique(cluster)) # Counts how many distinct clusters there are 
    dfc <- G / (G - 1) * (n - 1) / (n - k)  # finite-sample correction - computes a degrees-of-freedom correction factor for small samples
    u <- X * e  # multiplies each column of X by residuals e 
    cluster_levels <- levels(cluster)
    cluster_sum <- matrix(0, nrow = G, ncol = k) # Creates a G x k matrix of zeros, to store cluster sums: sum{i in cluster g} (xi * ui) for each cluster 
    for (j in 1:G) { # Computes cluster sum: sum{i in cluster g} (xi * ui) for each cluster
      cl <- cluster_levels[j]
      cluster_sum[j, ] <- colSums(u[cluster == cl, , drop = FALSE])
    }
    meat <- t(cluster_sum) %*% cluster_sum  # multiplies cluster sum matrices to form a k x k matrix, ie the meat 
    bread_inv <- solve(t(X) %*% X)          # computes (X'X)^(-1) 
    vcov_cl <- dfc * bread_inv %*% meat %*% bread_inv # Computes the full cluster-robust covariance matrix, ie multiplies the correction factor, and bread*meat*bread
    return(vcov_cl)
  }
  
  
  
  # Model averaging loop
  for (i in 1:M) { # Loops over models, model i includes i-1 regressor Xi = (1,X1,...,Xi-1) 
    X <- as.matrix(x[, 1:i]) # Takes first i columns of X and converts them to a matrix
    ortho <- eigen(t(X) %*% X) # Computes eigen-decomposition of X'X = QΛQ′
    Q <- ortho$vectors # Extracts eigenvectors
    lambda <- ortho$values # Extracts eigenvalues
    x.tilda <- X %*% Q %*% diag(lambda^(-0.5), i, i) # Constructs orthonormal regressors, X~ =XQΛ^(−1/2)
    beta.star <- t(x.tilda) %*% Y # Regresses Y on orthonormal regressors, β* = (X~)′Y
    beta.hat <- Q %*% diag(lambda^(-0.5), i, i) %*% beta.star # Transforms coefficients back to original basis - OLS coefficients for model i
    beta[1:i, i] <- beta.hat # Stores coefficients for model i 
    e[, i] <- Y - x.tilda %*% beta.star # Computes and stores residuals
    bias.sq[, i] <- (beta[, i] - beta.full)^2 # Computes squared difference versus full model, Bias_ji^2 = (β_ji − βj,full)^2
    
    # Clustered SEs
    e_i <- e[, i] # Extracts reiduals for model i 
    cl_vcov <- cluster_vcov(X, e_i, cluster_id) # Computes cluster-robust covariance matrix using above-defined function
    var.matrix[1:i, i] <- diag(cl_vcov) # Stores diagonal variances 
    var.matrix[1:i, i] <- var.matrix[1:i, i] + bias.sq[1:i, i] # Adds squared bias to variance
  }
  
  
  # Model weights via QP
  e_k <- e[, M] # Extracts residuals from full model
  sigma_hat <- as.numeric((t(e_k) %*% e_k) / (n - M)) # Estimates error variance of full model sigma^2 = u'u/n-M 
  G <- t(e) %*% e # Computes M x M matrix with G_ij = u'_i * u_j
  a <- sigma_hat^2 * K_vector # Multiplies the scalar error variance of full model by model size vector
  A <- matrix(1, 1, M) # Creates a 1 x M vector of 1s, to impose sum(weights)=1
  b <- matrix(1, 1, 1) # Right hand side constraint for Aw = b
  u <- matrix(1, M, 1) # Creates a M x 1 vector of 1s, to impose w_m =< 1
  optim <- LowRankQP(Vmat = G, dvec = a, Amat = A, bvec = b, uvec = u, method = "LU", verbose = FALSE) # Solves for a Mallows-optimal weight vector 
  weights <- as.matrix(optim$alpha) # Extracts the optimal solution
  
  
  # Final estimates
  beta.scaled <- beta %*% weights
  final.beta <- beta.scaled / scale.vector
  std.scaled <- sqrt(var.matrix) %*% weights
  final.std <- std.scaled / scale.vector
  results.reduced <- cbind(final.beta, final.std)
  rownames(results.reduced) <- output.colnames
  colnames(results.reduced) <- c("Coefficient", "Sd. Err")
  
  
  # P-values and formatting
  MMA.fls <- round(results.reduced, 4)
  MMA.fls <- data.frame(MMA.fls)
  t <- MMA.fls$Coefficient / MMA.fls$Sd..Err
  MMA.fls$pv <- round((1 - pnorm(abs(t))) * 2, 3)
  MMA.fls$names <- rownames(MMA.fls)
  # Ensure correct row order
  names <- c(colnames(df_FMA), "const_")
  MMA.fls <- MMA.fls[match(names, MMA.fls$names), ]
  MMA.fls$names <- NULL
  
  # Output results
  return(MMA.fls)
}

run_FMA(df_FMA)


# print table of BMA and FMA results
{
bma <- coef(BMA_dilut, order.by.pip = FALSE, exact = TRUE, include.constant = TRUE) %>%
  as.data.frame()

bma$Variable <- rownames(bma)
bma$Post_Mean <- bma$`Post Mean`
bma$Post_SD <- bma$`Post SD`

fma <- run_FMA(df_FMA) %>% as.data.frame()

fma$Variable <- rownames(fma)

bma_fma <- full_join(bma, fma, by = "Variable")

fmt <- function(x, d = 4) {
  ifelse(is.na(x), "NA", formatC(x, digits = d, format = "f"))
}

bma_fma <- bma_fma %>%
  mutate(
    Mean = fmt(Post_Mean),
    SD   = fmt(Post_SD),
    PIP  = fmt(PIP),
    Coef = fmt(Coefficient),
    SE   = fmt(Sd..Err),
    pval = ifelse(is.na(pv), "--", fmt(pv, 3)),
    Variable = str_replace_all(Variable, " x ", " $\\\\times$ "),
    Variable = ifelse(Variable == "const_", "Constant", Variable)
  )

order_vec <- c(
  "Constant",
  "SE",
  "SE $\\times$ Field",
  "SE $\\times$ Switch Point",
  "SE $\\times$ Real Effort",
  
  "Preferred Estimate", "Appendix", "Individual", "Treatment",
  
  "Survey", "Longitudinal",
  
  "Sample Size", "University Students", "Adolescents", "Subsample",
  "Female", "Age Mean",
  
  "Field", "Online", "School/Work",
  
  "Africa", "North America", "Asia", "Continent Other",
  
  "Real", "Real Effort", "Reward Other",
  
  "Soon Check", "Soon Bank", "Soon PayPal", "Payment Same",
  "Day", "Diff Day", "Delivery Other",
  
  "Work for Wage", "Elicitation Other", "Num Sooner", "Num Delay", "FED",
  "Deal Uncertainty", "Deal Transaction Cost",
  
  "Utility Control", "CRRA", "Switch Point",
  "NLS", "Tobit", "Estimation Other",
  "Fixed", "Estimated", "Joint Estimation",
  
  "Published", "Pub Year", "Log Citations per Year",
  "Impact Factor", "Discipline Other"
)

bma_fma <- bma_fma %>%
  mutate(
    Variable = case_when(
      Variable == "(Intercept)" ~ "Constant",
      Variable == "const_" ~ "Constant",
      TRUE ~ Variable
    )
  ) %>%
  filter(!is.na(Variable), Variable != "NA")

bma_fma$Variable <- factor(bma_fma$Variable, levels = order_vec)
bma_fma <- bma_fma %>% arrange(Variable)
bma_fma$Variable <- as.character(bma_fma$Variable)


for (i in 1:nrow(bma_fma)) {
  cat(
    "\\hspace{0.5cm}", bma_fma$Variable[i], " & ",
    bma_fma$Mean[i], " & ",
    bma_fma$SD[i], " & ",
    bma_fma$PIP[i], " & ",
    bma_fma$Coef[i], " & ",
    bma_fma$SE[i], " & ",
    bma_fma$pval[i], " \\\\\n",
    sep = ""
  )
}
}


#------------------------------
# Best-Practice Estimate(s) - from incentives.R (altered)
#------------------------------

# Define function for calculating best practice

# Extract posterior means from the BMA, calculate sample means for all variables in the dataset
# Define the function (start with sample means, set coefficients for methodological variables reflecting best practice, then apply scenario-specific overrides
# Calculate prediction as "intercept + sum(posterior mean * best-practice value)

bma_coefs <- coef(BMA_dilut, include.constant = TRUE, exact = TRUE)
post_means <- bma_coefs[, "Post Mean"]
input_vars <- df_BMA[, -1] 
sample_means <- colMeans(input_vars, na.rm = TRUE)
pred_df <- df_BMA
names(pred_df) <- make.names(names(pred_df))
OLS_pred <- lm(Present.Bias.Estimate ~ ., data = pred_df)
summary(OLS_pred)

calc_lincom <- function(scenario_name, overrides = list()) {
  current_values <- sample_means

# all SEs to zero -> no publication bias  
  if("SE" %in% names(current_values)) current_values["SE"] <- 0
  if("SE x Field" %in% names(current_values)) current_values["SE x Field"] <- 0
  if("SE x Real Effort" %in% names(current_values)) current_values["SE x Real Effort"] <- 0
  if("SE x Switch Point" %in% names(current_values)) current_values["SE x Switch Point"] <- 0
  
# methodological best-practice choices
  if("Utility Control" %in% names(current_values)) current_values["Utility Control"] <- 1
  if("Deal Transaction Cost" %in% names(current_values)) current_values["Deal Transaction Cost"] <- 1
  if("Deal Uncertainty" %in% names(current_values)) current_values["Deal Uncertainty"] <- 1
  if("FED" %in% names(current_values)) current_values["FED"] <- 1
  if("Diff Day" %in% names(current_values)) current_values["Diff Day"] <- 0
  if("Joint Estimation" %in% names(current_values)) current_values["Joint Estimation"] <- 1
  
# aggregate estimate, incentivized experiment
  if("Individual" %in% names(current_values)) current_values["Individual"] <- 0
  if("Switch Point" %in% names(current_values)) current_values["Switch Point"] <- 0
  if("Survey" %in% names(current_values)) current_values["Survey"] <- 0
  if("Real" %in% names(current_values)) current_values["Real"] <- 1
  
# published, most recent, most cited, most impactful journal
  if("Published" %in% names(current_values)) current_values["Published"] <- 1
  if("Pub Year" %in% names(current_values)) current_values["Pub Year"] <- 2025
  if("Log Citations per Year" %in% names(current_values)) current_values["Log Citations per Year"] <- 3.51 # 95 percentile
  if("Impact Factor" %in% names(current_values)) current_values["Impact Factor"] <- 2.62 # 95 percentile
  
  for (var in names(overrides)) {
    if (var %in% names(current_values)) {
      current_values[var] <- overrides[[var]]
    } else {
      warning(paste("Variable", var, "not found in data. Ignoring."))
    }
  }
  vars_in_model <- setdiff(names(post_means), c("(Intercept)", "Intercept"))
  aligned_values <- current_values[vars_in_model]
  slope_coefs <- post_means[vars_in_model]
  intercept   <- post_means[which(names(post_means) %in% c("(Intercept)", "Intercept"))]
  if(length(intercept) == 0) intercept <- 0
  if(any(is.na(aligned_values))) warning("Some variables in the model have NA means.")
  prediction <- as.numeric(intercept + sum(slope_coefs * aligned_values, na.rm = TRUE))
  
  newdata <- as.data.frame(as.list(current_values))
  pred <- predict(OLS_pred, newdata = newdata, interval = "confidence", level = 0.95, se.fit = TRUE)
  
  return(data.frame(Scenario = scenario_name, Mean_Effect = prediction, CI_lower = prediction-1.96*pred$se.fit, CI_upper = prediction+1.96*pred$se.fit))
}

# Compute best practice including different scenarios

results_list <- list()
results_list[[1]] <- calc_lincom("Mean best practice")
results_list[[2]] <- calc_lincom("Lab", list("Field" = 0,"Online" = 0,"School/Work" = 0))
results_list[[3]] <- calc_lincom("Field", list("Field" = 1,"Online" = 0,"School/Work" = 0))
results_list[[4]] <- calc_lincom("Online", list("Field" = 0,"Online" = 1,"School/Work" = 0))
results_list[[5]] <- calc_lincom("School/Work", list("Field" = 0,"Online" = 0,"School/Work" = 1))
results_list[[6]] <- calc_lincom("Money", list("Real Effort" = 0,"Reward Other" = 0))
results_list[[7]] <- calc_lincom("Real Effort", list("Real Effort" = 1,"Reward Other" = 0))
results_list[[8]] <- calc_lincom("Reward Other", list("Real Effort" = 0,"Reward Other" = 1))
results_list[[9]] <- calc_lincom("Europe", list("Africa" = 0,"North America" = 0,"Asia" = 0,"Continent Other" = 0))
results_list[[10]] <- calc_lincom("Asia", list("Africa" = 0,"North America" = 0,"Asia" = 1,"Continent Other" = 0))
results_list[[11]] <- calc_lincom("North America", list("Africa" = 0,"North America" = 1,"Asia" = 0,"Continent Other" = 0))
results_list[[12]] <- calc_lincom("Africa", list("Africa" = 1,"North America" = 0,"Asia" = 0,"Continent Other" = 0))


final_table <- do.call(rbind, results_list)
row.names(final_table) <- NULL 
print(final_table)








