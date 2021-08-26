#### Power Analysis ####
# Appealing to the local voter 
# Date: 26.08.2021
# R version 4.0.3
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Author: Dominik Schraff

library(DeclareDesign) # Version 0.26.0
library(DesignLibrary) # Version 0.1.6
library(lme4) # Version 1.1-25
library(ggplot2) # Version 3.3.2

# Pooled

appeal <- multi_arm_designer(N = 2500, m_arms = 4, outcome_means = c(0,1,2.5,2),
                             outcome_sds =  c(2.5,2.5,2.5,2.5),
                             conditions = c("control", "symbolic", "cultural", "economic"))

diagnosis <- diagnose_design(appeal, sims = 500)
diagnosis

pooled.report <- data.frame(EstimatorLabel = diagnosis$diagnosands_df$estimator_label,
                           power = diagnosis$diagnosands_df$power,
                           mean_estimate = diagnosis$diagnosands_df$mean_estimate)

ggplot(data=pooled.report, aes(x=EstimatorLabel, y=power, fill = EstimatorLabel)) +
  geom_bar(stat="identity") + ggtitle("Pooled sample (N=2500)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Rural 

appeal.rural <- multi_arm_designer(N = 1250, m_arms = 4, outcome_means = c(0,1.5,3,2),
                                   outcome_sds =  c(2.5,2.5,2.5,2.5),
                             conditions = c("control", "symbolic", "cultural", "economic"))

diagnosis.rural <- diagnose_design(appeal.rural, sims = 500)
diagnosis.rural

rural.report <- data.frame(EstimatorLabel = diagnosis.rural$diagnosands_df$estimator_label,
          power = diagnosis.rural$diagnosands_df$power,
          mean_estimate = diagnosis.rural$diagnosands_df$mean_estimate)

ggplot(data=rural.report, aes(x=EstimatorLabel, y=power, fill = EstimatorLabel)) +
  geom_bar(stat="identity") + ggtitle("Rural identifiers (N=1250)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Urban

appeal.urban <- multi_arm_designer(N = 1250, m_arms = 4, outcome_means = c(0,1,1,2),
                                   outcome_sds =  c(2.5,2.5,2.5,2.5),
                                   conditions = c("control", "symbolic", "cultural", "economic"))

diagnosis.urban <- diagnose_design(appeal.urban, sims = 500)
diagnosis.urban

urban.report <- data.frame(EstimatorLabel = diagnosis.urban$diagnosands_df$estimator_label,
                           power = diagnosis.urban$diagnosands_df$power,
                           mean_estimate = diagnosis.urban$diagnosands_df$mean_estimate)

ggplot(data=urban.report, aes(x=EstimatorLabel, y=power, fill = EstimatorLabel)) +
  geom_bar(stat="identity") + ggtitle("Urban identifiers (N=1250)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Simulate data

rural.dat <- draw_data(appeal.rural)
rural.dat$rural <- 1

urban.dat <- draw_data(appeal.urban)
urban.dat$rural <- 0

pooled.dat <- rbind(urban.dat, rural.dat)

# Analysis

lm0 <- lm(Y ~ Z, data = pooled.dat)
summary(lm0)

lm1 <- lm(Y ~ Z*rural, data = pooled.dat)
summary(lm1)


