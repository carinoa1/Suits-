## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
table(data$QUANT_VAR1)
mean(data$QUANT_VAR1)
sd(data$QUANT_VAR1)
summary(data$QUANT_VAR1)
describe(data$QUANT_VAR1)

table(data$QUANT_VAR2)
mean(data$QUANT_VAR2)
sd(data$QUANT_VAR2)
summary(data$QUANT_VAR2)
describe(data$QUANT_VAR2)

table(data$QUAL_VAR1)
mean(data$QUAL_VAR1)
sd(data$QUAL_VAR1)
summary(data$QUAL_VAR1)
describe(data$QUAL_VAR1)

table(data$QUAL_VAR2)
mean(data$QUAL_VAR2)
sd(data$QUAL_VAR2)
summary(data$QUAL_VAR2)
describe(data$QUAL_VAR2)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$QUAL_VAR1,data$QUAL_VAR2)


##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(table(data$QUAL_VAR1,data$QUAL_VAR2))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
# Perform ANOVA
anova_adapted <- aov(QUAL_VAR1 ~ QUANT_VAR2, data = raw_data)
# Summarize ANOVA results
summary(anova_adapted)
# total SS; TSS
5+0.1
# get R2
# between/total
# OR between/(between+within)
0.1/(0.1+5)
##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$QUAL_VAR1, data$QUANT_VAR2)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
lm(data$QUANT_VAR2 ~ data$QUAL_VAR1, data = raw_data)
summary(linear_relationship)
##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
linear_plot <- plot(raw_data$QUAL_VAR1, raw_data$QUANT_VAR3)
print(linear_plot)
linear_relationship <- lm(QUAL_VAR1 ~ QUANT_VAR3, data = raw_data)
summary(linear_relationship)
abline(linear_relationship, col = "red")
abline(h=mean(raw_data$QUAL_VAR1))
abline(h=mean(raw_data$QUANT_VAR3))
##################################################################################
####################  STEP 8: Examine residuals                     ####################   
###############################################################################
plot(raw_data$QUAL_VAR2, residuals(linear_relationship))
abline(v=mean(raw_data$QUAL_VAR1)) 
