# ==============================================================================
# Lab 7: Inference for Categorical Data - R Code
# A Modern Approach with Tidyverse
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup and Package Loading
# ------------------------------------------------------------------------------

# Install tidyverse if needed (uncomment to run)
# install.packages("tidyverse")

# Load tidyverse
library(tidyverse)

# ------------------------------------------------------------------------------
# Importing the Data
# ------------------------------------------------------------------------------

# Set working directory (change to your folder)
# setwd("~/Your/Working/Directory/")

# Read in the data
gardasil <- read.table("gardasil.txt", header = TRUE)

# Explore the data
str(gardasil)
summary(gardasil)
glimpse(gardasil)

# ------------------------------------------------------------------------------
# Two Sample Z Test - Completion by Age Group
# ------------------------------------------------------------------------------

# Create frequency table
Age_Completion_Table <- table(gardasil$AgeGroup, gardasil$Completed)

# View table
Age_Completion_Table

# Add summary margins
addmargins(Age_Completion_Table)

# Calculate row proportions
prop.table(Age_Completion_Table, margin = 1)

# Two sample proportion test - manual input
prop.test(c(247, 222), c(701, 712), correct = FALSE)

# Two sample proportion test using table (INCORRECT at first)
prop.test(Age_Completion_Table, correct = FALSE)

# Create new variable with correct ordering
gardasil$Completed2 <- factor(gardasil$Completed, 
                              levels = c("yes", "no"))

# Create new table
Age_Completion_Table2 <- table(gardasil$AgeGroup, 
                               gardasil$Completed2)

# View table
Age_Completion_Table2

# Two sample proportion test with corrected table (CORRECT)
prop.test(Age_Completion_Table2, correct = FALSE)

# Calculate z-statistic from chi-squared
sqrt(2.62)

# ------------------------------------------------------------------------------
# Chi-Square Test - Completion by Insurance Type
# ------------------------------------------------------------------------------

# Table of frequencies
Insurance_Completion_Table <- table(gardasil$InsuranceType, 
                                    gardasil$Completed)

# View table
Insurance_Completion_Table

# Add summary margins
addmargins(Insurance_Completion_Table)

# Calculate row proportions
prop.table(Insurance_Completion_Table, margin = 1)

# Chi-Square Test for completion by insurance type - using variables
chisq.test(gardasil$Completed, gardasil$InsuranceType, 
           correct = FALSE)

# Chi-Square Test using table
chisq.test(Insurance_Completion_Table, correct = FALSE)

# Run and save chi square test
Ins.Comp.test <- chisq.test(gardasil$Completed, 
                            gardasil$InsuranceType, 
                            correct = FALSE)

# View expected cell counts
Ins.Comp.test$expected

# View observed counts
Ins.Comp.test$observed

# View test statistic
Ins.Comp.test$statistic

# View p-value
Ins.Comp.test$p.value

# ------------------------------------------------------------------------------
# Fisher's Exact Test
# ------------------------------------------------------------------------------

# Run Fisher's Exact with variables
fisher.test(gardasil$Completed, gardasil$InsuranceType)

# Run Fisher's Exact with existing table
fisher.test(Insurance_Completion_Table)

# ------------------------------------------------------------------------------
# Chi-squared Distribution
# ------------------------------------------------------------------------------

# Calculate p-value manually for age group test
1 - pchisq(2.62, df = 1)

# Compare to result from prop.test
prop.test(Age_Completion_Table2, correct = FALSE)$p.value

# Calculate p-value for insurance type test
1 - pchisq(Ins.Comp.test$statistic, df = 3)


# ------------------------------------------------------------------------------
# Visualizing Chi-squared Distribution
# ------------------------------------------------------------------------------

# Create chi-squared distribution with df = 1
x_values <- seq(0, 10, length.out = 100)
y_values <- dchisq(x_values, df = 1)

chisq_dist <- tibble(
  x = x_values,
  density = y_values
)

# Plot with our test statistic from age group test
ggplot(chisq_dist, aes(x = x, y = density)) +
  geom_line() +
  geom_vline(xintercept = 2.62, color = "blue")

# Create chi-squared distribution with df = 3 for insurance test
x_values <- seq(0, 20, length.out = 100)
y_values <- dchisq(x_values, df = 3)

chisq_dist_df3 <- tibble(
  x = x_values,
  density = y_values
)

# Get the test statistic
test_stat <- Ins.Comp.test$statistic

# Plot
ggplot(chisq_dist_df3, aes(x = x, y = density)) +
  geom_line() +
  geom_vline(xintercept = test_stat, color = "blue")

# ------------------------------------------------------------------------------
# End of Lab 7 Code
# ------------------------------------------------------------------------------
