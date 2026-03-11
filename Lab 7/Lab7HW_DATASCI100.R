
# ------------------------------------------------------------------------------
# Exercise Solutions (Examples)
# ------------------------------------------------------------------------------

# Easy Exercise 1: LocationType and Completed
# Create contingency table
Location_Completion_Table <- table(gardasil$LocationType, 
                                   gardasil$Completed)
Location_Completion_Table

# Calculate row proportions
prop.table(Location_Completion_Table, margin = 1)

# Completion rate for suburban (LocationType = 0)
prop.table(Location_Completion_Table, margin = 1)[1, 2]

# Completion rate for urban (LocationType = 1)
prop.table(Location_Completion_Table, margin = 1)[2, 2]

# Easy Exercise 2: Test difference in completion by location type
# First, create corrected Completed variable if not already done
gardasil$Completed2 <- factor(gardasil$Completed, 
                              levels = c("yes", "no"))

Location_Completion_Table2 <- table(gardasil$LocationType, 
                                    gardasil$Completed2)

# Two-sample proportion test
prop.test(Location_Completion_Table2, correct = FALSE)

# Intermediate Exercise 1: Practice type analysis
# Create contingency table
Practice_Completion_Table <- table(gardasil$PracticeType, 
                                   gardasil$Completed)

Practice_Completion_Table

# Row proportions
prop.table(Practice_Completion_Table, margin = 1)

# Chi-square test
practice_test <- chisq.test(gardasil$Completed, 
                            gardasil$PracticeType, 
                            correct = FALSE)
practice_test

# Check expected counts
practice_test$expected

# Visualization
practice_completion_data <- gardasil %>%
  group_by(PracticeType) %>%
  summarize(completion_rate = mean(Completed == "yes"))

ggplot(practice_completion_data, aes(x = factor(PracticeType), 
                                     y = completion_rate)) +
  geom_bar(stat = "identity")


# Intermediate Exercise 6: Chi-squared distributions with different df
# Create data for multiple distributions
df_values <- c(1, 3, 5, 10)
x_values <- seq(0, 20, length.out = 100)

chisq_distributions <- expand.grid(
  x = x_values,
  df = df_values
) %>%
  mutate(
    density = dchisq(x, df),
    df_label = paste("df =", df)
  )

# Plot all distributions
ggplot(chisq_distributions, aes(x = x, y = density, 
                                color = df_label)) +
  geom_line()

# Calculate critical values for alpha = 0.05
critical_values <- tibble(
  df = df_values,
  critical_value = qchisq(0.95, df = df_values)
)
critical_values

# Add critical values to plot
ggplot(chisq_distributions, aes(x = x, y = density, 
                                color = df_label)) +
  geom_line() +
  geom_vline(data = critical_values, 
             aes(xintercept = critical_value, color = paste("df =", df)),
             linetype = "dashed")

# Calculate p-values for test statistic of 7.5 with different df
test_stat_75 <- 7.5
p_values_75 <- tibble(
  df = c(1, 3, 5),
  p_value = 1 - pchisq(test_stat_75, df = c(1, 3, 5)),
  significant = p_value < 0.05
)
p_values_75
