# ------------------------------------------------------------------------------
# Exercise Solutions (Examples)
# ------------------------------------------------------------------------------

# Exercise 1: Average and median gestation
babies %>%
  summarize(
    mean_gestation = mean(gestation, na.rm = TRUE),
    median_gestation = median(gestation, na.rm = TRUE)
  )

# Exercise 2: Mothers younger than 25
babies %>%
  filter(age < 25) %>%
  count()

babies %>%
  summarize(
    n_under_25 = sum(age < 25, na.rm = TRUE),
    total = n(),
    proportion = n_under_25 / total
  )

# Exercise 3: Histogram of mother's height
babies %>%
  ggplot(aes(x = height)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Mother's Height",
    x = "Height (inches)",
    y = "Count"
  ) +
  theme_minimal()

# Exercise 4: Gestation by parity
babies %>%
  group_by(parityf) %>%
  summarize(
    mean_gestation = mean(gestation, na.rm = TRUE),
    sd_gestation = sd(gestation, na.rm = TRUE),
    n = n()
  )

# Exercise 6: Create BMI variable
babies <- babies %>%
  mutate(bmi = (weight * 703) / (height^2))

babies %>%
  summarize(mean_bmi = mean(bmi, na.rm = TRUE))

# Intermediate Exercise 2: BMI categories
babies <- babies %>%
  mutate(
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obese",
      TRUE ~ NA_character_
    ),
    bmi_category_f = factor(bmi_category,
                            levels = c("Underweight", "Normal", 
                                       "Overweight", "Obese"))
  )

babies %>%
  count(bmi_category_f) %>%
  mutate(proportion = n / sum(n))

# Exercise 7: Boxplot by BMI category
babies %>%
  filter(!is.na(bmi_category_f)) %>%
  ggplot(aes(x = bmi_category_f, y = bwt, fill = bmi_category_f)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Birth Weight by Mother's BMI Category",
    x = "BMI Category",
    y = "Birth Weight (ounces)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
