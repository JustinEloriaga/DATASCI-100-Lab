
# ------------------------------------------------------------------------------
# Exercise Solutions (Examples)
# ------------------------------------------------------------------------------

# Basic Exercise 1: Clean income variable
# Examine income
summary(acs$Income)
acs %>% count(Income > 500)

# Create cleaned income variable
acs <- acs %>%
  mutate(Income2 = if_else(Income > 500, NA_real_, Income))

# Check how many affected
acs %>%
  summarize(
    n_affected = sum(Income > 500, na.rm = TRUE),
    n_total = n()
  )

# Basic Exercise 2: Create ChildStatus variable
acs <- acs %>%
  mutate(
    ChildStatus = if_else(Age2 < 18, "Minor", "Adult"),
    ChildStatus = factor(ChildStatus)
  )

# Verify
table(acs$ChildStatus, acs$Age2 < 18)

# Basic Exercise 3: Create Married variable
# First examine MarStat
acs %>% count(MarStat)

# Create binary married variable (assuming "married" is one of the values)
acs <- acs %>%
  mutate(
    Married = if_else(MarStat == "married", "Yes", "No")
  )

# Verify
table(acs$MarStat, acs$Married)

# Basic Exercise 4: Histogram of HoursWk
ggplot(acs, aes(x = HoursWk)) +
  geom_histogram()

# Examine unusual values
summary(acs$HoursWk)
acs %>% count(HoursWk) %>% filter(HoursWk > 80 | HoursWk < 0)

# Basic Exercise 5: Mean and median income by race
# By original Race variable
acs %>%
  group_by(Race) %>%
  summarize(
    mean_income = mean(Income, na.rm = TRUE),
    median_income = median(Income, na.rm = TRUE),
    n = n()
  )

# By RaceNew variable
acs %>%
  group_by(RaceNew) %>%
  summarize(
    mean_income = mean(Income, na.rm = TRUE),
    median_income = median(Income, na.rm = TRUE),
    n = n()
  )

# Intermediate Exercise 1: Full-time worker analysis
acs <- acs %>%
  mutate(FullTime = HoursWk >= 35)

# Proportion by age category and sex
acs %>%
  group_by(AgeCategory, Sex) %>%
  summarize(
    n_total = n(),
    n_fulltime = sum(FullTime, na.rm = TRUE),
    prop_fulltime = n_fulltime / n_total,
    .groups = "drop"
  )

# Intermediate Exercise 2: Insurance gap
acs <- acs %>%
  mutate(
    InsuranceGap = Age2 >= 18 & Age2 < 65 & HealthInsurance == "no"
  )

# Count and proportion
acs %>%
  filter(Age2 >= 18 & Age2 < 65) %>%
  summarize(
    n_working_age = n(),
    n_no_insurance = sum(InsuranceGap, na.rm = TRUE),
    prop_no_insurance = n_no_insurance / n_working_age
  )

# Intermediate Exercise 3: Hours worked categories with visualization
acs <- acs %>%
  mutate(
    HoursCategory = case_when(
      HoursWk == 0 ~ "Not working",
      HoursWk > 0 & HoursWk < 35 ~ "Part-time",
      HoursWk >= 35 & HoursWk <= 44 ~ "Full-time",
      HoursWk >= 45 ~ "Overtime",
      is.na(HoursWk) ~ "Unknown"
    ),
    HoursCategory = factor(HoursCategory, 
                           levels = c("Not working", "Part-time", 
                                      "Full-time", "Overtime", "Unknown"))
  )

# Stacked bar chart by sex
ggplot(acs, aes(x = Sex, fill = HoursCategory)) +
  geom_bar()

# Advanced Exercise 1: Data quality investigation
# People with 0 hours but positive income
acs %>%
  filter(HoursWk == 0 & Income > 0) %>%
  count()

# People with many hours but zero income
acs %>%
  filter(HoursWk > 35 & Income == 0) %>%
  count()

# Children reporting work hours
acs %>%
  filter(Age2 < 16 & HoursWk > 0) %>%
  select(Age2, HoursWk, Income) %>%
  head(20)

# Advanced Exercise 3: Missing data analysis
acs <- acs %>%
  mutate(
    missing_count = rowSums(is.na(.))
  )

# Investigate patterns
acs %>%
  group_by(missing_count) %>%
  summarize(
    n = n(),
    mean_age = mean(Age2, na.rm = TRUE),
    prop_not_citizen = mean(USCitizen == "no", na.rm = TRUE)
  )

# Visualize
ggplot(acs, aes(x = factor(missing_count))) +
  geom_bar()

# Advanced Exercise 4: Hourly income calculation
acs <- acs %>%
  mutate(
    AnnualHours = HoursWk * 52,
    HourlyIncome = case_when(
      HoursWk == 0 | is.na(HoursWk) ~ NA_real_,
      Income == 0 | is.na(Income) ~ NA_real_,
      TRUE ~ Income * 1000 / AnnualHours  # Income is in thousands
    )
  )

# Examine distribution
summary(acs$HourlyIncome)

# Create categories (assuming minimum wage ~$10/hr)
acs <- acs %>%
  mutate(
    HourlyCategory = case_when(
      HourlyIncome < 10 ~ "Below minimum",
      HourlyIncome >= 10 & HourlyIncome < 20 ~ "Low wage",
      HourlyIncome >= 20 & HourlyIncome < 40 ~ "Middle wage",
      HourlyIncome >= 40 ~ "High wage"
    )
  )

acs %>% 
  filter(!is.na(HourlyCategory)) %>%
  count(HourlyCategory) %>%
  mutate(percentage = n / sum(n) * 100)

# Advanced Exercise 5: Multidimensional typology
acs <- acs %>%
  mutate(
    AgeType = case_when(
      Age2 < 18 ~ "Child",
      Age2 >= 18 & Age2 < 65 ~ "Working-age",
      Age2 >= 65 ~ "Senior",
      TRUE ~ NA_character_
    ),
    EmpType = if_else(HoursWk > 0, "Employed", "Not employed"),
    InsType = if_else(HealthInsurance == "yes", "Insured", "Not insured"),
    Typology = paste(AgeType, EmpType, InsType, sep = "-")
  )

# Tabulate
acs %>% count(Typology) %>% arrange(desc(n))

# Three most common types
acs %>% 
  count(Typology) %>% 
  arrange(desc(n)) %>% 
  head(3)

# Average income by type
acs %>%
  group_by(Typology) %>%
  summarize(
    n = n(),
    mean_income = mean(Income, na.rm = TRUE)
  ) %>%
  arrange(desc(n))

# Visualize
acs %>%
  count(Typology) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(Typology, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Advanced Exercise 6: Comprehensive data quality report
# Function to create quality report for a variable
quality_report <- function(data, var) {
  data %>%
    summarize(
      variable = deparse(substitute(var)),
      n_obs = n(),
      n_missing = sum(is.na({{var}})),
      pct_missing = round(n_missing / n_obs * 100, 2),
      n_unique = n_distinct({{var}}, na.rm = TRUE)
    )
}

# Apply to all numeric variables
numeric_vars_report <- acs %>%
  summarize(across(where(is.numeric), 
                   list(
                     n_missing = ~sum(is.na(.)),
                     pct_missing = ~round(sum(is.na(.)) / n() * 100, 2),
                     mean = ~mean(., na.rm = TRUE),
                     sd = ~sd(., na.rm = TRUE),
                     min = ~min(., na.rm = TRUE),
                     max = ~max(., na.rm = TRUE)
                   ))) %>%
  pivot_longer(everything(), 
               names_to = c("variable", "statistic"),
               names_sep = "_",
               values_to = "value") %>%
  pivot_wider(names_from = statistic, values_from = value)

numeric_vars_report

# Identify outliers (> 3 SD from mean)
outlier_check <- acs %>%
  summarize(across(where(is.numeric),
                   list(
                     n_outliers = ~sum(abs(. - mean(., na.rm = TRUE)) > 
                                         3 * sd(., na.rm = TRUE), na.rm = TRUE)
                   ))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "n_outliers",
               names_pattern = "(.*)_n_outliers")

outlier_check