
# ------------------------------------------------------------------------------
# Exercise Solutions (Examples)
# ------------------------------------------------------------------------------

# Easy Exercise 1: Probability of lifespan > 65
pnorm(q = 65, mean = 57.4, sd = 17.6, lower.tail = FALSE)
# Or equivalently
1 - pnorm(q = 65, mean = 57.4, sd = 17.6)

# Easy Exercise 2: 25th percentile
q25 <- qnorm(p = 0.25, mean = 57.4, sd = 17.6)
q25

# Proportion in actual data below this value
fruitfly %>%
  summarize(prop_below_q25 = mean(lifespan <= q25))

# Easy Exercise 3: Histogram of thorax
ggplot(fruitfly, aes(x = thorax)) +
  geom_histogram()

# Summary statistics for thorax
fruitfly %>%
  summarize(
    mean_thorax = mean(thorax),
    sd_thorax = sd(thorax)
  )

# Easy Exercise 4: Exactly 7 out of 10 survive
dbinom(x = 7, size = 10, prob = 0.663)

# Easy Exercise 5: Create table for size = 10
Fruitfly10 <- tibble(
  x = 0:10,
  prob = dbinom(x = 0:10, size = 10, prob = 0.663)
)

Fruitfly10

# Find the most likely outcome
Fruitfly10 %>%
  filter(prob == max(prob))

# Easy Exercise 6: At least 8 out of 10 survive
# Method 1: Using dbinom with sum
sum(dbinom(x = 8:10, size = 10, prob = 0.663))

# Method 2: Using pbinom
1 - pbinom(q = 7, size = 10, prob = 0.663)
# Or
pbinom(q = 7, size = 10, prob = 0.663, lower.tail = FALSE)

# Intermediate Exercise 3: Compare across experimental groups
fruitfly %>%
  group_by(type) %>%
  summarize(
    mean_lifespan = mean(lifespan),
    sd_lifespan = sd(lifespan),
    n = n()
  ) %>%
  arrange(desc(mean_lifespan))

# Visualize with boxplots
ggplot(fruitfly, aes(x = factor(type), y = lifespan)) +
  geom_boxplot()

# Intermediate Exercise 4: Long-lived fruitflies
# Calculate threshold (mean + 1 SD)
threshold <- 57.4 + 17.6

# Empirical proportion
fruitfly %>%
  summarize(prop_long_lived = mean(lifespan > threshold))

# Theoretical proportion (from normal distribution)
pnorm(threshold, mean = 57.4, sd = 17.6, lower.tail = FALSE)


# Intermediate Exercise 7: Sleep variable analysis
# Histogram
ggplot(fruitfly, aes(x = sleep)) +
  geom_histogram()

# Summary statistics
sleep_stats <- fruitfly %>%
  summarize(
    mean_sleep = mean(sleep),
    sd_sleep = sd(sleep)
  )

sleep_stats

# Probability of sleeping > 30% of day
pnorm(30, mean = sleep_stats$mean_sleep, 
      sd = sleep_stats$sd_sleep, lower.tail = FALSE)
