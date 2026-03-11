# ------------------------------------------------------------------------------
# Lab HW 1 Base
# ------------------------------------------------------------------------------

# Exercise 1: What years are included?
range(arbuthnot$year)
# or
arbuthnot %>% summarize(min_year = min(year), max_year = max(year))

# Exercise 2: Total baptisms in 1710
# Base R approach
arbuthnot$total[arbuthnot$year == 1710]
# Tidyverse approach
arbuthnot %>% filter(year == 1710) %>% pull(total)

# Exercise 3: Create boy-girl ratio
arbuthnot <- arbuthnot %>%
  mutate(boy_girl_ratio = boys / girls)

# Mean ratio
mean(arbuthnot$boy_girl_ratio)
# or
arbuthnot %>% summarize(mean_ratio = mean(boy_girl_ratio))

# Exercise 4: Years with girls > 7000
sum(arbuthnot$girls > 7000)
# or
arbuthnot %>% filter(girls > 7000) %>% nrow()

# Exercise 5: Total baptisms over time
arbuthnot %>%
  ggplot(aes(x = year, y = total)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Total Baptisms in London, 1629-1710",
    x = "Year",
    y = "Total Baptisms"
  ) +
  theme_light()

# Exercise 6: Histogram of proportion of boys
arbuthnot %>%
  ggplot(aes(x = prop_boys)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Proportion of Boys",
    x = "Proportion of Boys",
    y = "Count"
  ) +
  theme_minimal()

# Exercise 7: Scatter plot colored by more_boys
arbuthnot %>%
  ggplot(aes(x = year, y = total, color = more_boys)) +
  geom_point(size = 3) +
  labs(
    title = "Total Baptisms Over Time",
    subtitle = "Colored by whether boys outnumbered girls",
    x = "Year",
    y = "Total Baptisms",
    color = "More Boys?"
  ) +
  scale_color_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "coral"),
    labels = c("No", "Yes")
  ) +
  theme_minimal()
