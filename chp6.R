library(tidyverse)
library(gapminder)
library(finalfit)

theme_set(theme_bw())


# 6.3 The Data ====
gapdata <- gapminder
# gapdata |> 
#     glimpse()

# gapdata |>
#     missing_glimpse()

# gapdata |>
#     ff_glimpse()


# 6.4 Plot Data ====
gapdata |> 
    filter(year %in% c(2002, 2007)) |> 
    ggplot(aes(x = lifeExp)) +
    geom_histogram(bins = 20) +
    facet_grid(year ~ continent, scales = "free_y") # y vs x, y ~ x

gapdata |> 
    filter(year %in% c(2002, 2007)) |> 
    ggplot() +
    geom_qq(aes(sample = lifeExp)) +
    geom_qq_line(aes(sample = lifeExp), color = "deepskyblue") +
    facet_grid(year ~ continent)
                
gapdata |> 
    filter(year %in% c(2002, 2007)) |>
    ggplot(aes(x = factor(year), y = lifeExp)) +
    geom_boxplot(aes(fill = continent), show.legend = FALSE) +
    geom_jitter(
        aes(color = continent), alpha = 0.4,
        position = position_jitter(seed = 1)) +
    facet_wrap( ~ continent, ncol = 5) +
    labs(
        x = "Year",
        y = "Life Expectancy",
        title = "Life Expectancy by Continent. 2002 vs 2007"
    ) +
    theme(legend.position = "none")


# 6.5 Comparing means of two groups ====
# Two sample t-test and paired t-test

# Two sample t-Test
# Let's compare Asia and Europe life expectancy in 2007
ttest_data <- gapdata |> 
    filter(year == 2007) |> 
    filter(continent %in% c("Asia", "Europe"))

ttest_result <- ttest_data |> 
    t.test(lifeExp ~ continent, data = _) # Native pipe use `_` for placeholder
ttest_result

# Paired t-Test
# Compare Asia lifeExp between 2002 and 2007
paired_data <- gapdata |> 
    filter(year %in% c(2002, 2007)) |> 
    filter(continent == "Asia")
paired_data

# paired_data |> 
#     ggplot(aes(
#         x = year,
#         y = lifeExp,
#         group = country)) +
#     geom_line()

paired_table <- paired_data |> 
    select(country, continent, year, lifeExp) |> 
    pivot_wider(
        names_from = year,
        values_from = lifeExp,
        names_prefix = "year_"
    ) |> 
    mutate(deltaLifeExp = year_2007 - year_2002)
paired_table

# Average difference in life expectancy
paired_table |> 
    summarize(mean(deltaLifeExp))

t.test(paired_table$year_2002, paired_table$year_2007, paired = TRUE)
t.test(paired_table$year_2002, paired_table$year_2007, paired = FALSE) # Unpaired, wrong


# 6.6 One sample t-test ====
library(broom)
gapdata %>%
    filter(year == 2007) %>%
    group_by(continent) %>% 
    do(
        t.test(.$lifeExp, mu = 77, data = .) %>% 
            tidy()
    )

# Identical to paired t test from before...
paired_table %>% 
    t.test(.$deltaLifeExp, mu = 0, data = .)


# 6.7 Comparing means across more than two groups ====
# May want to compare a distribution for more than three groups
#   such as average life expectancy in 3 continents.
#   Use ANOVA

gapdata %>% 
    filter(year == 2007) %>% 
    filter(continent %in% 
               c("Americas", "Europe", "Asia")) %>% 
    ggplot(aes(x = continent, y=lifeExp)) +
    geom_boxplot()

aov_data <- gapdata %>% 
    filter(year == 2007) %>% 
    filter(continent %in% c("Americas", "Europe", "Asia"))

fit = aov(lifeExp ~ continent, data = aov_data) 
summary(fit)

aov_data %>% 
    aov(lifeExp ~ continent, data = .) %>% 
    tidy()

library(ggfortify)
autoplot(fit)