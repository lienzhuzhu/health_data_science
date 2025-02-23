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


# 6.8 Multiple Testing ====
pairwise.t.test(aov_data$lifeExp, aov_data$continent, 
                p.adjust.method = "bonferroni")

pairwise.t.test(aov_data$lifeExp, aov_data$continent, 
                p.adjust.method = "fdr")


# 6.9 Non-parametric Testing ====
africa2002 <- gapdata %>%              # save as africa2002
    filter(year == 2002) %>%             # only 2002
    filter(continent == "Africa") %>%    # only Africa
    select(country, lifeExp) %>%         # only these variables
    mutate(
        lifeExp_log = log(lifeExp)         # log life expectancy
    )
head(africa2002)                       # inspect

africa2002 %>% 
    # pivot lifeExp and lifeExp_log values to same column (for easy plotting):
    pivot_longer(contains("lifeExp")) %>% 
    ggplot(aes(x = value)) +             
    geom_histogram(bins = 15) +          # make histogram
    facet_wrap(~name, scales = "free")    # facet with axes free to vary

africa_data <- gapdata %>%                          
    filter(year %in% c(1982, 2007)) %>%      # only 1982 and 2007
    filter(continent %in% c("Africa"))       # only Africa

p1 <- africa_data %>%                      # save plot as p1
    ggplot(aes(x = lifeExp)) + 
    geom_histogram(bins = 15) +
    facet_wrap(~year)

p2 <- africa_data %>%                      # save plot as p2
    ggplot(aes(sample = lifeExp)) +          # `sample` for Q-Q plot
    geom_qq() + 
    geom_qq_line(colour = "blue") + 
    facet_wrap(~year)

p3 <- africa_data %>%                      # save plot as p3
    ggplot(aes(x = factor(year),             # try without factor(year) to
               y = lifeExp)) +               # see the difference
    geom_boxplot(aes(fill = factor(year))) + # colour boxplot
    geom_jitter(alpha = 0.4) +               # add data points
    theme(legend.position = "none")          # remove legend

library(patchwork)                         # great for combining plots
p1 / p2 | p3

africa_data %>% 
    wilcox.test(lifeExp ~ year, data = .) # Reveals insignificant difference

# Multiple groups
library(broom)
gapdata %>% 
    filter(year == 2007) %>% 
    filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
    kruskal.test(lifeExp~continent, data = .) %>% 
    tidy()


## Exercises ====

# 6.12.1
# Make a histogram, Q-Q plot, and a box-plot for the life expectancy for 
#   a continent of your choice, but for all years. 
#   Do the data appear normally distributed?
## The data appear normal.
asia_data <- gapdata %>% 
    filter(continent == "Asia")
    
p1 <- asia_data %>% 
    ggplot() +
    geom_histogram(aes(x = lifeExp), bins = 30)

p2 <- asia_data %>% 
    ggplot() +
    geom_boxplot(aes(x = "", y = lifeExp)) +
    geom_jitter(aes(x = "", y = lifeExp), alpha = 0.4) +
    theme(legend.position = "none")

p3 <- asia_data %>% 
    ggplot(aes(sample = lifeExp)) +
    geom_qq() +
    geom_qq_line()

p1 / p3 | p2

# 6.12.2 
# Select any 2 years in any continent and perform a t-test to determine
#   whether mean life expectancy is significantly different. 
#   Remember to plot your data first.
p1 <- gapdata %>% 
    filter(
        continent == "Asia", 
        year %in% c(1957, 1992)) %>% 
    ggplot(aes(x = lifeExp)) +
    geom_histogram(bins = 30) +
    facet_wrap( ~ year)

p2 <- gapdata %>% 
    filter(
        continent == "Asia", 
        year %in% c(1957, 1992)) %>% 
    ggplot(aes(x = factor(year), y = lifeExp)) +
    geom_boxplot(aes(fill = factor(year)), show.legend = FALSE) +
    geom_jitter(aes(color = factor(year)), alpha = 0.4, show.legend = FALSE)

gapdata %>% 
    filter(
        continent == "Asia", 
        year %in% c(1957, 1992)) %>% 
    select(country, continent, lifeExp, year) %>% 
    pivot_wider(names_from = year, 
                values_from = lifeExp, 
                names_prefix = "year_") %>% 
    mutate(deltaLifeExp = year_1992 - year_1957) %>% 
    do(
        t.test(.$deltaLifeExp, mu = 0) %>% 
            tidy()
    )

p1 | p2

asia_table <- gapdata %>% 
    filter(
        continent == "Asia", 
        year %in% c(1957, 1992)) %>% 
    select(country, continent, lifeExp, year) %>% 
    pivot_wider(names_from = year, 
                values_from = lifeExp, 
                names_prefix = "year_") %>% 
    mutate(deltaLifeExp = year_1992 - year_1957)

result <- t.test(asia_table$deltaLifeExp, mu = 0)
result

# Extract only the p-value from your t.test() output.
result$p.value


# 6.12.3
# In 2007, in which continents did mean life expectancy differ from 70?
## All except Asia.
gapdata %>% 
    filter(year == 2007) %>% 
    group_by(continent) %>% 
    do(
        t.test(.$lifeExp, mu = 70) %>% 
            tidy()
    )


# 6.12.4
# Use ANOVA to determine if the population changed significantly 
#   through the 1990s/2000s in individual continents.
pop_data <- gapdata %>% 
    filter(year >= 1990) %>% 
    group_by(continent)
    
pop_data %>% 
    ggplot(aes(x = factor(year), y = pop)) +
    geom_boxplot() +
    facet_wrap( ~ continent, scales = "free_y")

# Provides individual continent results...
pop_data %>% 
    do(
        kruskal.test(pop ~ year, data = .) %>%
            tidy()
    )

# Only provides one result...
pop_data %>% 
    kruskal.test(pop ~ year, data = .) %>%
    tidy()