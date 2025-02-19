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
    geom_jitter(aes(color = continent), alpha = 0.4, position = position_jitter(seed = 1)) +
    facet_wrap( ~ continent, ncol = 5) +
    labs(
        x = "Year",
        y = "Life Expectancy",
        title = "Life Expectancy by Continent. 2002 vs 2007"
    ) +
    theme(legend.position = "none")

# 6.5 Comparing means of two groups ====
