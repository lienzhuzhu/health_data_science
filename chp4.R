library(tidyverse)
library(gapminder)

# 4.1 ========
gapminder |> 
    glimpse()

gapminder$year |> unique()
gapminder$country |> n_distinct()
gapminder$continent |> unique()

gapdata2007 <- gapminder |> 
    filter(year == 2007)

gapdata2007

gapdata <- gapminder

gapdata


# 4.2 ========
# Plot 1
gapdata2007 |> 
    ggplot(aes(x = gdpPercap, y = lifeExp))

# Plot 2
gapdata2007 |> 
    ggplot(aes(x = gdpPercap, y = lifeExp)) +
    geom_point()

# A brief intermission: Plotting a continuous variable (lifeExp) against 
#   a categorical one (continent) results in a strip plot
gapdata2007 |> 
    ggplot(aes(x = continent, y = lifeExp)) +
    geom_point()

# Plot 3
gapdata2007 |> 
    ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
    geom_point() +
    theme_classic()

# Plot 4
gapdata2007 |> 
    ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
    geom_point(shape = 1)

# Plot 5
gapdata2007 |> 
    ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
    geom_point(shape = 1) +
    facet_wrap( ~ continent, ncol = 3)

gapdata2007 |> 
    ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
    geom_point(shape = 1) +
    facet_wrap( ~ continent, nrow = 2)

gapdata2007 |> 
    ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
    geom_point(shape = 1) +
    facet_wrap( ~ continent)

# plot 6.0
gapdata2007 |> 
    ggplot(aes(x = gdpPercap/1000, y = lifeExp, color = continent)) +
    geom_point(shape = 1) +
    facet_wrap( ~ continent)

# plot 6.1
gapdata2007 |> 
    ggplot(aes(x = gdpPercap/1000, y = lifeExp, color = continent)) +
    geom_point(shape = 1) +
    facet_wrap( ~ continent) +
    theme_bw()


# 4.3 ========
theme_set(theme_bw())


# 4.4 Scatter Plots ========
gapdata2007 |> 
    ggplot(aes(x = gdpPercap/1000, y = lifeExp, size = pop)) +
    geom_point()

gapdata2007 |> 
    ggplot(aes(x = gdpPercap/1000, y = lifeExp, size = pop)) +
    geom_point(shape = 1, alpha = 0.5)


# 4.5 Line Plots and Time Series ======== 
gapdata |> 
    filter(country == "United Kingdom") |> 
    ggplot(aes(x = year, y = lifeExp)) +
    geom_line()

gapdata |> 
    filter(country == "United Kingdom") |> 
    ggplot(aes(x = year, y = lifeExp)) +
    geom_point()

# Bad, undesired behavior
gapdata |> 
    ggplot(aes(x = year, y = lifeExp)) +
    geom_line()

# Good, desired behavior
gapdata |> 
    ggplot(aes(x = year, y = lifeExp)) +
    geom_line()

# Exercise 4.5.1
gapdata |> 
    ggplot(aes(x = year, y = lifeExp, group = country, color = continent)) +
    geom_line()

gapdata |> 
    ggplot(aes(x = year, y = lifeExp, group = country, color = continent)) +
    geom_line() +
    facet_wrap( ~ continent)

gapdata |> 
    ggplot(aes(x = year, y = lifeExp, group = country, color = continent)) +
    geom_line() +
    facet_wrap( ~ continent) +
    scale_color_brewer(palette = "Paired")

# 4.6 Bar Plots ========
# Use a column plot when the data already has a summary for a categorical variable
gapdata2007 |> 
    filter(country %in% c("United Kingdom", "France", "Germany")) |> 
    ggplot(aes(x = country, y = lifeExp)) +
    geom_col()
    
gapdata2007 |> 
    filter(country %in% c("United Kingdom", "France", "Germany")) |> 
    ggplot(aes(x = country, y = lifeExp)) +
    geom_col() +
    coord_cartesian(ylim = c(79, 81))

# Use a bar plot when counting rows with some value of a categorical variable
gapdata2007 |>  # gapdata2007 has one row per country
    count(continent)

# Boring...
gapdata2007 |> 
    ggplot(aes(x = continent)) +
    geom_bar()

# ...so color each individual country within a continent
gapdata2007 |> 
    ggplot(aes(x = continent)) +
    geom_bar(aes(color = country), fill = NA) +
    theme(legend.position = "none")

gapdata2007 |> 
    ggplot(aes(x = "Global", fill = continent)) +
    geom_bar()


# Exercise 4.6.5 ========
gapdata2007 |> 
    filter(continent == "Europe") |> 
    ggplot(aes(x = fct_reorder(country, lifeExp), y = lifeExp)) +
    geom_col(color = "deepskyblue", fill = NA) +
    coord_flip()


# 4.7 Histograms ======== 