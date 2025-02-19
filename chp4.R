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
