library(tidyverse)
library(gapminder)

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