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
    # No sorting function needed in fct_reorder because only one lifeExp
    #   value and only one year and one country
    ggplot(aes(x = fct_reorder(country, lifeExp), y = lifeExp)) +
    geom_col(color = "deepskyblue", fill = NA) +
    coord_flip() +
    theme_classic()


# 4.7 Histograms ========
gapdata2007 |> 
    ggplot(aes(x = lifeExp)) +
    geom_histogram(binwidth = 10)

# 4.8 Box Plots ========
gapdata2007 |> 
    ggplot(aes(x = continent, y = lifeExp, color = continent)) +
    geom_boxplot(show.legend = FALSE) +
    scale_color_brewer(palette = "Paired")

# 4.9 Multiple geoms ======== 
# Experiment
gapdata2007 |> 
    group_by(continent) |> 
    summarize(lifeExp = mean(lifeExp)) |> 
    ggplot(aes(x = continent, y = lifeExp)) +
    geom_col()

gapdata2007 |> 
    ggplot(aes(x = continent, y = lifeExp)) +
    geom_boxplot() +
    geom_point()

gapdata2007 |> 
    ggplot(aes(x = continent, y = lifeExp)) +
    geom_boxplot() +
    geom_jitter()

gapdata2007 |> 
    ggplot(aes(x = continent, y = lifeExp, color = continent)) +
    geom_boxplot() +
    geom_jitter() +
    theme(legend.position = "none")

gapdata2007 |> 
    ggplot(aes(x = continent, y = lifeExp, color = continent)) +
    geom_boxplot(show.legend = FALSE) +
    geom_jitter(show.legend = FALSE, position = position_jitter(seed = 1))

gapdata2007 |> 
    ggplot(aes(x = continent, y = lifeExp)) +
    geom_boxplot() +
    geom_jitter(aes(color = continent), position = position_jitter(seed = 1))

label_data <- gapdata2007 |> 
    group_by(continent) |> 
    filter(lifeExp == max(lifeExp)) |> 
    select(country, continent, lifeExp)

label_data

gapdata2007 |> 
    ggplot(aes(x = continent, y = lifeExp)) +
    geom_boxplot() +
    geom_jitter(aes(color = continent)) +
    geom_label(data = label_data, aes(label = country))

# Experiment: label = lifeExp
gapdata2007 |> 
    ggplot(aes(x = continent, y = lifeExp)) +
    geom_boxplot() +
    geom_jitter(aes(color = continent)) +
    geom_label(data = label_data, aes(label = lifeExp))

# Experiment: geom_text instead of geom_label
gapdata2007 |> 
    ggplot(aes(x = continent, y = lifeExp)) +
    geom_boxplot() +
    geom_jitter(aes(color = continent)) +
    geom_text(data = label_data, aes(label = country))

gapdata |> 
    filter(continent == "Europe") |> 
    # fct_reorder needs a sorting function specified to get the desired effect
    #   otherwise it defaults to median and will order countries by
    #   greatest median
    ggplot(aes(y        = fct_reorder(country, lifeExp, .fun = max),
               x        = lifeExp,
               color    = year)) +
    geom_point(shape = 15) +
    scale_color_distiller(direction = 1)

gapdata2007 |> 
    group_by(continent) |> 
    mutate(country_number = seq_along(country)) |> 
    ggplot(aes(x = continent)) +
    geom_bar(aes(color = continent), fill = NA, show.legend = FALSE) +
    geom_text(aes(y = country_number, label = country), vjust = 1) +
    geom_label(aes(label = continent), y = -1) +
    theme_void()