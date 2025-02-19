library(tidyverse)
library(gapminder)

# 5.1 ====
p0 <- gapminder |> 
    filter(year == 2007) |> 
    ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
    geom_point(alpha = 0.3) +
    theme_bw() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_brewer(palette = "Set1")

p0

# 5.2 ====
p1 <- p0 + scale_x_log10()
p1

p2 <- p0 + expand_limits(y = 0)
p2

p3 <- p0 + expand_limits(y = c(0, 100))
p3

p4 <- p3 + 
    expand_limits(x = c(-2000, 55000)) +
    coord_cartesian(expand = FALSE)
p4

library(patchwork)
p1 + p2 + p3 + p4 + plot_annotation(tag_levels = "1", tag_prefix = "p")

p5 <- p0 +
    coord_cartesian(ylim = c(70, 85), 
                    xlim = c(20000, 40000))
# Retains excluded points in geom_smooth calculations
p5

p6 <- p0 +
    scale_y_continuous(limits = c(70, 85)) +
    scale_x_continuous(limits = c(20000, 40000)) 
# It looks like the points displayed didn't change, but the points included
#   in the interpolation for geom_smooth did change.
#   This removes excluded points from geom_smooth calculation.
p6

p5 + labs(tag = "p5") + p6 + labs(tag = "p6")