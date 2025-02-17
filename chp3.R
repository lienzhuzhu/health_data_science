library(tidyverse)
library(scales)

# 3.1
gbd_full <- read_csv("data/global_burden_disease_cause-year-sex-income.csv")

# Creating a single-year tibble for printing and simple examples:
gbd2017 <- gbd_full |> 
    filter(year == 2017)

# 3.3
gbd2017 |> 
    summarize(sum(deaths_millions))

gbd2017 |> 
    group_by(cause, sex) |> 
    summarize(sum(deaths_millions))

# 3.4
gbd2017 |> 
    group_by(cause, sex) |> 
    summarise(deaths_per_group = sum(deaths_millions), .groups = "keep") |> 
    mutate(deaths_total = sum(deaths_per_group))

gbd2017 |> 
    group_by(cause, sex) |> 
    summarise(deaths_per_group = sum(deaths_millions)) |> 
    ungroup() |>
    mutate(
        deaths_total = sum(deaths_per_group),
        deaths_relative = percent(deaths_per_group / deaths_total))

gbd2017 |> 
    group_by(cause, sex) |> 
    summarise(deaths_per_group = sum(deaths_millions)) |> 
    ungroup() |>
    mutate(
        deaths_total = sum(deaths_per_group),
        deaths_relative = deaths_per_group / deaths_total,
        deaths_percent = percent(deaths_relative))

# 3.5
gbd_summarised <- gbd2017 |> 
    group_by(cause, sex) |> 
    summarise(deaths_per_group = sum(deaths_millions))
 
gbd_summarised

gbd_summarised <- gbd_summarised |> 
    arrange(sex)

gbd_summarised

gbd_summarised |> 
    group_by(sex) |> 
    mutate(deaths_per_sex = sum(deaths_per_group))

gbd2017 |> 
    group_by(cause, sex) |> 
    summarise(deaths_per_group = sum(deaths_millions)) |> 
    group_by(sex) |> 
    mutate(deaths_per_sex = sum(deaths_per_group),
           sex_cause_percentage = percent(deaths_per_group / deaths_per_sex)) |> 
    arrange(sex, deaths_per_group)

# 3.7
gbd_2rows <- gbd_full |> 
    slice(1:2)

gbd_2rows

gbd_2rows |> 
    select(cause, deaths_millions)