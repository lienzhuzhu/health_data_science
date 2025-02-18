library(tidyverse)
library(scales)

# 3.1 --------
gbd_full <- read_csv("data/global_burden_disease_cause-year-sex-income.csv")

# Creating a single-year tibble for printing and simple examples:
gbd2017 <- gbd_full |> 
    filter(year == 2017)

# 3.3 --------
gbd2017 |> 
    summarize(sum(deaths_millions))

gbd2017 |> 
    group_by(cause, sex) |> 
    summarize(sum(deaths_millions))

# 3.4 --------
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

# 3.5 --------
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

# 3.7 ---------
gbd_2rows <- gbd_full |> 
    slice(1:2)

gbd_2rows

gbd_2rows |> 
    select(cause, deaths_millions)


# 3.8 ---------
gbd_wide <- read_csv("data/global_burden_disease_wide-format.csv")
gbd_long <- read_csv("data/global_burden_disease_cause-year-sex.csv")

gbd_wide

gbd_wide |> 
    select(matches("Female|Male"))

gbd_wide |> 
    pivot_longer(
        matches("Female|Male"),
        names_to = "sex_year",
        values_to = "deaths_millions"
    ) |> 
    separate(sex_year, into = c("sex", "year"), sep = "_", convert = TRUE)


# 3.9 --------
gbd_long |> 
    arrange(cause)

gbd_factored <- gbd_long |> 
    mutate(cause = factor(cause))

gbd_factored$cause |> levels()

gbd_factored <- gbd_factored |> 
    mutate(cause = cause |> 
               fct_relevel("Injuries"))

gbd_factored$cause |> levels()

gbd_factored |> 
    arrange(cause)

gbd_test <- gbd_long |> 
    mutate(cause = factor(cause) |> 
               fct_relevel("Injuries"))

gbd_test |> 
    arrange(cause)


# Exercises --------

# 3.10.1
gbd_long <- read_csv("data/global_burden_disease_cause-year-sex.csv")

gbd_long

gbd_long |> 
    pivot_wider(
        names_from = cause,
        values_from = deaths_millions
    )

# 3.10.2
gbd_full <- read_csv("data/global_burden_disease_cause-year-sex-income.csv")

summary_data1 <- gbd_full |> 
    group_by(year) |> 
    summarize(total_per_year = sum(deaths_millions))

summary_data1

summary_data2 <- gbd_full |> 
    group_by(year, cause) |> 
    summarize(total_per_cause = sum(deaths_millions))

summary_data2

# summary_data2 has 3 time as many data points (observations, rows) as 
#   summary_data1 because it separates the year totals by cause as well

# All variables that are not relevant to the summary are left out of the tibble
#   by summarize(). Variables used in the summary and variables created stay.

# 3.10.3
summary_data1
summary_data2

gbd_summary <- full_join(summary_data1, summary_data2) |> 
    mutate(
        percent_cause_per_year = total_per_cause / total_per_year
    )

gbd_summary

gbd_summary |> 
    mutate(percent_cause_per_year = percent(percent_cause_per_year)) |> 
    relocate(total_per_year, .after = total_per_cause)

# 3.10.4
gbd_one_liner <- gbd_full |> 
    group_by(year, cause) |> 
    summarize(total_per_cause_year = sum(deaths_millions)) |> 
    group_by(year) |> 
    mutate(total_per_year = sum(total_per_cause_year)) |> 
    mutate(percentage = percent(total_per_cause_year / total_per_year))

gbd_one_liner

gbd_one_liner |> 
    select(year, cause, percentage) |> 
    pivot_wider(
        names_from = cause,
        values_from = percentage
    )