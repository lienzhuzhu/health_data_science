library(tidyverse)
library(lubridate)

typesdata <- read_csv("data/typesdata.csv")

typesdata

typesdata |> 
    count(group, id)

typesdata |> 
    add_row(id = "ID3", group = "Treatment") |> 
    count(id, sort = TRUE)

current_datetime <- Sys.time()
current_datetime


mydata <- tibble(
    id   = 1:4,
    sex  = c("Male", "Female", "Female", "Male"),
    var1 = c(4, 1, 2, 3),
    var2 = c(NA, 4, 5, NA),
    var3 = c(2, 1, NA, NA)
)


gbd_short <- read_csv("data/global_burden_disease_cause-year.csv")

gbd_short


mydata

typesdata

typesdata |> 
    mutate(above_threshold = if_else(measurement > 3,
                                     "Higher",
                                     "Lower"))