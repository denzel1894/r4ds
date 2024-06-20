library(tidyverse)

treatment <- tribble(
  ~person, ~treatment, ~response,
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, NA,
  "Katherine Bruke", 1, 4
)
treatment

treatment |> 
  fill(everything())

x <- c (1, 4, NA)
coalesce(x, 0)

x <- c(1, 4, -99)
na_if(x, -99)

x <- c(NA, NaN)
x
x *10
x==1
is.na(x)
is.nan(x)

0/0
0*Inf
Inf - Inf
sqrt(-1)

stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks

stocks |> 
  pivot_wider(
    names_from = qtr,
    values_from = price
  )

stocks |> 
  complete(year,qtr)

stocks |> 
  complete(year = 2019:2021, qtr)

library(nycflights13)

flights |> 
  distinct(faa = dest) |> 
  anti_join(airports)

flights |> 
  distinct(tailnum) |> 
  anti_join(planes)

health <- tibble(
  name   = c("Ikaia", "Oletta", "Leriah", "Dashay", "Tresaun"),
  smoker = factor(c("no", "no", "no", "no", "no"), levels = c("yes", "no")),
  age    = c(34, 88, 75, 47, 56),
)
health

health |> count(smoker)

health |> count(smoker, .drop = FALSE)

ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete()

ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

health |> 
  group_by(smoker, .drop = FALSE) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  )

health |> 
  group_by(smoker) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  ) |> 
  complete(smoker)
