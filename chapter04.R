install.packages("styler") # code style
library(styler)
library(tidyverse)
library(nycflights13)

# tidyverse style ----

z <- (a + b)^2 / d
x <- c(1, 2, 3)
mean(x, na.rm = TRUE)

flights |>
  mutate(
    speed = distance / air_time,
    dep_hour = dep_time %/% 100,
    dep_minute = dep_time %% 100,
    .before = year
  )
flights |>
  filter(!is.na(arr_delay), !is.na(tailnum)) |>
  count(dest)

flights |>
  group_by(tailnum) |>
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

# ggplot2 style ----
flights |> 
  group_by(month) |> 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE)
  ) 

flights |> 
  group_by(month) |> 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = month, y = delay)) +
  geom_point() +
  geom_line()

flights |> 
  group_by(dest) |> 
  summarize(
    distance = mean(distance),
    speed = mean(distance / air_time, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = distance, y = speed)) +
  geom_smooth(
    method = "loess",
    span = 0.5,
    se = FALSE, 
    color = "white", 
    linewidth = 4
  ) +
  geom_point()
?geom_smooth # lm, linear method


# load data ---------------------------------------------------------------


# Plot data ---------------------------------------------------------------




