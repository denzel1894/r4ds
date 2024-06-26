x <- c(1, 2, 2, 20, 1, 3, 5, 6)
x == c(1, 2)

library(tidyverse)
library(nycflights13)

# parse_double() and parse_number() ----
x <- c("1.2", "5.6", "1e3")
parse_double(x)

x <- c("$1,234", "USD 3,513", "59%")
parse_number(x)

# count() ----
flights |> count(dest)
flights |>
  count(dest) |>
  View()
flights |>
  count(dest) |>
  print(n = Inf)

# summarize(), n()----
flights |>
  group_by(dest) |>
  summarise(
    n = n(),
    delay = mean(arr_delay, na.rm = TRUE)
  )

flights |>
  group_by(dest) |>
  summarise(carriers = n_distinct(carrier)) |>
  arrange(desc(carriers))

flights |>
  group_by(tailnum) |>
  summarize(miles = sum(distance))

flights |> count(tailnum, wt = distance)
flights |> count(tailnum)

flights |>
  group_by(dest) |>
  summarize(n_cancelled = sum(is.na(dep_time)))

# numeric transformations
x <- c(1, 2, 10, 20)
x / 5

x / c(5, 5, 5, 5)
x / c(5, 5)
c(5, 5) / x

flights |>  
  filter(month == c(1,2)) # error not shown
flights |> 
  filter(month %in% c(1,2))

# minimum and maximum
df <- tribble(
  ~x, ~y,
  1, 3,
  5, 2,
  7, NA,
)
df
df <- tribble(
  ~x, ~y,
  1, 3,
  5, 2,
  7, NA
)
df

df |> 
  mutate(
    min = pmin(x, y, na.rm = TRUE),
    max = pmax(x, y, na.rm = TRUE)
  )

df |> 
  mutate(
    min = min(x, y, na.rm = TRUE),
    max = max(x, y, na.rm = TRUE)
  )

# modular arithmetic
1:10 %/% 3
1:10 %% 3

flights |> 
  mutate(
    hour = sched_dep_time %/% 100,
    minute = sched_dep_time %% 100,
    .keep = "used"
  )

flights |> 
  group_by(
    hour = sched_dep_time %/% 100
  )
flights |> 
  group_by(
    hour = sched_dep_time %/% 100
  ) |> 
  summarise(
    prop_cancelled = mean(is.na(dep_time)), 
    n = n()
  )

flights |> 
  group_by(
    hour = sched_dep_time %/% 100
  ) |> 
  summarise(
    prop_cancelled = mean(is.na(dep_time)), 
    n = n()
  ) |> 
  filter(hour > 1) |> 
  ggplot(aes(x = hour, y = prop_cancelled)) +
  geom_line(color = "grey50") +
  geom_point(aes(size = n))

# logarithms ----
log(3)
log2(2)
log10(10)

# rounding ----
round(123.458)

round(123.456, 2)  # two digits
#> [1] 123.46
round(123.456, 1)  # one digit
#> [1] 123.5
round(123.456, -1) # round to nearest ten
#> [1] 120
round(123.456, -2) # round to nearest hundred
#> [1] 100

round(c(1.5, 2.5))
#> [1] 2 2

x <- 123.456

floor(x)
#> [1] 123
ceiling(x)
#> [1] 124

# Round down to nearest two digits
floor(x / 0.01) * 0.01
#> [1] 123.45
# Round up to nearest two digits
ceiling(x / 0.01) * 0.01
#> [1] 123.46
# Round to nearest multiple of 4
round(x / 4) * 4
#> [1] 124

# Round to nearest 0.25
round(x / 0.25) * 0.25
#> [1] 123.5

# cutting numbers into ranges
x <- c(1, 2, 5, 10, 15, 20)
cut(x, breaks = c(0, 5, 10, 15, 20))
cut(x, breaks = c(0, 5, 10, 100))

cut(x, 
    breaks = c(0, 5, 10, 15, 20), 
    labels = c("sm", "md", "lg", "xl")
)

y <- c(NA, -10, 5, 10, 30)
cut(y, breaks = c(0, 5, 10, 15, 20))

# cumulative and rolling aggregates ----
# slider package 
x <- 1:10
cumsum(x)

# ranks ----
x = c(1,2,2,3,4, NA)
min_rank(x)
desc(x)
min_rank(desc(x))

df <- tibble(x = x)
df
df |> 
  mutate(
    row_number = row_number(x),
    dense_rank = dense_rank(x),
    percent_rank = percent_rank(x),
    cume_dist = cume_dist(x)
  )

df <- tibble(id = 1:10)
df

df |> 
  mutate(
    row0 = row_number() - 1,
    three_groups = row0 %% 3, #ramainer of 3
    three_in_each_group = row0 %/% 3 # number of 3
  )

# offsets ----

x <- c(2, 5, 11, 11, 19, 35)
lag(x)
lead(x)
time = c(0, 1, 2, 3, 5, 10, 12, 15, 17, 19, 20, 27, 28, 30)
# consecutive identifiers
events <- tibble(
  time = c(0, 1, 2, 3, 5, 10, 12, 15, 17, 19, 20, 27, 28, 30)
)
lag(time, default = first(time))

events <- events |> 
  mutate(
    diff = time - lag(time, default = first(time)),
    has_gap = diff >= 5
  )
events

events |> mutate(
  group = cumsum(has_gap)
) |> 
  View()

cumsum(1:10)

df <- tibble(
  x = c("a", "a", "a", "b", "c", "c", "d", "e", "a", "a", "b", "b"),
  y = c(1, 2, 3, 2, 4, 1, 3, 9, 4, 8, 10, 199)
)
df |> 
  group_by(id = consecutive_id(x)) 

df |> 
  group_by(id = consecutive_id(x)) |> 
  slice_head(n = 1)

flights |>
  group_by(year, month, day) |>
  summarize(
    mean = mean(dep_delay, na.rm = TRUE),
    median = median(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = mean, y = median)) + 
  geom_abline(slope = 1, intercept = 0, color = "white", linewidth = 2) +
  geom_point()

flights |>
  group_by(year, month, day) |>
  summarize(
    max = max(dep_delay, na.rm = TRUE),
    min = min(dep_delay, na.rm = TRUE),
    q95 = quantile(dep_delay, 0.95, na.rm = TRUE),
    .groups = "drop"
  )
quantile(x <- rnorm(1001))

flights |> 
  group_by(origin, dest) |> 
  summarize(
    distance_sd = IQR(distance),
    n = n(),
    .groups = "drop"
  ) |> 
  filter(distance_sd > 0 )

# toy example
x <- 1:10
x
quantile(x, 0.75) - quantile(x, 0.25)
IQR(x)

# distributions
flights |>
  filter(dep_delay < 120) |> 
  ggplot(aes(x = dep_delay, group = interaction(day, month))) + 
  geom_freqpoly(binwidth = 5, alpha = 1/5)

y <- 2:11
y
interaction(x, y)

flights |>
  filter(dep_delay < 120) |> 
  ggplot(aes(x = dep_delay)) + 
  geom_freqpoly(binwidth = 5, alpha = 1/5)

flights |> 
  group_by(year, month, day) |> 
  summarize(
    first_dep = first(dep_time, na_rm = TRUE),
    fifth_dep = nth(dep_time, n = 5, na_rm = TRUE),
    last_dep = last(dep_time, na_rm = TRUE)
  )

flights |> 
  group_by(year, month, day) |> 
  mutate(r = min_rank(sched_dep_time), .keep = "used") 

flights |> 
  group_by(year, month, day) |> 
  mutate(r = min_rank(sched_dep_time), .keep = "used") |> 
  filter(r %in% c(1, max(r)))

