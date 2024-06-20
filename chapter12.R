# logical vectors ----
library(tidyverse)
library(nycflights13)

x <- c(1, 2, 3, 5, 7)
x * 2

df <- tibble(x)
df |>
  mutate(y = x * 2)

flights |>
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)

# .keep = "used" ----
flights |>
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used"
  )
flights |>
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
  )

flights |>
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20
  ) |>
  filter(daytime & approx_ontime)

# floating point comparison ----
x <- c(1 / 49 * 49, sqrt(2)^2)
x
x == c(1, 2)

print(x, digits = 16)

# dplyr::near()
near(x, c(1, 2))

NA > 5
10 == NA

NA == NA

# We don't know how old Mary is
age_mary <- NA

# We don't know how old John is
age_john <- NA

# Are Mary and John the same age?
age_mary == age_john
#> [1] NA
# We don't know!

flights |>
  filter(dep_time == NA)
#> # A tibble: 0 × 19
#> # ℹ 19 variables: year <int>, month <int>, day <int>, dep_time <int>,
#> #   sched_dep_time <int>, dep_delay <dbl>, arr_time <int>, …

# is.na() ----

is.na(c(TRUE, NA, FALSE))
#> [1] FALSE  TRUE FALSE
is.na(c(1, NA, 3))
#> [1] FALSE  TRUE FALSE
is.na(c("a", NA, "b"))
#> [1] FALSE  TRUE FALSE

flights |>
  filter(is.na(dep_time))

flights |>
  filter(month == 1, day == 1) |>
  arrange(dep_time)

flights |>
  filter(month == 1, day == 1) |>
  arrange(desc(is.na(dep_time)), desc(dep_time))

# boolean algebra
df <- tibble(x = c(TRUE, FALSE, NA))
df |>
  mutate(
    and = x & NA,
    or = x | NA
  )

flights |>
  filter(month == 11 | month == 12)

flights |>
  filter(month == 11 | 12)
flights |>
  mutate(
    nov = month == 11,
    final = nov | 12,
    .keep = "used"
  )
flights |>
  mutate(
    nov = month == 11,
    final = nov | 12,
    .keep = "used"
  ) |>
  filter(nov)

flights |>
  mutate(
    nov = month == 11,
    oct = month == 12,
    final = nov | 12,
    .keep = "used"
  ) |>
  filter(oct)

# %in% ----

1:12 %in% c(1, 5, 11)
letters[1:10] %in% c("a", "e", "i", "o", "u")
letters

flights |>
  filter(month %in% c(11, 12))

c(1, 2, NA) == NA
#> [1] NA NA NA
c(1, 2, NA) %in% NA
#> [1] FALSE FALSE  TRUE

# summaries ---
flights |>
  group_by(year, month, day) |>
  summarize(
    all_delayed = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop" # cancel groups
  )
# Groups:   year, month [12]
flights |>
  group_by(year, month, day) |>
  summarize(
    all_delayed = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
  )
# Groups:   year, month, day [365]
flights |>
  group_by(year, month, day)

flights |>
  group_by(year, month, day) |>
  summarize(
    all_delayed = mean(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

# [ ]operator
flights |>
  group_by(year, month, day) |>
  summarize(
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# if_else()
x <- c(-3:3, NA)
if_else(x > 0, "+ve", "-ve")
if_else(x > 0, "+ve", "-ve", "???")
if_else(x < 0, -x, x)
if_else(x < 0, -x, x, 999)

x1 <- c(NA, 1, 2, NA)
y1 <- c(3, NA, 4, 6)
if_else(is.na(x1), y1, x1)
#> [1] 3 1 2 6

if_else(x == 0, "0", if_else(x < 0, "-ve", "+ve"), "???")

# case_when()
x <- c(-3:3, NA)
case_when(
  x == 0 ~ "0",
  x < 0 ~ "-ve",
  x > 0 ~ "+ve",
  is.na(x) ~ "???"
)
#> [1] "-ve" "-ve" "-ve" "0"   "+ve" "+ve" "+ve" "???"

case_when(
  x < 0 ~ "-ve",
  x > 0 ~ "+ve"
)

case_when(
  x < 0 ~ "-ve",
  x > 0 ~ "+ve",
  .default = "???"
)

case_when(
  x > 0 ~ "+ve",
  x > 2 ~ "big"
)
flights |> 
  mutate(
    status = case_when(
      is.na(arr_delay)      ~ "cancelled",
      arr_delay < -30       ~ "very early",
      arr_delay < -15       ~ "early",
      abs(arr_delay) <= 15  ~ "on time",
      arr_delay < 60        ~ "late",
      arr_delay < Inf       ~ "very late",
    ),
    .keep = "used"
  )
