library(tidyverse)
library(nycflights13)

df <- tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5),
  d = rnorm(5)
)

df

df |> mutate(
  a = (a - min(a, na.rm = TRUE)) /
    (max(a, na.rm = TRUE) - min(a, na.rm = TRUE)),
  b = (b - min(b, na.rm = TRUE)) /
    (max(b, na.rm = TRUE) - min(a, na.rm = TRUE)), # wrong
  c = (c - min(c, na.rm = TRUE)) /
    (max(c, na.rm = TRUE) - min(c, na.rm = TRUE)),
  d = (d - min(d, na.rm = TRUE)) /
    (max(d, na.rm = TRUE) - min(d, na.rm = TRUE)),
)

rescale01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))
rescale01(c(1, 2, 3, 5))

df |> mutate(
  a = rescale01(a),
  b = rescale01(b),
  c = rescale01(c),
  d = rescale01(d),
)
df |> mutate(across(a:d, rescale01)) # next chapter studied

range(c(1, 2, 3))

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

x <- c(1:10, Inf)
x
rescale01(x)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

clamp <- function(x, min, max) {
  case_when(
    x < min ~ min,
    x > max ~ max,
    .default = x
  )
}
clamp(1:10, min = 2, max = 7)

# make the first character upper case
x = "hello"
str_sub(x, 1, 1) <- "H"
x

first_upper <- function(x) {
  str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1,1))
  x
}
first_upper(x)

clear_number <- function(x) {
  is_pct <- str_detect(x, "%")
  num <- x |> 
    str_remove_all("%") |> 
    str_remove_all(",") |> 
    str_remove_all(fixed("$")) |> 
    as.numeric()
  if_else(is_pct, num / 100, num)
}

clear_number("$12,300")
clear_number("45%")

fix_na <- function(x) {
  if_else(x %in% c(997, 998, 999), NA, x)
}

fix_na(c(998,999,1))

commas <- function(x) {
  str_flatten(x, collapse = ", ", last = " and ")
}

str_flatten(c("haha", "hehe"), collapse = ", ", last = " and ")

cv <- function(x, na.rm = FALSE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
cv(runif(100, min = 0, max = 50))

n_missing <- function(x) {
  sum(is.na(x))
} 
n_missing(c(1, 2, NA, NA))

mape <- function(actual, predicted) {
  sum(abs((actual - predicted) / actual)) / length(actual)
}

grouped_mean <- function(df, group_var, mean_var) {
  df |> 
    group_by(group_var) |> 
    summarize(mean(mean_var))
}

diamonds |> grouped_mean(cut, carat) # indirection

df <- tibble(
  mean_var = 1,
  group_var = "g",
  group = 1,
  x = 10,
  y = 100
)
df
df |> grouped_mean(group, x)
df |> grouped_mean(group, y)

# embracing ----
grouped_mean <- function(df, group_var, mean_var) {
  df |> 
    group_by({{ group_var}}) |> 
    summarize(mean({{ mean_var}}))
}

df |> grouped_mean(group, x)
df |> grouped_mean(group, y)


summary6 <- function(data, var) {
  data |> summarize(
    min = min({{ var }}, na.rm = TRUE),
    mean = mean({{ var }}, na.rm = TRUE),
    median = median({{ var }}, na.rm = TRUE),
    max = max({{ var }}, na.rm = TRUE),
    n = n(),
    n_miss = sum(is.na({{ var }})),
    .groups = "drop"
  )
}

diamonds |> summary6(carat)

diamonds |> 
  group_by(cut) |> 
  summary6(carat)

diamonds |> 
  group_by(cut) |> 
  summary6(log10(carat))

count_prop <- function(df, var, sort = FALSE) {
  df |> 
    count({{var}}, sort = sort) |> 
    mutate(prop = n /sum(n))
}

diamonds |> count_prop(clarity)

unique_where <- function(df, condition, var) {
  df |> 
    filter({{ condition }}) |> 
    distinct({{ var }}) |> 
    arrange({{ var }})
}

flights |> unique_where(month == 12, dest)

subset_flights <- function(rows, cols) {
  flights |> 
    filter({{ rows }}) |> 
    select(time_hour, carrier, flight, {{ cols }})
} # hardcode


# data-masking vs. tidy-selection
count_missing <- function(df, group_vars, x_var) {
  df |> 
    group_by({{ group_vars }}) |> 
    summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
    )
}

flights |> 
  count_missing(c(year, month, day), dep_time)


# pick()
count_missing <- function(df, group_vars, x_var) {
  df |> 
    group_by(pick({{ group_vars }})) |> 
    summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
    )
}

flights |> 
  count_missing(c(year, month, day), dep_time)

count_wide <- function(data, rows, cols) {
  data |> 
    count(pick(c({{ rows }}, {{ cols }}))) |> 
    pivot_wider(
      names_from = {{ cols }}, 
      values_from = n,
      names_sort = TRUE,
      values_fill = 0
    )
  
}

diamonds |> count_wide(c(clarity, color), cut)
diamonds |> count_wide(c(clarity, cut), color)
count_wid

diamonds |> 
  count(clarity, color)
diamonds |> 
  count(pick(c(clarity, color)))

diamonds |> 
  pick(c(clarity, color)) 
# Must only be used inside data-masking verbs like

count_wide2 <- function(data, rows, cols) {
  data |> 
    count(pick(c({{ rows }}, {{ cols }})))
}
diamonds |> count_wide2(c(clarity, color), cut) |> 
  pivot_wider(
    names_from = cut,
    values_from = n
  )


# plot functions ----
diamonds |> 
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

diamonds |> 
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.05)


histogram <- function(df, var, binwidth = NULL) {
  df |> 
    ggplot(aes(x = {{var}})) +
    geom_histogram(binwidth = binwidth)
}

diamonds |> histogram(var = carat, 0.1)
diamonds |> histogram(var = carat, 0.2)

diamonds |> histogram(var = carat, 0.3) + 
  labs(x = "Size", y="Number of diamonds")

# more variables ----
linearity_check <- function(df, x, y) {
  df |> 
    ggplot(aes(x = {{x}}, y={{y}})) +
    geom_point() +
    geom_smooth(method = "loess", formula = y ~ x, color = "red", se = FALSE) +
    geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE)
}

starwars |> 
  filter( mass < 1000) |> 
  linearity_check(mass, height)

# https://twitter.com/ppaxisa/status/1574398423175921665
hex_plot <- function(df, x, y, z, bins = 20, fun = "mean") {
  df |> 
    ggplot(aes(x = {{ x }}, y = {{ y }}, z = {{ z }})) + 
    stat_summary_hex(
      aes(color = after_scale(fill)), # make border same color as fill
      bins = bins, 
      fun = fun,
    )
}

diamonds |> hex_plot(carat, price, depth)

# walrus operator ----
sorted_bars <- function(df, var) {
  df |> 
    mutate({{ var }} := fct_rev(fct_infreq({{ var }})))  |>
    ggplot(aes(y = {{ var }})) +
    geom_bar()
}

diamonds |> sorted_bars(clarity)

conditional_bars <- function(df, condition, var) {
  df |> 
    filter({{ condition }}) |> 
    ggplot(aes(x = {{ var }})) + 
    geom_bar()
}

diamonds |> conditional_bars(cut == "Good", clarity)


histogram <- function(df, var, binwidth) {
  label <- rlang::englue("A histogram of {{var}} with bindwidth {{binwidth}}")
  
  df |> 
    ggplot(aes(x = {{var}})) +
    geom_histogram(binwidth = binwidth) +
    labs(title = label)
}

diamonds |> histogram(carat, 0.2)
