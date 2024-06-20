# load packages ----
library(tidyverse)

# variation ----
diamonds |> ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.5)

smaller <- diamonds |>
  filter(carat < 3)
smaller
View(smaller)
minus_ones <- diamonds |>
  filter(carat < 0)
minus_ones

smaller |>
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

## Unusual values
diamonds |>
  ggplot(aes(x = y)) +
  geom_histogram(binwidth = 0.5)

# zoom to small values of the y-axis
diamonds |>
  ggplot(aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
diamonds |>
  ggplot(aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(20, 40), ylim = c(0, 50))

unusual <- diamonds |> 
  filter(y <3 | y > 20) |> 
  select(price, x, y, z)
unusual
unusual <- diamonds |> 
  filter(y <3 | y > 20) |> 
  select(price, x, y, z) |> 
  arrange(y)
unusual

# Exercises
diamonds |> ggplot(aes(x = price)) +
  geom_histogram(binwidth = 500)
diamonds |> 
  filter(carat == 0.99) |> 
  count(carat)
diamonds |> 
  filter(carat == 1) |> 
  count(carat)

diamonds2 <- diamonds |> 
  filter(between(y, 3, 20))
diamonds2

diamonds2 <- diamonds |> 
  mutate(y = if_else(y < 3|y>20, NA, y))
diamonds2

diamonds2 |> 
  ggplot(aes(x = x, y = y)) +
  geom_point()
diamonds2 |> 
  ggplot(aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

# is.na()
View(nycflights13::flights)
df_sched_dep_time <- nycflights13::flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  )
View(df_sched_dep_time)

df_sched_dep_time |> 
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4)
df_sched_dep_time |> 
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(aes(color = cancelled), binwidth = 1/2)

df_sched_dep_time |> 
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4) +
  facet_wrap(~cancelled, scales = "free_y")

df_sched_dep_time |> 
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4) +
  facet_wrap(~cancelled, scales = "free_y")

# Covariation -----
diamonds |> ggplot(aes(x = price)) +
  geom_freqpoly(aes(color = cut), binwidth = 250, linewidth = 0.75)
# ordered factor variable for cut..
glimpse(diamonds) 
diamonds

diamonds |> 
  ggplot(aes(x = price, y = after_stat(density))) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linwidth = 0.75)
  
diamonds |> 
  ggplot() +
  geom_boxplot(aes(x = cut, y=price))

mpg |> 
  ggplot(aes(x = class, y = hwy)) +
  geom_boxplot()

mpg |> 
  ggplot(aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot()

mpg |> 
  ggplot(aes(x = hwy, y = fct_reorder(class, hwy, median))) +
  geom_boxplot()
mpg |> 
  ggplot(aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot()+
  coord_flip()

## two categorical variables
diamonds |> ggplot(aes(x = cut, y = color)) +
  geom_count()

diamonds |> 
  count(color, cut)

diamonds |> 
  count(color, cut) |> 
  ggplot(aes(x = color, y=cut)) +
  geom_tile(aes(fill = n))

diamonds |> 
  count(color, cut) |> 
  ggplot(aes(x = color, y = n, fill = cut)) +
  geom_bar(stat = "identity", position = "fill")
diamonds |> 
  count(color, cut) |> 
  ggplot(aes(x = color, y = n, fill = cut)) +
  geom_bar(stat = "identity", position = "dodge")
diamonds |> 
  count(color, cut) |> 
  ggplot(aes(x = color, y = n, fill = cut)) +
  geom_bar(stat = "identity", position = "identity")

ggplot(smaller, aes(x = carat, y = price)) +
  geom_point()
ggplot(smaller, aes(x = carat, y = price)) +
  geom_point(alpha=1/100)

#use bin ----
ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d()         
ggplot(smaller, aes(x = carat, y = price)) +
  geom_hex()

#use boxplot, cut_width()
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group=cut_width(carat, 0.1)))
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group=cut_width(carat, 0.1)),varwidth = TRUE)

diamonds |> 
  filter(x >= 4) |> 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

# 10.6 patterns and models ----
library(tidymodels)

diamonds <- diamonds |> 
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  )
View(diamonds)

diamonds_fit <- linear_reg() |> 
  fit(log_price ~ log_carat, data = diamonds)
diamonds_fit

diamonds_aug <- augment(diamonds_fit, new_data = diamonds) |> 
  mutate(.resid = exp(.resid))

augment(diamonds_fit, new_data = diamonds) 
glimpse(augment(diamonds_fit, new_data = diamonds) )
diamonds_aug
glimpse(diamonds_aug)


ggplot(diamonds_aug, aes(x = carat, y = .resid)) + 
  geom_point()
ggplot(diamonds_aug, aes(x = cut, y = .resid)) + 
  geom_boxplot()
