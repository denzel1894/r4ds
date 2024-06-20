# install.packages("nycflights13")
library(nycflights13)
library(tidyverse)

# stats::filter()和stats::lag()，这是R包中的基本函数，与dplyr::filter()等冲突
nycflights13::flights
View(flights)
glimpse()
glimpse
View(flights)
?flights
glimpse(flights)
print(flights, width = Inf)

# a example
flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

flights |>
  filter(dest == "IAH") |>
  group_by(year, day)

# rows
# filter() # nolint: commented_code_linter.
flights %>%
  filter(dep_delay > 120)
flights %>%
  filter(month == 1 & day == 1)
flights %>%
  filter(month == 1, day == 1)
flights %>%
  filter(month == 1 | month == 2)
# A shorter way to select flights that departed in January or February
flights %>%
  filter(month %in% c(1, 2))
jan1 <- flights %>%
  filter(month == 1 & day == 1)
jan1

flights |>
  filter(month == 1 | 2) # this "works",but not wanted

# arrange
flights %>%
  arrange(dep_time)
flights %>%
  arrange(month, dep_time)
flights %>%
  arrange(month, day, dep_time) # day1.1 dep_time ordered
flights |>
  arrange(month, day, desc(dep_time)) # 利用desc实现dep-time的倒序排列

dep_time_arranged <- flights %>%
  arrange(month, day, dep_time)
View(dep_time_arranged)

flights %>%
  arrange(desc(dep_delay))


#------ distinct()
flights
flights %>%
  distinct()
# find the first occurrence of a unique row in the dataset and discard the rest.
flights %>%
  distinct(origin, dest) |>
  arrange(origin, dest)
flights %>%
  distinct(origin, dest, .keep_all = TRUE)

# count()
flights %>%
  count(origin, dest, sort = TRUE)



# exercises
flights %>%
  filter(arr_delay %in% c(1, 2)) %>%
  filter(dest %in% c("IAH", "HOU"))
View(flights %>%
  filter(arr_delay %in% c(1, 2)) %>%
  filter(dest %in% c("IAH", "HOU")))

# arrange()
flights %>%
  arrange(desc(dep_delay))
flights %>%
  arrange(flight, air_time)
View(flights %>%
  arrange(tailnum, air_time))

# columns
# mutate()
mutate_test <- flights %>%
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  )

glimpse(mutate_test)

flights %>%
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = year
  )

flights %>%
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = year
  )

flights %>%
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .keep = "used"
  )

# select()
flights %>%
  select(year)
flights %>%
  select(year, month, day)
flights %>%
  select(year:day)
flights %>%
  select(!year:day)
flights %>%
  select(where(is.character)) # where接收一个函数名称

is.character(flights$carrier) # 判断向量是否是chr类型
?select

flights |>
  select(c(year, day))

flights %>%
  select(tail_num = tailnum) # rename变量名称

# rename()
rename_test <- flights %>%
  rename(tail_num = tailnum)
View(rename_test)

# 批量修改名字再看 todo:
?janitor::clean_names

# relocate()
flights %>%
  relocate(time_hour, air_time)

flights %>%
  relocate(time_hour, air_time, .before = month)
flights %>%
  relocate(year:dep_time, .after = sched_dep_time)
flights %>%
  relocate(starts_with("arr"), .before = dep_time)

# pipe
flights |>
  filter(dest == "IAH") |>
  mutate(speed = distance / air_time * 60) |>
  select(year:day, dep_time, carrier, flight, speed) |>
  arrange(desc(speed))

# if not pipe
arrange(
  select(
    mutate(
      filter(
        flights,
        dest == "IAH"
      ),
      speed = distance / air_time * 60
    ),
    year:day, dep_time, carrier, flight, speed
  ),
  desc(speed)
)

# group_by()
flights |>
  group_by(month)

flights |>
  group_by(month) |>
  summarize(
    avg_delay = mean(dep_delay)
  )

flights |>
  group_by(month) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

flights |>
  group_by(month) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  )

flights |>
  group_by(month, year) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  )
flights |> group_by(year)

flights |>
  group_by(dest)

flights |>
  group_by(dest) |>
  slice_max(arr_delay, n = 1) |>
  relocate(dest)

flights |>
  group_by(dest) |>
  slice_max(arr_delay, n = 1) |>
  relocate(dest) |>
  relocate(arr_delay)

flights |>
  group_by(dest) |>
  slice_head(n = 2)
flights |>
  slice_head(n = 2)


flights |>
  group_by(dest) |>
  slice_tail(n = 1)

flights |>
  group_by(dest) |>
  slice_sample(n = 2)

flights |>
  group_by(dest) |>
  slice_min(dep_time, n = 2) |>
  relocate(dest)

flights |>
  slice_max(dep_time, prop = 0.1) |>
  relocate(dep_time, .before = day)

flights |>
  group_by(dest) |>
  slice_min(dep_time, n = 1, with_ties = FALSE) |>
  relocate(dest) # exactly one row per group

daily <- flights |>
  group_by(year, month, day)
daily

daily_flights <- daily |>
  summarize(n = n())
daily_flights
View(daily_flights)

daily_flights <- daily |>
  summarize(
    n = n(),
    .groups = "drop_last"
  )
daily_flights
daily

daily_flights <- daily |>
  summarize(
    n = n(),
    .groups = "keep"
  )
daily_flights
# 这里的.groups，主要用于summary后保留所有的分组信息。
# 而n的数值是，year、month和day的多变量分365组后每组的个数。
# 和.groups具体什么数值没有关系。




daily |>
  ungroup()

daily |>
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    flights = n()
  )

# .by 操作符，可以不用管，比较新 ------

flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = month
  )

flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = c(origin, dest)
  )
# case study : aggregates and sample size
library(Lahman)
Lahman::Batting
View(Batting)
glimpse(Batting)

batters <- Lahman::Batting |>
  group_by(playerID) |>
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
batters

batters <- Lahman::Batting |>
  group_by(playerID) |>
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = n()
  )
batters

Batting |>
  filter(playerID == "aardsda01") |>
  select(H, AB, playerID)

batters <- Lahman::Batting |>
  group_by(playerID) |>
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
batters

batters |>
  filter(n > 100) |>
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE)

batters |>
  filter(n > 100) |>
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = TRUE) # confidence region ?

?geom_smooth
