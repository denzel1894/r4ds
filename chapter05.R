library(tidyverse)

# pivot_longer() ----
billboard
View(billboard)
glimpse(billboard)


billboard |> 
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  )

# delete NA data in column "week"
billboard |> 
  pivot_longer(
    cols = !c(artist, track, date.entered),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )

billboard |> 
  pivot_longer(
    cols = !c(artist, track, date.entered),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week) #replace original "week" to num
  )

billboard |> 
  pivot_longer(
    cols = !c(artist, track, date.entered),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week2 = parse_number(week) 
  )

billboard_longer <- billboard |> 
  pivot_longer(
    cols = !c(artist, track, date.entered),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week) 
  )


# plot ----
# group aesthetic is by default set to the interaction of all discrete variables in the plot. 
# mapping group to a variable that has a different value for each group.
# different group, different line, not one line
billboard_longer |> 
  ggplot(aes(x = week, y = rank, group = track)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()

billboard_longer |> 
  ggplot(aes(x = week, y = rank)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()


# manual pivoting ----
df <- tribble(
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)

df |> 
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

# col name includes several variables
who2

who2 |> 
  pivot_longer(
    cols = !(country:year),
    names_to = "haha",
    values_to = "count"
  )

who2 |> 
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "genger", "age"),
    names_sep = "_",
    values_to = "count"
  )

household
View(household)

household |> 
  pivot_longer(
    cols = !family, 
    names_to = c(".value", "child"), 
    names_sep = "_", 
    values_drop_na = TRUE
  )

# pivot_wider() ----
df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "B",        "bp1",    140,
  "B",        "bp2",    115, 
  "A",        "bp2",    120,
  "A",        "bp3",    105
)
df
df |> 
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

# explain process
df |> 
  distinct(measurement)

df |> 
  distinct(measurement) |> 
  pull()

df |> 
  select(-measurement, -value)
df |> 
  select(!measurement & !value)
df |> 
  select(!measurement & !value) |> 
  distinct()

df$measurement

df |> 
  select(-measurement, -value) |> 
  distinct() |> 
  mutate(x = NA, y = NA, z = NA)


df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "A",        "bp1",    102,
  "A",        "bp2",    120,
  "B",        "bp1",    140, 
  "B",        "bp2",    115
)

df |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

df |> 
  group_by(id, measurement) |> 
  summarize(n = n(), .groups = "drop") |> 
  filter(n > 1)

# 修复破坏
df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "A",        "bp2",    120,
  "B",        "bp1",    140, 
  "B",        "bp2",    115
)

df |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  ) |> 
  pivot_longer(
    cols = c(bp1,bp2),
    names_to = "measurement2",
    values_to = "value2"
  )
df

