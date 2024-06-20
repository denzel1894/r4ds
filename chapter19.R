library(tidyverse)
library(nycflights13)

airlines
airports
planes
weather
View(weather)

planes |> 
  count(tailnum) |> 
  filter(n > 1)
planes |> 
  count(tailnum) 

weather |> 
  count(time_hour, origin) |> 
  filter(n > 1)

planes |> 
  filter(is.na(tailnum))
weather |> 
  filter(is.na(time_hour) | is.na(origin))

flights |> 
  count(time_hour, carrier, flight) |> 
  filter(n > 1)

flights |> 
  count(time_hour, carrier, flight) 
airports |>
  count(alt, lat) |> 
  filter(n > 1)

flights2 <- flights |> 
  mutate(id = row_number(), .before = 1)

flights2

flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)
flights2

flights2 |> 
  left_join(airlines) # oining with `by = join_by(carrier)`
flights2 |> 
  left_join(weather |> select(origin, time_hour, temp, wind_speed))
flights2 |> 
  left_join(planes |> select(tailnum, type, engines, seats))

flights2 |> 
  filter(tailnum == "N3ALAA") |> 
  left_join(planes |> select(tailnum, type, engines, seats))

flights2 |> 
  left_join(planes, join_by(tailnum))

airports |> 
  semi_join(flights2, join_by(faa == origin))
airports |> 
  semi_join(flights2, join_by(faa == dest))

flights2 |> 
  anti_join(airports, join_by(dest == faa)) |> 
  distinct(dest)

airports |> 
  select(faa) |> 
  filter(faa == "STT")

flights2 |>
  anti_join(planes, join_by(tailnum)) |> 
  distinct(tailnum)
