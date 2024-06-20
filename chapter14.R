library(tidyverse)
library(babynames)

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'
string1
string2

double_quote <- "\""
double_quote

double_quote <- '"'
double_quote

single_quote <- '\''
single_quote

single_quote <- "'"
single_quote

backslash <- "\\"
backslash

x <- c(single_quote, double_quote, backslash)
x
str_view(x)

tricky <- "double_quote <- \"\\\"\" # or '\"'
single_quote <- '\\'' # or \"'\""
tricky
str_view(tricky)

tricky <- r"(double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'")"
tricky
str_view(tricky)

tricky <- r"--(double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'")--"
tricky
str_view(tricky)

x <- c("one\ntwo", "one\ttwo", "\u00b5", "\U0001f604")
x
str_view(x)
str_c("x", "y")
str_c("x", "y", "z")
str_c("Hello ", c("John", "Susan"))

df <- tibble(name = c("Flora", "David", "Terra", NA))
df
df |> mutate(
  greeting = str_c("Hi ", name, "!")
)

df |> 
  mutate(
    greeting1 = str_c("Hi ", coalesce(name, "you"), "!"),
    greeting2 = coalesce(str_c("Hi ", name, "!"), "Hi")
  )

df |> 
  mutate(
    greeting = str_glue("Hi {name}")
  )
df |> mutate(greeting = str_glue("{{Hi {name}!}}"))

str_flatten(c("x", "y", "z"))
str_flatten(c("x","y", "z"), ", ")
str_flatten(c("x", "y", "z"), ", ", last = ", and ")

df <- tribble(
  ~ name, ~ fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin"
)
df |>
  group_by(name) |> 
  summarize(fruits = str_flatten(fruit, ", "))

# extracting data from strings ----
# rows
df1 <- tibble(x = c("a,b,c", "d,e", "f"))
df1
df1 |> 
  separate_longer_delim(x, delim = ",")

df2 <- tibble(x = c("1211", "131", "21"))
df2
df2 |> 
  separate_longer_position(x, width = 1)

# colomns
df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3
df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", "edition", "year")
  )

df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", NA, "year")
  )

df4 <- tibble(x = c("202215TX", "202122LA", "202325CA")) 
df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4, age = 2, state = 2)
  )
df4

df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4,2,2)
  )
df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4,2, state = 2)
  )

df <- tibble(x = c("1-1-1", "1-1-2", "1-3", "1-3-2", "1"))

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z")
  )