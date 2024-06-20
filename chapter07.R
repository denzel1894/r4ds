library(tidyverse)
students <- read_csv("data/student.csv")
students
glimpse(students)

students <- read_csv("data/student.csv", na = c("N/A"))
students
students |>
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )

students |>
  janitor::clean_names()

students <- students |>
  janitor::clean_names()

students <- students |>
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )
students

read_csv(
  "a,b,c
  1,2,3
  4,5,6"
)

read_csv(
  "The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3",
  skip = 2
)

read_csv(
  "# A comment I want to skip
  x,y,z
  1,2,3",
  comment = "#"
)

read_csv(
  "1,2,3
  4,5,6",
  col_names = FALSE
)

read_csv(
  "1,2,3
  4,5,6",
  col_names = c("x", "y", "z")
)

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
) # `1`是变量名，R通过``符号使其名称non-syntactic names 合法
annoying
rnorm(10)

annoying |>
  select(`1`)
annoying |>
  ggplot(aes(x = `1`, y = `2`)) +
  geom_point()
annoying |>
  mutate(
    `3` = `2` / `1`
  )

annoying |>
  mutate(
    `3` = `2` / `1`
  ) |>
  rename(
    one = `1`,
    two = `2`,
    three = `3`
  )

# 7.3 controlling column types

read_csv("
  logical,numeric,date,string
  TRUE,1,2021-01-15,abc
  false,4.5,2021-02-15,def
  T,Inf,2021-02-16,ghi
")

simple_csv <-
  "x
  10
  .
  20
  30"
simple_csv
read_csv(simple_csv)

df <- read_csv(
  simple_csv,
  col_types = list(x = col_double())
)

problems(df)

read_csv(simple_csv, na = ".")

another_csv <- "
x,y,z
1,2,3"

df <- read_csv(
  another_csv,
  col_types = list(x = col_integer(), y = col_character())
)
df

another_csv <- "
x,y,z
1,2,3"

read_csv(
  another_csv,
  col_types = cols(.default = col_character())
)

read_csv(
  another_csv,
  col_types = cols(.default = col_integer())
)

df_specify <- read_csv(
  another_csv,
  col_types = cols_only(x = col_character())
)
df_specify

df_specify <- read_csv(
  another_csv,
  col_types = cols_only(x = col_character(), y = col_integer())
)
df_specify

# Reading data from multiple files ----

sales_files <- c(
  "data/01-sales.csv",
  "data/02-sales.csv",
  "data/03-sales.csv"
)
sales_files
read_csv(sales_files, id = "file")
read_csv(sales_files) # set id file, show content source

sales_files2 <- list.files("data", pattern = "sales\\.csv$", full.names = TRUE)
sales_files2

write_csv(students, "data/student2.csv") # mean_plan设置的变量类型fct会丢失
read_csv("data/student2.csv")
students

write_rds(students, "data/students.rds")
read_rds("data/students.rds")

library(arrow)
write_parquet(students, "data/students.parquet")
read_parquet("data/students.parquet")

# data entry ----
tibble(
  x = c(1, 2, 5),
  y = c("h", "m", "g"),
  z = c(0.08, 0.83, 0.60)
)
# transposed tibble --> tribble
tribble(
  ~x, ~y, ~z,
  1, "h", 0.08,
  2, "m", 0.83,
  5, "g", 0.60
) # 与read_csv 格式不同，换行时候也得加","
