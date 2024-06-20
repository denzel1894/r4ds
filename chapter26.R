library(tidyverse)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df |> summarize(
  n = n()
)

df |> summarize(
  n = n(),
  a = median(a)
)

df |> summarize(
  n = n(),
  median_a = (max(a) + min(a))/2, # The method of cac median not like this, ref the doc
  max_a = max(a),
  min_a = min(a),
  median_a_2 = median(a)
)
df |> 
  arrange(a)
df |> 
  arrange(desc(a))
df |> arrange_var(a)
df |> arrange_var(desc(a))

df |> summarize(
  n= n(),
  across(a:d, median)
)
df |> summarize(
  n= n(),
  across(a:d, c(median,max,min))
)

df |> summarize(
  n= n(),
  across(everything(), median)
)

sample(2, 10, replace = TRUE)
sample(2, 10, replace = FALSE)

df <- tibble(
  grp = sample(2,10, replace = TRUE),
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10),
  e = c("haha","haha","haha","haha","haha","haha","haha","haha","haha","heeh")
)
df
df |> 
  group_by(grp) |> 
  summarize(across(everything(),median))
df |> 
  group_by(grp) |> 
  summarize(across(starts_with("a"),median))


df |> 
  group_by(grp) |> 
  summarize(across(where(is.numeric),median))
df |> 
  group_by(grp) |> 
  summarize(across(!where(is.character),median))

rnorm_na <- function(n, n_na, mean = 0, sd = 1) {
  sample(
    c(
      rnorm(n-n_na, mean=mean, sd = sd),
      rep(NA, n_na)
    )
  )
}
rnorm_na(5,1)

df_miss <- tibble(
  a = rnorm_na(5, 1),
  b = rnorm_na(5, 1),
  c = rnorm_na(5, 2),
  d = rnorm(5)
)
df_miss

df_miss |> 
  summarize(
    across(a:d, median),
    n = n()
  )

df_miss |> 
  summarize(
    across(a:d, function(x) median(x, na.rm=TRUE)),
    n = n()
  )
df_miss |> 
  summarize(
    across(a:d, 
           function(x) {
             median(x, na.rm=TRUE)
             }
           ),
    n = n()
  )

df_miss |> 
  summarize(
    across(a:d, \(x) median(x, na.rm = TRUE)),
    n = n()
  )

df_miss |> 
  summarize(
    across(
      a:d,
      list(
        median = \(x) median(x, na.rm= TRUE),
        n_miss = \(x) sum(is.na(x))
      )
    ),
    n = n()
  )


df_miss |> 
  summarize(
    across(
      a:d,
      list(
        median = \(x) median(x, na.rm= TRUE),
        n_miss = \(x) sum(is.na(x))
      ),
      .names = "{.fn}_haha_{.col}"
    ),
    n = n()
  )

df_miss
df_miss |> 
  mutate(
    across(a:d, \(x) coalesce(x, 0))
  )

x <- sample(c(1:5, NA, NA, NA))
x
coalesce(x, 0L)

df_miss |> 
  mutate(
    across(
      a:d, 
      \(x) coalesce(x, 0),
      .names = "{.col}_na_zero"
    )
  )

df_miss |> 
  filter(if_any(a:d, is.na))
df_miss |> 
  filter(if_all(a:d, is.na))

df_date <- tibble(
  name = c("Amy", "Bob"),
  date = ymd(c("2009-08-03", "2010-01-16"))
)
df_date

expand_dates <- function(df) {
  df |> 
    mutate(
      across(
        where(is.Date),
        list(
          year = \(x) year(x),
          month = month,
          day = mday
        ),
        .names = "kong_{.fn}"
      )
    )
}

df_date |> 
  expand_dates()

summarize_means <- function(df, summary_vars = where(is.numeric)) {
  df |> 
    summarize(
      across({{ summary_vars }},
             \(x) mean(x, na.rm = TRUE)),
      n=n(),
      .groups = "drop"
    )
}

diamonds |> 
  group_by(cut) |> 
  summarize_means()

diamonds |> 
  group_by(cut) |> 
  summarize_means(c(carat, x:z))

df |> 
  summarize(across(a:d, list(median = median, mean = mean)))

df |> 
  pivot_longer(a:d)

long <- df |> 
  pivot_longer(a:d) |> 
  group_by(name) |> 
  summarize(
    median = median(value),
    mean = mean(value)
  )
long

long |> 
  pivot_wider(
    names_from = name,
    values_from = c(median, mean),
    names_vary = "slowest",
    names_glue = "{name}_{.value}"
  )

df_paired <- tibble(
  a_val = rnorm(10),
  a_wts = runif(10),
  b_val = rnorm(10),
  b_wts = runif(10),
  c_val = rnorm(10),
  c_wts = runif(10),
  d_val = rnorm(10),
  d_wts = runif(10)
)
df_paired

df_long <- df_paired |> 
  pivot_longer(
    everything(), 
    names_to = c("group", ".value"), 
    names_sep = "_"
  )
df_long

df_long |> 
  group_by(group) |> 
  summarize(mean = weighted.mean(val, wts))


paths <- list.files("data", pattern = "^[a-zA-Z0-9]+.+([.]xlsx)$", full.names = TRUE)
paths

data_theoretical_prediction <- readxl::read_excel("data/theoretical_prediction.xlsx")
data_theoretical_prediction

files <- list(
  readxl::read_excel("data/theoretical_prediction.xlsx"),
  readxl::read_excel("data/CopyOftheoretical_prediction.xlsx")
)
files[[2]]

files <- map(paths, readxl::read_excel)
files
length(files)
files[[1]]
View(files[[1]])

list_rbind(files)

paths |> 
  map(readxl::read_excel) |> 
  list_rbind()

paths |> 
  map(\(path) readxl::read_excel(path, n_max = 2)) |> 
  list_rbind()

paths |> set_names(basename)
paths


files <- paths |> 
  set_names(basename) |> 
  map(readxl::read_excel)
files
files[["CopyOftheoretical_prediction.xlsx"]]

paths |> 
  set_names(basename) |> 
  map(readxl::read_excel) |> 
  list_rbind(names_to = "name_from_file")

paths |> 
  set_names() |> 
  map(readxl::read_excel) |> 
  list_rbind(names_to = "year") |> 
  separate_wider_delim(year, delim = "/", names = c("DIR", "FILE")) |> 
  separate_wider_delim(FILE, delim=".", names = c("file", "ext"))

data_origin_copy <- paths |> 
  set_names() |> 
  map(readxl::read_excel) |> 
  list_rbind(names_to = "path_and_filename")

data_origin_copy

write_csv(data_origin_copy, "data/data_origin_copy.csv")


# many simple iterations ----

library(readxl)

files <- paths |> 
  map(readxl::read_excel)
files

df_types <- function(df) {
  tibble(
    col_name = names(df),
    col_type = map_chr(df,vctrs::vec_ptype_full),
    n_miss = map_int(df, \(x) sum(is.na(x)))
  )
}

df_types(data_origin_copy)

files |> 
  map(df_types) |> 
  list_rbind(names_to = "file_name") |> 
  select(-n_miss) |> 
  pivot_wider(names_from = col_name, values_from = col_type)

files <- paths |> 
  map(possibly(\(path) readxl::read_excel(path), NULL))

files

data <- files |> list_rbind()
data

failed <- map_vec(files, is.null)
failed
paths[failed]
paths[c(TRUE, FALSE)]


# writing to a database ----

con <- DBI::dbConnect(duckdb::duckdb())
con
paths
duckdb::duckdb_read_csv(con, "gapminder", paths)

template <- readxl::read_excel(paths[[1]])
template$year <- 1952
template

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbCreateTable(con, "gapminder",template)

con |> tbl("gapminder")

append_file <- function(path) {
  df <- readxl::read_excel(path)
  df$year <- 1952
  
  DBI::dbAppendTable(con, "gapminder",df)
}

paths
paths |> map(append_file)

paths |> walk(append_file)

con |> 
  tbl("gapminder") |> 
  count(year)

# writing csv files ----

by_clarity <- diamonds |> 
  group_nest(clarity)
by_clarity

by_clarity$data[[1]]

by_clarity <- by_clarity |> 
  mutate(path = str_glue("data/diamonds/diamonds-{clarity}.csv"))
by_clarity
write_csv(by_clarity$data[[1]], by_clarity$path[[1]])

walk2(by_clarity$data, by_clarity$path, write_csv)

# saving plots ----

carat_histogram <- function(df) {
  ggplot(df, aes(x = carat)) + geom_histogram(binwidth = 0.1)  
}

carat_histogram(by_clarity$data[[1]])

by_clarity <- by_clarity |> 
  mutate(
    plot = map(data, carat_histogram),
    path = str_glue("data/clarity/clarity-{clarity}.png")
  )
by_clarity

walk2(
  by_clarity$path,
  by_clarity$plot,
  \(path, plot) ggsave(path, plot, width = 6, height = 6)
)
