# make the first character upper case ----
first_upper <- function(x) {
  str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1,1))
  x
}

# strip percent signs, commas, and dollar signs 
# from a string before converting it into a number
clear_number <- function(x) {
  is_pct <- str_detect(x, "%")
  num <- x |> 
    str_remove_all("%") |> 
    str_remove_all(",") |> 
    str_remove_all(fixed("$")) |> 
    as.numeric()
  if_else(is_pct, num / 100, num)
}

fix_na <- function(x) {
  if_else(x %in% c(997, 998, 999), NA, x)
}

commas <- function(x) {
  str_flatten(x, collapse = ", ", last = " and ")
}


cv <- function(x, na.rm = FALSE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

n_missing <- function(x) {
  sum(is.na(x))
} 

mape <- function(actual, predicted) {
  sum(abs((actual - predicted) / actual)) / length(actual)
}


# arrange, sort the df by the column name
arrange_var <- function(df, var) {
  df |> arrange({{var}})
}

# capture the structure of the data frames
# returns a tibble with one row for each column
df_types <- function(df) {
  tibble(
    col_name = names(df),
    col_type = map_chr(df,vctrs::vec_ptype_full),
    n_miss = map_int(df, \(x) sum(is.na(x)))
  )
}
