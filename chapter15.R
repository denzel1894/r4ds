library(tidyverse)
library(babynames)

str_view(fruit)

str_view(c("a", "ab", "ae", "bd", "ea", "eab"), "a.")
str_view(c("a", "ab", "ae", "bd", "ea", "eab"))

str_view(c("a", "ab", "abb"), "ab?")
str_view(c("a", "ab", "abb"), "ab+")
str_view(c("a", "ab", "abb"), "ab*")

str_view(words)
str_view(words, "[aeiou]x[aeiou]")
str_view(words, "[^aeiou]y[^aeiou]")

str_view(fruit, "apple|melon|nut")
str_view(fruit, "aa|ee|ii|oo|uu")

str_detect(c("a", "b", "c"), "[aeiou]")

babynames |> 
  filter(str_detect(name, "x")) |> 
  count(name, wt = n, sort = TRUE)

babynames |> 
  group_by(year) |> 
  summarize(prop_x = mean(str_detect(name, "x")))

babynames |> 
  group_by(year) |> 
  summarize(sum_x = sum(str_detect(name, "x")))
babynames |> 
  group_by(year) |> 
  summarize(sum_x = sum(str_detect(name, "x")), 
            n = n(),
            prop_x = mean(str_detect(name, "x")))
babynames |> 
  group_by(year) |> 
  summarize(prop_x = mean(str_detect(name, "x"))) |> 
  ggplot(aes(x = year, y = prop_x)) + 
  geom_line()

str_detect(babynames$name, "x")
str_subset(babynames$name, "x")
str_which(babynames$name, "x")

# str_count()-----

x <- c("apple", "banana", "pear")
str_count(x, "p")
str_count("abababa", "aba")
str_view("abababa", "aba")

babynames |> 
  count(name) |> 
  mutate(
    vowels = str_count(name, "[aeiou]"), 
    consonants = str_count(name, "[^aeiou]")
  )
babynames |> 
  count(name) |> 
  mutate(
    vowels = str_count(name, "[aeiouAEIOU]"), 
    consonants = str_count(name, "[^aeiou]")
  )
babynames |> 
  count(name) |> 
  mutate(
    vowels = str_count(name, regex("[aeiou]", ignore_case = TRUE)), 
    consonants = str_count(name, "[^aeiou]")
  )
babynames |> 
  count(name) |> 
  mutate(
    vowels = str_count(str_to_lower(name), "[aeiou]"), 
    consonants = str_count(name, "[^aeiou]")
  )

babynames |> 
  count(name) |> 
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

#str_replace() ----
x <- c("apple", "pear", "banana")
str_replace_all(x, "[aeiou]", "-")
x <- c("apple", "pear", "banana")
str_remove_all(x, "[aeiou]")


df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45", 
  "<Brandon>-N_33",
  "<Sharon>-F_38", 
  "<Penny>-F_58",
  "<Justin>-M_41", 
  "<Patricia>-F_84", 
)
df

df |> 
  separate_wider_regex(
    str,
    patterns = c(
      "<", 
      name = "[A-Za-z]+", 
      ">-", 
      gender = ".",
      "_",
      age = "[0-9]+"
    )
  )

#pattern details ----
#Escaping----

dot <- "\\."
dot
str_view(dot)
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
str_view(x)
str_view(x, "\\\\")

# raw strings
str_view(x, r"{\\}")
str_view(x, r"(\\)")

str_view(c("abc", "a.c", "a*c", "a c", "b.c"), "b[.]c")
str_view(c("abc", "b.c", "b*c", "a c"), ".[*]c")

#Anchors ----
str_view(fruit, "^a")
str_view(fruit, "a$")
str_view(fruit, "apple")
str_view(fruit, "^apple$")

x <- c("summary(x)", "summarize(df)", "rowsum(x)", "sum(x)")
str_view(x, "sum")
str_view(x, "\\bsum\\b") #boundary

str_view("abc", c("$", "^", "\\b"))
str_view("abc", c("$", "^"))

str_replace_all("abc", c("$", "^", "\\b"), "--")

#Character classes ----
x <- "abcd ABCD 12345 -!@#%."
str_view(x, "[abc]+")
str_view(x, "[a-z]+")
str_view(x, "[^a-z0-9]+")

# You need an escape to match characters that are otherwise
# special inside of []
str_view("a-b-c", "[a-c]")
str_view("a-b-c", "[a\\-c]")

x <- "abcd ABCD 12345 -!@#%."
str_view(x, "\\d+")
str_view(x, r"{\d+}") #equal
str_view(x, "\\D+")

str_view(x, "\\s+")
str_view(x, "\\S+")
str_view(x, "\\w+")
str_view(x, "\\W+")

# Quantifiers ----

# Grouping and capturing ----
str_view(fruit, "(..)\\1")
str_view(fruit, r"{(..).*\1}")
str_view(words, "^(..).*\\1$")
str_view(words, "(..).*\\1")

sentences |> 
  str_replace("(\\w+) (\\w+) (\\w+)", "\\1 \\3 \\2") |> 
  str_view()
sentences |> 
  str_view()

sentences |> 
  str_match("the (\\w+) (\\w+)") |> 
  head()

sentences |> 
  str_match("the (\\w+) (\\w+)")

sentences |> 
  str_match("the (\\w+) (\\w+)") |> 
  as_tibble(.name_repair = "minimal") |> 
  set_names("match", "word1", "word2")

x <- c("a gray cat", "a grey dog")
str_match(x, "gr(e|a)y")
str_match(x, "gr(?:e|a)y")

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine 3"
x |> str_view()
str_view(x, ".Line")
str_view(x, regex(".Line", dotall = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_view(x, "^Line")
str_view(x, regex("^Line", multiline = TRUE))

phone <- regex(
  r"(
    \(?     # optional opening parens
    (\d{3}) # area code
    [)\-]?  # optional closing parens or dash
    \ ?     # optional space
    (\d{3}) # another three numbers
    [\ -]?  # optional space or dash
    (\d{4}) # four more numbers
  )", 
  comments = TRUE
)
str_extract(c("514-791-8141", "(123) 456 7890", "123456"), phone)


str_view(c("", "a", "."), fixed("."))
str_view("x X", "X")
str_view("x X", fixed("X", ignore_case = TRUE))
str_view("i İ ı I", fixed("İ", ignore_case = TRUE))
str_view("i İ ı I", coll("İ", ignore_case = TRUE, locale = "tr"))

str_view(sentences, "^The")
str_view(sentences, "^The\\b")
str_view(sentences, "^She|He|It|They\\b")
str_view(sentences, "^(She|He|It|They)\\b")

pos <- c("He is a boy", "She had a good time")
neg <- c("Shells come from the sea", "Hadley said 'It's a great day'")

pattern <- "^(She|He|It|They)\\b"
str_detect(pos, pattern)
#> [1] TRUE TRUE
str_detect(neg, pattern)
#> [1] FALSE FALSE

str_view(words, "^[^aeiou]+$")
str_view(words[!str_detect(words, "[aeiou]")])

str_detect(words, "[aeiou]")
!str_detect(words, "[aeiou]")
words[1]
words
words[c(TRUE, FALSE, TRUE)]
words[1:3][c(TRUE, FALSE, TRUE)] |> str_view()

str_view(words, "a.*b|b.*a")

words[str_detect(words, "a") & str_detect(words, "b")]


words[
  str_detect(words, "a") &
    str_detect(words, "e") &
    str_detect(words, "i") &
    str_detect(words, "o")
]
words[
  str_detect(words, "a") &
    str_detect(words, "e") &
    str_detect(words, "i") &
    str_detect(words, "o") &
    str_detect(words, "u")
] # none

#In general, 
#if you get stuck trying to create a single regexp that solves your problem, 
#take a step back and think if you could break the problem down into smaller pieces, 
#solving each challenge before moving onto the next one.

str_view(sentences, "\\b(red|green|blue)\\b")
rgb <- c("red", "green", "blue")

str_flatten(rgb, "|")
str_c("\\b(", str_flatten(rgb, "|"), ")\\b")

colors() |> str_view()
cols <- colors()
cols <- cols[!str_detect(cols, "\\d")]
cols |> str_view()

pattern <- str_c("\\b(", str_flatten(cols, "|"), ")\\b")
str_view(sentences, pattern)

pattern
?str_escape

str_detect(c("a", "."), str_escape("."))
str_detect(c("a", "."), ".")

apropos("replace")
head(list.files(pattern = "\\.Rmd$"))
list.files()
list.files(pattern = r"{\.R$}")


