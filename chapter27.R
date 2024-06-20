library(tidyverse)

x <- c("one", "two", "three", "four", "five")
x
x[c(3,2,5)]
x[c(1,1,1,3,3,5)]
x[1]
x[0]

x[c(-1)]
x[-1]
x[-2]
x[c(-1,-2)]

x <- c(10,3,NA,5,8,1,NA)
x
x[c(TRUE, FALSE)]
x[!is.na(x)]
x[x %% 2 == 0]

x <- c(abc = 1, def=2, xyz =5)
x
View(x)

x[c("xyz")]
x[c("xyz", "def")]

x[]

df <- tibble(
  x = 1:3,
  y = c("a", "e", "f"),
  z = runif(3)
)
df
df[1,2]
df[,c("x", "y")]
df[1:2, c("x", "y")]

df[df$x >1,]

df1 <- data.frame(x = 1:3)
df1
df1 <- data.frame(
  x=1:3,
  y=2:4
)
df1
df1[,"x"]

df2 <- tibble(x=1:3)
df2
df2[2,"x"]

df1[,"x",drop = FALSE] # keep the tibble form

# dplyr equivalents ----
df <- tibble(
  x = c(2,3,1,1,NA),
  y = letters[1:5],
  z = runif(5)
)
df
df |> filter(x>1)

# same as 
df[!is.na(df$x),]
df[!is.na(df$x) & df$x > 1, ]

which(df$x >1,)
df[which(df$x > 1),]

df |> arrange(x, y)
df |> arrange(x)
df[order(df$x,df$y),]

df[1,1] <- 1
df
df |> arrange(x)
df |> arrange(x, y)

df[order(df$x,df$y,decreasing = TRUE),]

df |> select(x, z)
df[, c("x", "z")]

df |> 
  filter(x >1) |> 
  select(y,z)

df |> subset(x>1, c(y,z))

# select a single element ----
tb <- tibble(
  x = 1:4,
  y = c(10, 4,1,21)
)
tb
tb[[1]]
tb[["x"]]
tb$x
tb$z <- tb$x + tb$y
tb

max(diamonds$carat)
levels(diamonds$cut)

# pull() ----
diamonds |> pull(carat) |> max()
diamonds |> pull(cut) |> levels()

# tibbles ----
df <- data.frame(x1=1)
df
df$x
df$x2
df$x1
df$z

tb <- tibble(x1 = 1)
tb$x1
tb$z

# Lists ----
l <- list(
  a = 1:3,
  b = "a string",
  c = pi,
  d = list(-1,-5)
)
l

l[1:2]
str(l[1:2])

l[1]
l[4]

l[[1]]
l[[4]]
l[4]
str(l[4])

df <- tibble(a = 1, b = 2, c = "a", d = "b", e = 4)
df

num_cols <- sapply(df,is.numeric)
num_cols

df[, num_cols] <- lapply(df[, num_cols, drop=FALSE], \(x) x*2)
df

df[, num_cols] <- lapply(df[, num_cols], \(x) x*2)
df

vapply(df, is.numeric, logical(1))


diamonds |> 
  group_by(cut) |> 
  summarize(price = mean(price))

tapply(diamonds$price, diamonds$cut, mean)


# for loops ----

paths <- dir("data", pattern = "student.*.csv", full.names = TRUE)
paths

length(paths)

files <- vector("list", length = length(paths))
files

a <- vector("integer", length = 2)
a

seq_along(paths)

for (i in seq_along(paths)) {
  files [[i]] <- read_csv(paths[[i]])
}
files
paths

do.call(rbind, files)

out <- NULL
for (path in paths) {
  out <- rbind(out, readxl::read_excel(path))
}


# plots ----
hist(diamonds$carat)
plot(diamonds$carat, diamonds$price)
