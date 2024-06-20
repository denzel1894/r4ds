library(tidyverse)

x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")

sort(x1)

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1
y1 |> str_view()
sort(y1)
y1
y1 |> sort()

y2 <- factor(x2, levels = month_levels)
y2
y2 <- fct(x2, levels = month_levels)


factor(x1)
fct(x1)
levels(y2)
levels(y1)

csv <- "
month,value
Jan,12
Feb,56
Apr,12"

str_view(csv)
df <- read_csv(csv, col_types = cols(month = col_factor(month_levels)))
df$month |> str_view()
df$month

df

# gss_cat data ----
gss_cat

gss_cat |> 
  count(race)

# modifying factor order

relig_summary <- gss_cat |> 
  group_by(relig) |> 
  summarise(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

relig_summary |> 
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()
relig_summary |> 
  ggplot(aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()

relig_reorder <- relig_summary |> 
  mutate(
    relig = fct_reorder(relig, tvhours)
  )

levels(relig_summary$relig)
levels(relig_reorder$relig)

relig_summary |>
  mutate(
    relig = fct_reorder(relig, tvhours)
  ) |>
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()

rincome_summary <- gss_cat |>
  group_by(rincome) |>
  summarize(
    age = mean(age, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) + 
  geom_point()

ggplot(rincome_summary, aes(x = age, y = fct_relevel(rincome, "Not applicable"))) +
  geom_point()
ggplot(rincome_summary, aes(x = age, y = rincome)) +
  geom_point()


gss_cat |> 
  filter(!is.na(age)) |> 
  count(age, marital) |> 
  group_by(age) |> 
  mutate(
    prop = n / sum(n),
    sum = sum(n) # in group sum number
  )
  
by_age <- gss_cat |>
  filter(!is.na(age)) |> 
  count(age, marital) |>
  group_by(age) |>
  mutate(
    prop = n / sum(n)
  )

ggplot(by_age, aes(x = age, y = prop, color = marital)) +
  geom_line(linewidth = 1) + 
  scale_color_brewer(palette = "Set1")

ggplot(by_age, aes(x = age, y = prop, color = fct_reorder2(marital, age, prop))) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") + 
  labs(color = "marital2") 

gss_cat |>
  mutate(marital = marital |> fct_infreq() |> fct_rev()) |>
  ggplot(aes(x = marital)) +
  geom_bar()

# modifying factor levels ----
gss_cat |> count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat"
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat",
                         "Other"                 = "No answer",
                         "Other"                 = "Don't know",
                         "Other"                 = "Other party"
    )
  ) |> 
  count(partyid)

gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
                           "other" = c("No answer", "Don't know", "Other party"),
                           "rep" = c("Strong republican", "Not str republican"),
                           "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  count(partyid)

gss_cat |> count(relig, sort = TRUE)
gss_cat |>
  mutate(relig = fct_lump_lowfreq(relig)) |>
  count(relig)

gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10)) |>
  count(relig, sort = TRUE)

ordered(c("a", "b", "c"))
