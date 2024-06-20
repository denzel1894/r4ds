library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)

# labs() ----
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    color = "Car type",
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )

?plotmath

df <- tibble(
  x = 1:10,
  y = cumsum(x^2)
)
df

# quote
df |>
  ggplot(aes(x, y)) +
  geom_point() +
  labs(
    x = quote(x[i]),
    y = quote(sum(x[i]^2, i == 1, n))
  )

# geom_text() ----

label_info <- mpg |>
  group_by(drv)
label_info
label_info <- mpg |>
  group_by(drv) |>
  arrange(displ)
label_info
mpg |>
  group_by(drv) |>
  arrange(desc(displ))

mpg |>
  group_by(drv) |>
  arrange(desc(displ)) |>
  slice_head(n = 1)

# case_when() ----
label_info <- mpg |>
  group_by(drv) |>
  arrange(desc(displ)) |>
  slice_head(n = 1) |>
  mutate(
    drive_type = case_when(
      drv == "f" ~ "front-wheel drive",
      drv == "4" ~ "4-wheel drive",
      drv == "r" ~ "rear-wheel drive"
    )
  ) |>
  select(displ, hwy, drv, drive_type)
label_info

mpg |>
  ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3, position = "jitter") +
  geom_smooth(se = FALSE) +
  geom_text(
    data = label_info,
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold",
    size = 5,
    hjust = "right",
    vjust = "bottom"
  ) +
  theme(legend.position = "none")

mpg |>
  ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3, position = "jitter") +
  geom_smooth(se = FALSE) +
  geom_text(
    data = label_info,
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold",
    size = 5
  ) +
  theme(legend.position = "none")

# geom_label_repel() ----
mpg |>
  ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3, position = "jitter") +
  geom_smooth(se = FALSE) +
  geom_label_repel(
    data = label_info,
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold",
    size = 5
  ) +
  theme(legend.position = "none")

mpg |>
  ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3, position = "jitter") +
  geom_smooth(se = FALSE) +
  geom_label_repel(
    data = label_info,
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold",
    size = 5,
    nudge_y = 5
  ) +
  theme(legend.position = "none")

mpg |>
  ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3, position = "jitter") +
  geom_smooth(se = FALSE) +
  geom_label_repel(
    data = label_info,
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold",
    size = 5,
    nudge_y = 5,
    nudge_x = -2
  ) +
  theme(legend.position = "none")

potential_outliers <- mpg |>
  filter(hwy > 40 | (hwy > 20 & displ > 5))
potential_outliers |>
  select(hwy, displ)

# geom_point(), shape, circle open ----
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_text_repel(
    data = potential_outliers,
    aes(label = model)
  ) +
  geom_point(
    data = potential_outliers,
    color = "red"
  ) +
  geom_point(
    data = potential_outliers,
    color = "red",
    size = 8,
    shape = "circle open"
  ) +
  geom_hline(yintercept = 13, linewidth = 2, color = "white") +
  geom_vline(xintercept = 2.3, linewidth = 3, color = "blue")


# geom_vline(), geom_hline(), geom_rect() ----
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_rect(xmin = 3, xmax = 5, ymin = 20, ymax = 30) +
  geom_hline(yintercept = 13, linewidth = 2, color = "white") +
  geom_vline(xintercept = 2.3, linewidth = 3, color = "blue") +
  geom_point() +
  geom_text_repel(
    data = potential_outliers,
    aes(label = model),
  ) +
  geom_point(
    data = potential_outliers,
    color = "red"
  ) +
  geom_point(
    data = potential_outliers,
    color = "red",
    size = 4,
    shape = "circle open"
  )

# ggforce package using, geom_mark_hull() ----
library(ggforce)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_text_repel(
    data = potential_outliers,
    aes(label = model)
  ) +
  geom_point(
    data = potential_outliers,
    color = "red"
  ) +
  geom_point(
    data = potential_outliers,
    color = "red",
    size = 8,
    shape = "circle open"
  ) +
  geom_mark_hull(x = 3, y = 20)

# geom_segment(), with the arrow ----
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_text_repel(
    data = potential_outliers,
    aes(label = model)
  ) +
  geom_point(
    data = potential_outliers,
    color = "red"
  ) +
  geom_point(
    data = potential_outliers,
    color = "red",
    size = 8,
    shape = "circle open"
  ) +
  geom_segment(
    aes(x = 1, y = 2, xend = 3, yend = 8),
    arrow = arrow(length = unit(0.8, "cm"))
  )

# annotate() ----
trend_text <- "Larger engine sizes tend to have lower fuel economy." |>
  str_wrap(width = 30)
trend_text

mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  annotate(
    geom = "label", x = 3.5, y = 38,
    label = trend_text,
    hjust = "left",
    color = "red"
  )

# annotate() ----
mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  annotate(
    geom = "label", x = 3.5, y = 38,
    label = trend_text,
    color = "red"
  ) +
  annotate(
    geom = "segment",
    x = 3, y = 35, xend = 5, yend = 25, color = "red",
    arrow = arrow(type = "closed")
  ) +
  annotate(
    geom = "segment",
    x = 3, y = 35, xend = 6, yend = 40, color = "blue",
    arrow = arrow(type = "open")
  )

# 11.4 scales ----
mpg |> ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()

# breaks ----
mpg |> ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by=5))

mpg |> ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  scale_x_continuous(breaks = seq(3, 6, by=1.5))
mpg |> ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 7, by=1))

# labels ----
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  scale_color_discrete(labels = c("4" = "4-wheel", "f" = "front", "r" = "rear"))

# label_dollar() ----
ggplot(diamonds, aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05) +
  scale_x_continuous(labels = label_dollar())

ggplot(diamonds, aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05) +
  scale_x_continuous(
    labels = label_dollar(scale = 1/1000, suffix = "K"), 
    breaks = seq(1000, 19000, by = 6000)
  )

# label_percent() ----
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Percentage")
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Percentage", labels = label_percent())
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar() +
  scale_y_continuous(name = "coun t")

# scale_x_date
presidential |> 
  mutate( id = 33 +row_number()) |> 
  ggplot(aes(x = start, y = end)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = end))

presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id))+
  scale_x_date(
    name = NULL, 
    breaks = presidential$start,
    date_labels = "'%y")

# legend layout
base <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class))
base

base + theme(legend.position = "right") # the default
base + theme(legend.position = "left")
base + 
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 3))
base + 
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 3))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 4)))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 4, override.aes = list(size = 8)))

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv))

# colorbrewer
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  scale_color_brewer(palette = "Set1")

presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(values = c(Republican = "#E81B23", Democratic = "#00AEF3"))

# coord_fixed() ----
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
df
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  labs(title = "Default, continuous", x = NULL, y = NULL)
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  scale_fill_viridis_c() +
  labs(title = "Viridis, continuous", x = NULL, y = NULL)
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  scale_fill_viridis_b() +
  labs(title = "Viridis, binned", x = NULL, y = NULL)
# error
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  scale_fill_viridis_d() +
  labs(title = "Viridis, binned", x = NULL, y = NULL)
# ensures that the ranges of axes are equal to the specified ratio by
# adjusting the plot aspect ratio

p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p + coord_fixed(ratio = 1)
p + coord_fixed(ratio = 5)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth()

mpg |>
  filter(displ >= 5 & displ <= 6 & hwy >= 10 & hwy <= 25) |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth()

# reducing the limits is equivalent to subsetting the data
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth() +
  scale_x_continuous(limits = c(5, 6)) +
  scale_y_continuous(limits = c(10, 25))

# Zoom
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 6), ylim = c(10, 25))

# ----

suv <- mpg |> filter(class == "suv")
compact <- mpg |> filter(class == "compact")

ggplot(suv, aes(x = displ, y = hwy, color = drv)) +
  geom_point()

ggplot(compact, aes(x = displ, y = hwy, color = drv)) +
  geom_point()

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))

# Left
ggplot(suv, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

# Right
ggplot(compact, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

# themes ----
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()

library(ggthemes)
# themes ----
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_classic()

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  labs(
    title = "Larger engine sizes tend to have lower fuel economy",
    caption = "Source: https://fueleconomy.gov."
  ) +
  theme(
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal",
    legend.box.background = element_rect(color = "black"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )
# ggthemes package ----
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  ggthemes::theme_base()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  ggthemes::theme_clean()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  ggthemes::theme_excel()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  ggthemes::theme_solid()

# layout ----
p1 <- ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy)) + 
  geom_boxplot() + 
  labs(title = "Plot 2")
p1 + p2

p3 <- ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 3")
(p1 | p3) / p2


# patchwork package using
p1 <- ggplot(mpg, aes(x = drv, y = cty, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 2")
p3 <- ggplot(mpg, aes(x = cty, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 3")
p4 <- ggplot(mpg, aes(x = hwy, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 4")
p5 <- ggplot(mpg, aes(x = cty, y = hwy, color = drv)) + 
  geom_point(show.legend = FALSE) + 
  facet_wrap(~drv) +
  labs(title = "Plot 5")

(guide_area() /(p1+p2)/(p3+p4)/p5) +
  plot_annotation(
    title = "City and highway mileage for cars with different drive trains",
    caption = "Source: https://fueleconomy.gov."
  ) +
  plot_layout(
    guides = "collect",
    heights = c(1,3,5,7)
  ) &
  theme(legend.position = "top")
