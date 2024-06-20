library(tidyverse)
View(mpg)
mpg |>
  ggplot(aes(x = displ, y = hwy, color = class)) +
  geom_point()

mpg |>
  ggplot(aes(x = displ, y = hwy, shape = class)) +
  geom_point() # 6 shape in R

mpg |>
  ggplot(aes(x = displ, y = hwy, size = class)) +
  geom_point()

mpg |>
  ggplot(aes(x = displ, y = hwy, alpha = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue")

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 1)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 5)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(shape = 1, size = 5)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(shape = 24, color = "blue", fill = "red")
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(shape = 2, color = "blue", fill = "red")

# 9.2.1 exercises ----
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = "blue")) # why, outside aes, but red?
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = class))

# Left
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

# Right
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

mpg |>
  ggplot(aes(x = displ, y = hwy, shape = drv)) +
  geom_smooth() # ignore the shape of line
mpg |>
  ggplot(aes(x = displ, y = hwy, linetype = drv)) +
  geom_smooth() # not ignore the linetype of line

mpg |>
  ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(aes(linetype = drv))
mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(aes(linetype = drv))
mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(shape = drv)) +
  geom_smooth(aes(linetype = drv, color = drv))

mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_smooth()
mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_smooth(aes(group = drv))
mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv))
mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv), show.legend = FALSE)

mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

# 定制图层，选取特定数据样式 ----
mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  geom_point(
    data = mpg |> filter(class == "2seater"),
    color = "red"
  )

mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  geom_point(
    data = mpg |> filter(class == "2seater"),
    color = "red"
  ) +
  geom_point(
    data = mpg |> filter(class == "2seater"),
    shape = "circle open",
    size = 3,
    color = "blue"
  )

# Geoms 模块 -----
mpg |>
  ggplot(aes(x = hwy)) +
  geom_histogram(binwidth = 2)
mpg |>
  ggplot(aes(x = hwy)) +
  geom_density()
mpg |>
  ggplot(aes(x = hwy)) +
  geom_boxplot()

# 40 geoms of ggplot2
# look into extension packages first
# eg: ggridges package for ridgeline plots

library(ggridges)
mpg |>
  ggplot(aes(x = hwy, y = drv, fill = drv, color = drv)) +
  geom_density_ridges(alpha = 0.5, show.legend = FALSE)
mpg |>
  ggplot(aes(x = hwy, y = drv, fill = drv, color = drv)) +
  geom_density_ridges(alpha = 0.7, show.legend = TRUE)

# facets ----
mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point()

# facet_wrap()
mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~cyl)

# facet_grid()
mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl)
mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(cyl ~ drv)

mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl, scales = "free_x")

mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~hwy)

# . do
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .) # 变成一列
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl) # 变成一行

mpg |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~cyl)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~class, nrow = 2)
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~class, ncol = 2)

ggplot(mpg, aes(x = displ)) +
  geom_histogram() +
  facet_grid(drv ~ .)
ggplot(mpg, aes(x = displ)) +
  geom_histogram() +
  facet_grid(. ~ drv)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(drv ~ .) # 1 行

# statistical transformations ----
ggplot(diamonds, aes( x = cut)) +
  geom_bar()
?geom_bar

diamonds |> count(cut)
diamonds |> 
  count(cut) |> 
  ggplot(aes(x = cut, y= n)) +
  geom_bar(stat = "identity")

ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()

ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 2)) + 
  geom_bar()
?after_stat
ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = TRUE)) + 
  geom_bar()
ggplot(diamonds, aes(x = cut, y = after_stat(prop))) + 
  geom_bar()
ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 100)) + 
  geom_bar()

ggplot(diamonds, aes(x = cut, y = after_stat(prop))) + 
  geom_bar()
ggplot(diamonds, aes(x = cut, fill = color, y = after_stat(prop))) + 
  geom_bar()
ggplot(diamonds, aes(x = cut, fill = color, y = after_stat(prop)),group=1) + 
  geom_bar()
ggplot(diamonds, aes(x = cut, fill = color, y = after_stat(prop),group=1)) + 
  geom_bar()
ggplot(diamonds, aes(x = cut, y = after_stat(prop),group=1)) + 
  geom_bar()
View(diamonds)

ggplot(mpg, aes(x = drv, color = drv)) + 
  geom_bar()
ggplot(mpg, aes(x = drv, fill = drv)) + 
  geom_bar()
ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar()


ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(alpha = 0.25, position = "identity")
ggplot(mpg, aes(x = drv, color = class)) + 
  geom_bar(fill = NA, position = "identity")

ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "fill")
ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "dodge")

# important !!! ----
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(position = "jitter")
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_jitter()


ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point()
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_jitter()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "identity")

# coordinates ----
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = clarity, fill = clarity), 
    show.legend = FALSE,
    width = 0.7 
  ) + 
  theme(aspect.ratio = 1)

bar

bar + coord_flip()
bar + coord_polar()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point()
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline()
