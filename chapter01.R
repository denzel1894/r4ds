library(tidyverse)
library(palmerpenguins)
library(ggthemes)

penguins
glimpse(penguins)
View(penguins)

?penguins



ggplot(data = penguins)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()


ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm")

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm")

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species"
  )

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species",
    shape = "Species"
  ) +
  scale_color_colorblind()
#---------
# pipe using
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
penguins %>% ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
penguins |> ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
penguins %>% ggplot(aes(
  x = penguins$flipper_length_mm,
  y = penguins$body_mass_g
)) +
  geom_point()
#-------

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )
#----------

# 1.4 Visualizing distributions
ggplot(penguins, aes(x = species)) +
  geom_bar()

ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

?fct_infreq
example("fct_infreq")

ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red") # should do like this
ggplot(penguins, aes(x = species, fill = "red")) +
  geom_bar() # right?

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "blue")
ggplot(penguins, aes(x = species, fill = "white")) +
  geom_bar() # wrong

# 1.5 Visualizing relationships
ggplot(penguins, aes(x = species, body_mass_g)) +
  geom_boxplot()
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 2)

ggplot(penguins, aes(
  x = penguins$body_mass_g,
  color = penguins$species,
  fill = penguins$species
)) +
  geom_density(alpha = 0.5)
ggplot(penguins, aes(
  x = penguins$body_mass_g,
  color = penguins$species,
  fill = penguins$species
)) +
  geom_density(alpha = 0.8)
ggplot(penguins, aes(
  x = penguins$body_mass_g,
  color = penguins$species,
  fill = penguins$species
)) +
  geom_density(alpha = 0.3)

penguins %>% ggplot() +
  geom_density(aes(
    x = penguins$body_mass_g,
    color = penguins$species,
    fill = penguins$species,
    alpha = 0.5
  ))


penguins %>% ggplot() +
  geom_density(
    aes(
      x = penguins$body_mass_g,
      color = penguins$species,
      fill = penguins$species
    ),
    alpha = 0.5
  )

penguins %>% ggplot(aes(x = island, fill = species)) +
  geom_bar()
penguins %>% ggplot() +
  geom_bar(aes(x = island, fill = species))

penguins %>% ggplot() +
  geom_bar(position = "fill", aes(x = island, fill = species))
penguins %>% ggplot() +
  geom_bar(aes(x = island, fill = species), position = "fill")

penguins %>% ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

penguins %>% ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

# using facets
penguins %>% ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)
#----
mpg
View(mpg)
glimpse(mpg)
?mpg
#----
mpg %>% ggplot(aes(x = hwy, y = displ)) +
  geom_point()
mpg %>% ggplot(aes(x = hwy, y = displ, color = displ)) +
  geom_point()
mpg %>% ggplot(aes(x = hwy, y = displ, size = displ)) +
  geom_point()
mpg %>% ggplot(aes(x = hwy, y = displ, color = displ, size = displ)) +
  geom_point()
mpg %>% ggplot(aes(x = hwy, y = displ, color = displ, size = displ)) +
  geom_point() +
  labs(color = "displ", size = "Displ")
mpg %>% ggplot(aes(x = hwy, y = displ, color = fl, size = displ, shape = fl)) +
  geom_point() +
  scale_color_colorblind()

mpg %>% ggplot(aes(x = hwy, y = displ)) +
  geom_point()
mpg %>% ggplot(aes(x = hwy, y = displ, linewidth = 2)) +
  geom_point()

penguins %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm, color = species)) +
  geom_point() +
  facet_wrap(~species)
penguins %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point() +
  facet_wrap(~species) # no color

ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm,
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species")
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm,
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species", shape = "Species")

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")

# 1.6 saving your plots
ggplot(
  data = penguins,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g
  )
) +
  geom_point()
ggsave(filename = "penguin-plot.pdf")
ggplot(
  data = penguins,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g
  )
) +
  geom_point()
ggsave(filename = "penguin-plot.png")
?ggsave
