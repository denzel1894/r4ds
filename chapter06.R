library(tidyverse)
usethis::use_blank_slate()


ggplot(diamonds, aes(x = carat, y = price))+
  geom_hex()
ggsave("diamonds.png")
write_csv(diamonds, "data/diamonds.csv")
