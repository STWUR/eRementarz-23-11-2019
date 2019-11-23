library(ggplot2)
library(dplyr)
# library(readr)
# dat <- read_csv("./dane/heart.csv")
# class(dat)
# 
# ggplot(data = dat, aes(x = age, y = sex)) +
#   geom_point()
# 
# ggplot(data = dat, aes(x = sex, y = age)) +
#   geom_boxplot()

head(diamonds)

ggplot(diamonds, aes(x = cut)) +
  geom_bar()

ggplot(diamonds, aes(x = cut)) +
  geom_bar() +
  geom_label(aes(label = ..count..), stat = "count", color = "red")

ggplot(diamonds, aes(x = cut, fill= color)) +
  geom_bar(position = "dodge") 

ggplot(diamonds, aes(x = cut, fill= color)) +
  geom_bar(position = position_dodge(width = 1)) +
  geom_label(aes(label = ..count..), stat = "count", color = "red",
             position = position_dodge(width = 1))

ggplot(diamonds, aes(x = cut)) +
  geom_bar() +
  facet_wrap(~ color, labeller = label_both)

ggplot(diamonds, aes(x = price)) +
  geom_histogram()

ggplot(diamonds, aes(x = price)) +
  geom_density()

ggplot(diamonds, aes(x = price)) +
  geom_density() +
  facet_wrap(~ cut, labeller = label_both)

ggplot(diamonds, aes(x = price)) +
  geom_density() +
  facet_wrap(~ cut + color, labeller = label_both)

ggplot(diamonds, aes(x = price)) +
  geom_density() +
  facet_grid(color ~ cut, labeller = label_both)

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

# filter(diamonds, cut == "Ideal") %>% 
#   ggplot(aes(x = carat, y = price)) +
#   geom_point() +
#   geom_smooth(method = "lm")


ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ cut)

p_dens <- ggplot(diamonds, aes(x = price)) +
  geom_density()
p_dens

p_dens + 
  facet_wrap(~ color)

p_dens + 
  theme_bw()

p_dens + 
  geom_density(aes(fill = cut))

png("obrazek.png")
p_dens
dev.off()

pdf("obrazek.pdf")
ggplot(diamonds, aes(x = price)) +
  geom_density() +
  theme_bw()
dev.off()
