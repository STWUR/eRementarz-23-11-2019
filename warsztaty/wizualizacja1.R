library(ggplot2)

iris

ggplot(data = iris, 
       mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species, shape = Species)) +
  geom_point(size = 9)

ggplot(data = iris, 
       mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species, shape = Species,
                     size = Sepal.Length)) +
  geom_point()

ggplot(data = iris, 
       mapping = aes(x = Species, y = Petal.Length)) +
  geom_point(position = "jitter") +
  geom_boxplot()

ggplot(data = iris, 
       mapping = aes(x = Species, y = Petal.Length)) +
  geom_boxplot()

ggplot(data = iris, 
       mapping = aes(x = Species, y = Petal.Length)) +
  geom_violin()

ggplot(data = iris, 
       mapping = aes(x = Species)) +
  geom_bar()

library(dplyr)

iris %>% 
  group_by(Species) %>% 
  summarise(mean_petal = mean(Petal.Length)) %>% 
  ggplot(mapping = aes(x = Species, y = mean_petal, fill = Species)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#99d8c9", "pink", "purple")) +
  theme(panel.background = element_rect(fill = NA),
        legend.position = "bottom")

# library(RColorBrewer)
