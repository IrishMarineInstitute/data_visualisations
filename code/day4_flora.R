# 30 Day Chart Challenge 2022: Day 4 Flora
# MSFD Biodiversity Species Groups Flower Chart
library(tidyverse)
library(ggforce)
library(rphylopic)

data = data.frame(biodiversity_group=c('Fish', 'Mammals', 'Birds', 'Reptiles', 'Cephalapods'),
                  percent=rep(100/5, 5),
                  phylopic = c("1cc605fb-013f-4997-b88c-6d2c71952e8d", # fish
                               "ce70490a-79a5-47fc-afb9-834e45803ab4", # humpback
                               "c91c655d-a2d6-485d-8702-b3143e1eaf81", # heron
                               "1c65c811-4caa-4be3-9936-7a16e6905131", # turtle
                               "d115b40d-efe3-487e-bed1-4f6aace5c814"  # octopus
                  )
)

fish_pic = image_data(data$phylopic[1], size=256)[[1]]
mammal_pic = image_data(data$phylopic[2], size=256)[[1]]
bird_pic = image_data(data$phylopic[3], size=256)[[1]]
reptile_pic = image_data(data$phylopic[4], size=256)[[1]]
octopus_pic = image_data(data$phylopic[5], size=256)[[1]]

plot_data = data %>%
  mutate(angle = seq(0, 290, length.out=5),
         radians = angle*pi/180,
         x0 = percent * cos(radians),
         y0 = percent * sin(radians))


ggplot(plot_data) +
  geom_ellipse(aes(x0=x0, y0=y0, a=percent, b=percent/3, angle=radians,
                   fill=biodiversity_group),
               alpha=0.8,
               size=0,
               colour='white') +
  add_phylopic(fish_pic, x=32.5, y=0, alpha=1, color='white', ysize=7) +
  add_phylopic(mammal_pic, x=8.5, y=28.5, alpha=1, color='white', ysize=4.25) +
  add_phylopic(bird_pic, x=-25.5, y=17.5, alpha=1, color='white', ysize=10) +
  add_phylopic(reptile_pic, x=-25.5, y=-20.5, alpha=1, color='grey30', ysize=7) +
  add_phylopic(octopus_pic, x=11, y=-30.5, alpha=1, color='white', ysize=10) +
  geom_text(aes(label=biodiversity_group, x=x0, y=y0),
            fontface='bold',
            colour=ifelse(plot_data$biodiversity_group=='Reptiles', 'grey30', 'white'),
            size=4) +
  scale_fill_viridis_d(option='plasma') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_equal() +
  theme_void() +
  labs(title='Marine Strategy Framework Directive: D1 Biodiversity',
       subtitle = 'Species Groups assessed under MSFD Descriptor 1') +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=13),
        legend.position = 'none')
ggsave('./flora_day4_msfd.png')