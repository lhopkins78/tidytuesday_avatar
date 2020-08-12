library(tidyverse)
library(tidytuesdayR)
library(ggthemes)
library(cowplot)
library(magick)
library(ggrepel)

tuesdata <- tidytuesdayR::tt_load('2020-08-11')
tuesdata <- tidytuesdayR::tt_load(2020, week = 33)

avatar <- tuesdata$avatar

avatar_books <- avatar %>% group_by(book, chapter, imdb_rating, director, writer) %>% summarise(n=n())

avatar_analyse <- avatar_books %>% group_by(book) %>% 
  summarise(imdb_rating=mean(imdb_rating, na.rm=T), count=n()) %>%
  arrange(desc(imdb_rating))

director_rank <- avatar_books %>% group_by(director) %>% 
  summarise(median_imdb=median(imdb_rating, na.rm=T)) %>%
  arrange(median_imdb)

p1 <- ggplot(avatar_books, aes(y=imdb_rating, x=factor(director, levels=director_rank$director),  col=book)) + 
  geom_boxplot(col="grey81", alpha=0.2) + 
  geom_point(shape=15,alpha=0.5, size=7, position=position_dodge(width=0.3)) + 
  geom_text_repel(data=avatar_books %>% 
              filter(imdb_rating>9.5, book=="Fire"), aes(label=chapter, y=imdb_rating, 
                                           x=factor(director, levels=director_rank$director)),
            col="black", nudge_y = -0.6, nudge_x=0.3, family="Avenir", size=5) +
  geom_text(data=avatar_books %>% 
              filter(imdb_rating>9.5, book=="Earth"), aes(label=chapter, y=imdb_rating, 
                                                         x=factor(director, levels=director_rank$director)),
            col="black", nudge_y = -0.3, nudge_x=0.3, family="Avenir", size=5) +
  geom_text(data=avatar_books %>% 
              filter(imdb_rating<7.7), aes(label=chapter, y=imdb_rating, 
                                                          x=factor(director, levels=director_rank$director)),
            col="black", nudge_y = 0.1, nudge_x=0.3, family="Avenir", size=5) +
  coord_flip() + theme_few() +
  theme(text= element_text(family="Avenir", size=18),
        plot.title= element_text(family="Marker Felt Thin", size=30),
        plot.background = element_rect(fill="grey91")) +
  labs(x="Director", y="IMDB rating", col="Book",
       title="The Last Air Bendirectors",
       subtitle="Ranking Avatar directors by IMDB scores") +
  scale_color_manual(values=c("#06c258","#ff5f0d","#3eacec"))

img_avatar <- "https://guriguriblog.files.wordpress.com/2009/01/avatar-logo2.png"
img_ava <- "https://www.liveabout.com/thmb/7LQKAPcHZ1oWoZuxEn1rsBYryMc=/768x0/filters:no_upscale():max_bytes(150000):strip_icc()/Avatar_12_HR_01-56a00ca93df78cafda9fd17c.jpg"

ggdraw() +
  draw_plot(p1) +
  draw_image(img_avatar, scale=0.2, y=0.30, x=-0.22) +
  draw_image(img_ava, scale=0.17, y=-0.35, x=0.3)
ggsave("avatar.png", dpi="retina", width=18, height=11.5)

book_levels <- c("Water","Earth", "Fire")

p2 <- ggplot(avatar_books, aes(imdb_rating, fill=factor(book, levels=book_levels))) + 
  geom_density(alpha=0.9) +
  geom_vline(data=avatar_analyse, aes(xintercept=imdb_rating)) +
  theme_few() +
  theme(text= element_text(family="Avenir", size=25),
        plot.title= element_text(family="Marker Felt Thin", size=30),
        plot.caption= element_text(size=10),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        plot.margin = unit(c(5.5,1,1,1), "cm"),
        legend.position = "none") +
  labs(x="IMDB rating", y="Density", fill="Book",
       title="Distribution of IMDB scores by book",
       caption="Source: Avery Robbins (appa package). Visualisation: @lauriejhopkins") +
  scale_fill_manual(values=c("#3eacec","#06c258","#ff5f0d")) + 
  facet_wrap(~factor(book, levels=book_levels), ncol=1)

ggdraw() +
  draw_plot(p2) +
  draw_image(img_avatar, scale=0.25, y=0.4, x=0.3) +
  draw_image(img_ava, scale=0.12, y=-0.3, x=-0.35)
ggsave("avatar2.png", dpi="retina", width=18, height=11.5)
