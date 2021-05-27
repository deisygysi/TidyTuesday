require(tidyr)
require(igraph)
require(dplyr)
require(magrittr)
require(ggplot2)
require(igraph)
library(extrafont)
require(ggraph)

`%ni%` <- Negate(`%in%`)

tuesdata <- tidytuesdayR::tt_load('2021-05-25')
records = tuesdata$records %>%
  filter(type == "Single Lap")
players = tuesdata$drivers

df = players %>% 
  select(player, nation) %>% 
  unique() %>%
  dplyr::inner_join(records, .)


nodes = data.frame(ID = c(df$track, df$player)) %>%
  mutate(class = ifelse(ID %in% df$player, "player", "track"),
         type = ifelse(ID %in% df$player, TRUE, FALSE)) %>%
  unique()

g = df %>%
  group_by(track, player, nation) %>% 
  summarise(, weight = n()) %>%
  mutate(nation = ifelse(is.na(nation), "Other", nation)) %>%
  graph_from_data_frame(., directed = FALSE, vertices = nodes)


V(g)$degree <- strength(g)

library(showtext)
font_add_google(name = "Bungee Shade", family = "bungee-shade") 
showtext_auto()

p = ggraph(g, 'hive', axis = class, sort.by = degree) + 
  geom_edge_diagonal0(aes(width = weight, 
                          color = nation, 
                          edge_alpha = weight), 
  ) + 
  geom_node_point(aes(size = degree, color = class)) + 
  geom_node_text(aes(label = name, filter = degree > 10), 
                 color = '#EBFFFE', 
                 size = 3) + 
  scale_color_manual(values = c("#ff3939",
                                "#00ce00"))+
  scale_edge_width(range = c(0, 2))+
  scale_edge_color_manual(values = paste0("#",c("00ce00","00a525",
                                                "00ada5","00deff",
                                                "40b5ce","808c9c",
                                                "c0636b","e04e52",
                                                "ff3939","ff8431",
                                                "FFA92D","ffce29")))+
  theme_void()+
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold", color = "#EBFFFE", family = "bungee-shade"), 
        plot.subtitle = element_text(face = "italic", color = "#EBFFFE"), 
        axis.title.y = element_text(face = "bold", color = "#EBFFFE"),
        axis.title.x = element_text(face = "bold", color = "#EBFFFE"), 
        plot.background = element_rect(fill = "#001413", color = "#EBFFFE"), 
        panel.grid = element_line(color = NA), 
        legend.text = element_text(color = "#EBFFFE"), 
        plot.caption = element_text(face = "bold", color = "#EBFFFE")
  ) +
  guides(size = FALSE, 
         edge_width = FALSE, 
         edge_alpha = FALSE) + 
  labs(color = NULL,
       edge_color = NULL, 
       x = NULL, 
       y = NULL, 
       title = "Mario Kart 64 Wold Records... and their tracks", 
       subtitle = "Multiple players made records in more than one track.", 
       caption =  "@GysiDeisy | #TidyTuesday \nSource: Mario Kart 64 Wold Records") 

png(filename = "result.png",
    width = 20, 
    height = 15, 
    units = 'cm',
    res = 400)
p
dev.off()

