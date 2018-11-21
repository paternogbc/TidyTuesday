# Script for Tidy Tuesday: 2018-11-13 
# Malaria deaths for Brasil, Peru, Colômbia e Bolívia

## packages----
library(tidyverse)
library(gganimate)
library(RCurl)
library(RColorBrewer)
library(extrafont)

### Load data----
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-13/malaria_deaths.csv"
df <- read_csv(getURL(url))

glimpse(df)
skimr::skim(df)

### Simplify colnames
dc <- df %>% 
  rename(deaths = colnames(df)[4],
         country = Entity) 
dc %>% head()

### Filter for some countries
d <- dc %>%
  filter(country %in% c("Brazil", "Colombia", "Peru", "Bolivia")) %>%
  select(country, Year, deaths)
head(d)

## Plot-----
# Malaria Deaths/100.000 hab ~ year | by country
g1 <- 
  ggplot(d, aes(y = deaths, x = Year, color = country, label = country)) +
  geom_line(size = 4, alpha = .7) +
  geom_point(size = 5, alpha = .9) + 
  scale_x_continuous(breaks = seq(1990, 2016, 2)) +
  scale_color_manual(values = brewer.pal(n = 11, name = "RdBu")[c(10,1,3,8)], name = "") +
  theme_bw(base_size = 25) +
  theme(panel.grid = element_blank(), legend.position = c(.8,.9),
        legend.background = element_blank(),
        text = element_text(family = "Times New Roman", colour =gray(.2))) + 
  labs(x ="Year", y = "Malaria deaths per 100,000 people", 
       title = "Great Decline in Malaria Deaths",
       caption = "Source: https://ourworldindata.org/malaria")
g1

## Save graph
ggsave(plot = g1, filename = "2018-11-13_malaria/malaria_decline.png", 
       device='png', width = 12, height = 8)

## Animate plot----
options(gganimate.dev_args = list(width = 800, height = 600))
g1a <- g1 + gganimate::transition_reveal(id = country, along = Year)
animate(g1a, nframes = 100)  
anim_save(filename = "2018-11-13_malaria/malaria_decline.gif")
##END---
