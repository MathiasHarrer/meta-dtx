# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   DTX FOR COMMON MENTAL DISORDERS                                           #
#   2024-11-08, MH                                                            #
#   01. Global Analysis ----
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(tidyverse)
library(xlsx)
library(metapsyTools)
library(maps)
library(paletteer)
library(cowplot)
library(gt)
library(sjPlot)
library(zoo)
library(caret)
library(metafor)
library(emmeans)
library(RANN)


## World Maps ------------------------------------------------------------------

# Load world map data
world = map_data("world")

# Check countries not matched by worldmap
setdiff(dat.country$country_coded, world$region)

# Rename to fit 'world' object
dat.country %>% 
  mutate(country_coded = 
           recode(country_coded, 
                  "United Kingdom" = "UK",
                  "Saudia Arabia" = "Saudi Arabia")
) -> dat.country

# Create world plot data
dat.country %>% 
  group_by(country_coded) %>% 
  dplyr::summarise(k = n()) %>% 
  dplyr::rename(region = country_coded) %>% 
  full_join(world, "region") -> dat.plt

# Define theme for world map
plain = theme(
  text = element_text(family="Roboto Slab"),
  legend.title = element_blank(),
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0)
)

# Plot 1: Trials per country
dat.plt %>% 
  filter(region != "Antarctica") %>% 
  ggplot(mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = k), color = "black", linewidth = .2) +
  scale_fill_distiller(palette ="Purples", direction = 1, na.value="lightblue") + 
  ggtitle("Number of Digital Intervention Trials") +
  plain -> p1

# Save world map
png("results/world-map.png", 
    3000*10, 1500*10, res = 300*10, bg = "white")
p1; dev.off()


# Trials per country, per capita
# Worldwide population estimates 1960-2017
# Source: World Bank, https://data.worldbank.org/indicator/SP.POP.TOTL
data("countrypops")  
countrypops %>% filter(year == 2017) -> countrypops

# Check countries not matched by countrypops
setdiff(dat.plt %>% filter(k>0) %>% pull(region), 
        countrypops$country_name)

# Adapt country names in countrypop 
countrypops %>% 
  mutate(region = 
           recode(country_name, 
                  "United States" = "USA",
                  "Korea (Republic of)" = "South Korea",
                  "United Kingdom" = "UK",
                  "Iran (Islamic Republic)" = "Iran")) -> countrypops

# Generate data
full_join(dat.plt, countrypops, "region") %>% 
  mutate(k.s = k/(population/1e6)) %>% 
  filter(region != "Antarctica") %>% 
  distinct(region, .keep_all = TRUE) %>% 
  filter(!is.na(k)) %>% 
  arrange(-k.s) %>% 
  select(country_name, k, k.s) %>% 
  write.xlsx("results/trials-by-country.xlsx")
