# spatiotemporal plot

# load sample data:

load('data/sampledata.Rdata')

d <- d2 #make a copy

## filter by TagID, DateTimeUTC, Station, slice the first detection at each station for each fish on each day of detection

dd <- d %>%
  arrange(DateTimeUTC) %>% 
  group_by(TagID, year, julianday, Rkm) %>% 
  filter(row_number() == 1) %>% 
  ungroup()
dd

#create df of tagids and sp
csp <- dd %>% 
  select(Sp, TagID) %>% 
  filter(!duplicated(TagID))

csp %>% group_by(Sp) %>% summarise(count = n()) # need a vector of colors from 1-129 for Chn and 1-92 for wst
csp <- arrange(csp, Sp)


## Working Plot ##
library(ggplot2)
#create color palettes:
# colfunc <- colorRampPalette(c("#d8b365", "#003c30"))
# wstcolors <- colfunc(92)
# colfunc <- colorRampPalette(c("#e9a3c9", "#276419"))
# chncolors <- colfunc(129)


### Manual color scales
wstcolors <- c("#edf8b1","#7fcdbb","#2c7fb8",
               "#ffffcc","#a1dab4","#41b6c4","#225ea8",
               "#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494",
               "#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494",
               "#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84",
               "#ffffd9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84",
               "#ffffd9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58", "#ece2f0","#a6bddb","#1c9099",
               "#f6eff7","#bdc9e1","#67a9cf","#1c9099","#016c59",
               "#f6eff7","#d0d1e6","#a6bddb","#67a9cf","#1c9099","#016c59",
               "#f6eff7","#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016450",
               "#fff7fb","#ece2f0","#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016450",
               "#fff7fb","#ece2f0","#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636", "#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8", "#f6eff7","#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a", "#0c2c84")

chncolors <- c("#ffeda0","#feb24c","#f03b20",
               "#ffffb2","#fecc5c","#fd8d3c","#e31a1c",
               "#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026",
               "#ffffb2","#fed976","#feb24c","#fd8d3c","#f03b20","#bd0026",
               "#ffffb2","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026",
               "#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026",
               "#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c", "#bd0026","#800026", "#fff7bc","#fec44f","#d95f0e",
               "#ffffd4","#fed98e","#fe9929","#cc4c02",
               "#ffffd4","#fed98e","#fe9929","#d95f0e","#993404",
               "#ffffd4","#fee391","#fec44f","#fe9929","#d95f0e","#993404",
               "#ffffd4","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04",
               "#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04",
               "#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#993404","#662506", "#ffffb2","#fed976","#feb24c","#fd8d3c","#f03b20","#bd0026",
               "#ffffb2","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026",
               "#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026",
               "#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c", "#bd0026","#800026", "#fff7bc","#fec44f","#d95f0e",
               "#ffffd4","#fed98e","#fe9929","#cc4c02",
               "#ffffd4","#fed98e","#fe9929","#d95f0e","#993404", "#ffffb2","#fed976","#feb24c")


fishcolors <- c(chncolors, wstcolors)
csp <- as.data.frame(cbind(csp, fishcolors))
csp <- select(csp, TagID, fishcolors)

dd3 <- inner_join(dd, csp, by = "TagID")
dd3

library(ggthemes)
breaks <- sort(unique(dd$Rkm))
ylabels <- c("106", "114", "121", "135", " ", "136", "142", "144", " ", "148", "159", "161", "165")
g <- ggplot(dd3, aes(x = DateTimeUTC, y = Rkm)) + 
  geom_point(aes(color = fishcolors, size = ndets, shape = Sp), alpha = 0.25) + scale_color_identity() +
  scale_size_continuous(name = 'ndets', range = c(10, 35)) +
  scale_y_continuous(breaks = breaks, limits = c(103, 167), labels = ylabels) +  scale_shape_manual(values = c('0', '|')) +
  theme_fivethirtyeight() +  
  scale_x_datetime(date_breaks = '3 month', date_labels = '%b') + 
  theme(legend.position = 'none') + theme(axis.text = element_text(size = 24))  
g