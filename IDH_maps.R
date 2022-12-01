library(dplyr)
library(RColorBrewer)
library(stringr)
library(ggplot2)
library(maps)
library(tidyverse)
options(scipen = 999) ## To disable scientific notation 
library(readxl)
library(writexl)
idhmap <- read_excel("/Users/lucasordonez/Desktop/base de datos/idhxlsx.xlsx")

world <- map_data("world")

world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot


idhmap <- select(idhmap, region = "Country", "2000", "2010","2019")

idhok <- filter(idhmap, region %in% c("Argentina", "Venezuela (Bolivarian Republic of", "Uruguay", "Paraguay", "Brazil", "Germany", "Canada", "France", "Italy", "Japan", "United Kingdom","United States" ))



idh2000<- select(idhok, "region", "2000") 
idh2010<- select(idhok, "region", "2010")
idh2019<- select(idhok, "region", "2019")


# rename variables
idh2000ok <- idh2000 %>% mutate(region = recode(str_trim(region), "United States" = "USA",
                                              "United Kingdom" = "UK"))

idh2010ok <- idh2010 %>% mutate(region = recode(str_trim(region), "United States" = "USA",
                                                "United Kingdom" = "UK"))

idh2019ok <- idh2019 %>% mutate(region = recode(str_trim(region), "United States" = "USA",
                                                "United Kingdom" = "UK"))



## Make the HDI numeric

idh2000ok$`2000` <- as.numeric(as.character(idh2000ok$`2000`))

idh2010ok$`2010` <- as.numeric(as.character(idh2010ok$`2010`))

idh2019ok$`2019` <- as.numeric(as.character(idh2019ok$`2019`))


## inner join data bases(world and idh)


idh2000map <- inner_join(world, idh2000ok, by = "region")

idh2010map <- inner_join(world, idh2010ok, by = "region")

idh2019map <- inner_join(world, idh2019ok, by = "region")



# plot mapa

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

#### 2000 "Global Human Development Index (HDI)" ####
colnames(idh2000map)[7] <- "HDI"

worldidh2000 <- ggplot(data = idh2000map, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = HDI)) +
  scale_fill_distiller(palette = "Spectral", direction = 1) + # or direction=1
  ggtitle("Global Human Development Index (HDI) G7-Mercosur 2000") + plain

worldidh2000


#### 2010 "Global Human Development Index (HDI)" ####
colnames(idh2010map)[7] <- "HDI"

worldidh2010 <- ggplot(data = idh2010map, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = HDI)) +
  scale_fill_distiller(palette ="Spectral", direction = 1) + # or direction=1
  ggtitle("Global Human Development Index (HDI) G7-Mercosur 2010") + plain

worldidh2010


#### 2000 "Global Human Development Index (HDI)" ####
colnames(idh2019map)[7] <- "HDI"

worldidh2019 <- ggplot(data = idh2019map, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = HDI)) +
  scale_fill_distiller(palette ="Spectral", direction = 1) + # or direction=1
  ggtitle("Global Human Development Index (HDI) G7-Mercosur 2019") + plain

worldidh2019

