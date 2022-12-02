#Intra G7
#Cargamos librerías
library(DT)
library(readxl)
library(tidyverse)
library(igraph)
library(ggplot2)
library(ggnetwork)
library(network)
library(ggraph)
library(countrycode)
library(magrittr)
library(ggflags)



# Importamos datos --------------------------------------------------------
#Para importar Fran RStudio
g7_intra <- read_excel("econ-708/proyectos/Proyecto-Final-Datos/Bases/g7.xlsx")



# Creamos los nodos g7 (y eliminamos duplicados) ---------------

nodos_g7 <- g7_intra %>% select(Reporter)                       
nodos_g7 <- nodos_g7 [!duplicated(nodos_g7), ]

g7_intra <- g7_intra %>% relocate(Year, .after = last_col())  



# Armamos nodos para cada año ---------------------------------------------

nodos_g7_2000 <- mutate(nodos_g7, value = 0)
nodos_g7_2010 <- mutate(nodos_g7, value = 0)
nodos_g7_2019 <- mutate(nodos_g7, value = 0)


# Sumamos los value trade asignado a cada nodo/país para cada año analizado-------------------------------------------------

for (i in 1:7) {
  
  nodos_g7_2000$value[i] <- sum(g7_intra[g7_intra$Partner == nodos_g7_2000$Reporter[i] & g7_intra$Year == 2000, ]$`Trade Value (US$)`)  
}

for (i in 1:7) {
  
  nodos_g7_2010$value[i] <- sum(g7_intra[g7_intra$Partner == nodos_g7_2010$Reporter[i] & g7_intra$Year == 2010, ]$`Trade Value (US$)`)  
}

for (i in 1:7) {
  
  nodos_g7_2019$value[i] <- sum(g7_intra[g7_intra$Partner == nodos_g7_2019$Reporter[i] & g7_intra$Year == 2019, ]$`Trade Value (US$)`)  
}

# Creamos la red para transacciones de al menos 1M U$D----------------------------------------------------------


#2000
intra_g7_2000 <- graph_from_data_frame(d= filter (g7_intra,                
                                                    Year == "2000" & "Trade Value (us$)">100000),
                                         vertices=nodos_g7_2000, directed=T)

intra_g7_2000 <- ggnetwork(intra_g7_2000)


#2010
intra_g7_2010 <- graph_from_data_frame(d= filter (g7_intra,                
                                                  Year == "2010" & "Trade Value (us$)">100000),
                                       vertices=nodos_g7_2010, directed=T)

intra_g7_2010 <- ggnetwork(intra_g7_2010)  

#2019
intra_g7_2019 <- graph_from_data_frame(d= filter (g7_intra,                
                                                  Year == "2019" & "Trade Value (us$)">100000),
                                       vertices=nodos_g7_2019, directed=T)

intra_g7_2019 <- ggnetwork(intra_g7_2019)  



# Codigos de paises (ggflags) ---------------------------------------------


intra_g7_2000$country<-countrycode(intra_g7_2000$name,"country.name","iso2c")
intra_g7_2000$country <- intra_g7_2000$country %>% tolower()

intra_g7_2010$country<-countrycode(intra_g7_2010$name,"country.name","iso2c")
intra_g7_2010$country <- intra_g7_2010$country %>% tolower()

intra_g7_2019$country<-countrycode(intra_g7_2019$name,"country.name","iso2c")
intra_g7_2019$country <- intra_g7_2019$country %>% tolower()


# Visualización -----------------------------------------------------------


#Red año 2000

ggplot(intra_g7_2000, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_node_point(aes(size = value), show.legend = F) + 
  geom_edges(color="grey") +
  geom_flag(aes(size = value), show.legend = F) +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  ggtitle("2000") + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),  
        axis.title.y=element_blank()
  ) 
theme_blank()


#Red año 2010

ggplot(intra_g7_2010, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_node_point(aes(size = value), show.legend = F) + 
  geom_edges(color="grey") +
  geom_flag(aes(size = value), show.legend = F) +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  ggtitle("2010") + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),  
        axis.title.y=element_blank()
  ) 
theme_blank()


#Red año 2019

ggplot(intra_g7_2019, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_node_point(aes(size = value), show.legend = F) + 
  geom_edges(color="grey") +
  geom_flag(aes(size = value), show.legend = F) +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  ggtitle("2019") + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),  
        axis.title.y=element_blank()
  ) 
theme_blank()





