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



merc <- read_excel("ECON - 708/econ-708/proyectos/proyecto_final/Proyecto-Final-Datos-main/Bases_ultimas/mercosur.xlsx")


merc <- merc %>% relocate(Year, .after = last_col())                    #Pasa la columna año al ultimo lugar

nodos_merc <- merc %>% select(Reporter)                                     #Creamos los nodos mercosur
nodos_merc <- nodos_merc [!duplicated(nodos_merc), ]

nodos_merc <- mutate(nodos_merc, value = 0)

for (i in 1:11) {
  
  nodos_merc$value[i] <- sum(merc[merc$Reporter == nodos_merc$Reporter[i],]$`Trade Value (US$)`)
}

mutate(nodos_merc, value =
for (i in nodos_merc) {
  
  sum(merc[merc$Reporter == i,]$`Trade Value (US$)`)
  
}
)

merc_net_2000 <- graph_from_data_frame(d= filter (merc,                # Mercosur y G7 año 2000
                                                  Year == "2000" & `Trade Value (US$)` > 1000000 ),
                                       vertices=nodos_merc, directed=T)



merc_net_2000 <- ggnetwork(merc_net_2000)                                 # Transofrmamos la net en un objeto tipo gg network



merc_net_2000$country<-countrycode(merc_net_2000$name,                    # Agregamos los codigos de paises para la visaulización con ggflags
                                   "country.name","iso2c")
merc_net_2000$country <- merc_net_2000$country %>% tolower()


ggplot(merc_net_2000, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_node_point(aes(size = value)) +
  geom_edges(color="grey") +
  geom_flag(aes(size = value)) +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  theme_blank()















