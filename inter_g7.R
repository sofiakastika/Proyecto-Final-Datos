#Inter G7 
library(tidyverse)
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

inter_g7 <- read_excel("C:/Users/sofi1/OneDrive/Escritorio/Económicas/Segundo Cuatri 2022/Datos/Curso/econ-708/proyectos/proyecto_final/Bases/g7_mercosur.xlsx")
inter_g7 <- filter(inter_g7, Partner %in% c("Argentina","Brazil", "Uruguay", "Paraguay", "Venezuela"))
inter_g7 <- inter_g7 %>% relocate(Year, .after = last_col())                    

#Generamos los nodos 
inter_g7 <- inter_g7 %>% relocate(Year, .after = last_col())  


nodos_g7 <- inter_g7 %>% select(Reporter)                           
nodos_g7 <- nodos_g7 [!duplicated(nodos_g7), ]

nodos_mercosur <- inter_g7 %>% select(Partner)                                        #Creamos los nodos G7
nodos_mercosur <- nodos_mercosur [!duplicated(nodos_mercosur), ]
colnames(nodos_mercosur)[1] <-  "Reporter"

nodos <- rbind(nodos_g7, nodos_mercosur)                                            #Creamos los nodos de toda la red


# Creamos las redes
#2000
g7_merc_net_2000 <- graph_from_data_frame(d= filter (inter_g7,              
                                                     Year == "2000"),
                                          vertices=nodos, directed=T)

g7_merc_net_2000 <- ggnetwork(g7_merc_net_2000)                                 

#2010
g7_merc_net_2010 <- graph_from_data_frame(d= filter (inter_g7,              
                                                     Year == "2010"),
                                          vertices=nodos, directed=T)

g7_merc_net_2010 <- ggnetwork(g7_merc_net_2010) 

#2019
g7_merc_net_2019 <- graph_from_data_frame(d= filter (inter_g7,              
                                                     Year == "2019"),
                                          vertices=nodos, directed=T)

g7_merc_net_2019 <- ggnetwork(g7_merc_net_2019)                                 


# Codigos de paises
#2000
g7_merc_net_2000$country<-countrycode(g7_merc_net_2000$name,                    
                                      "country.name","iso2c")
g7_merc_net_2000$country <- g7_merc_net_2000$country %>% tolower()


#2010
g7_merc_net_2010$country<-countrycode(g7_merc_net_2010$name,                    
                                      "country.name","iso2c")
g7_merc_net_2010$country <- g7_merc_net_2010$country %>% tolower()

#2019
g7_merc_net_2019$country<-countrycode(g7_merc_net_2019$name,                    
                                      "country.name","iso2c")
g7_merc_net_2019$country <- g7_merc_net_2019$country %>% tolower()


#Visualización
ggplot(g7_merc_net_2000, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_edges(color="grey", curvature = 10) +    # cHEQUEAR OPCIONES CURVATURA
  geom_flag() +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  theme_blank() + ggtitle ("HOLA") + plain 

geom_node_point()

#2010
ggplot(g7_merc_net_2010, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_edges(color="grey") +
  geom_flag() +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  theme_blank()

#2019
ggplot(g7_merc_net_2019, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_edges(color="grey") +
  geom_flag() +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  theme_blank()
