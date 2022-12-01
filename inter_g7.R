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

#Valor total de las exportaciones 
nodos_mercosur <- mutate(nodos_mercosur, value = 0)
for (i in 1:5) {
  
  nodos_mercosur$value[i] <- sum(inter_g7[inter_g7$Partner == nodos_mercosur$Reporter[i], ]$`Trade Value (US$)`)  
}

nodos_g7 <- mutate(nodos_g7, value = 0)
for (i in 1:7) {
  
  nodos_g7$value[i] <- sum(inter_g7[inter_g7$Reporter == nodos_g7$Reporter[i], ]$`Trade Value (US$)`)  
}

nodos <- rbind(nodos_g7, nodos_mercosur)                                            #Creamos los nodos de toda la red

# Creamos las redes
#2000
g7_merc_net_2000 <- graph_from_data_frame(d= filter (inter_g7,              
                                                     Year == "2000" & `Trade Value (US$)` >= 1000000),
                                          vertices=nodos, directed=T)

g7_merc_net_2000 <- ggnetwork(g7_merc_net_2000)                                 

#2010
g7_merc_net_2010 <- graph_from_data_frame(d= filter (inter_g7,              
                                                     Year == "2010" & `Trade Value (US$)` >= 1000000),
                                          vertices=nodos, directed=T)

g7_merc_net_2010 <- ggnetwork(g7_merc_net_2010) 

#2019
g7_merc_net_2019 <- graph_from_data_frame(d= filter (inter_g7,              
                                                     Year == "2019" & `Trade Value (US$)` >= 1000000),
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
#2000
ggplot(g7_merc_net_2000, 
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

#2010
ggplot(g7_merc_net_2010, 
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

#2019
ggplot(g7_merc_net_2019, 
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
