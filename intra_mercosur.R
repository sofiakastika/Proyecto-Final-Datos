#Intra Mercosur 

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


#Importamos datos
#mercosur_intra <- read_excel("econ-708/proyectos/proyecto_final/Bases/mercosur.xlsx")

#Para importar Sofi RStudio
mercosur_intra <- read_excel("C:/Users/sofi1/OneDrive/Escritorio/Económicas/Segundo Cuatri 2022/Datos/Curso/econ-708/proyectos/proyecto_final/Bases/mercosur.xlsx")

#Filtramos los países de Mercosur (tanto para Reporter como para Partner)

mercosur_intra1 <- filter(mercosur_intra, Reporter %in% c("Argentina","Brazil", "Uruguay", "Paraguay", "Venezuela"))
mercosur_intra2 <- filter(mercosur_intra1, Partner %in% c("Argentina","Brazil", "Uruguay", "Paraguay", "Venezuela"))
mercosur_intra2 <- filter(mercosur_intra2, 'Trade Value (US$)' >= 1000000)
  
view(mercosur_intra2)
mercosur_intra2 <- mercosur_intra2 %>% relocate(Year, .after = last_col())

View(mercosur_intra2)

#Creamos los nodos mercosur y g7 (y eliminamos duplicados)

nodos_merc <- mercosur_intra2 %>% select(Reporter)                       
nodos_merc <- nodos_merc [!duplicated(nodos_merc), ]

nodos_merc1 <- mercosur_intra2 %>% select(Partner)   
nodos_merc1 <- nodos_merc1 [!duplicated(nodos_merc1), ]
colnames(nodos_merc1)[1] <-  "Reporter"
                                        

#Valor total de exportaciones 
nodos_merc <- mutate(nodos_merc, value = 0)
for (i in 1:5) {
  
  nodos_merc$value[i] <- sum(mercosur_intra2[mercosur_intra2$Reporter == nodos_merc$Reporter[i], ]$`Trade Value (US$)`)  
}


#Creamos la red
#2000
intra_merc_2000 <- graph_from_data_frame(d= filter (mercosur_intra2,                
                                                     Year == "2000" & `Trade Value (US$)` >= 1000000),
                                          vertices=nodos_merc, directed=T)

intra_merc_2000 <- ggnetwork(intra_merc_2000)  

#2010
intra_merc_2010 <- graph_from_data_frame(d= filter (mercosur_intra2,                
                                                    Year == "2010" & `Trade Value (US$)` >= 1000000),
                                         vertices=nodos_merc, directed=T)

intra_merc_2010 <- ggnetwork(intra_merc_2010)  

#2019
intra_merc_2019 <- graph_from_data_frame(d= filter (mercosur_intra2,                
                                                    Year == "2019" & `Trade Value (US$)` >= 1000000),
                                         vertices=nodos_merc, directed=T)

intra_merc_2019 <- ggnetwork(intra_merc_2019)  


# Codigos de paises (ggflags)

intra_merc_2000$country<-countrycode(intra_merc_2000$name,"country.name","iso2c")
intra_merc_2000$country <- intra_merc_2000$country %>% tolower()

intra_merc_2010$country<-countrycode(intra_merc_2010$name,"country.name","iso2c")
intra_merc_2010$country <- intra_merc_2010$country %>% tolower()

intra_merc_2019$country<-countrycode(intra_merc_2019$name,"country.name","iso2c")
intra_merc_2019$country <- intra_merc_2019$country %>% tolower()


#Visualización


#Visualización
#2000
ggplot(intra_merc_2000, 
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
ggplot(intra_merc_2010, 
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
ggplot(intra_merc_2019, 
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








