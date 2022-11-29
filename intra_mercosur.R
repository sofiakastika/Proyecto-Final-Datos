#Intra Mercosur 

install.packages('DT')

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

mercosur_intra <- read_excel("C:/Users/sofi1/OneDrive/Escritorio/Económicas/Segundo Cuatri 2022/Datos/Curso/econ-708/proyectos/proyecto_final/Bases/mercosur.xlsx")

#Selecciono solo los países con de mercosur (tanto para Reporter como para Partner)
mercosur_intra1 <- filter(mercosur_intra, Reporter %in% c("Argentina","Brazil", "Uruguay", "Paraguay", "Venezuela"))
mercosur_intra2 <- filter(mercosur_intra1, Partner %in% c("Argentina","Brazil", "Uruguay", "Paraguay", "Venezuela"))

View(mercosur_intra2)

faculty %>%

#Creamos los nodos mercosur y g7 (y eliminamos duplicados)

nodos_merc <- mercosur_intra2 %>% select(Reporter)                       
nodos_merc <- nodos_merc [!duplicated(nodos_merc), ]

nodos_merc1 <- mercosur_intra2 %>% select(Partner)   
nodos_merc1 <- nodos_merc1 [!duplicated(nodos_merc1), ]
colnames(nodos_merc1)[1] <-  "Reporter"

nodos <- rbind(nodos_merc, nodos_merc1)                                            #Creamos los nodos de toda la red


#Creamos la red

intra_merc_2000 = filter(mercosur_intra2, Year=="2000")
net <- graph_from_data_frame(d=intra_merc_2000, vertices=nodos, directed=T)





