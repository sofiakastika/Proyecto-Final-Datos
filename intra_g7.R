#Intra G7


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
#mercosur_intra <- read_excel("econ-708/proyectos/proyecto_final/Bases/g7.xlsx")

#Para importar Sofi RStudio
g7_intra <- read_excel("C:/Users/sofi1/OneDrive/Escritorio/Económicas/Segundo Cuatri 2022/Datos/Curso/econ-708/proyectos/proyecto_final/Bases/g7.xlsx")

#Creamos los nodos mercosur y g7 (y eliminamos duplicados)

nodos_g7 <- g7_intra %>% select(Reporter)                       
nodos_g7 <- nodos_g7 [!duplicated(nodos_g7), ]

g7_intra <- g7_intra %>% relocate(Year, .after = last_col())  

#Creamos la red
#2000
intra_g7_2000 <- graph_from_data_frame(d= filter (g7_intra,                
                                                    Year == "2000"),
                                         vertices=nodos_g7, directed=T)

intra_g7_2000 <- ggnetwork(intra_g7_2000)  

#2010
intra_g7_2010 <- graph_from_data_frame(d= filter (g7_intra,                
                                                  Year == "2010"),
                                       vertices=nodos_g7, directed=T)

intra_g7_2010 <- ggnetwork(intra_g7_2010)  

#2019
intra_g7_2019 <- graph_from_data_frame(d= filter (g7_intra,                
                                                  Year == "2019"),
                                       vertices=nodos_g7, directed=T)

intra_g7_2019 <- ggnetwork(intra_g7_2019)  


# Codigos de paises (ggflags)

intra_g7_2000$country<-countrycode(intra_g7_2000$name,"country.name","iso2c")
intra_g7_2000$country <- intra_g7_2000$country %>% tolower()

intra_g7_2010$country<-countrycode(intra_g7_2010$name,"country.name","iso2c")
intra_g7_2010$country <- intra_g7_2010$country %>% tolower()

intra_g7_2019$country<-countrycode(intra_g7_2019$name,"country.name","iso2c")
intra_g7_2019$country <- intra_g7_2019$country %>% tolower()

#Visualización
ggplot(intra_g7_2000, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_edges(color="grey") +
  geom_flag() +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  theme_blank()

ggplot(intra_g7_2010, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_edges(color="grey") +
  geom_flag() +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  theme_blank()

ggplot(intra_g7_2019, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_edges(color="grey") +
  geom_flag() +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  theme_blank()






