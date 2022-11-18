#Proyecto
library(readxl)
library(tidyverse)

Nodos_Mundo
intra_g7 <- read_excel("econ-708/proyectos/intrabloques g7.xlsx")
intra_m <- read_excel("econ-708/proyectos/intrabloques mercosur.xlsx")
Nodos_Mundo <- read_excel ("econ-708/proyectos/Nodos Mundo.xlsx")
Nodos_g7 <- read_excel ("econ-708/proyectos/Nodos g7.xlsx")
Nodos_mercosur <- read_excel ("econ-708/proyectos/Nodos mercosur.xlsx")
inter_bloq <- read_excel ("econ-708/proyectos/Base de datos exportaciones e importaciones.xltx")

#Depuramos los datasets
colnames(intra_m)[11] <- "Value"
intra_m = subset(intra_m, select = c(Exporter, Importer, Resource, Year, Value))
intra_m <- subset(intra_m, Exporter != Importer) #Quitamos "auto exportaciones"

colnames(inter_bloq)[11] <- "Value"
inter_bloq = subset(inter_bloq, select = c(Exporter, Importer, Resource, Year, Value))
inter_bloq <- subset(inter_bloq, Exporter != Importer) #Quitamos "auto exportaciones"

colnames(intra_g7)[11] <- "Value"
intra_g7 = subset(intra_g7, select = c(Exporter, Importer, Resource, Year, Value))
intra_g7 <- subset(intra_g7, Exporter != Importer)

Nodos_Mundo$Exporter <- tolower(Nodos_Mundo$Exporter) #Esto es pq la base de datos con lat y long esta en minuscula
intra_g7$Exporter <- tolower(intra_g7$Exporter)
intra_g7$Importer <- tolower(intra_g7$Importer)

#Instalamos y cargamos paquetes para redes y mapas
library(igraph)
library(network)

# Creamos las redes intra g7  ---------------------------------------------
#g7 2000
intra_g7_2000 = filter(intra_g7, Year=="2000")
net <- graph_from_data_frame(d=intra_g7_2000, vertices=Nodos_g7, directed=T) 
net
plot(net)

#g7 2010
intra_g7_2010 = filter(intra_g7, Year=="2010") 
net <- graph_from_data_frame(d=intra_g7_2010, vertices=Nodos_g7, directed=T) 
net
plot(net)

#g7 2019
intra_g7_2019 = filter(intra_g7, Year=="2019") 
net <- graph_from_data_frame(d=intra_g7_2019, vertices=Nodos_g7, directed=T) 
net
plot(net)

# Creamos las redes intra Mercosur ----------------------------------------
#Mercosur 2000
intra_m_2000 = filter(intra_m, Year=="2000")
net <- graph_from_data_frame(d=intra_m_2000, vertices=Nodos_mercosur, directed=T) 
net
plot(net)

#Mercosur 2010
intra_m_2010 = filter(intra_m, Year=="2010")
net <- graph_from_data_frame(d=intra_m_2010, vertices=Nodos_mercosur, directed=T) 
net
plot(net)

intra_m_2019 = filter(intra_m, Year=="2019")
net <- graph_from_data_frame(d=intra_m_2019, vertices=Nodos_mercosur, directed=T) 
net
plot(net)
























