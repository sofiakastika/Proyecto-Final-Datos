#Proyecto
library(readxl)
library(tidyverse)


intra_g7 <- read_excel("ECON - 708/econ-708/proyectos/proyecto_final/Proyecto-Final-Datos-main/Bases Marco/intrabloques g7.xlsx")
intra_m <- read_excel("econ-708/proyectos/intrabloques mercosur.xlsx")
Nodos_Mundo <- read_excel ("ECON - 708/econ-708/proyectos/proyecto_final/Proyecto-Final-Datos-main/Bases Marco/Nodos Mundo.xlsx")
inter_bloq <- read_excel ("econ-708/proyectos/Base de datos exportaciones e importaciones.xltx")

#Depuramos los datasets
colnames(intra_m)[11] <- "Value"
intra_m <- subset(intra_m, select = c(Exporter, Importer, Resource, Year, Value))
intra_m <- subset(intra_m, Exporter != Importer) #Quitamos "auto exportaciones"

colnames(inter_bloq)[11] <- "Value"
inter_bloq <- subset(inter_bloq, select = c(Exporter, Importer, Resource, Year, Value))
inter_bloq <- subset(inter_bloq, Exporter != Importer) #Quitamos "auto exportaciones"

colnames(intra_g7)[11] <- "Value"
intra_g7 <- subset(intra_g7, select = c(Exporter, Importer, Resource, Year, Value))
intra_g7 <- subset(intra_g7, Exporter != Importer)

Nodos_Mundo$Exporter <- tolower(Nodos_Mundo$Exporter) #Esto es pq la base de datos con lat y long esta en minuscula
intra_g7$Exporter <- tolower(intra_g7$Exporter)   # --------------------------- Hay que hacerlo para todas las Df la parte del tolower
intra_g7$Importer <- tolower(intra_g7$Importer)

nodos_g7 <- Nodos_Mundo %>% filter(`Exporter region` != "Latin America and the Caribbean")
nodos_merc <- Nodos_Mundo %>% filter(`Exporter region` == "Latin America and the Caribbean")

#Trasnformamos Value a Mill. de U$D        -------------------------- Hay que hacerlo para todas las que tengan Value
intra_g7$Value <- intra_g7$Value * 0.001 


#Instalamos y cargamos paquetes para redes y mapas
library(igraph)
library(network)
library(ggraph)

# Creamos las redes intra g7  ---------------------------------------------
#g7 2000
intra_g7_2000 <- filter(intra_g7, Year=="2000" & Value < 100)   #--------------------------------- Hay que determinar el valor de corte de Value ya que con valores mayores a 100 los edges
# se vuelven demasiado masivos y no se llega a visualizar nada, o habria que buscar la forma de "Separar" mas a los edges entree si o algo asi
net_g72000 <- graph_from_data_frame(d=intra_g7_2000, vertices=nodos_g7, directed=T) 
net_g72000
plot(net_g72000, 
     vertex.label.color = "black", 
     edge.width = intra_g7_2000$Value)



#g7 2010
intra_g7_2010 = filter(intra_g7, Year=="2010") 
net <- graph_from_data_frame(d=intra_g7_2010, vertices=nodos_g7, directed=T) 
net
plot(net)

#g7 2019
intra_g7_2019 = filter(intra_g7, Year=="2019") 
net <- graph_from_data_frame(d=intra_g7_2019, vertices=nodos_g7, directed=T) 
net
plot(net)

# Creamos las redes intra Mercosur ----------------------------------------
#Mercosur 2000
intra_m_2000 = filter(intra_m, Year=="2000")
net <- graph_from_data_frame(d=intra_m_2000, vertices=nodos_mercosur, directed=T) 
net
plot(net)

#Mercosur 2010
intra_m_2010 = filter(intra_m, Year=="2010")
net <- graph_from_data_frame(d=intra_m_2010, vertices=nodos_mercosur, directed=T) 
net
plot(net)

intra_m_2019 = filter(intra_m, Year=="2019")
net <- graph_from_data_frame(d=intra_m_2019, vertices=nodos_mercosur, directed=T) 
net
plot(net)



## Quedaria despues tambien ponerle colores a los edges segun el tipo de producto 






















## Hola :D















