#Trade Network
#Importamos datasets
library(readxl)

intra_m <- read_excel("econ-708/proyectos/proyecto_final/Proyecto-Final-Datos-main/Bases/intrabloques mercosur.xlsx")

intra_g7 <- read_excel("econ-708/proyectos/proyecto_final/Proyecto-Final-Datos-main/Bases/intrabloques g7.xlsx")

inter_bloq <- read_excel ("econ-708/proyectos/proyecto_final/Proyecto-Final-Datos-main/Bases/Base de datos exportaciones e importaciones.xltx")

Nodos_Mundo <- read_excel("econ-708/proyectos/proyecto_final/Proyecto-Final-Datos-main/Bases/Nodos Mundo.xlsx")


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
install.packages("igraph") 
install.packages("network") 
library(igraph)
library(network)
library(maps)

rawnodes<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_FREQ.csv')
names(rawnodes)

rawnodes <- subset(rawnodes, select=c("ID","lat", "lon"))
colnames(rawnodes) <- c('Exporter','lat','lon')


Nodos_Mundo = merge(x = Nodos_Mundo, y = rawnodes, by = "Exporter")
Nodos_Mundo

#Realizamos la red 
net <- graph_from_data_frame(d=g7_g7_2019, vertices=Nodos_Mundo, directed=T) 
net
plot(net)


net <- network(g7_g7_2019, directed=TRUE) 
net

