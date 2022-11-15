#g7-g7 2019 

#Importamos dataset con Expos e Impos de g7 2019
library(readxl)
g7_g7_2019 <- read_excel("econ-708/proyectos/g7-g7 2019.xlsx")
View(g7_g7_2019)
#Importamos dataset de nodos
Nodos_Mundo <- read_excel("econ-708/proyectos/Nodos Mundo.xlsx")
View(Nodos_Mundo)

#Instalamos paquetes para redes
install.packages("igraph") 
install.packages("network") 
library(igraph)
library(network)

#Realizamos la red 
net <- graph_from_data_frame(d=g7_g7_2019, vertices=Nodos_Mundo, directed=T) 
net
plot(net)

