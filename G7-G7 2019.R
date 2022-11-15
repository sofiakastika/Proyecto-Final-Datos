#g7-g7 2019 
library(readxl)
g7_g7_2019 <- read_excel("econ-708/proyectos/g7-g7 2019.xlsx")
View(g7_g7_2019)

Nodos_Mundo <- read_excel("econ-708/proyectos/Nodos Mundo.xlsx")
View(Nodos_Mundo)

install.packages("igraph") 
install.packages("network") 

library(igraph)
library(network)
net <- graph_from_data_frame(d=g7_g7_2019, vertices=Nodos_Mundo, directed=T) 
net
plot(net)

