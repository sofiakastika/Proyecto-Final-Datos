#Proyecto
# Cargamos Librerias
install.packages("ggplot2")
install.packages("ggnetwork")
install.packages("countrycode")
install.packages("devtools")
install.packages("Rtools")
devtools::install_github("rensa/ggflags")
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


# Cargamos datasets
arg <- read_excel("econ-708/proyectos/proyecto_final/Bases/argentina.xlsx")
br <- read_excel("econ-708/proyectos/proyecto_final/Bases/brasil.xlsx")
par <- read_excel("econ-708/proyectos/proyecto_final/Bases/paraguay.xlsx")
uy <- read_excel("econ-708/proyectos/proyecto_final/Bases/uruguay.xlsx")
ven <- read_excel("econ-708/proyectos/proyecto_final/Bases/venezuela.xlsx")

#Sofi RStudio
#arg <- read_excel("C:/Users/sofi1/OneDrive/Escritorio/Económicas/Segundo Cuatri 2022/Datos/Curso/econ-708/proyectos/proyecto_final/Bases/argentina.xlsx")
#br <- read_excel("C:/Users/sofi1/OneDrive/Escritorio/Económicas/Segundo Cuatri 2022/Datos/Curso/econ-708/proyectos/proyecto_final/Bases/brasil.xlsx")
#uy <- read_excel("C:/Users/sofi1/OneDrive/Escritorio/Económicas/Segundo Cuatri 2022/Datos/Curso/econ-708/proyectos/proyecto_final/Bases/uruguay.xlsx")
#par <- read_excel("C:/Users/sofi1/OneDrive/Escritorio/Económicas/Segundo Cuatri 2022/Datos/Curso/econ-708/proyectos/proyecto_final/Bases/paraguay.xlsx")
#ven <- read_excel("C:/Users/sofi1/OneDrive/Escritorio/Económicas/Segundo Cuatri 2022/Datos/Curso/econ-708/proyectos/proyecto_final/Bases/venezuela.xlsx")

#Trabajamos los datasets
mercosur <- rbind (arg, br, uy, par, ven)  

#colnames(mercosur)[5] <-  "Value"

mercosur <- mercosur %>% relocate(Year, .after = last_col())                    #Pasa la columna año al ultimo lugar

nodos_merc <- mercosur %>% select(Reporter)                                     #Creamos los nodos mercosur
nodos_merc <- nodos_merc [!duplicated(nodos_merc), ]

nodos_g7 <- mercosur %>% select(Partner)                                        #Creamos los nodos G7
nodos_g7 <- nodos_g7 [!duplicated(nodos_g7), ]
colnames(nodos_g7)[1] <-  "Reporter"

nodos <- rbind(nodos_merc, nodos_g7)                                            #Creamos los nodos de toda la red




# Creamos las redes
merc_g7_net_2000 <- graph_from_data_frame(d= filter (mercosur,                # Mercosur y G7 año 2000
                                                       Year == "2000"),
                                            vertices=nodos, directed=T)


merc_g7_net_2000 <- ggnetwork(merc_g7_net_2000)                                 # Transofrmamos la net en un objeto tipo gg network

merc_g7_net_2010 <- graph_from_data_frame(d= filter (mercosur,                # Mercosur y G7 año 2010
                                                       Year == "2010"),
                                            vertices=nodos, directed=T)


merc_g7_net_2010 <- ggnetwork(merc_g7_net_2010) 

merc_g7_net_2019 <-  graph_from_data_frame(d= filter (mercosur,                # Mercosur y G7 año 2019
                                                       Year == "2019"),
                                            vertices=nodos, directed=T)


merc_g7_net_2019 <- ggnetwork(merc_g7_net_2019)

# Codigos de paises
merc_g7_net_2000$country<-countrycode(merc_g7_net_2000$name,                    # Agregamos los codigos de paises para la visaulización con ggflags
                         "country.name","iso2c")
merc_g7_net_2000$country <- merc_g7_net_2000$country %>% tolower()

merc_g7_net_2010$country<-countrycode(merc_g7_net_2010$name,                    
                                      "country.name","iso2c")
merc_g7_net_2010$country <- merc_g7_net_2010$country %>% tolower()

merc_g7_net_2019$country<-countrycode(merc_g7_net_2019$name,                    
                                      "country.name","iso2c")
merc_g7_net_2019$country <- merc_g7_net_2019$country %>% tolower()

#Visualización
ggplot(merc_g7_net_2000, 
       aes(x = x, y = y, xend = xend, yend = yend,
                country=country)) +
  geom_edges(color="grey") +
  geom_flag() +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  theme_blank()


ggplot(merc_g7_net_2010, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_edges(color="grey") +
  geom_flag() +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  theme_blank()

ggplot(merc_g7_net_2019, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_edges(color="grey") +
  geom_flag() +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  theme_blank()


#Cosas a resolver. Ver como hacer para que las bolitas sean de distintos tamanos y ver como hacer para cambiarle el color a los edges
  
  
  
filter(merc_g7_net_2000, Commodity == "Live animals")















