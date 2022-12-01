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



merc <- read_excel("ECON - 708/econ-708/proyectos/proyecto_final/Proyecto-Final-Datos-main/Bases_ultimas/mercosur.xlsx")


merc <- merc %>% relocate(Year, .after = last_col())                    #Pasa la columna año al ultimo lugar

nodos_merc <- merc %>% select(Reporter)                                     #Creamos los nodos mercosur
nodos_merc <- nodos_merc [!duplicated(nodos_merc), ]

nodos_merc <- mutate(nodos_merc, value = 0)

for (i in 1:11) {
  
  nodos_merc$value[i] <- sum(merc[merc$Reporter == nodos_merc$Reporter[i],]$`Trade Value (US$)`)
}


merc_net_2000 <- graph_from_data_frame(d= filter (merc,                # Mercosur y G7 año 2000
                                                  Year == "2000" & `Trade Value (US$)` > 1000000 ),
                                       vertices=nodos_merc, directed=T)



merc_net_2000 <- ggnetwork(merc_net_2000)                                 # Transofrmamos la net en un objeto tipo gg network



merc_net_2000$country<-countrycode(merc_net_2000$name,                    # Agregamos los codigos de paises para la visaulización con ggflags
                                   "country.name","iso2c")
merc_net_2000$country <- merc_net_2000$country %>% tolower()


ggplot(merc_net_2000, 
       aes(x = x, y = y, xend = xend, yend = yend,
           country=country)) +
  geom_node_point(aes(size = value)) +
  geom_edges(color="grey") +
  geom_flag(aes(size = value)) +
  scale_country() +
  scale_size(range = c(2, 25))+ 
  theme_blank()








mercosur <- rbind (arg, br, uy, par, ven)          

#colnames(mercosur)[5] <-  "Value"

mercosur <- mercosur %>% relocate(Year, .after = last_col())                    #Pasa la columna año al ultimo lugar

nodos_merc <- mercosur %>% select(Reporter)                                     #Creamos los nodos mercosur
nodos_merc <- nodos_merc [!duplicated(nodos_merc), ]

nodos_g7 <- mercosur %>% select(Partner)                                        #Creamos los nodos G7
nodos_g7 <- nodos_g7 [!duplicated(nodos_g7), ]
colnames(nodos_g7)[1] <-  "Reporter"

                                            #Creamos los nodos de toda la red


#Valor total de las exportaciones 

nodos_merc_2000 <- mutate(nodos_merc, value = 0)
nodos_merc_2010 <- mutate(nodos_merc, value = 0)
nodos_merc_2019 <- mutate(nodos_merc, value = 0)

for (i in 1:5) {
  
  nodos_merc_2000$value[i] <- sum(mercosur[mercosur$Reporter == nodos_merc_2000$Reporter[i] & mercosur$Year == 2000, ]$`Trade Value (US$)`)  
}


for (i in 1:5) {
  
  nodos_merc_2010$value[i] <- sum(mercosur[mercosur$Reporter == nodos_merc_2010$Reporter[i] & mercosur$Year == 2010, ]$`Trade Value (US$)`)  
}

for (i in 1:5) {
  
  nodos_merc_2019$value[i] <- sum(mercosur[mercosur$Reporter == nodos_merc_2019$Reporter[i] & mercosur$Year == 2019, ]$`Trade Value (US$)`)  
}


nodos_g7_2000 <- mutate(nodos_g7, value = 0)
nodos_g7_2010 <- mutate(nodos_g7, value = 0)
nodos_g7_2019 <- mutate(nodos_g7, value = 0)


for (i in 1:7) {
  
  nodos_g7_2000$value[i] <- sum(mercosur[mercosur$Partner == nodos_g7_2000$Reporter[i] & mercosur$Year == 2000, ]$`Trade Value (US$)`)  
}

for (i in 1:7) {
  
  nodos_g7_2010$value[i] <- sum(mercosur[mercosur$Partner == nodos_g7_2010$Reporter[i] & mercosur$Year == 2010, ]$`Trade Value (US$)`)  
}

for (i in 1:7) {
  
  nodos_g7_2019$value[i] <- sum(mercosur[mercosur$Partner == nodos_g7_2019$Reporter[i] & mercosur$Year == 2019, ]$`Trade Value (US$)`)  
}

nodos_2000 <- rbind(nodos_merc_2000, nodos_g7_2000)
nodos_2010 <- rbind(nodos_merc_2010, nodos_g7_2010)
nodos_2019 <- rbind( nodos_merc_2019, nodos_g7_2019)

#Creamos las redes
merc_g7_net_2000 <- graph_from_data_frame(d= filter (mercosur,                
                                                     Year == "2000" & `Trade Value (US$)` >= 1000000),
                                          vertices= nodos_2000, directed=T)

merc_g7_net_2000 <- ggnetwork(merc_g7_net_2000)                                 


merc_g7_net_2010 <- graph_from_data_frame(d= filter (mercosur,                # Mercosur y G7 año 2010
                                                     Year == "2010" & `Trade Value (US$)` >= 1000000),
                                          vertices= nodos_2010, directed=T)

merc_g7_net_2010 <- ggnetwork(merc_g7_net_2010) 

merc_g7_net_2019 <-  graph_from_data_frame(d= filter (mercosur,                # Mercosur y G7 año 2019
                                                      Year == "2019" & `Trade Value (US$)` >= 1000000),
                                           vertices= nodos_2019, directed=T)

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














