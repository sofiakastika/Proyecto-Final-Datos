#Proyecto
# Cargamos Librerias
install.packages("tidyverse")
install.packages("ggraph")
install.packages("ggplot2")
install.packages("ggnetwork")
install.packages("countrycode")
install.packages("devtools")
install.packages("Rtools")
devtools::install_github("rensa/ggflags")
library(tidyverse)
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

#Trabajamos los datasets

mercosur <- rbind (arg, br, uy, par, ven)          

#colnames(mercosur)[5] <-  "Value"

mercosur <- mercosur %>% relocate(Year, .after = last_col())                    #Pasa la columna año al ultimo lugar

nodos_merc <- mercosur %>% select(Reporter)                                     #Creamos los nodos mercosur
nodos_merc <- nodos_merc [!duplicated(nodos_merc), ]

nodos_g7 <- mercosur %>% select(Partner)                                        #Creamos los nodos G7
nodos_g7 <- nodos_g7 [!duplicated(nodos_g7), ]
colnames(nodos_g7)[1] <-  "Reporter"

nodos_merc2 <- filter(nodos_merc, Reporter != "Venezuela")


#Valor total de las exportaciones 
#Creamos los nodos MERCOSUR
nodos_merc_2000 <- mutate(nodos_merc, value = 0)
nodos_merc_2010 <- mutate(nodos_merc, value = 0)
nodos_merc_2019 <- mutate(nodos_merc, value = 0)
nodos_merc2_2019 <- mutate(nodos_merc2, value = 0)

#Valores 2000
for (i in 1:5) {
  
  nodos_merc_2000$value[i] <- sum(mercosur[mercosur$Reporter == nodos_merc_2000$Reporter[i] & mercosur$Year == 2000, ]$`Trade Value (US$)`)  
}

#Valores 2010 
for (i in 1:5) {
  
  nodos_merc_2010$value[i] <- sum(mercosur[mercosur$Reporter == nodos_merc_2010$Reporter[i] & mercosur$Year == 2010, ]$`Trade Value (US$)`)  
}

#Valores 2019 
for (i in 1:5) {
  
  nodos_merc_2019$value[i] <- sum(mercosur[mercosur$Reporter == nodos_merc_2019$Reporter[i] & mercosur$Year == 2019, ]$`Trade Value (US$)`)  
}

#Valores 2019 sin Venezuela
for (i in 1:4){

  nodos_merc2_2019$value[i] <- sum(mercosur[mercosur$Reporter == nodos_merc2_2019$Reporter[i] & mercosur$Year == 2019, ]$`Trade Value (US$)`)  
}

#Creamos nodos G7

nodos_g7_2000 <- mutate(nodos_g7, value = 0)
nodos_g7_2010 <- mutate(nodos_g7, value = 0)
nodos_g7_2019 <- mutate(nodos_g7, value = 0)

#VALORES 2000
for (i in 1:7) {
  
  nodos_g7_2000$value[i] <- sum(mercosur[mercosur$Partner == nodos_g7_2000$Reporter[i] & mercosur$Year == 2000, ]$`Trade Value (US$)`)  
}

#VALORES 2010
for (i in 1:7) {
  
  nodos_g7_2010$value[i] <- sum(mercosur[mercosur$Partner == nodos_g7_2010$Reporter[i] & mercosur$Year == 2010, ]$`Trade Value (US$)`)  
}

#VALORES 2019 
for (i in 1:7) {
  
  nodos_g7_2019$value[i] <- sum(mercosur[mercosur$Partner == nodos_g7_2019$Reporter[i] & mercosur$Year == 2019, ]$`Trade Value (US$)`)  
}

#Unimos todos los nodos para cada año
nodos_2000 <- rbind(nodos_merc_2000, nodos_g7_2000)
nodos_2010 <- rbind(nodos_merc_2010, nodos_g7_2010)
nodos_2019 <- rbind( nodos_merc_2019, nodos_g7_2019)
nodos_2019_sinv <- rbind(nodos_merc2_2019, nodos_g7_2019)


#Creamos las redes
#2000
merc_g7_net_2000 <- graph_from_data_frame(d= filter (mercosur,                
                                                     Year == "2000" & `Trade Value (US$)` >= 1000000),
                                          vertices=nodos_2000, directed=T)

merc_g7_net_2000 <- ggnetwork(merc_g7_net_2000)                                 

#2010
merc_g7_net_2010 <- graph_from_data_frame(d= filter (mercosur,                # Mercosur y G7 año 2010
                                                     Year == "2010" & `Trade Value (US$)` >= 1000000),
                                          vertices=nodos_2010, directed=T)

merc_g7_net_2010 <- ggnetwork(merc_g7_net_2010) 

#2019
merc_g7_net_2019 <-  graph_from_data_frame(d= filter (mercosur,                
                                                      Year == "2019" & `Trade Value (US$)` >= 1000000),
                                           vertices= nodos_2019, directed=T)

merc_g7_net_2019 <- ggnetwork(merc_g7_net_2019)


#2019 sin Venezuela
merc_g7_net2_2019 <-  graph_from_data_frame(d= filter (mercosur,                # Mercosur y G7 año 2019
                                                      Year == "2019" & `Trade Value (US$)` >= 1000000),
                                           vertices= nodos_2019_sinv, directed=T)

merc_g7_net2_2019 <- ggnetwork(merc_g7_net2_2019)



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


merc_g7_net2_2019$country<-countrycode(merc_g7_net2_2019$name,                    
                                      "country.name","iso2c")
merc_g7_net2_2019$country <- merc_g7_net2_2019$country %>% tolower()



#Visualización
#2000
ggplot(merc_g7_net_2000, 
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
  ggplot(merc_g7_net_2010, 
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
  ggplot(merc_g7_net_2019, 
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
  
  
  #2019 Sin venezuela
  ggplot(merc_g7_net2_2019, 
         aes(x = x, y = y, xend = xend, yend = yend,
             country=country)) +
    geom_node_point(aes(size = value), show.legend = F) + 
    geom_edges(color="grey") +
    geom_flag(aes(size = value), show.legend = F) +
    scale_country() +
    scale_size(range = c(2, 25))+ 
    ggtitle("2019 sin Venezuela") + 
    theme(axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(), 
          axis.title.x=element_blank(),
          axis.text.y=element_blank(),  
          axis.ticks.y=element_blank(),  
          axis.title.y=element_blank()
    ) 
  theme_blank()
  
 
