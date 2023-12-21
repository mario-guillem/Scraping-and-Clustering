#clusters segmentados por posiciones

library(tidyverse)

load("C:/Users/gomar/OneDrive/Escritorio/BIA/4r curs/1r Quatrimestre/Dades no estructurades/Practica/LALIGAAAA/datos.Rda")
datos <- columnas_seleccionadas


datos$Edad <- as.numeric(datos$Edad)
datos$Nacimiento <- as.numeric(datos$Nacimiento)
datos$`90 s.x` <- as.numeric(datos$`90 s.x`)
datos$Gls. <- as.numeric(datos$Gls.)
datos$`T/90` <- as.numeric(datos$`T/90`)
datos$por_pases_completados <- as.numeric(datos$por_pases_completados)
datos$GC90<- as.numeric(datos$GC90)
datos$`% Salvadas`<-as.numeric(datos$`% Salvadas`)
datos$`PaC%`<- as.numeric(datos$`PaC%`)
datos$`Tkl%`<- as.numeric(datos$`Tkl%`)
datos$Ass<- as.numeric(datos$Ass)



#DUPLICADOS
duplicados <- duplicated(datos$Jugador)
sum(duplicados)
datos_duplicados <- filter(datos, duplicados)


datos<-datos[-429,]
datos<-datos[-430,]


datos$Jugador[94] <- "José Carlos Lazo 1"
datos$Jugador[96] <- "Carmona 1"
datos$Jugador[158] <- "Adri Embarba 1"
datos$Jugador[185] <- "Fernando 1"
datos$Jugador[232] <- "Sergi Guardiola 1"
datos$Jugador[314] <- "Diego López 1"
datos$Jugador[342] <- "Roger Martí 1"
datos$Jugador[358] <- "Memphis 1"
datos$Jugador[376] <- "Johan Mojica 1"
datos$Jugador[392] <- "Manu Morlanes 1"
datos$Jugador[414] <- "Randy Ntekja 1"
datos$Jugador[428] <- "Fernando Pacheco 1"
datos$Jugador[508] <- "Umar Sadiq 1"
datos$Jugador[548] <- "Ramón Terrats 1"
datos$Jugador[579] <- "Toni Villa 1"


# SEGMENTAR DATAFRAMES POR POSICIONES

datos$Posc <- as.factor(datos$Posc)
posiciones <- table(datos$Posc)
posiciones <- as.data.frame(posiciones)

porteros <- filter(datos, datos$Posc=="PO")
defensas <- filter(datos, datos$Posc=="DF" | datos$Posc=="DF,CC"| datos$Posc=="DF,DL")
mediocentros <- filter(datos, datos$Posc=="CC" | datos$Posc=="CC,DF"| datos$Posc=="CC,DL")
delanteros <- filter(datos, datos$Posc=="DL" | datos$Posc=="DL,CC"| datos$Posc=="DL,DF")


# PORTEROS ----------------------------------------------------------------



porteros<-porteros %>% filter(`90 s.x`>10)
df_porteros <- porteros[,6:16]

df_porteros<-select(df_porteros,c(-Nacimiento,-Edad,-`90 s.x`,-Gls.,-`T/90`,-por_pases_completados,-Bloqueos,-`Tkl%`))
df_porteros$`PaC%`[is.na(df_porteros$`PaC%`)] <- 0
porteros_escalados <- scale(df_porteros)
porteros_escalados <- as.data.frame(porteros_escalados)
row.names(porteros_escalados) <- porteros$Jugador

set.seed(123)
library(factoextra)
a<-fviz_nbclust(x = porteros_escalados, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(porteros_escalados, method = "euclidean"), nstart = 50)

a
a+ ggtitle("Figura 1: Número óptimo de clústeres Porteros")+
  labs(caption = "Fuente: Elaboración propia", x="Número de clusters",y="WSS")

#El gráfico anterior proporciona una guía visual para determinar el número óptimo de clusters en el análisis
#de K-means. En este caso, el "codo" en 4 agrupaciones sugiere que ese número puede ser una elección apropiada
#para equilibrar la cohesión dentro de los clusters y la simplicidad del modelo.

km_clusters <- kmeans(x = porteros_escalados, centers = 4, nstart = 50)

b<-fviz_cluster(object = km_clusters, data = porteros_escalados, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Figura 2: Resultados clustering K-means Porteros") +
  theme_bw() +
  theme(legend.position = "none")

b+   labs(caption = "Fuente: Elaboración propia")
#Los cuatro agrupamientos construido por el método K-means son los siguientes:
  
#Cluster 1: Giorgi Mamardashvili, Iván Villar, Jordi Masip, Paulo Gazzaniga, Pepe Reina,
#Rui Silva, Sergio Herrera, Stole Dimitrievski, Unai Simón

#Cluster 2: Aitor Fernández, Álex Remiro, Claudio Bravo, David Soria, Ivo Grbić, Jan Oblak,
#Jeremías Ledesma, Marko Dmitrović, Predrag Rajković, Thibaut Courtois

#Cluster 3: Gerónimo Rulli, Marc-André ter Stegen

#Cluster 4: Agustín Marchesín, Álvaro Fernández, Édgar Badía, Fernando, 
#Fernando Pacheco, Yassine Bounou

cluster_porteros<-data.frame(km_clusters$cluster)

cluster_porteros<- cluster_porteros %>% rename(cluster = km_clusters.cluster)

cluster_porteros$Jugador <- rownames(cluster_porteros)

rownames(cluster_porteros) <- NULL

porteros_con_clusters <- merge(porteros, cluster_porteros, by.x = "Jugador", by.y = "Jugador")


porteros_con_clusters$cluster <- as.factor(porteros_con_clusters$cluster)


porteros_con_clusters%>%select(Jugador,cluster)



#SALVADAS ------------------------------------------

# Realizar un ANOVA para evaluar las diferencias entre clústeres para la variable Salvadas
anova_result_salv <- aov(`% Salvadas` ~ cluster, data = porteros_con_clusters)

# Mostrar los resultados del ANOVA
summary(anova_result_salv)


#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (0.000000259) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable porcentaje de salvadas

library(agricolae)

scheffe_salv <-scheffe.test(anova_result_salv,"cluster")

#El análisis de Scheffé ha agrupado las medias en dos grupos significativamente diferentes.
#Por un lado tenemos los cluster 2 y 3 que conforman el grupo "a" y los clusters 1 y 4 que conforman
# el grupo "b".

#El método de las comparaciones múltiples scheffe.test muestra que los porteros del cluster 2 y el cluster 3
#tienen un porcentaje de paradas significativamente superior a los porteros que conforman los clusters 1 y 4.

# PORTERÍAS A CERO----------------------------------------

anova_result_paC <- aov(`PaC%` ~ cluster, data = porteros_con_clusters)

summary(anova_result_paC)

#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (0.000000541) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable porcentaje de porterías a cero


scheffe_paC <-scheffe.test(anova_result_paC,"cluster")


#El análisis de Scheffé ha agrupado las medias en tres grupos significativamente diferentes.
#Por un lado tenemos el cluster 3 que conforma el grupo "a", el cluster 2 que conforma el grupo "b"
#y los clusters 1 y 4 que conforman el grupo "c".

#El método de las comparaciones múltiples scheffe.test muestra que los porteros del cluster 3
#tienen en media un porcentaje de porterías a cero significativamente superior a los porteros que conforman
#los cluster 1 y 4 y también sobre el cluster 2 el cual quedaría en un punto intermedio.

# GOLES EN CONTRA X90 MIN -------------------------------
anova_result_GC <- aov(GC90 ~ cluster, data = porteros_con_clusters)

summary(anova_result_GC)

#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (0.0000000206) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable goles en contra por 90 min.

scheffe_GC<-scheffe.test(anova_result_GC,"cluster")



#El análisis de Scheffé ha agrupado las medias en cuatro grupos significativamente diferentes.
#Por un lado tenemos el cluster 4 que conforma el grupo "a", luego el cluster 1 que conforma el grupo "b",
# el cluster 2 que conforma el grupo "c" y finalmente el cluster 3 que conforma el grupo "d"

# El método de las comparaciones múltiples scheffe.test muestra que los porteros del cluster 4 tienen en media 
# el mayor número de goles en contra por cada 90 minutos jugados. La diferencia en la media es significativamente 
#superior a los demás grupos, siendo los porteros del grupo 3 los que en media menos goles en contra tienen
# por cada 90 minutos jugados.

# DEFENSAS ----------------------------------------------------------------

defensas<-defensas %>% filter(`90 s.x`>10)

df_defensas <- defensas[,6:12]
row.names(df_defensas) <- defensas$Jugador
defensas_escalados <- scale(df_defensas)
defensas_escalados <- as.data.frame(defensas_escalados)
defensas_escalados <- na.omit(defensas_escalados)

df_defensas <- defensas[,6:13]

defensas2 <- defensas[, c(2, 13), drop = FALSE]

df_defensas<- select(df_defensas,c(-Nacimiento,-Edad,-`T/90`,-por_pases_completados))


defensas_escalados <- scale(df_defensas)
defensas_escalados <- as.data.frame(defensas_escalados)
defensas_escalados <- na.omit(defensas_escalados)
defensas2 <- na.omit(defensas2)
row.names(defensas_escalados) <- defensas2$Jugador


a<-fviz_nbclust(x = defensas_escalados, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(defensas_escalados, method = "euclidean"), nstart = 50)

a + ggtitle("Figura 3: Número óptimo de clústeres Defensas")+
  labs(caption = "Fuente: Elaboración propia", x="Número de clusters",y="WSS")


#El gráfico anterior proporciona una guía visual para determinar el número óptimo de clusters en el análisis
#de K-means. En este caso, el "codo" en 4 agrupaciones sugiere que ese número puede ser una elección apropiada
#para equilibrar la cohesión dentro de los clusters y la simplicidad del modelo.

km_clusters_def <- kmeans(x = defensas_escalados, centers = 4, nstart = 50)

b<- fviz_cluster(object = km_clusters_def, data = defensas_escalados, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Figura 4: Resultados clustering K-means Defensas") +
  theme_bw() +
  theme(legend.position = "none")

b + labs(caption = "Fuente: Elaboración propia")

#Los cuatro agrupamientos construido por el método K-means son los siguientes:

#Cluster 1: Abdul Mumin, Aihen Muñoz, Aïssa Mandi, Aitor Ruibal, Alberto Moreno, Alejandro Pozo Pozo,
#Aridane Hernández, Aritz Elustondo, Arnau Martinez, Bernardo Espinosa, Carlos Clerc, Catena, Copete,
#Damián Suárez, Dani Carvajal, Diego González, Diego Rico, Éder Militão, Édgar González, Eray Cömert, 
#Eric García, Ferland Mendy, Gastón Álvarez, Javi Hernández, Jawad El Yamiq, Jon Pacheco, Juan Cruz Armada,
#Juan Foyth, Juanpe, Karim Rekik, Kiko Femenía, Loïc Bade, Martin Hongla, Nacho, Nacho Vidal, Omar Alderete, 
#Pedro Bigas, Raúl Albiol, Rubén Peña, Sergio Akieme, Sergio Escudero, Toni Lato, Víctor Chust, Yan Couto

#Cluster 2: Álex Centelles, Alfonso Pedraza, Andoni Gorosabel, Andreas Christensen, Cenk Özkacar,
#Domingos Duarte, Enzo Roco, Fali, Gabriel Paulista, Gonzalo Montiel, Helibelton Palacios, Igor Zubeldia,
#Iñigo Lekue, Iñigo Martínez, Iván Fresneda, Javi Galán, Javi Sánchez, Jesús Navas, Joaquín Fernández, 
#Joseph Aidoo, Juan Brandáriz, Karim Rekik, Lucas Olaza, Luis Pérez, Marcos Alonso, Matija Nastasić,
#Nahuel Molina, Nemanja Gudelj, Omar Alderete, Óscar Mingueza, Pau Torres, Raúl Albiol, Reinildo Mandava, 
#Robin Le Normand, Rodrigo Ely, Rubén Peña, Sergi Gómez, Stefan Mitrović, Stefan Savić, Tanguy Nianzou, 
#Thierry Correia, Unai García, Yeray Álvarez, Youssouf Sabaly

#Cluster 3: Antonio Raillo, Fran Garcia, Gastón Álvarez, Juan Miranda, Luis Hernández, Mario Hermoso,
#Miguel Gutiérrez, Mouctar Diakhaby, Óscar Mingueza, Pedro Bigas, Santiago Bueno, Srđan Babić, Thierry Correia,
#Unai García, Víctor Chust, Yan Couto, Yannick Carrasco, Youssouf Sabaly

#Cluster 4: Alejandro Balde, Alfonso Espino, Axel Witsel, Brian Oliván, David Alaba, David García,
#Diego Rico, Florian Lejeune, Hugo Mallo, Iglesias, Isaac Carcelen, Iván Balliu, Jaume Costa, Javi Hernández, 
#Jon Pacheco, José Luis Gayà, Leandro Cabrera, Lucas Oliveira Rosa, Marcos Acuña, Martin Valjent, 
#Óscar de Marcos, Óscar Gil, Pablo Maffeo, Pedro Bigas, Ronald Araújo, Sergi Roberto, Sergio Akieme,
#Sergio Escudero, Srđan Babić, Thierry Correia, Unai Núñez, Víctor Chust, Yan Couto, Yuri Berchiche

cluster_defensas<-data.frame(km_clusters_def$cluster)

cluster_defensas<- cluster_defensas %>% rename(cluster = km_clusters_def.cluster)

cluster_defensas$Jugador <- rownames(cluster_defensas)

rownames(cluster_defensas) <- NULL

defensas_con_clusters <- merge(defensas, cluster_defensas, by.x = "Jugador", by.y = "Jugador")


defensas_con_clusters$cluster <- as.factor(defensas_con_clusters$cluster)


defensas_con_clusters%>% select(Jugador,cluster)



#GOLES------------------------------------

# Realizar un ANOVA para evaluar las diferencias entre clústeres para la variable Salvadas
anova_result_goles<- aov(`Gls.` ~ cluster, data = defensas_con_clusters)

# Mostrar los resultados del ANOVA
summary(anova_result_goles)

#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (<0.0000000000000002) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable goles.

scheffe_goles<-scheffe.test(anova_result_goles,"cluster")

#El análisis de Scheffé ha agrupado las medias en dos grupos significativamente diferentes.
#Por un lado tenemos el cluster 3 que conforma el grupo "a",y por otro lado los clusters
# 1,2 y 4 que conforma el grupo "b".

#El método de las comparaciones múltiples scheffe.test muestra que los defensas del cluster 3
# tienen en media un números de goles significativamente  superior respecto a los defensas que conforman
#los cluster 1, 2 y 4.

#BLOQUEOS--------------------------------------
# Realizar un ANOVA para evaluar las diferencias entre clústeres para la variable Salvadas
anova_result_bloqueos<- aov(Bloqueos ~ cluster, data = defensas_con_clusters)

# Mostrar los resultados del ANOVA
summary(anova_result_bloqueos)

#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (<0.0000000000000002) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable bloqueos.

scheffe_bloqueos<-scheffe.test(anova_result_bloqueos,"cluster")

#El análisis de Scheffé ha agrupado las medias en cuatro grupos significativamente diferentes.
#Por un lado tenemos el cluster 4 que conforma el grupo "a",luego el cluster 3 que conforma el grupo "b",
#el cluster 2 que conforma el grupo "bc" y finalmente el cluster 1 que conforma el cluster "c"

#El método de las comparaciones múltiples scheffe.test muestra que los defensas del cluster 4
# tienen en media un números de bloqueos significativamente  superior respecto a los defensas que conforman
# los grupos 1, 2 y 3, respectivamente, en ese orden.


#REGATEORES TACLEADOS-----------------------------

# Realizar un ANOVA para evaluar las diferencias entre clústeres para la variable Salvadas
anova_result_tkl<- aov(`Tkl%` ~ cluster, data = defensas_con_clusters)

# Mostrar los resultados del ANOVA
summary(anova_result_tkl)

#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (0.00000000000000143) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable tacleados.

scheffe_tkl<-scheffe.test(anova_result_tkl,"cluster")

#El análisis de Scheffé ha agrupado las medias en cuatro grupos significativamente diferentes.
#Por un lado tenemos el cluster 2 que conforma el grupo "a",luego el cluster 4 que conforma el grupo "b",
#el cluster 3 que conforma el grupo "bc" y finalmente el cluster 1 que conforma el cluster "c".

#El método de las comparaciones múltiples scheffe.test muestra que los defensas del cluster 2
# tienen en media un número de tacleos a regateadores significativamente  superior respecto a los defensas
#que conforman los clusters 1, 3 y 4, respectivamente, en ese orden.


# MEDIO CENTROS ----------------------------------------------------------------

mediocentros<-mediocentros %>% filter(`90 s.x`>10)

mediocentros<-mediocentros[-196,]

df_mediocentros <- mediocentros[, c(6:11, 17), drop = FALSE]
df_mediocentros<-select(df_mediocentros,c(-Nacimiento,-Edad,-`T/90`))
medios_escalados <- scale(df_mediocentros)
medios_escalados <- as.data.frame(medios_escalados)
row.names(medios_escalados) <- mediocentros$Jugador


a<-fviz_nbclust(x = medios_escalados, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(medios_escalados, method = "euclidean"), nstart = 50)

a+ ggtitle("Figura 5: Número óptimo de clústeres Defensas")+
  labs(caption = "Fuente: Elaboración propia", x="Número de clusters",y="WSS")


#El gráfico anterior proporciona una guía visual para determinar el número óptimo de clusters en el análisis
#de K-means. En este caso, el "codo" en 4 agrupaciones sugiere que ese número puede ser una elección apropiada
#para equilibrar la cohesión dentro de los clusters y la simplicidad del modelo.

km_clusters_med <- kmeans(x = medios_escalados, centers = 4, nstart = 50)

b<-fviz_cluster(object = km_clusters_med, data = medios_escalados, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Figura 6: Resultados clustering K-means Mediocentros") +
  theme_bw() +
  theme(legend.position = "none")

b + labs(caption = "Fuente: Elaboración propia")

cluster_mediocentros<-data.frame(km_clusters_med$cluster)

cluster_mediocentros<- cluster_mediocentros %>% rename(cluster = km_clusters_med.cluster)

cluster_mediocentros$Jugador <- rownames(cluster_mediocentros)

rownames(cluster_mediocentros) <- NULL

mediocentros_con_clusters <- merge(mediocentros, cluster_mediocentros, by.x = "Jugador", by.y = "Jugador")


mediocentros_con_clusters$cluster <- as.factor(mediocentros_con_clusters$cluster)

mediocentros_con_clusters%>%select(Jugador,cluster)


#Cluster 1: Álex Fernández, Álvaro Aguado, Andrés Guardado, Ángel Algobia, Asier Illarramendi, César de la Hoz,
#Dani García, Darko Brašanac, Domingos André Ribeiro Almeida, Eduardo Camavinga, Étienne Capoue,
#Fede San Emeterio, Fernando 1, Franck Kessié, Geoffrey Kondogbia, Giovani Lo Celso, Guido Rodríguez,
#Iddrisu Baba, Iker Muniain, Ilaix Moriba, Íñigo Eguaras, Iñigo Ruiz de Galarreta, Iván Martín, 
#Ivan Rakitić, Iván Sánchez, Joan Jordán, John Donald, Kike Pérez.

#Cluster 2:
 # Aimar Oroz, Aleix García, Aurélien Tchouaméni, Carles Pérez, Dani Ceballos, Daniel Parejo,
#David Silva, Edu Expósito, Frenkie de Jong, Gabriel Veiga, Gavi, Jon Moncayola, Luca de la Torre,
#Lucas Robertone, Luka Modrić, Martín Zubimendi, Mikel Merino, Mikel Vesga, Moi Gómez, Pablo Ibáñez,
#Santi Comesaña, Sergio Busquets, Thomas Lemar, Toni Kroos.

#Cluster 3:
#Aleix Vidal, Amath, Antonio Sánchez, Brais Méndez, Brian Ocampo, Carles Aleñá, Fran Beltrán,
#Franco Cervi, Iván Alejo, Iván Trejo, Gonzalo Escalante, Gonzalo Melero, Iván Martin, Nico Ribaudo
#, Oriol Romeu, Óscar Rodríguez Arnaiz, Óscar Trejo, Theo Bongonda, Tete Morente, Valery Fernández,
#Viktor Tsyhankov.

#Cluster 4:
#Alex Baena, Brais Méndez, Denis Suárez, Fede Valverde, Gabriel Veiga, Lee Kang-in, Omar Mascarell, 
#Pedri, Rubén Sobrino, Sergi Darder, Suso, Tete Morente, Theo Bongonda, Vinicius Souza.

#GOLES-------------------------------------------

# Realizar un ANOVA para evaluar las diferencias entre clústeres para la variable Salvadas
anova_result_goles<- aov(`Gls.` ~ cluster, data = mediocentros_con_clusters)

# Mostrar los resultados del ANOVA
summary(anova_result_goles)


#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (<0.0000000000000002) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable goles.

scheffe_goles<-scheffe.test(anova_result_goles,"cluster")

#El análisis de Scheffé ha agrupado las medias en cuatro grupos significativamente diferentes.
#Por un lado tenemos el cluster 4 que conforma el grupo "a",luego el cluster 2 que conforma el grupo "b",
#el cluster 3 que conforma el grupo "bc" y finalmente el cluster 1 que conforma el cluster "c".

#El método de las comparaciones múltiples scheffe.test muestra que los mediocentros del cluster 4
# marcan en media un número de goles significativamente  superior respecto a los mediocentros
#que conforman los clusters 1, 3 y 2, respectivamente, en ese orden.

# PASES COMPLETADOS -------------------------------
# Realizar un ANOVA para evaluar las diferencias entre clústeres para la variable Salvadas
anova_result_pases<- aov(por_pases_completados ~ cluster, data = mediocentros_con_clusters)

# Mostrar los resultados del ANOVA
summary(anova_result_pases)

#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (<0.0000000000000002) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable porcentaje de pases completados.

scheffe_pases<-scheffe.test(anova_result_pases,"cluster")

#El análisis de Scheffé ha agrupado las medias en cuatro grupos significativamente diferentes.
#Por un lado tenemos el cluster 2 que conforma el grupo "a",luego el cluster 1 que conforma el grupo "ab",
#el cluster 4 que conforma el grupo "b" y finalmente el cluster 3 que conforma el cluster "c".

#El método de las comparaciones múltiples scheffe.test muestra que los mediocentros del cluster 2
# completan en media un porcentaje de pases significativamente  superior respecto a los mediocentros
#que conforman los clusters 3, 4 y 1 respectivamente, en ese orden.


#ASISTENCIAS-------------------------------
# Realizar un ANOVA para evaluar las diferencias entre clústeres para la variable Salvadas
anova_result_ass<- aov(Ass ~ cluster, data = mediocentros_con_clusters)

# Mostrar los resultados del ANOVA
summary(anova_result_ass)

#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (0.00000000000000322) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable asistencias.

scheffe_ass<-scheffe.test(anova_result_ass,"cluster")


#El análisis de Scheffé ha agrupado las medias en cuatro grupos significativamente diferentes.
#Por un lado tenemos el cluster 2 que conforma el grupo "a",luego el cluster 4 que conforma el grupo "ab",
#el cluster 3 que conforma el grupo "bc" y finalmente el cluster 1 que conforma el cluster "c".

#El método de las comparaciones múltiples scheffe.test muestra que los mediocentros del cluster 2
# asisten en media un número de goles significativamente  superior respecto a los mediocentros
#que conforman los clusters 1,3 y 4 respectivamente, en ese orden.


# DELANTEROS ----------------------------------------------------------------

delanteros<-delanteros %>% filter(`90 s.x`>10)



df_delanteros <- delanteros[, c(6:11, 17), drop = FALSE]
df_delanteros<-select(df_delanteros,c(-Nacimiento,-Edad,-por_pases_completados))
delanteros_escalados <- scale(df_delanteros)
delanteros_escalados <- as.data.frame(delanteros_escalados)
row.names(delanteros_escalados) <- delanteros$Jugador


a<-fviz_nbclust(x = delanteros_escalados, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(delanteros_escalados, method = "euclidean"), nstart = 50)


a+ ggtitle("Figura 7: Número óptimo de clústeres Delantera")+
  labs(caption = "Fuente: Elaboración propia", x="Número de clusters",y="WSS")

#El gráfico anterior proporciona una guía visual para determinar el número óptimo de clusters en el análisis
#de K-means. En este caso, el "codo" en 4 agrupaciones sugiere que ese número puede ser una elección apropiada
#para equilibrar la cohesión dentro de los clusters y la simplicidad del modelo.

km_clusters_del <- kmeans(x = delanteros_escalados, centers = 4, nstart = 50)

b<- fviz_cluster(object = km_clusters_del, data = delanteros_escalados, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Figura 8: Resultados clustering K-means Delantera") +
  theme_bw() +
  theme(legend.position = "none")

b + labs(caption = "Fuente: Elaboración propia")

cluster_delanteros<-data.frame(km_clusters_del$cluster)

cluster_delanteros<- cluster_delanteros %>% rename(cluster = km_clusters_del.cluster)

cluster_delanteros$Jugador <- rownames(cluster_delanteros)

rownames(cluster_delanteros) <- NULL

delanteros_con_clusters <- merge(delanteros, cluster_delanteros, by.x = "Jugador", by.y = "Jugador")


delanteros_con_clusters$cluster <- as.factor(delanteros_con_clusters$cluster)

delanteros_con_clusters%>%select(Jugador,cluster)

#Cluster 1:
 # Alexander Sørloth, Álvaro García, Ante Budimir, Borja Iglesias, Borja Mayoral, Enes Ünal,
  #Iago Aspas, Iñaki Williams, Isaac Palazón Camacho, Joselu, Juanmi, Lucas Boyé, Nico Williams, 
  #Nicolas Jackson, Raúl García, Samuel Chukwueze, Samuel Lino, Sergio Camello, Takefusa Kubo, 
  #Valentín Castellanos, Vedat Muriqi, Yeremi Pino.

#Cluster 2:
 # Antoine Griezmann, Karim Benzema, Raphinha, Robert Lewandowski, Rodrygo, Vinicius Júnior.

#Cluster 3:
 # Adri Embarba, Álvaro Morata, Ángel Correa, Ansu Fati, Cristhian Stuani, Cyle Larin, Ezequiel Ávila,
#Gerard Moreno, Gorka Guruzeta, Iñaki Williams, Isaac Palazón Camacho, Javi Puado, Jose Luis Morales, 
#Justin Kluivert, Kiké, Marco Asensio, Ousmane Dembélé, Rafa Mir, Youssef En-Nesyri.

#Cluster 4:
#Abdessamad Ezzalzouli, Álex Berenguer, Anthony Lozano, Ayoze Pérez, Bryan Gil, Edinson Cavani, 
#El Bilal Touré, Érik Lamela, Ezequiel Ponce, Ferrán Torres, Gonzalo Plata, Hugo Duro, Jørgen Strand Larsen
#, Kike Barja, Largie Ramazani, Léo Baptistão, Lucas Ocampos, Luis Javier Suárez, Luiz Henrique, 
#Mikel Oyarzabal, Óscar Plano, Papu Gómez, Pere Milla, Rodri, Samu Castillejo, Sergi Guardiola,
#Sergio Canales, Sergio León, Vinicius Júnior.

#GOLES----------------------------

# Realizar un ANOVA para evaluar las diferencias entre clústeres para la variable Salvadas
anova_result_goles<- aov(`Gls.` ~ cluster, data = delanteros_con_clusters)

# Mostrar los resultados del ANOVA
summary(anova_result_goles)

#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (0.00000000000277) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable goles.

scheffe_goles<-scheffe.test(anova_result_goles,"cluster")


#El análisis de Scheffé ha agrupado las medias en tres grupos significativamente diferentes.
#Por un lado tenemos el cluster 2 que conforma el grupo "a",luego el cluster 1 y 3 que conforma el grupo "b",
#y finalmente  el cluster 4 que conforma el grupo "c".

#El método de las comparaciones múltiples scheffe.test muestra que los delanteros del cluster 2
# marcan en media un número de goles significativamente  superior respecto a los delanteros
#que conforman los clusters 4, 3 y 1 respectivamente, en ese orden.

#DISPAROS/90min--------------------------

# Realizar un ANOVA para evaluar las diferencias entre clústeres para la variable Salvadas
anova_result_dis<- aov(`T/90` ~ cluster, data = delanteros_con_clusters)

# Mostrar los resultados del ANOVA
summary(anova_result_dis)

#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (0.00000000000000268) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable disparos por cada 90 min.

scheffe_dis<-scheffe.test(anova_result_dis,"cluster")

#El análisis de Scheffé ha agrupado las medias en tres grupos significativamente diferentes.
#Por un lado tenemos el cluster 2 y 3 que conforman el grupo "a",luego el cluster 1 que conforma el grupo "b",
#y finalmente  el cluster 4 que conforma el grupo "c".

#El método de las comparaciones múltiples scheffe.test muestra que los delanteros del cluster 2 y 3
# disparan en media un número de veces significativamente superior respecto a los delanteros
#que conforman los clusters 4, 1 y 3 respectivamente, en ese orden.


#ASISTENCIAS------------------------

# Realizar un ANOVA para evaluar las diferencias entre clústeres para la variable Salvadas
anova_result_ass<- aov(Ass ~ cluster, data = delanteros_con_clusters)

# Mostrar los resultados del ANOVA
summary(anova_result_ass)

#La hipótesis nula (H0) es que no hay efecto significativo del factor cluster en la variable % Salvadas.
#El valor p extremadamente bajo (0.00000000158) indica que hay evidencia significativa para rechazar
#la hipótesis nula.

#Por lo tanto, se concluye que hay al menos una diferencia significativa entre los grupos definidos
#por la clusterización en términos de la variable asistencias.


scheffe_ass<-scheffe.test(anova_result_ass,"cluster")

#El análisis de Scheffé ha agrupado las medias en dos grupos significativamente diferentes.
#Por un lado tenemos el cluster 2 conforma el grupo "a", y por otro lado los cluster 1, 3 y 4 que conforman
# el grupo "b".

#El método de las comparaciones múltiples scheffe.test muestra que los delanteros del cluster 2
# asisten en media un número de goles significativamente superior respecto a los delanteros
#que conforma los clusters 4, 3 y 1 respectivamente, en ese orden.

