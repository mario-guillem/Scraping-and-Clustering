options(scipen = 999)

library(rvest)

extraer_datos_automatico <- function(palabra_clave) {
  # Verificar si la palabra clave es "keepers" y ajustar la URL y XPath en consecuencia
  if (palabra_clave == "keepers") {
    url <- paste('https://fbref.com/es/comps/12/2022-2023/', palabra_clave, '/Estadisticas-2022-2023-La-Liga', sep = '')
    xpath <- '//*[@id="stats_keeper"]'
  } else {
    # Construir la URL
    url <- paste('https://fbref.com/es/comps/12/2022-2023/', tolower(palabra_clave), '/Estadisticas-2022-2023-La-Liga', sep = '')
    
    # Construir el XPath
    xpath <- paste('//*[@id="stats_', palabra_clave, '"]', sep = '')
  }
  
  # Utilizar la función original
  datos_lista <- data.frame()
  
  # Leer la página web
  pagina <- read_html(url)
  
  # Extraer el contenido de los comentarios y descomentarlo
  contenido_comentarios <- html_nodes(pagina, xpath = "//comment()")
  texto_completo <- html_text(contenido_comentarios)
  
  # Eliminar los caracteres de comentario <!-- y -->
  texto_sin_comentarios <- gsub("<!--|-->","",texto_completo)
  
  # Convertir el texto a HTML
  html_sin_comentarios <- read_html(paste(texto_sin_comentarios, collapse = ""))
  
  # Extraer la tabla por su id
  tabla <- html_node(html_sin_comentarios, xpath = xpath)
  
  # Convertir la tabla a un dataframe
  datos_tabla <- html_table(tabla)
  
  colnames(datos_tabla) <- datos_tabla[1, ] # La primera fila tiene los nombres de columnas
  datos_tabla <- datos_tabla[-1, ]
  datos_tabla <- datos_tabla[datos_tabla$RL != "RL", ]
  datos_tabla$País <- gsub("[^A-Z]", "", datos_tabla$País)
  
  # Asignar a la lista
  datos_lista <- datos_tabla
  
  return(datos_lista)
}

extraer_datos <- function(palabra_clave) {
  # Utilizar la función automatizada
  datos <- extraer_datos_automatico(palabra_clave)
  
  return(datos)
}

# Uso de la función con tus palabras clave
tiros <- extraer_datos("shooting")
pases <- extraer_datos("passing")
defensa <- extraer_datos("defense")
porteros <- extraer_datos("keepers")

tiros$Gls.<- as.numeric(tiros$Gls.)
regresion<- glm(Gls.~País, data=tiros, family= poisson) 
summary(regresion)

regresion<- glm(Gls.~Posc, data=tiros, family= poisson) 
summary(regresion)


colnames(pases)[11]<-"por_pases_completados"
pases$por_pases_completados<- as.numeric(pases$por_pases_completados)
regresion<- glm(por_pases_completados~Equipo, data=pases, family= poisson) 
summary(regresion) 

colnames(defensa)[9]<-"Bloqueos"
defensa$Bloqueos<- as.numeric(defensa$Bloqueos)
regresion <- lm(Bloqueos~Equipo,data=defensa)
summary(regresion) #no hay significativos

pasess<-pases[,1:11]

pasess <-pases[, c(1:11, 23), drop = FALSE]   
# Comprobar duplicados

duplicados <- duplicated(tiros$Jugador)
sum(duplicados)
tiros_duplicados <- filter(tiros, duplicados)


duplicados <- duplicated(pasess$Jugador)
sum(duplicados)
pases_duplicados <- filter(pasess, duplicados)

# Unir data frames
library(tidyverse)
tabla_completa<- tiros %>% inner_join(pasess, by ="RL")

"
Método del join, lo hace por 'RL'.
Hay jugadores que han cambiado de equipo y aparecen repetidos.
Realizando el join con 'Jugador' el número de observaciones cambiaba porque se repetian.
"

defensa <- defensa[, c(1:9, 16), drop = FALSE]
tabla_completa<- tabla_completa%>% inner_join(defensa,by="RL")


colnames(defensa)

porteros<- porteros[,1:21]
porteros<- porteros%>% select(Jugador,GC90,`% Salvadas`,`PaC%`)

tabla_completa<- tabla_completa%>% full_join(porteros,by="Jugador")

tabla_completa$`T/90`

columnas_seleccionadas <- tabla_completa %>%
  select(RL, Jugador, País, Posc, Equipo, Edad, Nacimiento,`90 s.x`, `Gls.`, `T/90`, por_pases_completados, Bloqueos,`Tkl%`,GC90,`% Salvadas`,`PaC%`,Ass)

tabla_completa <- as.data.frame(tabla_completa)

#Guardamos el archivo como datos.Rda
save(columnas_seleccionadas,file="C:/Users/gomar/OneDrive/Escritorio/BIA/4r curs/1r Quatrimestre/Dades no estructurades/Practica/LALIGAAAA/datos.Rda")

