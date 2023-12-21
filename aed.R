library(ggplot2)
library(tidyverse)
library(plotly)
library(gt)
load("C:/Users/Mario/Desktop/datos.Rda")
datos <- columnas_seleccionadas


# LIMPIEZA DE DATOS -------------------------------------------------------


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
#datos_duplicados <- filter(datos, duplicados)


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


# PRUEBA CORRELACIONES ------------------------------------------------------------------

correlaciones <- datos %>% select(Gls.,`T/90`,Ass,`90 s.x`,por_pases_completados,Bloqueos,`Tkl%`)
cor(correlaciones)
# DATOS GENERALES ---------------------------------------------------------

#Minutos totales jugados
gt(datos %>% select(Jugador, Equipo, `90 s.x`)%>% arrange(desc(`90 s.x`)) %>% head(8)) %>%
  tab_header(
    title = md("**JUGADORES CON MÁS PARTIDOS**"))  %>%
  tab_source_note(
    source_note = "Fuente: Elaboración Propia"
  )
"Mamardashvili, Alex Remiro y David Soria son los únicos que han jugado todos los partidos"

#Asistencias
gt(datos %>% select(Jugador, Equipo, Ass)%>% arrange(desc(Ass)) %>% head(6)) %>%
  tab_header(
    title = md("**TOP ASISTENCIAS**"))  %>%
  tab_source_note(
    source_note = "Fuente: Elaboración Propia"
  )

#Máximos Goleadores
gt(datos %>% select(Jugador, Equipo, Gls.)%>% arrange(desc(Gls.)) %>% head(10)) %>%
  tab_header(
    title = md("**TOP GOLEADORES**"))  %>%
  tab_source_note(
    source_note = "Fuente: Elaboración Propia"
  )

#Jugadores más jóvenes
gt(datos %>% select(Jugador, Equipo, Edad)%>% arrange((Edad)) %>% head(10)) %>%
  tab_header(
    title = md("**JUGADORES MÁS JÓVENES**"))  %>%
  tab_source_note(
    source_note = "Fuente: Elaboración Propia"
  )

#Jugadores más mayores
gt(datos %>% select(Jugador, Equipo, Edad)%>% arrange(desc(Edad)) %>% head(10)) %>%
  tab_header(
    title = md("**MAYOR EDAD**"))  %>%
  tab_source_note(
    source_note = "Fuente: Elaboración Propia"
  )



# PORTEROS ----------------------------------------------------------------

"seleccionamos los porteros que hayan jugado más de 10 partidos"
nrow(porteros)
porteros<-porteros %>% filter(`90 s.x`>10)
nrow(porteros)
#Pasamos de 44 porteros a 27, tan solo el 61% de los porteros han jugado más de 10 partidos

# Ranking goles en contra por 90 minutos
porteros %>% select(Jugador, GC90) %>% arrange(GC90)
hist(porteros$GC90)
plot_ly(x = porteros$GC90, type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>%
  layout(title = "Goles en contra/ 90 minutos", xaxis=list(title="Goles x 90 minutos"), yaxis=list(title="Nº porteros"))

# Ranking % porterias a 0
porteros %>% select(Jugador, `PaC%`) %>% arrange(desc(`PaC%`))%>% head(10)
hist(porteros$`PaC%`)
gt(porteros %>% select(Jugador, Equipo,`PaC%`) %>% arrange(desc(`PaC%`))%>% head(10)) %>%
  tab_header(
    title = md("**TOP 10 PORTEROS**"),
    subtitle = "% PORTERÍA A 0")
# % de Salvadas
porteros %>% select(Jugador, `% Salvadas`) %>% arrange(desc(`% Salvadas`))
hist(porteros$`% Salvadas`)

plot_ly(x = porteros$`% Salvadas`, type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>%
  layout(title = "Distribución % Salvadas", xaxis=list(title="% Salvadas"), yaxis=list(title="Nº porteros"))

# DEFENSAS ----------------------------------------------------------------

defensas<-defensas %>% filter(`90 s.x`>10)

# Minutos jugados
defensas %>% select(Jugador,`90 s.x` ) %>% arrange(desc(`90 s.x`))
hist(defensas$`90 s.x`)

# Bloqueos
defensas %>% select(Jugador, Bloqueos) %>% arrange(desc(Bloqueos)) %>% head(10)
hist(defensas$Bloqueos)

gt(defensas %>% select(Jugador, Equipo,Bloqueos) %>% arrange(desc(Bloqueos)) %>% head(10)) %>%
  tab_header(
    title = md("**TOP 10 DEFENSAS**"),
    subtitle = "Bloqueos totales")
# Tackle 
defensas %>% select(Jugador,Equipo,`Tkl%`) %>% arrange(desc(`Tkl%`)) %>% head(15)
hist(defensas$`Tkl%`)

plot_ly(x = defensas$`Tkl%`, type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>%
  layout(title = "Distribución Tackles", xaxis=list(title="% Tackles"), yaxis=list(title="Nº defensas"))



# Pases completados
defensas %>% select(Jugador, por_pases_completados) %>% arrange(desc(por_pases_completados))
hist(defensas$por_pases_completados)

gt(defensas %>% select(Jugador, Equipo,por_pases_completados) %>% arrange(desc(por_pases_completados)) %>% head(10)) %>%
  tab_header(
    title = md("**TOP 10 DEFENSAS**"),
    subtitle = "Pases")

plot_ly(x=defensas$por_pases_completados,type="histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>%
  layout(title = "Distribución de frecuencia de pases de los defensas")

# MEDIOCENTROS ------------------------------------------------------------
mediocentros<-mediocentros %>% filter(`90 s.x`>10)

#pases
mediocentros %>% select(Jugador,Equipo,por_pases_completados) %>% arrange(desc(por_pases_completados)) %>% head(10)

gt(mediocentros %>% select(Jugador,Equipo,por_pases_completados) %>% arrange(desc(por_pases_completados)) %>% head(10)) %>%
  tab_header(
    title = md("**TOP 10 PASADORES**"),
    subtitle = "% pases compeltados")  %>%
  tab_source_note(
      source_note = md("Nota: Fernando '1', jugador del Sevilla, utilizamos término 1 como diferenciador de Fernando jugador del Almería.")
    )
#top 4 real madrid


mediocentros %>% select(Jugador,Gls.) %>% arrange(desc(Gls.)) %>% head(10)

gt(mediocentros %>% select(Jugador,Equipo,Gls.) %>% arrange(desc(Gls.)) %>% head(10)) %>%
  tab_header(
    title = md("**TOP 10 MEDIOCENTROS GOLEADORES**"),
    subtitle = "Nº goles marcados")

mediocentros$`T/90` <- as.numeric(mediocentros$`T/90`)

plot_ly(x = mediocentros$`T/90`, type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>%
  layout(title = "Distribución Tiros", xaxis=list(title="Tiros/90 minutos"), yaxis=list(title="Nº mediocentros"))

# DELANTEROS --------------------------------------------------------------

delanteros<-delanteros %>% filter(`90 s.x`>10)

delanteros %>% select(Jugador,Equipo,Gls.) %>% arrange(desc(Gls.)) %>% head(15)
#Goles
gt(delanteros %>% select(Jugador,Equipo,Gls.) %>% arrange(desc(Gls.)) %>% head(10)) %>%
  tab_header(
    title = md("**TOP 10 DELANTEROS GOLEADORES**"),
    subtitle = "Nº goles marcados")

# Tiros por 90 minutos

gt(delanteros %>% select(Jugador,Equipo,`T/90`) %>% arrange(desc(`T/90`)) %>% head(10)) %>%
  tab_header(
    title = md("**Tiros por 90 minutos**"))

#Asistencia
gt(delanteros %>% select(Jugador,Equipo,Ass) %>% arrange(desc(Ass)) %>% head(10)) %>%
  tab_header(
    title = md("**TOP 10 ASISTENCIAS**"),
    subtitle = "Nº asistencias")

