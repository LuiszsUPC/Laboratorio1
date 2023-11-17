# Practica 1

#Librerias que vamos a utilizar
library(readr)
library(stringr)
library(tidyr)
library(dplyr)

# leemos el archivo
epa_http <- read_table("C:/Users/luiszs/Downloads/epa-http/epa-http.csv", 
+     col_names = FALSE)

# PREGUNTA 1
#Pregunta 1.1. Cuales son las dimensiones del dataset cargado (número de filas y columnas)

colum <- NROW(epa_http)
Filas <- NCOL(epa_http)

#Pregunta 1.2. Valor medio de la columna Bytes

media <- mean(epa_http$X7, na.rm=T)
# media <- mean(as.numeric(epa_http$X7), na.rm=T)

#PREGUNTA 2 De las diferentes IPs de origen accediendo al servidor, ¿cuantas pertenecen a una IP claramente educativa (que contenga ".edu")?

NcolumEDU <- NROW(filter(epa_http,grepl("edu",X1)==TRUE))

#PREGUNTA 3 De todas las peticiones recibidas por el servidor cual es la hora en la que hay mayor volumen  de peticiones HTTP de tipo "GET"?
#creamos una nueva variable df2 donde se separa la columna  en dia, hora, minuto, segundo

df2 <- tidyr::separate(eva_http, X2, c("dia","hora","minuto", "segundo"), sep = ":")

#CREAMOS UNA NUEVA VARIABLE Z DONDE SE FILTRAN SOLO LAS SOLICITUDES GET
z<-filter(df2,grepl("GET",X3)==TRUE)
# EN LA MISMA VARIABLE SACAMOS LA MODA DE CADA UNO DE LOS RANGOS DE HORAS
z<-z %>% count(hora)

#filtramos la hora de mayor uso 

Preg3 <- z %>% filter(n==max(z$n))

#PREGUNTA 4 De las peticiones hechas por instituciones educativas (.edu), ¿Cuantos bytes en total se  han transmitido, en peticiones de descarga de ficheros de texto ".txt"?

# filtramos en la variable edu todas las direcciones web que tienen .edu en su dominio

edu <- filter(epa_http,grepl("edu",X1)==TRUE)

# filtramos en la variable TXT todas las direcciones que descarguen archivos .txt
#TXT <- filter(edu,grepl(".txt",X4)==TRUE)
TXT <- filter (edu,substr(edu$X4, nchar(edu$X4)-3, nchar(edu$X4))==".txt")
Preg4 <- sum(TXT$X7, na.rm = TRUE)

#Pregunta 5 Si separamos la petición en 3 partes (Tipo, URL, Protocolo), usando str_split y el  separador " " (espacio), ¿cuantas peticiones buscan directamente la URL = "/"?

temp <- filter(epa_http, X4=="/")

Preg5<- NROW(temp)


#Pregunta 6 Aprovechando que hemos separado la petición en 3 partes (Tipo, URL, Protocolo) ¿Cuantas peticiones NO tienen como protocolo "HTTP/0.2"?

temp2 <- filter(epa_http, substr(X5, 1, 8)!="HTTP/0.2")

Preg6<- NROW(temp2)