library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
getwd()

#Marco Geoestadístico de INEGI
#https://www.inegi.org.mx/temas/mg/#Descargas

#-----------------------------------------------------
#------------------ejemplo 1: CDMX--------------------
#-----------------------------------------------------

#carga mapa CDMX Municipios
library(sf)
#carga shape file (mapa) *.shp
cdmx_mapa<-st_read("/Users/ubaldomtz/Desktop/Nosotricos/Sesion 6/09_ciudaddemexico/conjunto_de_datos/09mun.shp")

View(cdmx_mapa)
plot(cdmx_mapa$geometry, col = "gray")
names(cdmx_mapa)
class(cdmx_mapa$CVE_MUN)
levels(cdmx_mapa$CVE_MUN)

#preparacion de los datos:
  datos<-read.csv("capacidades_diferentes.csv")
  head(datos,6)
  names(datos)

  library(reshape)
  #agrupa por clave de municipio, sexo a total de caminar_moverse
  cast(datos,CVE_MUN~grupo_edad, value = "caminar_moverse", fun.aggregate = sum)
  temp<-cast(datos,CVE_MUN~sexo, value = "caminar_moverse", fun.aggregate = sum)

  #cambia los nombres de las columnas
  names(temp)
  names(temp)[names(temp) == "Hombres"] <- "caminar_moverse_H"
  names(temp)[names(temp) == "Mujeres"] <- "caminar_moverse_M"
  names(temp)[names(temp) == "Total"] <- "caminar_moverse_T"
  temp

  #prepara la CVE_MUN para unir los datos
  temp$CVE_MUN
  class(temp$CVE_MUN)

  library(stringr)
  #str_pad agrega un caracter (pad), hasta completar la longitud deseada
  temp$CVE_MUN<-str_pad(temp$CVE_MUN, 3, pad = "0")
  temp$CVE_MUN<-as.factor(as.character(temp$CVE_MUN))
  levels(temp$CVE_MUN)

  #library(dplyr)
  #temp$CVE_MUN<- recode_factor(temp$CVE_MUN,"10"="010", "11"="011", "12"="012",
  #                           "13"="013","14"="014", "15"="015", "16"="016",
  #                           "17"="017", "2"="002", "3"="003", "4"="004",
  #                           "5"="005","6"="006", "7"="007", "8"="008", "9"="009")

#agrega datos al mapa
cdmx_mapa<-merge(cdmx_mapa,temp,by = "CVE_MUN")
rm(temp)
names(cdmx_mapa)

#Graficas de los mapas
library(tmap)
qtm(cdmx_mapa)

tm_shape(cdmx_mapa) + tm_borders() + tm_fill() #mapa basico
tm_shape(cdmx_mapa) + tm_polygons("caminar_moverse_M") #mapa con una variable *MAPA RECOMENDADO*

#paletas de colors disponibles en R
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(5, "GnBu")
display.brewer.pal(3, "PuRd")

tm_shape(cdmx_mapa) + tm_polygons("caminar_moverse_T")
tm_shape(cdmx_mapa) + tm_polygons("caminar_moverse_T", palette = "GnBu")
tm_shape(cdmx_mapa) + tm_polygons("caminar_moverse_T", border.col="Blue")
tm_shape(cdmx_mapa) + tm_polygons("caminar_moverse_T", palette = "GnBu", border.alpha=0.1)

cdmx_mapa$caminar_moverse_H<-as.numeric(as.character(cdmx_mapa$caminar_moverse_H))
cdmx_mapa$caminar_moverse_M<-as.numeric(as.character(cdmx_mapa$caminar_moverse_M))
cdmx_mapa$caminar_moverse_T<-as.numeric(as.character(cdmx_mapa$caminar_moverse_T))

tm_shape(cdmx_mapa) +
  tm_polygons("caminar_moverse_H", palette = "GnBu", style = "quantile", n=6)

#summary(cdmx_mapa$caminar_moverse_M)
tm_shape(cdmx_mapa) +
  tm_polygons("caminar_moverse_M", palette = "BuPu", style = "fixed", breaks = c(0,8000,15000,30000,50000))

tm_shape(cdmx_mapa) +
  tm_polygons("caminar_moverse_M", palette = "BuPu", style = "fixed",
              breaks = c(0,10000,30000,50000), labels = c("Bajo", "Medio", "Alto"))

tm_shape(cdmx_mapa) +
  tm_polygons("caminar_moverse_H", palette = "GnBu", style = "quantile", n=6) +
  tm_layout(legend.outside = TRUE)

tm_shape(cdmx_mapa) + tm_polygons() +
  tm_bubbles(size = "caminar_moverse_M", col = "red")

tm_shape(cdmx_mapa) + tm_polygons() +
  tm_bubbles(col = "caminar_moverse_M")

tm_shape(cdmx_mapa) + tm_polygons() +
  tm_bubbles(col = "caminar_moverse_M", style = "quantile", n=6)

#grafica de 2 variables
tm_shape(cdmx_mapa) +
  tm_polygons("caminar_moverse_T", #variable1 - mapa
              title="Población con movilidad reducida", #titulo variable1
              palette = "Blues", #color variable1
              style = "quantile", n=4, labels = c("Bajo", "Medio", "Alto", "Muy alto"),#categorias y etiquetas var1
              border.alpha=0.2) + #% de transparencia en la linea del mapa
  tm_bubbles(col="caminar_moverse_M", #variable2 - burbujas
             title.col="Mujeres", #titulo variable2
             palette="Reds", #color variable2
             style="quantile", n=3,labels=c("Bajo", "Medio", "Alto"), #categorias y etiquetas var2
             border.alpha=0.5) + #% de transparencia en la linea de las burbujas
  tm_layout(legend.outside = TRUE) #coloca la leyenda afuera del cuadro del mapa


rm(datos)
rm(cdmx_mapa)
#-----------------------------------------------------
#------------------ejemplo 2: Veracruz----------------
#-----------------------------------------------------
library(rgdal)
veracruz <- readOGR(dsn="/Users/ubaldomtz/Desktop/Nosotricos/Sesion 6/30_veracruzignaciodelallave/conjunto_de_datos",
                    layer="30mun")
library(tmap)
qtm(veracruz) #mapa simple
View(veracruz@data)
veracruz$CVE_MUN

#*******************************************************
#preparacion de los datos: poblacion con lengua indigena
getwd()
datos<-read.csv("len_indigena_ver.csv")
head(datos,6)
names(datos)

library(reshape)
#agrupa por clave de municipio, sexo a total de caminar_moverse
temp<-cast(datos,CVE_MUN~sexo, value = "lengua_indigena", fun.aggregate = sum)
head(temp,8)

#cambia los nombres de las columnas
names(temp)
names(temp)[names(temp) == "Hombres"] <- "lengua_indigena_H"
names(temp)[names(temp) == "Mujeres"] <- "lengua_indigena_M"
names(temp)[names(temp) == "Total"] <- "lengua_indigena_T"
head(temp,8)

#prepara la CVE_MUN para unir los datos
head(temp$CVE_MUN,10)
class(temp$CVE_MUN)
library(stringr)
temp$CVE_MUN<-str_pad(temp$CVE_MUN, 3, pad = "0")
temp$CVE_MUN<-as.factor(as.character(temp$CVE_MUN))
levels(temp$CVE_MUN)

#agrega datos al mapa
#MUY IMPORTANTE: siempre es primero el mapa y luego los datos a agregar!!!
veracruz<-merge(veracruz,temp,by = "CVE_MUN")
rm(temp)
View(veracruz@data)

#-----------Graficas-----------
library(tmap)
tm_shape(veracruz) + tm_polygons("lengua_indigena_T")

#----alternativa al mapa rapido------
spplot(veracruz, "lengua_indigena_M")

spplot(veracruz, "lengua_indigena_M",
       main = "Mujeres que hablan alguna lengua indígena",
       sub = "Incluye aquellas que también hablan español",
       col = "transparent")
#----------------------------------

#Graficas de los mapas
tm_shape(veracruz) + tm_polygons("lengua_indigena_M", palette = "GnBu")
tm_shape(veracruz) + tm_polygons("lengua_indigena_H", palette = "BuPu")

tm_shape(veracruz) +
  tm_polygons("lengua_indigena_T",
              title="Población que habla alguna lengua indígena",
              palette = "Blues",
              style="quantile", n=4,
              border.alpha = 0.2) +
  tm_bubbles(size = "lengua_indigena_M",
             title.size="Mujeres",
             col="red",
             border.alpha=0.5) +
  tm_layout(legend.outside = TRUE)


 #*******************************************************
#preparacion de los datos: poblacion economicamente activa
getwd()
datos<-read.csv("act_economica_veracruz.csv")
head(datos,6)
names(datos)

View(veracruz@data)
library(reshape)
#agrupa por clave de municipio, sexo a total de caminar_moverse
temp<-cast(datos,CVE_MUN~sexo, value = "EcAc_ocupada", fun.aggregate = sum)

#cambia los nombres de las columnas
names(temp)
names(temp)[names(temp) == "Hombres"] <- "EcAc_ocupada_H"
names(temp)[names(temp) == "Mujeres"] <- "EcAc_ocupada_M"
names(temp)[names(temp) == "Total"] <- "EcAc_ocupada_T"
head(temp,8)

#prepara la CVE_MUN para unir los datos
head(temp$CVE_MUN,10)
class(temp$CVE_MUN)
library(stringr)
temp$CVE_MUN<-str_pad(temp$CVE_MUN, 3, pad = "0")
temp$CVE_MUN<-as.factor(as.character(temp$CVE_MUN))
levels(temp$CVE_MUN)

#agrega datos al mapa
veracruz<-merge(veracruz,temp,by = "CVE_MUN")
rm(temp)
head(veracruz@data,6)

library(tmap)
tm_shape(veracruz) +
  tm_polygons("EcAc_ocupada_M",
              title="Mujeres Económicamente Activas",
              style="quantile", n=9,
              palette = "Blues",
              border.alpha = 0) +
  tm_bubbles(col = "lengua_indigena_M",
             title.col="Mujeres que hablan lengua indígena",
             palette="Reds",
             style="quantile", n=3,
             scale=0.5,
             border.alpha=0) +
  tm_layout(legend.outside = TRUE)

rm(datos)
rm(veracruz)
#----------FIN---------
