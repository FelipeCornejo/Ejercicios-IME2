#Integrantes
#Nombre: Sofia Castro; RUT: 20.055.286-5
#Nombre: Felipe Cornejo; RUT: 20.427.782-6
#Nombre: Gianfranco Piccinini; Rut: 20.237.081-0
library(MASS)
library(dplyr)
library (tidyverse)
library(ggpubr)
library(ez)
datos <- birthwt
peso_raza <- datos %>% select(bwt,race)
peso_raza <- peso_raza %>% arrange(race)
#Dado que nos piden evaluar si existen diferencias significativas entre los pesos de los bebes de distintas
#raza, se deben realizar comparaciones de distintas muestras, lo que implica realizar una prueba ANOVA.
#Condiciones para aplicar ANOVA:
#1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
#iguales.
#2. Se asume que las muestras son obtenidas al azar.
#3. Para comprobar o suponer que las muestras provienen de una población con distribución normal, 
#se aplica el grafico q-q.
g <- ggqqplot (peso_raza,
                x = "bwt",
                y = "race",
                color = "race")
g <- g + facet_wrap (~race)
print (g)
# Se puede visualizar que los muestras tienden a una distribución normal, por lo que se asume que la 
#población tambien lo hace.
#4. Las k muestras tienen varianzas aproximadamente iguales.
muestra1 <- peso_raza %>% filter(race=="1")
var1 <- var(muestra1[,1])
muestra2 <- peso_raza %>% filter(race=="2")
var2 <- var(muestra2[,1])
muestra3 <- peso_raza %>% filter(race=="3")
var3 <- var(muestra3[,1])
cat("Var1")
print(var1)
cat("Var2")
print(var2)
cat("Var3")
print(var3)
dif <- var1/var2
cat("Razon entre variable mayor y menor")
print(dif)
#El resultado correspondiente es 1.298838 <=1.5, por lo que se cumple con lo solicitado.
#H0: No hay diferencias entre las relaciones de peso y raza.
#HA: Si hay diferencias entre las relaciones de peso y raza.
#Como se cumplen con las 4 condicione, se procede a aplicar ANOVA.
#p = 0.05
datos [["race"]] <- factor ( datos [["race" ]])
datos [["instancia"]] <- factor (1: nrow ( datos ))
cat ("\n\ nProcedimiento ANOVA usando ezANOVA \n\n")
prueba1 <- ezANOVA (
  data =datos ,
  dv = bwt,
  wid=instancia,
  between = race,
  return_aov = TRUE )
print(prueba1)
#Tamaño del efecto
g2 <- ezPlot (
  data =datos ,
  dv = bwt,
  wid=instancia,
  between = race,
  x = race)
print(g2)
# El p obtenido por la prueba ANOVA es 0.008336077, siendo menor al alfa estipulado (0.05), ademas el
# grafico del tamaño del efecto nos muestra las diferencias entre los promedios de cada muestra,indicando 
#la distancia entre ellos, por lo que se rechaza la hipotesis nula en favor de la hipotesis alternativa.
#Se concluye que si existen diferencias entre los pesos de los bebes al nacer segun la raza de la madre.


