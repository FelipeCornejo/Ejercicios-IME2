#Integrantes
#Nombre: Sofia Castro; RUT: 20.055.286-5
#Nombre: Felipe Cornejo; RUT: 20.427.782-6
#Nombre: Gianfranco Piccinini; Rut: 20.237.081-0
library(dplyr)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(readxl)
library(boot)
install.packages('simpleboot')
library(simpleboot)
datosI <- readxl::read_xls("/Users/macbookair/Downloads/Datos-Casen-v2.xls")
datosS <- datosI %>% select(esc,s4)
datosS12 <-datosS %>% filter(esc==12)
datosS12 <-datosS12 %>% select(s4)
datosS18 <- datosS %>% filter(esc==18)
datosS18 <- datosS18 %>% select(s4)

#El nivel de escolaridad de las personas, o sea la cantidad de años de estudios
#se relaciona con la cantidad de hijos que ha tenido. 
#Verificar que el que posee 12 años de estudios posee mayor cantidad de hijos que 
#una persona con 18 años de estudios.
#Se estipula un alfa de 0.05
#H0: No hay diferencia de medias entre las muestras.
#u12-u18=0
#HA: Existe una diferencia entre las muestras, donde la muestra con 12 años posee
#mayor cantidad de hijos que 18 años de escolaridad.
#u12-u18>0
datosS12[is.na(datosS12) ]<- 0
datosS18[is.na(datosS18) ]<- 0
set.seed(310)
R<-1000
n1<-sample (c(250:500), size=1, replace =F)
n2<-sample (c(250:500), size=1, replace =F)
muestra1 <- as.numeric(sample(datosS12[["s4"]],size=n1))
muestra1[is.na(muestra1) ]<- 0
muestra2 <- sample(datosS18[["s4"]],size=n2)
observado <-  mean(muestra1)-mean(muestra2)
distribucion <- c(1:R)
for (i in 1: R ) {
  n_1 <-length( muestra1)
  n_2 <-length( muestra2)
  permutacion <- sample(c( muestra1, muestra2) , size = n_1 + n_2,
                        replace = FALSE )
  permutacion1 <- permutacion [1 : n_1]
  permutacion2 <- permutacion [n_1 + 1 : n_2]
  distribucion[i] <- mean(permutacion1)-mean(permutacion2)
}

numerador <- sum( distribucion> observado ) + 1
denominador <- R + 1
valorp <- numerador / denominador

cat (" Valor p:", valorp, "\n\n")
observaciones <- data.frame( distribucion )
histograma <- gghistogram(observaciones,x="distribucion",
                          xlab = "Media",
                          ylab = " Frecuencia ",bins = 30)

qq <- ggqqplot (observaciones,x ="distribucion")

print(qq)
print(histograma)
#En los graficos se puede observar una distribucion normal en las permutaciones generadas.
#Dado que el  p obtenido es menor al alfa estipulado, se rechaza la hipotesis nula
#en favor de la hipotesis alternativa, por lo que las personas con 12 años de escolaridad
#poseen mayor cantidad de hijos que los de 18 años de escolaridad.

datos <- readxl::read_xls("/Users/macbookair/Downloads/Datos-Casen-v2.xls")

#PREGUNTA 2: Bootstrapping
#El ingreso per capita es similar en hombres solteros de la region metropolitana
#,de la region de valparaiso y de la region del biobio

#n entre 400 y 600

#H0: Las 3 medias se distribuyen de manera similar entre si
#HA: Las 3 medias se distribuyen de manera diferente entre si

B <- 2000
alfa <- 0.05
set.seed(523)

datos2 <- select(datos,region,sexo,ecivil,ytotcorh,numper)

hs_metropolitana <- filter(datos2, region == "Regi�n Metropolitana de Santiago", sexo == "Hombre", ecivil == "Soltero(a)")
hs_metropolitana <- mutate(hs_metropolitana, pib = ytotcorh / as.integer(numper)) %>% na.omit(hs_metropolitana)

hs_valparaiso <- filter(datos2, region == "Regi�n de Valpara�so", sexo == "Hombre", ecivil == "Soltero(a)")
hs_valparaiso <- mutate(hs_valparaiso, pib = ytotcorh / as.integer(numper))

hs_biobio <- filter(datos2, region == "Regi�n del Biob�o", sexo == "Hombre", ecivil == "Soltero(a)")
hs_biobio <- mutate(hs_biobio, pib = ytotcorh / as.integer(numper))

#Muestra de tamao n = 450
hs_metropolitana2 <- sample(hs_metropolitana$pib, size = 450)
hs_valparaiso2 <- sample(hs_valparaiso$pib, size = 450)
hs_biobio2 <- sample(hs_biobio$pib, size = 450)

#SHAPIRO TEST
print(shapiro.test(hs_metropolitana2))
print(shapiro.test(hs_valparaiso2))
print(shapiro.test(hs_biobio2))

#Segun los resultados del shapiro test, las tres muestras observadas estan alejadas de una distribucion normal

#DIFERENCIA DE MEDIAS
media_m <- mean(hs_metropolitana2, na.rm = TRUE)
media_v <- mean(hs_valparaiso2)
media_b <- mean(hs_biobio2)

#La media "media_m" es considerablemente mas grande que las otras 2

diffmv <- media_m - media_v
diffmb <- media_m - media_b
diffvb <- media_v - media_b

#BOOTSTRAPPING
bootstrapmv <- two.boot(hs_metropolitana2,hs_valparaiso2, FUN = mean, R = B)
valoresmv <- data.frame(bootstrapmv$t)
colnames(valoresmv) <- "valoresmv"

bootstrapmb <- two.boot(hs_metropolitana2,hs_biobio2, FUN = mean, R = B)
valoresmb <- data.frame(bootstrapmb$t)
colnames(valoresmb) <- "valoresmb"

bootstrapvb <- two.boot(hs_valparaiso2,hs_biobio2, FUN = mean, R = B)
valoresvb <- data.frame(bootstrapvb$t)
colnames(valoresvb) <- "valoresvb"

#HISTOGRAMA
histogramamv <- gghistogram(valoresmv, x = "valoresmv", color = "red",
                            fill= "red", bins = 100, 
                            xlab = "Diferencia de medias", ylab = "Frecuencia",
                            add = "mean")
histogramamb <- gghistogram(valoresmb, x = "valoresmb", color = "red",
                            fill= "red", bins = 100, 
                            xlab = "Diferencia de medias", ylab = "Frecuencia",
                            add = "mean")
histogramavb <- gghistogram(valoresvb, x = "valoresvb", color = "red",
                            fill= "red", bins = 100, 
                            xlab = "Diferencia de medias", ylab = "Frecuencia",
                            add = "mean")
print(histogramamv)
print(histogramamb)
print(histogramavb)

#GRAFICO QQ
qqmv <- ggqqplot(valoresmv, x = "valoresmv", color = "red")
qqmb <- ggqqplot(valoresmb, x = "valoresmb", color = "red")
qqvb <- ggqqplot(valoresvb, x = "valoresvb", color = "red")

print(qqmv)
print(qqmb)
print(qqvb)

cat("Distribuci�n bootstrap entre region m y region v:\n")
cat ("\tMedia :", mean(valoresmv$valoresmv,na.rm = TRUE),"\n")
cat ("\tDesviaci�n est�ndar:", sd(valoresmv$valoresmv,na.rm = TRUE) , "\n\n")

cat("Distribuci�n bootstrap entre region m y region b:\n")
cat ("\tMedia :", mean(valoresmb$valoresmb,na.rm = TRUE),"\n")
cat ("\tDesviaci�n est�ndar:", sd(valoresmb$valoresmb,na.rm = TRUE) , "\n\n")

cat("Distribuci�n bootstrap entre region v y region b:\n")
cat ("\tMedia :", mean(valoresvb$valoresvb,na.rm = TRUE),"\n")
cat ("\tDesviaci�n est�ndar:", sd(valoresvb$valoresvb,na.rm = TRUE) , "\n\n")

#INTERVALOS DE CONFIANZA
intervalomv <- boot.ci(bootstrapmv, conf = 1-alfa, type = "norm")
intervalomb <- boot.ci(bootstrapmb, conf = 1-alfa, type = "norm")
intervalovb <- boot.ci(bootstrapvb, conf = 1-alfa, type = "norm")

print(intervalomv)
print(intervalomb)
print(intervalovb)

#A partir de los resultados entregados por los histogramas y los graficos qq:
#-Las medias m y v tienen una distribucion distinta de la normal (inclinada a la izquierda)
#-Las medias m y b tienen una distribucion normal
#-Las medias v y b tienen una distribucion distinta de la normal (inclinada a la derecha)
#Por lo tanto, las medias se distribuyen de manera diferente entre si, entonces se rechaza H0.

