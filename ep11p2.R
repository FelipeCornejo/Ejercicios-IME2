library(dplyr)
library ( tidyverse )
library ( ggpubr )
library(ggplot2)
library(readxl)
library(boot)
install.packages('simpleboot')
library(simpleboot)

datos <- read_xls(choose.files())

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

hs_metropolitana <- filter(datos2, region == "Región Metropolitana de Santiago", sexo == "Hombre", ecivil == "Soltero(a)")
hs_metropolitana <- mutate(hs_metropolitana, pib = ytotcorh / as.integer(numper)) %>% na.omit(hs_metropolitana)

hs_valparaiso <- filter(datos2, region == "Región de Valparaíso", sexo == "Hombre", ecivil == "Soltero(a)")
hs_valparaiso <- mutate(hs_valparaiso, pib = ytotcorh / as.integer(numper))

hs_biobio <- filter(datos2, region == "Región del Biobío", sexo == "Hombre", ecivil == "Soltero(a)")
hs_biobio <- mutate(hs_biobio, pib = ytotcorh / as.integer(numper))

#Muestra de tamaño n = 450
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

cat("Distribución bootstrap entre region m y region v:\n")
cat ("\tMedia :", mean(valoresmv$valoresmv,na.rm = TRUE),"\n")
cat ("\tDesviación estándar:", sd(valoresmv$valoresmv,na.rm = TRUE) , "\n\n")

cat("Distribución bootstrap entre region m y region b:\n")
cat ("\tMedia :", mean(valoresmb$valoresmb,na.rm = TRUE),"\n")
cat ("\tDesviación estándar:", sd(valoresmb$valoresmb,na.rm = TRUE) , "\n\n")

cat("Distribución bootstrap entre region v y region b:\n")
cat ("\tMedia :", mean(valoresvb$valoresvb,na.rm = TRUE),"\n")
cat ("\tDesviación estándar:", sd(valoresvb$valoresvb,na.rm = TRUE) , "\n\n")

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