library(scatterplot3d)
library(dplyr)
library(ggpubr)
library( leaps )
library(car)
#PASO 1
set.seed(7782)

#PASO 2
datos <- read.csv("/Users/littl/Downloads/body.csv", sep=";")
filtro_w <- datos %>% filter(datos[["Gender"]] == 0)
muestra <- filtro_w[sample(nrow(filtro_w), 50),]

#PASO 3

header <- colnames(muestra)
#Se eliminan del header el cual se utilizará para escoger 8 variables aleatorias
#Ya que de la muestra son todas mujeres y no se puede utilizar el peso para predecir el peso
header <- header[header != "Gender"];
header <- header[header != "Weight"];
header <- sample(header,8)

#PASO 4
#Se hará uso de la variable Wrists.diameter debido a multiples alusiones a sobre determinar obesidad
#en base a estas:
#https://www.biobiochile.cl/noticias/2014/11/28/como-calcular-el-peso-ideal-utilizando-la-medida-de-la-muneca.shtml
#https://vanguardia.com.mx/vida/cuanto-mide-tu-muneca-el-resultado-hablara-de-la-composicion-de-tu-cuerpo-KPVG3341029

#Junto con la observación de que gente muy flaca tiene las muñecas huesudas y gente sobre peso las tiene hinchadas.

#PASO 5
modelo_simple <- lm(Weight ~ Wrists.diameter, data=muestra)
print(summary(modelo_simple))

#Los datos deben presentar una relacion lineal
cat("\ nPrueba de normalidad para los residuos :\n")
print( shapiro.test(modelo_simple$residuals))
#Los residuos siguen una distribución normal

#La variabilidad de los residuos debe ser aproximadamente constante.
cat(" Prueba de homocedasticidad para los residuos :\n")
print(ncvTest( modelo_simple ))

#Los residuos deben ser independientes entre sÃ.
cat("Prueba de Durbin - Watson para autocorrelaciones ")
cat("entre errores :\n")
print( durbinWatsonTest( modelo_simple ))

#4. Cada variable se relaciona linealmente con la respuesta.
p <- ggscatter ( muestra , x = "Wrists.diameter", y = "Weight", color = "blue", fill = "blue",
                 xlab = "Diametro de la muñeca [cm]", ylab = "Peso [kg]")

p <- p + geom_smooth ( method = lm , se = FALSE , colour = "red")
print ( p )

coef_corre <- cor(muestra$Wrists.diameter, muestra$Weight)




modelo <- lm(Weight ~ Height + Age + Hip.Girth + Chest.Girth, data = datos)

print(summary(modelo))






#condiciones para implementar RLM
#1. La distribuciÃ³n de los residuos debe ser cercana a la normal.
cat("\ nPrueba de normalidad para los residuos :\n")
print( shapiro.test(modelo$residuals))
#2. La variabilidad de los residuos debe ser aproximadamente constante.
cat(" Prueba de homocedasticidad para los residuos :\n")
print(ncvTest( modelo ))
#3. Los residuos deben ser independientes entre sÃ.
cat("Prueba de Durbin - Watson para autocorrelaciones ")
cat("entre errores :\n")
print( durbinWatsonTest( modelo ))
#4. Cada variable se relaciona linealmente con la respuesta.
vifs <- vif( modelo )
cat("\ nVerificar la multicolinealidad :\n")
cat("- VIFs :\n")
print( vifs )
cat("- Tolerancias :\n")
print(1 / vifs )
cat("- VIF medio :", mean(vifs), "\n")

predictores <- names(coef(modelo ))[ -1]
datos <- datos[,c(predictores , "Height")]
resultados <- data.frame( respuesta_predicha = fitted(modelo))
resultados[["residuos_estandarizados"]] <- rstandard(modelo)
resultados[["residuos_estudiantizados"]] <- rstudent(modelo)
resultados[["distancia_Cook"]] <- cooks.distance(modelo)
resultados[["dfbeta"]] <- dfbeta( modelo )
resultados[["dffit"]] <- dffits( modelo )
resultados[["apalancamiento"]] <- hatvalues( modelo )
resultados[["covratio"]] <- covratio( modelo )
sospechosos1 <- which(abs(resultados[["residuos_estandarizados"]]) > 1.96)

#5. 