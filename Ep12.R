library(scatterplot3d)
library( leaps )
library(car)
#por mientras
set.seed(1234)
datos <- read.csv("/Users/macbookair/Downloads/body.csv", sep="")
modelo <- lm(Weight ~ Height + Age + Hip.Girth + Chest.Girth, data = datos )
print(summary(modelo))
#condiciones para implementar RLM
#1. La distribución de los residuos debe ser cercana a la normal.
cat("\ nPrueba de normalidad para los residuos :\n")
print( shapiro.test(modelo$residuals))
#2. La variabilidad de los residuos debe ser aproximadamente constante.
cat(" Prueba de homocedasticidad para los residuos :\n")
print(ncvTest( modelo ))
#3. Los residuos deben ser independientes entre sí.
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
