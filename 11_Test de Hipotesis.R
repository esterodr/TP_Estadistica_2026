########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################




## Test de Hipótesis

#' En este ejercicio lanzaremos un millón de veces un dado supuestamente equilibrado, 
#' calculando la proporción de veces que salen el número 1 y el 6. 
#' Casi con seguridad, las proporciones obtenidas no serán exactamente iguales, 
#' por lo que realizaremos un Test de Hipótesis para diferencia de Proporciones para 
#' ver si podemos confiar en que el dado estaba efectivamente equilibrado. Las hipótesis 
#' del test son las siguientes
#' H_0:p_1-p_6=0
#' H_1:p_1-p_6≠0

#' Hagamos la simulación

###### Ejecutar desde aqui #################################
library(tidyverse)
lanzamientos_1m <- sample(c(1:6), 1000000, replace = TRUE)
prop_1 <-  mean(lanzamientos_1m==1)
prop_6 <-  mean(lanzamientos_1m==6)
###### Hasta aqui ##########################################

#' ¿Qué supuestos son necesarios para realizar el test de hipótesis? ¿Se cumplen? 

#' ¿Qué distribución utilizará? 
 
#' En este test se supone que ambas proporciones son iguales, para lo cual es 
#' necesario calcular un promedio ponderado de las proporciones muestrales. 
#' ¿Cuál es el valor de p̂_0?


###### Ejecutar desde aqui #################################
prop_0 <-   (prop_1+prop_6)*1000000/2000000
prop_0
###### Hasta aqui ##########################################


#' ¿Cuál es el z observado correspondiente al resultado de la simulación?


###### Ejecutar desde aqui #################################
z_obs <- (prop_1-prop_6)/sqrt(2*prop_0*(1-prop_0)/1000000)
z_obs
###### Hasta aqui ##########################################


#' Cuál es el p-value de este resultado


###### Ejecutar desde aqui #################################
p_value <- ifelse(z_obs<0,2*pnorm(z_obs),2*pnorm(z_obs,lower.tail = FALSE))
p_value
###### Hasta aqui ##########################################


#' En base a la respuesta anterior, considere si hay evidencia suficiente 
#' para rechazar o no la hipótesis nula. Justifique en interprete en el 
#' contexto del problema.


## Parte b

#' Haremos lo mismo que antes, pero esta vez con un dado cargado.
#' Luego, realizaremos un Test de Hipótesis para diferencia 
#' de Proporciones con las siguientes hipótesis:
#' H_0:p_1-p_6=0
#' H_1:p_1-p_6≠0

#' ¿Cuáles fueron la proporción muestrales de unos y seis 
#' que se obtuvieron en el Ejercicio 8.d)?


###### Ejecutar desde aqui #################################
lanzamientos_1mb <- sample(c(1:6), 1000000, prob = c(0.16,0.16,0.16,0.16,0.16,0.20), replace = TRUE)
prop_1 <- mean(lanzamientos_1mb==1)
prop_6 <- mean(lanzamientos_1mb==6)
###### Hasta aqui ##########################################


#' ¿Qué supuestos son necesarios para realizar el test de hipótesis? ¿Se cumplen? 

#' ¿Qué distribución utilizará? 

#' En este test se supone que ambas proporciones son iguales, para lo cual es 
#' necesario calcular un promedio ponderado de las proporciones muestrales. 
#' ¿Cuál es el valor de p̂_0?


###### Ejecutar desde aqui #################################
prop_0 <-   (prop_1+prop_6)*1000000/2000000
prop_0
###### Hasta aqui ##########################################


#' ¿Cuál es el z observado correspondiente al resultado de la simulación?


###### Ejecutar desde aqui #################################
z_obs <- (prop_1-prop_6)/sqrt(2*prop_0*(1-prop_0)/1000000)
z_obs
###### Hasta aqui ##########################################


#' Cuál es el p-value de este resultado


###### Ejecutar desde aqui #################################
p_value <- ifelse(z_obs<0,2*pnorm(z_obs),2*pnorm(z_obs,lower.tail = FALSE))
p_value
###### Hasta aqui ##########################################


#' En base a la respuesta anterior, considere si hay evidencia suficiente 
#' para rechazar o no la hipótesis nula. Justifique en interprete en el 
#' contexto del problema.

