########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################




## Intervalos de Confianza

#' En la primera parte de este ejercicio, vamos a estimar la probabilidad
#' de que salga el 1 de espadas en una mano de truco.
#' 
#' En primer lugar, calcularemos la probabilidad teórica. Luego, realizaremos una simulación
#' y analizaremos si esta simulación avala lo calculado analíticamente.
#' 
#' Si de un mazo de 40 cartas sacamos una de ellas, la probabilidad de que sea el 1
#' de espadas es 1/40. Pero cada mano consta de 3 cartas, por lo que el 1 de espada
#' puede salir en la primera, la segunda o la tercer carta. Calculemos el complemento:
#' la probabilidad de que ninguna de las 3 cartas sea el 1 de espadas es:
#' (39/40)x(38/39)x(37/38)
#' Por lo tanto, la probabilidad de que salga el 1 de espadas es 1 - (39/40)x(38/39)x(37/38) 

###### Ejecutar desde aqui #################################
library(tidyverse)
prob_teorica <- (1 - (39/40)*(38/39)*(37/38))
cat("La probabilidad teórica de que salga el 1 de espadas es",  )
###### Hasta aqui ##########################################
                                                                
#' Ahora simularemos 1 millón de manos y contaremos la cantidad
#'  de manos que contienen al 1 de espadas:

#' *(La simulación puede durar unos segundos, esperar un ratito antes de continuar)*.

###### Ejecutar desde aqui #################################
mazo <- paste(c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4),
                rep(6,4), rep(7,4), rep("Sota",4), rep("Caballo",4), rep("Rey",4)),
              c("Oros", "Espadas", "copas", "Bastos"))
repeticiones <- 1000000
manos <- replicate(repeticiones, {
  mano <- sample(mazo, 3, replace=FALSE)
  manos <- mano[order(mano)] }) 
carta_buscada <- t(manos) %>%
  as.data.frame() %>%
  filter(V1 == "1 Espadas" | V2 == "1 Espadas" | V3 == "1 Espadas")
cat("La cantidad de manos simuladas en las que salió el 1 de espadas es:",nrow(carta_buscada)) 

manos_con_1_de_espadas <- nrow(carta_buscada)
###### Hasta aqui ##########################################


#' De esta forma, a partir de esa muestra obtuvimos una proporción muestral con el siguiente valor:

###### Ejecutar desde aqui #################################
n <- repeticiones
p_muestral <- manos_con_1_de_espadas/n
p_muestral
###### Hasta aqui ##########################################


#' Este resultado muestral, ¿avala o rechaza la idea de que el verdadero valor 
#' de la proporción es p=0,075? Para dar una respuesta formal a esta pregunta, 
#' utilicemos el resultado muestral para construir intervalos de confianza para 
#' la proporción poblacional. Utilizaremos niveles de confianza del 90%, 95% y 99%.

#' ¿Qué supuestos son necesarios para construir el intervalo de confianza? ¿Se cumplen? 
  
#' ¿Qué distribución utilizará? 
  
#' ¿Cuáles son los valores críticos (z o t) para cada nivel de significatividad?
  
#' ¿Cuál es el margen de error para cada nivel de confianza?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
ME_90 <- qnorm(0.95)*sqrt(p_muestral*(1-p_muestral)/n)
ME_95 <- qnorm(0.975)*sqrt(p_muestral*(1-p_muestral)/n)
ME_99 <- qnorm(0.995)*sqrt(p_muestral*(1-p_muestral)/n)
paste("El ME para el IC al 90% es:",ME_90)
paste("El ME para el IC al 95% es:",ME_95)
paste("El ME para el IC al 99% es:",ME_99)
###### Hasta aqui ##########################################


#' ¿Cuál es el intervalo de confianza para la proporción poblacional al 
#' 90%, 95% y 99% respectivamente?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
paste("El IC al 90% es: (",p_muestral-ME_90," , ", p_muestral+ME_90,")")
paste("El IC al 95% es: (",p_muestral-ME_95," , ", p_muestral+ME_95,")")
paste("El IC al 99% es: (",p_muestral-ME_99," , ", p_muestral+ME_99,")")
###### Hasta aqui ##########################################


#' ¿Se encuentra el valor teórico contenido por estos intervalos de confianza?
  
#' En base a los resultados obtenidos, ¿considera que la probabilidad teórica de 
#' obtener un 1 de espadas en una mano de tres cartas estuvo bien calculada?
  

##' Parte b.

#' En el Ejercicio 2 Parte c del TP se simuló el lanzamiento de 
#' un millón de monedas y se calculó la proporción de veces que se obtuvo una 
#' “cara” como resultado. Como se suponía que la moneda estaba equilibrada, 
#' la teoría sugiere que se debería obtener una proporción cercana a p=0,5.

#' ¿Cuál fue la proporción muestral de caras que se obtuvo en el Ejercicio 2 Parte c?
  
# COMPLETAR CON EL RESULTADO DEL EJERCICIO 2c DE LA PRIMERA PARTE DEL TP


###### Ejecutar desde aqui #################################
p_muestral <-      ### COMPLETAR
###### Hasta aqui ##########################################


#' Este resultado muestral, ¿avala o rechaza la idea de que la moneda está equilibrada? 
#' Para dar una respuesta formal a esta pregunta, utilicemos el resultado muestral para 
#' construir intervalos de confianza para la proporción poblacional. Utilizaremos niveles 
#' de confianza del 90%, 95% y 99%.

#' ¿Qué supuestos son necesarios para construir el intervalo de confianza? ¿Se cumplen? 

#' ¿Qué distribución utilizará? 

#' ¿Cuáles son los valores críticos (z o t) para cada nivel de significatividad?

#' ¿Cuál es el margen de error para cada nivel de confianza?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
ME_90 <- qnorm(0.95)*sqrt(p_muestral*(1-p_muestral)/n)
ME_95 <- qnorm(0.975)*sqrt(p_muestral*(1-p_muestral)/n)
ME_99 <- qnorm(0.995)*sqrt(p_muestral*(1-p_muestral)/n)
paste("El ME para el IC al 90% es:",ME_90)
paste("El ME para el IC al 95% es:",ME_95)
paste("El ME para el IC al 99% es:",ME_99)
###### Hasta aqui ##########################################


#' ¿Cuál es el intervalo de confianza para la proporción poblacional al 
#' 90%, 95% y 99% respectivamente?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
paste("El IC al 90% es: (",p_muestral-ME_90," , ", p_muestral+ME_90,")")
paste("El IC al 95% es: (",p_muestral-ME_95," , ", p_muestral+ME_95,")")
paste("El IC al 99% es: (",p_muestral-ME_99," , ", p_muestral+ME_99,")")
###### Hasta aqui ##########################################


#' ¿Se encuentra el valor teórico contenido por estos intervalos de confianza?

#' En base a los resultados obtenidos, ¿considera que la moneda efectivamente 
#' era equilibrada?



##' Parte c

#' En el Ejercicio 2 Parte d del TP se simuló el lanzamiento de un 
#' millón de monedas y se calculó la proporción de veces que se obtuvo una “cara” 
#' como resultado. Se suponía que la moneda estaba cargada a favor de las caras, 
#' es decir, que la proporción teórica de caras que se debería obtener es mayor al 50%.
#' Veamos si los resultados obtenidos avalan este supuesto.

#' ¿Cuál fue la proporción muestral de caras que se obtuvo en el Ejercicio 2 Parte d?
  

# COMPLETAR CON EL RESULTADO DEL EJERCICIO 2d DE LA PRIMERA PARTE DEL TP


###### Ejecutar desde aqui #################################
p_muestral <-      ### COMPLETAR
  ###### Hasta aqui ##########################################


#' Este resultado muestral, ¿avala o rechaza la idea de que la moneda está cargada? 
#' Para dar una respuesta formal a esta pregunta, utilicemos el resultado muestral para 
#' construir intervalos de confianza para la proporción poblacional. Utilizaremos niveles 
#' de confianza del 90%, 95% y 99%.

#' ¿Qué supuestos son necesarios para construir el intervalo de confianza? ¿Se cumplen? 

#' ¿Qué distribución utilizará? 

#' ¿Cuáles son los valores críticos (z o t) para cada nivel de significatividad?

#' ¿Cuál es el margen de error para cada nivel de confianza?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
ME_90 <- qnorm(0.95)*sqrt(p_muestral*(1-p_muestral)/n)
ME_95 <- qnorm(0.975)*sqrt(p_muestral*(1-p_muestral)/n)
ME_99 <- qnorm(0.995)*sqrt(p_muestral*(1-p_muestral)/n)
paste("El ME para el IC al 90% es:",ME_90)
paste("El ME para el IC al 95% es:",ME_95)
paste("El ME para el IC al 99% es:",ME_99)
###### Hasta aqui ##########################################


#' ¿Cuál es el intervalo de confianza para la proporción poblacional al 
#' 90%, 95% y 99% respectivamente?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
paste("El IC al 90% es: (",p_muestral-ME_90," , ", p_muestral+ME_90,")")
paste("El IC al 95% es: (",p_muestral-ME_95," , ", p_muestral+ME_95,")")
paste("El IC al 99% es: (",p_muestral-ME_99," , ", p_muestral+ME_99,")")
###### Hasta aqui ##########################################


#' En base a los resultados obtenidos, ¿considera que la moneda efectivamente estaba 
#' cargada a favor de las caras?

