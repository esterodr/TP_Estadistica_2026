########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################




##' Fundamentos de la inferencia estadística: Intervalos de Confianza

##' Este ejercicio es una adaptación del incluido en el libro OpenIntro Statistics.
##' La versión original y en inglés del ejercicio se puede consultar en el siguiente vínculo:
##' http://htmlpreview.github.io/?https://github.com/andrewpbray/oiLabs-base-R/blob/master/confidence_intervals/confidence_intervals.html
##' 

##' En este ejercicio se utilizarán nuevamente datos del mercado inmobiliario de la ciudad 
##' de Ames, Iowa, EEUU. En particular, disponemos información de todas las ventas de hogares 
##' ocurridas en esa ciudad entre 2006 y 2010.

##' Si se tiene acceso a toda la población, pueden responderse preguntas tales como:
##' ¿Qué tan grande es una casa típia en Ames? o ¿cuánta variabilidad hay en el tamaño
##' de los hogares? 
##' Si sólo se tiene acceso a una pequeña muestra de los datos, como suele ser el caso,
##' responder a estas preguntas es más complicado. ¿Cuál es nuestra mejor estimación sobre
##' el tamaño de una casa típica en Ames, dado que sólo conocemos el tamaño de unos pocos 
##' hogares? Este tipo de situaciones requiere utilizar las muestras para hacer inferencias
##' sobre la población.

##' Utilizaremos sólo la variable que indica el tamaño del hogar (convertido a metros cuadrados).
##' Esta será nuestra población.

##' Tomamos una muestra de 60 hogares. Como ya se ha visto, se puede hacer una estimación 
##' puntual del promedio poblacional mediante
##' la media muestral. ¿Cuánto da el promedio muestral?


###### Ejecutar desde aqui #################################
library("tidyverse")
load("./archivos/ames.RData")
poblacion <- ames$Gr.Liv.Area*0.0929
muestra <- sample(poblacion, 60)
media_muestral <- mean(muestra)
media_muestral
###### Hasta aqui ##########################################


##' La media muestral es la mejor estimación puntual que podemos obtener, pero sería útil saber
##' también qué tan incierta es esta estimación. Esto puede hacerse mediante un 
##' *intervalo de confianza*

##' Primero calcularemos el error standard (se) para una muestra de 60 hogares:
##' ¿Qué resultado obtuvo?


###### Ejecutar desde aqui #################################
se <- sd(muestra) / sqrt(60)
se
###### Hasta aqui ##########################################


##' Luego, calcularemos un intervalo de confianza al 95% para la media muestral, sumando y
##' restando 1.96 errores standards a la media muestral.

##' ¿Por qué utilizamos el valor de 1.96? ¿Se requiere algún supuesto adicional?

##' Calculamos límites inferior y superior del intervalo
##' ¿Cuáles son los límites del intervalo de confianza?


###### Ejecutar desde aqui #################################
limite_inferior <- media_muestral - 1.96 * se
limite_superior <- media_muestral + 1.96 * se
c(limite_inferior, limite_superior)
###### Hasta aqui ##########################################


##' Se ha dado un paso muy grande para de conocer el tamaño promedio de los hogares en Ames.
##' Si bien no sabemos con exactitud cuál es su valor promedio, tenemos un 95% de confianza de que se
##' encuentre en los valores estimados.
##' 
##' ¿Qué significa que tenemos 95% de confianza?

##' Para satisfacer nuestra curiosidad, ¿cuál es el verdadero valor de la media poblacional? Recuerde
##' que esto sólo lo podemos calcular porque tenemos acceso a la población completa. No suele suceder 
##' esto en la vida real.


###### Ejecutar desde aqui #################################
mean(poblacion)
###### Hasta aqui ##########################################


##' ¿Se encuentra contenido en el intervalo de confianza estimado?

##' Cada estudiante de la clase debe haber obtenido un intervalo de confianza ligeramente
##' diferente. ¿Qué proporción de esos intervalos cree que contendrán la verdadera media
##' poblacional? ¿Por qué?

##' Vamos a simular que la clase tiene 50 estudiantes. De esta forma, vamos a obtener 
##' vamos a obtener 50 muestras distintas, construyendo un intervalo de confianza a partir 
##' de cada una de ellas. Todas las muestras estarán conformadas por 60 hogares
##' seleccionados al azar.



###### Ejecutar desde aqui #################################
medias_muestrales <- rep(NA, 50)
sd_muestrales <- rep(NA, 50)
n <- 60
##' Calculamos media y desvío para cada una de las 50 muestras
for(i in 1:50){
  muestra <- sample(poblacion, n) 
  medias_muestrales[i] <- mean(muestra)    
  sd_muestrales[i] <- sd(muestra)        
}
##' Construimos los intervalos de confianza
limite_inferior <- medias_muestrales - 1.96 * sd_muestrales / sqrt(n)
limite_superior <- medias_muestrales + 1.96 * sd_muestrales / sqrt(n)
##' Calculemos cuántos de estos intervalos contienen al verdadero valor de la media poblacional
contiene <- mean(poblacion)>=limite_inferior & mean(poblacion)<=limite_superior
paste(sum(contiene),"intervalos contienen la verdadera media poblacional")
###### Hasta aqui ##########################################


##' Graficamos los intervalos de confianza. La línea vertical señala el verdadero valor de 
##' la media poblacional (generalmente desconocido).


###### Ejecutar desde aqui #################################
data.frame(muestra = c(1:50), li = limite_inferior, ls = limite_superior, contiene = contiene) %>%
  ggplot() +
  geom_errorbar(aes(xmin = li, xmax = ls, y = muestra, color = contiene), orientation = "y") +
  geom_vline(xintercept = mean(poblacion)) +
  xlab("Metros cuadrados") +
  ylab("Intervalo de Confianza") +
  theme_classic() +
  ggtitle(paste("Intervalos de confianza\n\n", 
                sum(contiene), "intervalos contienen la verdadera media poblacional"))
###### Hasta aqui ##########################################


##' Utilizando las mismas muestras, construyamos intervalos de confianza al 90% y 99%.
##' Para ello, utilizaremos los valores críticos de 1.645 y 2.58.
##' ¿Por qué estos valores?


###### Ejecutar desde aqui #################################
##' Construimos los intervalos de confianza
limite_inferior_90 <- medias_muestrales - 1.645 * sd_muestrales / sqrt(n)
limite_superior_90 <- medias_muestrales + 1.645 * sd_muestrales / sqrt(n)
limite_inferior_99 <- medias_muestrales - 2.58 * sd_muestrales / sqrt(n)
limite_superior_99 <- medias_muestrales + 2.58 * sd_muestrales / sqrt(n)
##' Calculemos cuántos de estos intervalos contienen al verdadero valor de la media poblacional
contiene_90 <- mean(poblacion)>=limite_inferior_90 & mean(poblacion)<=limite_superior_90
paste(sum(contiene_90),"intervalos al 90% de confianza contienen la verdadera media poblacional")
contiene_99 <- mean(poblacion)>=limite_inferior_99 & mean(poblacion)<=limite_superior_99
paste(sum(contiene_99),"intervalos al 99% de confianza contienen la verdadera media poblacional")
###### Hasta aqui ##########################################


##' La cantidad de intervalos que contienen el verdadero valor de la media poblacional difiere
##' cuando construimos un intervalo al 90%, 95% o 99%. ¿Por qué?

##' Graficamos todos los intervalos de confianza juntos para ver las diferencias.
##' Para facilitar la visualización, ordenaremos los intervalos de acuerdo al valor de la
##' media muestral


###### Ejecutar desde aqui #################################
orden <- order(medias_muestrales)
data.frame(muestra = rep(c(1:50),3),
           li=c(limite_inferior_90[orden],limite_inferior[orden],limite_inferior_99[orden]),
           ls=c(limite_superior_90[orden],limite_superior[orden],limite_superior_99[orden]),
           contiene=c(contiene_90[orden],contiene[orden],contiene_99[orden]),
           confianza=c(rep(90,50),rep(95,50),rep(99,50)),
           medias=(rep(medias_muestrales[orden],3))) %>%
  ggplot() +
  geom_errorbar(aes(xmin = li, xmax = ls, y = muestra, color = contiene), orientation = "y") +
  geom_point(aes(x=medias,y=muestra)) +
  geom_vline(xintercept = mean(poblacion)) +
  xlab("Metros cuadrados") +
  ylab("Intervalo de Confianza") +
  theme_classic() +
  facet_wrap(.~confianza, nrow = 3) +
  xlim(110,180)
###### Hasta aqui ##########################################

  
##' ¿Qué diferencias observa entre los tres casos?
##' Relacione con la idea de un trade-off entre precisión y certeza

##' Recordar que cada una de las muestras que estamos tomando se compone de 60 hogares
##' seleccionados aleatoriamente. ¿Qué sucederá si, en lugar de 60 hogares, tomamos muestras de
##' 120 hogares?

##' Hagamos la prueba, repitiendo el ejercicio de construir 50 intervalos para cada nivel de confianza,
##' pero con muestras de tamaño 120 en lugar de 60.


###### Ejecutar desde aqui #################################
medias_muestrales <- rep(NA, 50)
sd_muestrales <- rep(NA, 50)
n <- 120
##' Calculamos media y desvío para cada una de las 500 muestras
for(i in 1:50){
  muestra <- sample(poblacion, n) 
  medias_muestrales[i] <- mean(muestra)    
  sd_muestrales[i] <- sd(muestra)        
}
##' Construimos los intervalos de confianza
limite_inferior <- medias_muestrales - 1.96 * sd_muestrales / sqrt(n)
limite_superior <- medias_muestrales + 1.96 * sd_muestrales / sqrt(n)
limite_inferior_90 <- medias_muestrales - 1.645 * sd_muestrales / sqrt(n)
limite_superior_90 <- medias_muestrales + 1.645 * sd_muestrales / sqrt(n)
limite_inferior_99 <- medias_muestrales - 2.58 * sd_muestrales / sqrt(n)
limite_superior_99 <- medias_muestrales + 2.58 * sd_muestrales / sqrt(n)
contiene <- mean(poblacion)>=limite_inferior & mean(poblacion)<=limite_superior
contiene_90 <- mean(poblacion)>=limite_inferior_90 & mean(poblacion)<=limite_superior_90
contiene_99 <- mean(poblacion)>=limite_inferior_99 & mean(poblacion)<=limite_superior_99
###### Hasta aqui ##########################################

##' Graficamos todos los intervalos, ordenados de acuerdo al tamaño de la media muestral


###### Ejecutar desde aqui #################################
orden <- order(medias_muestrales)
data.frame(muestra = rep(c(1:50),3),
           li=c(limite_inferior_90[orden],limite_inferior[orden],limite_inferior_99[orden]),
           ls=c(limite_superior_90[orden],limite_superior[orden],limite_superior_99[orden]),
           contiene=c(contiene_90[orden],contiene[orden],contiene_99[orden]),
           confianza=c(rep(90,50),rep(95,50),rep(99,50)),
           medias=(rep(medias_muestrales[orden],3))) %>%
  ggplot() +
  #geom_errorbarh(aes(xmin=li, xmax=ls, y=muestra, color=contiene)) +
  geom_errorbar(aes(xmin = li, xmax = ls, y = muestra, color = contiene), orientation = "y") +
  geom_point(aes(x=medias,y=muestra)) +
  geom_vline(xintercept = mean(poblacion)) +
  xlab("Metros cuadrados") +
  ylab("Intervalo de Confianza") +
  theme_classic() +
  facet_wrap(.~confianza, nrow = 3) +
  xlim(110,180)
###### Hasta aqui ##########################################


##' Compare este gráfico con el obtenido mediante muestras de tamaño 60.
##' ¿Qué diferencias observa en cuanto al ancho de los intervalos?
##' ¿A qué se deben estas diferencias?
