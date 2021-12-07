# ****************************************** ENUNCIADO PREGUNTA 1 ******************************************

# El artículo "Automatic Segmentation of Medical Images Using Image Registration: Diagnostic and
# Simulation Applications" (Journal of Medical Engeeniering and Technology 2005) propuso una nueva
# técnica para la identificación automática de los bordes de estructuras significativas en una imagen médica
# utilizando desplazamiento lineal promedio (ALD, por sus siglas en inglés). El artículo dio las siguientes
# observaciones de ALD con una muestra de 49 riñones (en pixeles y usando punto en vez de coma decimal):
# 1.38 1.28 1.09 1.07 0.96 1.28 0.91 1.49 1.11 0.66 1.14 1.13 0.91 0.94 1.30
# 0.87 0.73 0.92 1.00 1.05 1.12 1.10 0.95 1.29 0.86 0.96 0.94 1.45 1.12 1.06
# 0.71 0.88 0.96 1.14 1.03 0.89 0.81 1.04 1.15 0.75 1.12 1.01 1.11 0.64 1.25
# 0.68 1.44 1.28 1.21
# Los autores comentaron que el ALD medio sería de al menos 1.0 pixel. ¿Los datos soportan esta
# afirmación?


# Formulación de las hipótesis:

# H0: El ALD medio es de 1.0 pixel
# HA: El ALD medio es mayor a 1.0 pixel

# En este caso, el valor nulo es ??0 = 1.0 pixel. Matemáticamente, 
# las hipótesis anteriores se pueden formular como:
# H0: ?? = ??0, esto es ?? = 1.0
# HA: ?? > ??0, esto es ?? > 1.0

# Se procede a utilizar la prueba t de Student, dado que la prueba Z no es muy utilizada y asume 
# el supuesto de normalidad. Además, la prueba t es igualmente adecuada para muestras de tamaño grande (n>30).

# Se verifica el cumplimiento de las condiciones para poder usar la 
# prueba t:

# 1.- Si bien, no indica que las muestras fueron escogidas al azar,
# el conjunto de instancias posibles es muy grande, y las 49 seleccionadas
# no superan el 10% de la población. Además, las muestras provienen de un artículo de confianza.

# 2.- Al construir un gráfico Q-Q, se pueden observar algunos valores 
# atípicos, por lo que no podemos suponer con certeza que la muestra sigue
# una distribución normal. Es por lo anterior, que se realiza una prueba de
# Shapiro-Wilk, de donde se obtiene un valor p = 0.6202, el cual es
# superior a nuestro nivel de significación (0.01), por lo que
# podríamos decir que provienen de una distribución normal. Sin embargo,
# al construir un histograma y un gráfico de densidades, se puede apreciar 
# que no se aproxima mucho a una distribución normal. 
# Dado que se tienen dudas respecto a la distribución de la muestra,
# seremos más exigentes para llevar a cabo la prueba t, utilizando 
# un nivel de confianza del 99%.

# Observación: dado que es una hipótesis unilateral, el parámetro "alternative" de t.test toma el valor de "greater", 
# ya que la media de la población es mayor que el valor nulo en la hipótesis alternativa.

# ********************* CÓDIGO EN R PREGUNTA 1 *********************
library(ggpubr)

# Se carga el conjunto de datos

texto <- "1.38 1.28 1.09 1.07 0.96 1.28 0.91 1.49 1.11 0.66 1.14 1.13 0.91 0.94 1.30
0.87 0.73 0.92 1.00 1.05 1.12 1.10 0.95 1.29 0.86 0.96 0.94 1.45 1.12 1.06
0.71 0.88 0.96 1.14 1.03 0.89 0.81 1.04 1.15 0.75 1.12 1.01 1.11 0.64 1.25
0.68 1.44 1.28 1.21"
file <- textConnection(texto)
muestra <- scan(file)

# Construir gráfico Q-Q
datos <- data.frame(muestra)
g <- ggqqplot(datos, x = "muestra", color = "Steelblue", xlab = "Teórico", ylab = "Muestra",
              title = "Gráfico Q-Q muestra v/s distr. normal")
print(g)

# Verificar distribución muestral usando
# la prueba de normalidad de Shapiro-Wilk
normalidad <- shapiro.test(muestra)
print(normalidad)


#Para demostrar la forma de la distribución.
#Se grafica un histograma de la muestra
hist(muestra)
#Se construye un gráfico de densidades
ggdensity(data = muestra, color="black", fill="red", add="mean", title="Gráfico de densidades para la muestra")

# Fijamos el valor nulo
valor_nulo <- 1.0

# Fijamos el nivel de significación (en 0.01 en vista de que se tienen dudas con la forma de la distribución)
alfa <- 0.01

# Se obtiene la media de la muestra
media <- mean(muestra) 
cat("Media =", media, "pixel")

# Se obtiene la desviación estándar de la muestra
desv_est <- sd(muestra)
cat("Desviación estándar =", desv_est, "pixel")

# Se aplica la prueba t de Student para una muestra
prueba <- t.test(muestra, alternative = "greater", mu = valor_nulo, conf.level = 1 - alfa)

print(prueba)



# ********************* CONCLUSIONES PREGUNTA 1 *********************

# Los resultados obtenidos al aplicar la prueba t de Student son los siguientes:
# El valor para el estadístico de prueba T es t = 1.4887
# Se consideran df = 48 grados de libertad para la distribución t.
# El valor p obtenido es p = 0.07155
# El intervalo de confianza obtenido es [0.9726945; Inf)
# La media de la muestra es = 1.044286

# Como se puede notar, el valor medio se encuentra dentro del intervalo de confianza. Además,
# el valor de p es mayor que el nivel de significación (0.07155 > 0.01), por lo que se falla
# al rechazar la hipótesis nula. Por lo tanto, se puede afirmar con un 99% de confianza
# que el ALD medio es igual a 1.0 pixel.


# ****************************************** ENUNCIADO PREGUNTA 2 ******************************************
# Se sabe que la lactancia estimula una pérdida de masa ósea para proporcionar cantidades de calcio
# adecuadas para la producción de leche. Un estudio intentó determinar si madres adolescentes podían
# recuperar niveles más normales a pesar de no consumir suplementos (Amer. J. Clinical Nutr., 2004; 1322-
# 1326). El estudio obtuvo las siguientes medidas del contenido total de minerales en los huesos del cuerpo
# (en gramos) para una muestra de madres adolescentes tanto durante la lactancia (6-24 semanas
# postparto) y posterior a ella (12-30 semana postparto):

# Sujeto 1 2 3 4 5 6 7 8 9 10
# Lactancia 1928 2549 2825 1924 1628 2175 2114 2621 1843 2541
# Posdestete 2126 2885 2895 1942 1750 2184 2164 2626 2006 2627

# ¿Sugieren los datos que el contenido total de minerales en los huesos del cuerpo durante el posdestete
# excede el de la etapa de lactancia por más de 60 g?

# Formulación de las hipótesis:
# H0: la media de las diferencias entre el contenido total de minerales en los huesos del cuerpo durante el posdestete
# y el contenido total de minerales en los huesos del cuerpo durante la lactancia es igual a 60 g.

# HA: la media de las diferencias entre el contenido total de minerales en los huesos del cuerpo durante el posdestete
# y el contenido total de minerales en los huesos del cuerpo durante la lactancia es superior a 60 g.

# En este caso, el valor nulo es ??0 = 60 g. Matemáticamente:
# Denotando la media de las diferencias del contenido total de minerales en los huesos del cuerpo 
# como µdif:
# H0: µdif = ??0, esto es µdif = 60
# HA: µdif > ??0, esto es µdif > 60 

# Se procede a utilizar una prueba t de Student para dos muestras pareadas, dado que el tamaño de las muestras es muy pequeño, 
# y además, las dos muestras son dependientes, ya que fueron tomadas de las mismas personas. Cada observación de una muestra tiene una 
# correspondencia con una  observación de la otra muestra.

# Verificamos el cumplimiento de las condiciones para utilizar una prueba t para muestras pareadas:
# 1.- El conjunto de instancias posibles es muy grande y las 10 seleccionadas no superan el 10% de la población, por 
# lo que se puede suponer que las observaciones son independientes. Además, las muestras provienen de un estudio.

# 2.- Al construir un gráfico Q-Q para las diferencias, se puede observar que si bien los puntos de la muestra
# no forman una recta, no se observan valores atípicos que se alejen de la región aceptable. Además,
# al aplicar una prueba de normalidad de Shapiro-Wilk se obtiene un valor p = 0.1389, el cual es un valor 
# mayor a nuestro nivel de significación (0.05), por lo que podemos concluir que la diferencia del 
# contenido total de minerales en los huesos del cuerpo se acerca razonablemente a una distribución normal.

# Por lo tanto, se procede a usar una prueba t de Student para muestras pareadas con un nivel de confianza del 95%.

# Observación: dado que es una hipótesis unilateral, el parámetro "alternative" de t.test toma el valor de "greater", 
# ya que la media de la población es mayor que el valor nulo en la hipótesis alternativa.

# ********************* CÓDIGO EN R PREGUNTA 2 *********************
library(ggpubr)

# Se carga el conjunto de datos

texto1 <- "1928 2549 2825 1924 1628 2175 2114 2621 1843 2541"
texto2 <- "2126 2885 2895 1942 1750 2184 2164 2626 2006 2627"


fileLact <- textConnection(texto1)
filePos <- textConnection(texto2)

lactancia <- scan(fileLact)
posdes <- scan(filePos)

# Fijamos el valor nulo
valor_nulo = 60

#Se calcula la diferencia entre las muestras Posdestete y Lactancia
diferencia <- posdes - lactancia

# Construir gráfico Q-Q
datos <- data.frame(diferencia)
g <- ggqqplot(datos, x = "diferencia", color = "Steelblue", xlab = "Teórico", ylab = "Muestra",
              title = "Gráfico Q-Q muestra v/s distr. normal")
print(g)

# Se realiza la prueba de Shapiro-Wilk para ver si la distribución se acerca a una normal
normalidad <- shapiro.test(diferencia)
print(normalidad)


#Asignamos el nivel de significancia a utilizar
alfa <- 0.05

#Se aplica la prueba t de Student para estas muestras pareadas
prueba <- t.test(x = posdes,
                 y = lactancia,
                 paired = TRUE,
                 alternative = "greater",
                 mu = valor_nulo,
                 conf.level = 1- alfa)

print(prueba)         

# ********************* CONCLUSIONES PREGUNTA 2 *********************

# Los resultados obtenidos al aplicar la prueba t de Student son los siguientes:
# El valor para el estadístico de prueba T es t = 1.3917
# Se consideran df = 9 grados de libertad para la distribución t.
# El valor p obtenido es p = 0.09873
# El intervalo de confianza obtenido es [45.50299; Inf)
# La media de la muestra es = 105.7

# Como se puede notar, la media de las diferencias se encuentra dentro del intervalo de confianza,
# y además el valor p es mayor que el nivel de significación (0.09873 > 0.01), por lo que se falla 
# al rechazar la hipótesis nula. Por lo tanto, se puede afirmar con 95% de confianza que 
# pareciera que la diferencia entre el contenido total de minerales en los huesos del cuerpo durante el posdestete
# y el contenido total de minerales en los huesos del cuerpo durante la lactancia es igual a 60 g.
# Aunque sería necesario conseguir una muestra más grande para tener mayor certeza, 
# puesto que la muestra es demasiado pequeña.





# ****************************************** ENUNCIADO PREGUNTA 3 ******************************************
# La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rápido crecimiento de
# los pollitos es beneficioso, tanto para las avícolas como para los consumidores. En el paquete datasets
# de R están los datos (chickwts) de un experimento hecho para medir la efectividad de varios
# suplementos alimenticios en la tasa de crecimiento de las aves. Pollitos recién nacidos se separaron
# aleatoriamente en 6 grupos, y a cada grupo se le dio un suplemento distinto. Para productores de la 7ma
# región, es especialmente importante saber si existe diferencia en la efectividad entre el suplemento
# basado en linaza (linseed) y el basado en habas (horsebean).

# Formulación de las hipótesis:
# H0: no existe diferencia en la efectividad promedio entre el suplemento basado en linaza y el basado en habas.
# HA: existe diferencia en la efectividad promedio entre el suplemento basado en linaza y el basado en habas.

# Matemáticamente:
# Si µl y µh son las efectividades medias de los suplementes basados en linaza y habas, respectivamente, entonces:
# H0: µl = µh
# HA: µl ??? µh (ul distinto de uh)

# Se procede a utilizar una prueba t de Student para 2 muestras independientes, dado que las observaciones no tienen
# relación con ninguna de las otras observaciones, ni influyen en su selección, ni en la misma muestra ni en
# la otra muestra.

# Verificamos el cumplimiento de las condiciones para utilizar una prueba t de Student
# para 2 muestras independientes:
# Ambas muestras son independientes entre sí, ya que son diferentes pollitos y fueron designados aleatoriamente
# a cada grupo. Además, se puede decir que las observaciones son independientes, dado que cada muestra es mucho menor a la
# población total de pollitos. 
# Por otro lado, al construir un gráfico Q-Q para cada muestra, se puede observar sólo un valor atípico para el 
# caso de las habas, Sin embargo,
# al aplicar a cada muestra la prueba de Shapiro-Wilk, se obtiene un p = 0.9035 para la muestra de suplemento basado en linaza
# y un p = 0.5264 para la muestra de suplemento basado en habas. En ambos casos, el valor de p es lo suficientemente mayor 
# al nivel de significación (0.01) para concluir que ambas muestras provienen de poblaciones que se distribuyen
# de forma cercana a la normal.

# Por lo tanto, se procede a usar una prueba t de Student para dos muestras independientes con un nivel de confianza del 99%, 
# ya que para productores de la 7ma región, es especialmente importante saber si existe diferencia en la efectividad 
# entre el suplemento basado en linaza y el basado en habas, por lo que se necesita ser más exigentes. 

# Observación: dado que es una hipótesis bilateral, el parámetro "alternative" de t.test toma el valor de "two.sided"

# ********************* CÓDIGO EN R PREGUNTA 3 *********************
library(datasets)
library(dplyr)
library(ggpubr)

#Carga de datos
datos <- chickwts

#Se filtra la efectividad para el suplemento basado en linaza
linaza <- datos %>% filter ( feed == "linseed")

#Se filtra la efectividad para el suplemento basado en habas
habas <- datos %>% filter ( feed == "horsebean")


# Construir gráfico Q-Q para efectividad basada en linaza
datos1 <- data.frame(linaza[["weight"]])
g1 <- ggqqplot(datos1,x="linaza...weight...",color = "Steelblue", xlab = "Teórico", ylab = "Muestra",
               title = "Gráfico Q-Q muestra v/s distr. normal (Linaza)")
print(g1)

# Construir gráfico Q-Q para efectividad basada en habas
datos2 <- data.frame(habas[["weight"]])
g2 <- ggqqplot(datos2,x="habas...weight...",color = "Steelblue", xlab = "Teórico", ylab = "Muestra",
               title = "Gráfico Q-Q muestra v/s distr. normal (Habas)")
print(g2)

#Se verifica si las muestras se distribuyen de manera cercana a la normal 
normalidad_l <- shapiro.test(linaza[["weight"]])
print(normalidad_l)

normalidad_h <- shapiro.test(habas[["weight"]])
print(normalidad_h)

#Se asigna un valor de significación de 0.01 en vista de que es importante la precisión
alfa <- 0.01

#Se aplica la prueba t de Student para 2 muestras independientes
prueba3 <- t.test(x = linaza[["weight"]],
                  y = habas[["weight"]],
                  paired = FALSE ,
                  alternative = "two.sided",
                  mu = 0 ,
                  conf.level = 1 - alfa)
print(prueba3)

# Se calcula la diferencia entre las medias
media_l <- mean(linaza[["weight"]])
media_h <- mean(habas[["weight"]])
diferencia <- media_l - media_h
cat("Diferencias de las medias = ", diferencia, "[g]\n")

# ********************* CONCLUSIONES PREGUNTA 3 *********************
# Los resultados obtenidos al aplicar la prueba t de Student son los siguientes:
# El valor para el estadístico de prueba T es t = 3.0172
# Se consideran df = 19.769 grados de libertad para la distribución t.
# El valor p obtenido es p = 0.006869
# El intervalo de confianza obtenido es [3.267538; 113.832462]
# La media de la muestra para linaza es =  218.75
# La media de la muestra para habas es = 160.20 
# Diferencia de las medias =  58.55

# Como el valor de p es menor al nivel de significación (0.900686 < 0.01), se puede decir que 
# la evidencia en favor de HA es fuerte, por lo que se rechaza la hipótesis nula.

# Por lo tanto, podemos concluir con un 99% de confianza que sí existe diferencia en la efectividad 
# promedio entre el suplemento basado en linaza y el suplemento basado en habas.