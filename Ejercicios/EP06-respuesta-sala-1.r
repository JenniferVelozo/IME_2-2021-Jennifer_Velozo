# Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972) (Journal of chronic
# diseases, 25(12), 711-716) sobre la incidencia de la cantidad de alcohol y de tabaco que se consume en el
# riesgo de padecer c�ncer oral. Las tablas muestran el n�mero de personas que consumiendo una cierta
# cantidad de alcohol o de tabaco, mostrada en cada fila, desarrollaron o no desarrollaron (controles) la
# enfermedad durante su vida.

# OBSERVACI�N: Sea "p" el p con sombrero.

# ******************************* PREGUNTA 1 *******************************
# Estudios previos hab�an determinado que la incidencia de c�ncer oral en la poblaci�n general que bebe
# regularmente entre 10 y 44 ml de alcohol era de 50%. �Respaldan estos datos tal estimaci�n?


# Se pide determinar si la incidencia de padecer c�ncer oral en la poblaci�n general que bebe regularmente
# entre 10 y 44 ml es del 50%. Para esto podemos aplicar una prueba de proporci�n utilizando la funci�n 
# prop.test de R, el cual entregar�a el valor de p para rechazar o no rechazar la hip�tesis nula. 

# Comprobar que se cumplan las condiciones para determinar si el estimador se distribuye de manera cercana a la normal:

# 1.- Si bien, el enunciado no menciona expl�citamente que las observaciones fueron escogidas al azar, �stas provienen
# de un estudio hecho por Rothman & Keller, por lo que podemos asumir que las observaciones de la muestra son independientes. 
# Adem�s, �stas representan menos del 10% de las instancias posibles. 

# 2.- Por otro lado, seg�n los datos entregados por la tabla del enunciado, se cumple la condici�n de �xito-fracaso, dado que 
# se encontraron 109 casos de c�ncer oral y 91 controles, valores que son mucho mayores que 10. 

# Formulaci�n de las hip�tesis:
# H0: La incidencia de c�ncer oral en la poblaci�n general que bebe regularmente entre 
# 10 y 44 ml de alcohol es del 50%.

# H1: La incidencia de c�ncer oral en la poblaci�n general que bebe regularmente entre 
# 10 y 44 ml de alcohol es distinta del 50%

# Matem�ticamente, siendo p la proporci�n de incidencia de c�ncer oral en la poblaci�n general que bebe 
# regularmente entre 10 y 44 ml de alcohol, se tiene:

# H0: p = 0.5
# H1: p ??? 0.5

# Fijar valores conocidos
n <- 200
valor_nulo <- 0.5
exitos <- 109
alfa <- 0.05

prueba <- prop.test(exitos, n = n, p = valor_nulo, alternative = "two.sided", conf.level = 1 - alfa)

print(prueba)

# Resultados obtenidos:
# data:  exitos out of n, null probability valor_nulo
# X-squared = 1.445, df = 1, p-value = 0.2293
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#  0.4733037 0.6149394
# sample estimates:
#  p 
# 0.545

# Con la prueba realizada, se obtiene un valor de p (p-value)= 0.2293, el cual es mucho mayor a nuestro nivel de 
# significaci�n (alfa = 0.05), y la proporci�n de incidencia de c�ncer oral en la poblaci�n general que bebe 
# regularmente entre 10 y 44 ml de alcohol resulta ser de un 54.5%, valor que est� dentro del intervalo
# de confianza obtenido con la prueba, por lo que se falla al rechazar la hip�tesis nula y se puede afirmar
# con un 95% de confianza que no es cierto que la incidencia de c�ncer oral en la poblaci�n general que 
# bebe regularmente entre 10 y 44 ml de alcohol es distinta del 50%. Por lo tanto, los datos no respaldan
# la estimaci�n indicada. 

# ******************************* PREGUNTA 2 *******************************
# Seg�n estos datos, �da lo mismo beber de 10 a 44 ml de alcohol diariamente que hacerlo con 45 o m�s
# ml?

# Se pide estudiar la diferencia de 2 proporciones, por lo que se puede aplicar una prueba
# para 2 proporciones, considerando la proporci�n de personas que beben de 10 a 44 ml de alcohol diariamente
# y las que beben 45 ml o m�s, y que padecen c�ncer.

# Comprobar que se cumplan las condiciones para determinar si el estimador se distribuye de manera cercana a la normal:

# 1.- Podemos asumir que las observaciones de ambas muestras son independientes, dado que provienen de un estudio 
# realizado por Rothman & Keller, adem�s �stas representan menos del 10% de las instancias posibles. 
# Se obtiene la proporci�n agrupada para as� verificar la condici�n de �xito-fracaso:
n1 <- 200
n2 <- 349
p <- (109+242)/(n1+n2)
cat("Valor de p: ",p)

# Con esto se obtiene un p = 0.6393, con lo que, para el caso de las personas que beben 10 a 44 ml de alcohol diariamente
# se espera encontrar p*n1 > 128 �xitos y (1-p)*n1 > 73 fracasos. Luego, para el caso de las personas
# que beben 45 o m�s ml de alcohol diariamente se espera encontrar se espera encontrar p*n2 > 224 �xitos y 
# (1-p)*n2 > 126 fracasos, por lo que se cumple la condici�n de �xito fracaso.

# Por lo tanto, cada proporci�n por separado sigue un modelo normal.

# 2.- Por otro lado,  las muestras son independientes una de la otra, ya que la proporci�n de personas que 
# que beben 10 a 44 ml de alcohol diariamente no tienen alguna conexi�n o relaci�n con las que beben
# 45 o m�s ml. 

# Formulaci�n de hip�tesis:
# H0: No hay diferencia entre las proporciones de las personas que beben de 10 a 44 ml de alcohol diariamente 
# y las personas que beben 45 o m�s ml.

# H1: Hay diferencia entre las proporciones de las personas que beben de 10 a 44 ml de alcohol diariamente 
# y las personas que beben 45 o m�s ml.

# Denotando como p1 y p2 a las proporciones de personas que beben de 10 a 44 ml de alcohol diariamente 
# y las personas que beben 45 o m�s ml, matem�ticamente se tiene:

# H0: p1 - p2 = 0
# H1: p1 - p2 ??? 0 


# Se definen los valores conocidos (personas que beben de 10 a 44 ml, personas que beben 45 o m�s ml)

n <- c(200, 349)
exitos <- c(109, 242)
alfa <- 0.05
valor_nulo <- 0.0

#Prueba de wilson para diferencia de proporciones
prueba <- prop.test(exitos , n = n, alternative = "two.sided", conf.level = 1 - alfa)
print(prueba)

### RESULTADO DE LA PRUEBA ####
#data:  exitos out of n
#X-squared = 11.509, df = 1, p-value = 0.0006924
#alternative hypothesis: two.sided
#95 percent confidence interval:
# -0.2366214 -0.0601981
#sample estimates:
#   prop 1    prop 2 
#0.5450000 0.6934097 

# Con la prueba realizada, se obtiene un valor de p de 0.0006924
# el cual es menor que el nivel de significaci�n (alfa = 0.05)
# por lo que se rechaza la hip�tesis nula en favor de la hip�tesis alternativa, y por lo tanto se
# puede asegurar con un 95% de confianza que no es cierto que
# no hay diferencia entre las proporciones de las personas que beben de 10 a 44 ml de alcohol diariamente 
# y las personas que beben 45 o m�s ml. En otras palabras, se puede afirmar
# con un 95% de confianza que s� existe diferencia entre las proporciones de las personas que beben 
# de 10 a 44 ml de alcohol diariamente y las personas que beben 45 o m�s ml.



# ******************************* PREGUNTA 3 *******************************
# Suponiendo que la diferencia en la proporci�n de personas que desarrollan la enfermedad entre quienes
# beben de 10 a 44 ml de alcohol por d�a y aquellos que beben 45 o m�s ml al d�a es de 0.15. �Cu�nta
# gente deber�amos monitorear para obtener un intervalo de confianza del 95% y poder estad�stico de 90%?
# si se intente mantener aproximadamente la misma proporci�n de gente estudiada en cada caso.

# Se pide determinar el tama�o de la muestra para obtener un intervalo de confianza del 95%
# y un poder estad�stico de 90%, suponiendo que la diferencia en la proporci�n de personas 
# que desarrollan la enfermedad entre quienes
# beben de 10 a 44 ml de alcohol por d�a y aquellos que beben 45 o m�s ml al d�a es de 0.15.
# Para ello se utiliza la funci�n bsamsize del paquete Hmisc, la cual calcula los tama�os 
# de cada grupo. 

# Formulaci�n de las hip�tesis:
#H0: la diferencia entre la proporci�n de personas que desarrollan la enfermedad entre quienes
#    beben de 10 a 44 ml de alcohol por d�a y aquellos que beben 45 o m�s ml al d�a es de 0.15.

#HA: la diferencia entre la proporci�n de personas que desarrollan la enfermedad entre quienes
#    beben de 10 a 44 ml de alcohol por d�a y aquellos que beben 45 o m�s ml al d�a es distinta de 0.15.

# Matem�ticamente hablando, siendo p1 y p2 las proporciones de personas que desarrollan la
# enfermedad entre aquellos que beben de 10 a 44 ml de alcohol por d�a
# y aquellos que beben 45 o m�s ml al d�a, respectivamente, se tiene:

# H0: p1 - p2 = 0.15
# HA: p1 - p2 ??? 0.15

library(Hmisc)
# Asignando los valores conocidos
n1 <- 200
n2 <- 349
exitos1 <- 109
exitos2 <- 242
p1 <- exitos1/n1
p2 <- exitos2/n2
fraction <- (n1/(n1 +n2))
alfa <- 0.05
poder <- 0.9
h3 <- bsamsize(p1, p2, fraction, alfa, poder)
print(h3)

tam1 <- ceiling(h3[["n1"]])
cat("n1 = ", tam1, "\n")

tam2 <- ceiling(h3[["n2"]])
cat("n2 = ", tam2, "\n")

# Con lo anterior, se obtiene que el tama�o de la muestra para el caso de las personas
# que beben de 10 a 44 ml de alcohol por d�a debe ser igual a 175, mientras que 
# para el caso de las personas que beben 45 o m�s ml por d�a, el tama�o de la muestra
# debe ser 305, para obtener un intervalo de confianza del 95% y poder estad�stico de 90%. 
# Es decir, para obtener una probabilidad de cometer error tipo I igual a 0.05
# y una probabilidad de cometer error tipo II igual a 0.1, los tama�os de las muestras
# deben ser 175 y 305. 
