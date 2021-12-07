# Se sabe que el proceso de fabricación de barras de acero para concreto reforzado producen barras con
# medidas de dureza que siguen una distribución normal con desviación estándar de 10 kilogramos de
# fuerza por milímetro cuadrado. Usando una muestra aleatoria de tamaño 25, un ingeniero quiere averiguar
# si una línea de producción está generando barras con dureza media de 170 [kgf mm^-2].

# Observación: dado que es una muestra aleatoria a la cual se le conoce la desviación estándar
# y sigue una distribución normal, es posible aplicar la prueba t de Student para una muestra.

# Formulación de hipótesis:
# H0: La dureza media de las barras es de  exactamente 170 [kgf mm^-2].
# HA: La dureza media de las barras es distinta de 170 [kgf mm^-2].

# En este caso, el valor nulo es ??0 = 170 [kgf mm^-2]. Matemáticamente, 
# las hipótesis anteriores se pueden formular como:
# H0: ?? = ??0, esto es ?? = 170
# HA: ?? ??? ??0, esto es ?? ??? 170


# *********************************** PREGUNTA 1 ***********************************
#Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente una media menor a 
#167 [kgf mm^-2] o mayor a 173 [kgf mm^-2], ¿cuál es la probabilidad de que cometa un error de tipo 1?

# Se pide determinar el valor de alfa, ya que éste corresponde a la probabilidad de cometer
# un error de tipo I.

#El área roja del gráfico corresponde a la región de rechazo si la hipótesis nula fuera verdadera

library(ggpubr)
library(pwr)

# Fijar valores conocidos
sigma <- 10
n <- 25

# Calcular el error estándar
SE <- sigma/sqrt(n)
media_nula <- 170

# Graficar la distribución muestral de la dureza media de las barras si
# la hipótesis nula fuera verdadera
x <- seq(160, 180, 0.01 )

y <- dnorm(x, mean = media_nula, sd = SE)

g <- ggplot(data = data.frame(x,y), aes(x))

g <- g + stat_function(fun = dnorm, 
                       args = list(mean = media_nula, sd = SE),
                       colour = "red", size = 1)

g <- g + ylab(" ")
g <- g + scale_y_continuous(breaks =  NULL)
g <- g + scale_x_continuous(name = "Dureza media de las barras [kgf mm^-2]", breaks = seq(160,180,2))

g <- g + ggtitle("Distribución de la dureza media de las barras")
g <- g + geom_vline ( xintercept = media_nula , linetype = "dashed")
g <- g + theme_pubr()


# Se colorea el área del gráfico correspondiente a la región de rechazo de la hipótesis nula
g <- g + geom_area ( data = subset (data.frame(x,y), x < 167) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red",
                     alpha = 0.5)

g <- g + geom_area ( data = subset (data.frame(x,y) , x > 173 ) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red",
                     alpha = 0.5)

print(g)


# pnorm entrega la probabilidad de que la variable tome valores menores
# o iguales que un valor dado.
# Si lower.tail = FALSE, pnorm opera con la cola superior, es decir,
# entrega la probabilidad de que la variable tome valores mayores
# a un valor dado
alfa <- pnorm (167,mean = media_nula,sd = SE,lower.tail = TRUE ) + 
  pnorm (173,mean = media_nula ,sd = SE,lower.tail = FALSE )
cat (" Alfa = ", alfa , "\n")

# Para comprobar si el alfa obtenido anteriormente está bien calculado, se realiza el proceso 
# inverso para así obtener los q críticos, es decir, 167 y 173, los cuales resultan ser así.
media_nula <- 170
sigma <- 10
n <- 25
SE <- sigma/sqrt(n)
alfa <- 0.133614402537716
Z_critico <- qnorm ( alfa/2 , mean = 0 , sd = 1, lower.tail = FALSE)
q_critico_inferior <- media_nula - Z_critico*SE
q_critico_superior <- media_nula + Z_critico*SE
cat("q crítico inferior = ", q_critico_inferior, "\n")
cat("q crítico superior = ", q_critico_superior, "\n")

# Con esto se obtiene que la probabilidad de que el investigador cometa error tipo 1
# es de 0.13, es decir, un 13% de las veces cometerá error de tipo I cada vez que el estimador
# puntual esté a 1.5 o más errores estándar del parámetro de la población. 
# Cabe mencionar que esto puede ocurrir un 6.5% en cada cola de la distribución, ya que corresponde
# a una prueba bilateral. 


# *********************************** PREGUNTA 2 ***********************************
# Si la verdadera dureza media de la línea de producción fuera 172 [kgf mm^-2], ¿cuál sería la probabilidad de
# que el ingeniero, que obviamente no conoce este dato, cometa un error de tipo 2?

# Se pide determinar el valor de beta, ya que éste corresponde a la probabilidad de cometer
# un error de tipo II.

library(ggpubr)
library(pwr)

# Fijar valores conocidos
sigma <- 10
n <- 25
media_nula <- 170
# Calcular el error estándar
SE <- sigma/sqrt(n)
#Utilizando el alfa obtenido de la pregunta 1
alfa <- pnorm (167,mean = media_nula,sd = SE,lower.tail = TRUE ) + 
  pnorm (173,mean = media_nula ,sd = SE,lower.tail = FALSE )



# Graficar la distribución muestral de la dureza media si
# la hipótesis nula fuera verdadera
x <- seq(160, 180, 0.01 )
y <- dnorm(x, mean = media_nula, sd = SE)
g <- ggplot(data = data.frame(x,y), aes(x))

g <- g + stat_function(fun = dnorm, 
                       args = list(mean = media_nula, sd = SE),
                       colour = "red", size = 1)

g <- g + ylab(" ")
g <- g + scale_y_continuous(breaks =  NULL)
g <- g + scale_x_continuous(name = "Dureza media de las barras [kgf mm^-2]", breaks = seq(160,180,2))
g <- g + geom_vline ( xintercept = media_nula , linetype = "dashed")
g <- g + theme_pubr()


Z_critico <- qnorm ( alfa /2 , mean = 0 , sd = 1 , lower.tail = FALSE )
q_critico_inferior <- media_nula - SE*Z_critico
q_critico_superior <- media_nula + SE*Z_critico

g <- g + geom_area ( data = subset (data.frame(x,y), x < q_critico_inferior ) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red", alpha = 0.5)

g <- g + geom_area ( data = subset (data.frame(x,y) , x > q_critico_superior ) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red", alpha = 0.5)


media_efecto = 172
# Superponer la distribución muestral de la media de las diferencias
# si la media fuera de 172
g <- g + stat_function (fun = dnorm ,
                        args = list ( mean = media_efecto , sd = SE ) ,
                        colour = "blue", size = 1)

# Se colorea la región de la nueva curva situada en la región de
# rechazo de la curva original.
x1 <- seq ( 160, 180, 0.01)
y1 <- dnorm (x , mean = media_efecto , sd = SE )
g <- g + geom_area ( data = subset (data.frame (x1 ,y1 ), x < q_critico_inferior ) ,aes ( x = x1 , y = y1 ) ,
                     colour = "blue",
                     fill = "blue",
                     alpha = 0.5)


g <- g + geom_area ( data = subset ( data.frame (x1,y1 ) ,x > q_critico_superior ),
                     aes ( x = x1 , y = y1 ),
                     colour = "blue", fill = "blue", alpha = 0.5)
g <- g + geom_vline ( xintercept = media_efecto , linetype = "dashed")
print (g)

poder <- pnorm (q_critico_inferior,mean = media_efecto,sd = SE,lower.tail = TRUE ) + 
  pnorm (q_critico_superior,mean = media_efecto ,sd = SE,lower.tail = FALSE )

cat("Poder = ", poder, "\n")

beta <- 1 - poder

cat("Beta = ", beta, "\n")

# El área roja del gráfico corresponde a la región de rechazo si la hipótesis nula fuera verdadera,
# mientras que la región azul corresponde al poder de la prueba t.
# Con lo anterior tenemos que la probabilidad de cometer un error tipo II es de 0.685252795948237 o 68.525% aprox.
# lo que quiere decir que más de un 68% de las veces el investigador no detecta una 
# diferencia del tamaño del efecto de las durezas medias (2 [kgf mm-2])
# de las barras.
# Como se puede ver en los resultados, el valor de beta es una probabilidad muy grande, 
# esto es debido a que la muestra es muy pequeña, sin embargo,
# la probabilidad de cometer un error tipo II disminuirá a medida que el tamaño de la muestra crezca.



# *********************************** PREGUNTA 3 ***********************************
# Como no se conoce la verdadera dureza media, genere un gráfico del poder estadístico con las
# condiciones anteriores, pero suponiendo que las verdaderas durezas medias podrían variar de 162 a 178
# [kgf mm^-2].

# Se pide construir un gráfico de poder estadístico v/s el tamaño del efecto, ya que 
# se hace el supuesto de que las medias está en un cierto rango, y conocemos la media nula, 
# la cual corresponde 170 [kgf mm^-2]. En el gráfico, el eje y corresponde al poder de la prueba t
# y el eje x corresponde al tamaño del efecto.

library(ggpubr)
library(tidyverse)

# Datos entregados por el enunciado
media_nula <- 170
sigma <- 10
n <- 25
SE <- sigma/sqrt(n)

# Se utiliza el alfa obtenido de la pregunta 1
alfa <- pnorm (167,mean = media_nula,sd = SE,lower.tail = TRUE ) + 
  pnorm (173,mean = media_nula ,sd = SE,lower.tail = FALSE )
# Generar un vector con un rango de valores para las medias
medias <- seq(162, 178, 0.01)

# Se obtiene el tamaño del efecto, el cual corresponde a la diferencia
# entre la media muestral y el valor nulo, no estandarizada
efecto <- (medias - media_nula)
poder <- power.t.test(n = n, delta = efecto, sd = sigma, sig.level = alfa, 
                       type="one.sample", alternative = "two.sided")$power
datos <- data.frame(efecto, poder)


g <- ggplot(datos, aes(efecto, poder))
g <- g + geom_line(colour="red")
g <- g + ylab("Poder estadístico")
g <- g + xlab("Tamaño del efecto")
g <- g + scale_x_continuous(breaks = seq(-10,10,2))
g <- g + theme_pubr()

g <- g + ggtitle("Relación entre el poder y el tamaño del efecto")
g <- g + geom_vline ( xintercept = 0 , linetype = "dashed")
print(g)

# Como se puede notar en el gráfico, a medida que el valor absoluto del tamaño del 
# efecto aumenta, el poder se va acercando cada vez más a 1, es decir,
# a medida que aumenta el tamaño del efecto, aumentará la probabilidad de 
# correctamente rechazar H0 cuando es falsa. 

# *********************************** PREGUNTA 4 ***********************************
# ¿Cuántas barras deberían revisarse para conseguir un poder estadístico de 0,80 y un nivel de significación
# de 0,05?

# Se pide determinar el tamaño de la muestra con las condiciones indicadas, para lo 
# cual se utiliza la prueba de poder, dejando como NULL el parámetro "n".

# Obtener el tamaño de la muestra mediante la funcion de power.t.test
poder_muestra <- power.t.test(n = NULL,
                              delta = 172 - 170,
                              sd = 10,
                              sig.level = 0.05,
                              power = 0.80,
                              type = "one.sample",
                              alternative = "two.sided")
print(poder_muestra)
n <- ceiling(poder_muestra [["n"]])
cat("n = ", n, "\n")


# Deberían revisarse un total de 199 barras para conseguir un poder estadistico de 0.80 y 
# nivel de significación 0.05.
# Esto quiere decir que se necesitan 199 barras para cometer un error de tipo 1 el 5% de las veces, 
# y un error de tipo 2 el 20% de las veces. 


# *********************************** PREGUNTA 5 ***********************************
# ¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error de tipo 1 a un 1% solamente?

# Se pide determinar el tamaño de la muestra con las condiciones indicadas, para lo 
# cual se utiliza la prueba de poder, dejando como NULL el parámetro "n"..

poder_muestra <- power.t.test(n = NULL,
                              delta = 172 - 170,
                              sd = 10,
                              sig.level = 0.01,
                              power = 0.80,
                              type = "one.sample",
                              alternative = "two.sided")
print(poder_muestra)
n <- ceiling(poder_muestra [["n"]])
cat("n = ", n, "\n")


# Utilizandola función pwr.t.test del paquete pwr, se obtiene el mismo tamaño de muestra, 
# a diferencia que el tamaño de la muestra en este caso corresponde a la d de Cohen
library (pwr)
resultado <- pwr.t.test ( n = NULL ,
                          d = (172 - 170)/10 ,
                          sig.level = 0.01 ,
                          power = 0.80,
                          type = "one.sample",
                          alternative = "two.sided")

print (resultado)
n <- ceiling(resultado[["n"]])
cat("n (usando pwr.t.test) =", n, "\n")

# En conclusión, deberían revisarse un total de 296 barras para conseguir un poder estadistico de 0.80 y 
# nivel de significación 0.01.
# Esto quiere decir que se necesitan 296 barras para cometer un error de tipo 1 el 1% de las veces, 
# y un error de tipo 2 el 20% de las veces. 


# Como podemos notar, al usar un valor de alfa más exigente , es decir,disminuir el porcentaje de error de 
# tipo I obtenemos que se necesita un tamaño de muestra mayor para conseguir un poder de 0.80, con lo que podríamos decir
# que mientras mayor sea el tamaño de muestra, menor serán los porcentajes de error de tipo I.

# El siguiente gráfico presenta la relación entre el poder y el tamaño de la muestra con un nivel de 
# significación de 0.05 y 0.01

library(ggpubr)
library(tidyverse)
tam1 = 199
tam2 = 296
poder = 0.8
efecto = 172 - 170 
sigma = 10
# Generar un vector con un rango de valores para el tamaño de las muestras
n <- seq(5, 500, 0.01)

alfa_05 <- power.t.test(n = n, delta=efecto, sd = sigma, sig.level = 0.05, 
                            type="one.sample", alternative = "two.sided")$power
alfa_01 <- power.t.test(n = n, delta=efecto, sd = sigma, sig.level = 0.01, 
                            type="one.sample", alternative = "two.sided")$power

datos <- data.frame(n, alfa_05, alfa_01)
datos <- datos %>% pivot_longer(!n, names_to="nombre_alfa", values_to="valor_alfa")

niveles <- c("alfa_05", "alfa_01")
etiquetas <- c("alfa = 0.05", "alfa = 0.01")

datos[["nombre_alfa"]] <- factor(datos[["nombre_alfa"]], levels = niveles, labels = etiquetas)

g <- ggplot(datos, aes(n, valor_alfa, colour = factor(nombre_alfa)))
g <- g + geom_line()
g <- g + labs(colour = " ")
g <- g + ylab("Poder estadístico")
g <- g + xlab("Tamaño de la muestra")

g <- g + scale_color_manual(values=c("red", "blue"))
g <- g + scale_y_continuous(breaks = seq(0,1,0.1))
g <- g + scale_x_continuous(breaks = seq(0,500,50))
g <- g + geom_vline ( xintercept = tam1 , linetype = "dashed")
g <- g + geom_vline ( xintercept = tam2 , linetype = "dashed")
g <- g + geom_hline ( aes ( yintercept = poder ) ,
                        color = "red", linetype = 2)
g <- g + theme_pubr()

g <- g + ggtitle("Relación entre el poder y el tamaño de la muestra")

print(g)

# Al complementar con el gráfico de poder v/s el tamaño de la muestra, se comprueba lo dicho anteriormente.
# Si bien para ambos valores de alfa, el poder estadístico crece asintóticamente a 1, 
# para el caso de un alfa igual a 5%, desde un tamaño de muestra igual a 199, el poder comienza 
# a ser de un 0.8, mientras que para el caso de un alfa igual a 1%, el poder comienza a ser 0.8
# desde un tamaño de muestra igual a 296. 
# Para el caso del alfa igual a 0.01 podemos ver que su curva es más baja que la curva correspondiente
# al alfa de 0.05.




