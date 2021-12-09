# PEP 1

# Correo profesor: joseluis.jara@usach.cl

# Formulación de hipótesis
# H0: La estatura promedio de de los reclutas es igual a 1.65 [m].
# HA: La estatura promedio de de los reclutas es superior a 1.65 [m].

# Las hipótesis expresadas en lenguaje matemático con mu0 = 1.65 (valor nulo)
# H0: mu = mu0, mu = 1.65
# HA: mu > mu0, mu > 1.65

# Cumplimiento de condiciones para usar prueba T de Student:
# 1.- Dado que las muestras son escogidas al azar, usando una semilla de 523, se puede
# decir que son independientes.
# 2.- Al observar el gráfico Q-Q, es válido suponer una distribución cercana a la normal, ya que no se 
# observan valores atípicos que se alejen de la región aceptable.

# Como se cumplen las 2 condiciones, se procede a usar una prueba T de Student con un nivel de
# significación de 0.05.
# Cabe mencionar que corresponde a una prueba unilateral.



###### CÓDIGO R ######


library (dplyr)
library(pwr)
library ( ggpubr )

# Lectura del archivo 
dir <- "E:/IME_2-2021-Jennifer_Velozo"
base <- "Datos PEP 1.csv"
arch <- file.path(dir,base)
datos<-read.csv2(arch, fileEncoding = "UTF-8")

set.seed(523) # semilla inicial
# Formatear sexo y planeta como variable categórica
datos[["sexo"]] <- factor ( datos [["sexo"]] , levels =c("M", "F"),
                              labels = c("Masculino", "Femenino"))

datos[["planeta"]] <- factor ( datos [["planeta"]] , levels =c("Tatooine", "Naboo","Coruscant", "Alderaan"))



data <- datos[["estatura"]]
estaturas<-sample(data,40)


# Construir gráfico Q-Q
estaturas <- data.frame(estaturas)
g <- ggqqplot(estaturas, x = "estaturas", color = "Steelblue", xlab = "Teórico", ylab = "Muestra",
              title = "Gráfico Q-Q muestra v/s distr. normal")
print(g)

valor_nulo <- 1.95
alfa <- 0.05

prueba <- t.test (estaturas, alternative = "greater",mu = valor_nulo ,conf.level = 1 - alfa )

print ( prueba )


################## CONCLUSIONES ##################
# De acuerdo a los resultados arrojados por la prueba, se obtiene un p-value = 1, 
# el cual es mucho mayor que nuestro nivel de significación alfa = 0.05. Es por lo anterior, que se 
# se falla al rechazar la hipótesis nula. Es decir, no hay evidencia suficiente para concluir que 
# la estatura promedio de los reclutas es superior a 1.65 [m].
