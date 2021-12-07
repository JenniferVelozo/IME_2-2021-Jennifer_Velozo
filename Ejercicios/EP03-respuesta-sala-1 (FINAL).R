#Actividad Práctica 3
#Integrantes: Marcelo Álvarez - Jennifer Velozo - Ignacio Villarroel

library(ggplot2)
library(ggpubr)
# LECTURA DEL ARCHIVO
dir <- "E:/IME"
basename <- "Casen 2017 (4).csv"
file <- file.path(dir, basename)
población <- read.csv(file = file, fileEncoding = "UTF-8")


tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado) #se obtiene la media de los ingresos
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado ) #se obtiene la desviación estándar
set.seed(561) # semilla inicial
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
normal_1<-data.frame(ingreso.normal)

#Se grafica la distribución normal
ggdensity(x='ingreso.normal', data = normal_1, color="black", fill="red", add="mean", title="Distribución normal")


# Se genera la distribución Z y se grafica
z <- (ingreso.normal - media.ingreso)/sd.ingreso
dist_z1<-data.frame(z)
ggdensity(x='z', data = dist_z1, color="black", fill="red", add="mean", title="Distribución Z")

#Se construyen las dos distribuciones chi chuadrado

# 5 grados de libertad
chi5 <- c() #vector vacío
for (i in 1:5000) {
  #elegir 5 valores aleatorios
  x<-sample(z,5)
  x<-x^2
  chi5 <- c(chi5,sum(x))
}

chi5_frame = data.frame(chi5)
ggdensity(x='chi5', data = chi5_frame, color="black", fill="red", add="mean", title="Chi cuadrado con 5 grados de libertad")

# 13 grados de libertad
chi13 <- c() #vector vacío
for (i in 1:5000) {
  #elegir 13 valores aleatorios
  x<-sample(z,13)
  x<-x^2
  chi13 <- c(chi13,sum(x))
}
chi13_frame = data.frame(chi13)
ggdensity(x='chi13', data = chi13_frame, color="black", fill="red", add="mean", title="Chi cuadrado con 13 grados de libertad")

#Se construye una distribución F
f <- (chi5/5)/(chi13/13)
f_frame = data.frame(f)
ggdensity(x='f', data = f_frame, color="black", fill="red", add="mean", title="Distribución F")
