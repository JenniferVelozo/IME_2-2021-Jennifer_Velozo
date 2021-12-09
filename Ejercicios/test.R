library (dplyr)
library(pwr)

# Lectura del archivo 
dir <- "E:"
base <- "IME-2020-2-PE1-datos.csv"
arch <- file.path(dir,base)
datos<-read.csv(arch, fileEncoding = "UTF-8")
set.seed(127) # semilla inicial

no_anemia <- datos %>% filter(anaemia == 0)
no_anemia <- no_anemia[["creatinine_phosphokinase"]]

si_anemia <- datos %>% filter(anaemia == 1)
si_anemia <- si_anemia[["creatinine_phosphokinase"]]
muestra_si_anemia<-sample(si_anemia,25)
muestra_no_anemia<-sample(no_anemia,25)



# Verifiación de condiciones para usar t de Student
normalidad_no <- shapiro.test (muestra_no_anemia)
print (normalidad_no)
normalidad_si <- shapiro.test (muestra_si_anemia)
print (normalidad_si)

# Se obtienen las medias de cada muestra 
media_si_anemia <- mean(muestra_si_anemia)
media_no_anemia <- mean(muestra_no_anemia)

# Se obtiene la desviación estándar de cada muestra
sd_si_anemia <- sd(muestra_si_anemia)
sd_no_anemia <- sd(muestra_no_anemia)

alfa <- 0.05
n= 25

# Se realiza la prueba t de Student para muestras independientes
prueba <- t.test(x = muestra_no_anemia,
                 y = muestra_si_anemia, 
                 paired = FALSE,
                 alternative = "two.sided",
                 mu = 0,
                 conf.level = 1- alfa)

print(prueba)

# Se calcula la desviación estándar agrupada
sp <- sqrt((sd_si_anemia^2*(n - 1)+sd_no_anemia^2*(n - 1)) / (n+n-2))

# Se calcula la d de cohen
factor_correccion <- (n+n-3)/(n+n-2.25)
d <- ((media_si_anemia-media_no_anemia)/sp)*factor_correccion

# Se calcula el poder
poder <- pwr.t.test ( n =n,
                          d =d,
                          sig.level = alfa,
                          power = NULL,
                          type = "two.sample",
                          alternative = "two.sided")
print(poder)



# Tiene una prob de 66% de rechazar correctamente h0 cuando en realidad es falsa, y una prob
# de 34% de cometer un error de tipo 2. 









