# Configurar carpeta de trabajo
library(dplyr)
dir <- "E:/IME"
base <- "datoscovid.csv"

arch <- file.path(dir,base)

datos<-read.csv(arch, fileEncoding = "UTF-8")

antofa <- datos %>% filter(Region == "Antofagasta")
antofaT <- t(antofa[, 2:ncol(antofa)]) # traspuesta

fechas <- row.names(antofaT)
fechas

i <- fechas > "X2020.11.01" & fechas < "x2020.12.31"
i
rango.fechas <- fechas[i]

rango.casos <- antofaT[i, ]
rango.casos
print(class(rango.casos))
imax <- which.max(rango.casos)
print(rango.fechas[imax])
