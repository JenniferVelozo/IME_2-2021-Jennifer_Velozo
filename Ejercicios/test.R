library ( dplyr )

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
muestra_no_anemia<-sample(no_anemia,25)
muestra_si_anemia<-sample(si_anemia,25)

# Verifiación de condiciones para usa t de Student
normalidad_no <- shapiro.test (muestra_no_anemia)
print (normalidad_no)
normalidad_si <- shapiro.test (muestra_si_anemia)
print (normalidad_si)

alfa <- 0.05

prueba <- t.test ( x = muestra_no_anemia,
                  y = muestra_si_anemia,
                   paired = FALSE ,
                    alternative = "two.sided",
                    mu = 0 ,
                    conf.level = 1 - alfa )

print ( prueba )

desv_no <- sd(muestra_no_anemia)
desv_si <- sd(muestra_si_anemia)
diferencia <- mean(muestra_no_anemia) - mean(muestra_si_anemia)

poder <- power.t.test(n=c(25,25), delta=diferencia, sd=c(desv_no,desv_si), sig.level = alfa, power=NULL, 
                      type = "two.sample", alternative = "two.sided")
print(poder)







