############################### PREGUNTA 1 ###############################

# Se realizó un ensayo clínico para estudiar la toxina botulínica, una toxina muy potente que puede ser
# usado como medicamento en dosis diminutas, como posible tratamiento para el dolor de espalda crónico.
# Un total de 31 pacientes participaron del estudio, de los cuales 15 fueron asignados aleatoriamente al
# grupo de tratamiento y los otros 16 al grupo de control (placebo). Luego de ocho semanas, 9 personas
# voluntarias del grupo de tratamiento reportaron alivio del dolor, mientras que 2 personas lo hicieron en el
# grupo de control. ¿Qué se podría decir del tratamiento?

# Como podemos leer en el enunciado de la pregunta se distingue de manera clara el uso de
# la prueba de Fisher para su resolución, esto porque primero notamos que el tamaño de la
# muestra de la poblacion es pequeño, luego vemos que las varibles del problema son ambas
# dicotómicas, ya que la variable grupo toma los valores "Tratamiento" o "Control", y la 
# variable estado toma los valores "Alivio" o "Sin Alivio". También las observaciones de la muestra 
# son independientes entre sí ya que el tamaño de 
# la muestra no supera el 10% de la población total y provienen de un ensayo clínico.

# Las hipótesis a contrastar son:

# H0: las variables son independientes.
# HA: las variables están relacionadas.


library(ggpubr)

tratamiento  <- c(9,6)
control <- c(2,14)

tabla  <- as.table(rbind(tratamiento , control))
dimnames(tabla) <- list(grupo = c("Tratamiento", "Control"),
                        estado = c("Alivio", "Sin alivio"))

print(tabla)

# Aplicar  prueba  exacta  de  Fisher utilizando un nivel de significación de 0.05.
alfa  <- 0.05
prueba  <- fisher.test(tabla , 1-alfa)

print(prueba)

#Resultados de la prueba:
# data:  tabla
# p-value = 0.009147
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   1.407043 117.982202
# sample estimates:
#   odds ratio 
# 9.614213 

# Considerando un alfa 0.05 y p = 0.009147, se tiene que p<alfa, por lo que
# se rechaza la hipotesis nula. Entonces se puede afirmar con un 95% de confianza que hay 
# una asociación estadísticamente significativa entre la cantidad de personas 
# con alivio del dolor de espalda y el tratamiento.



############################### PREGUNTA 2 ###############################

# La escuela de psicología quiere evaluar el impacto de una intervención que han diseñado para ayudar a
# dejar de fumar. Con este fin, reclutaron 25 personas fumadoras que tenían la intención de dejar de hacerlo
# y 25 personas fumadoras que no consideraban esta opción. Dos semanas después de finalizada la
# intervención (que consistía en mirar videos emotivos que mostraban el impacto que las muertes por cáncer
# asociado al cigarrillo tenía en las familias, seguidas de rondas de conversación), se les preguntó a las y los
# participantes si tenían intención de intentar dejar de fumar. 5 personas que tenían la intención de hacerlo
# antes de la intervención, cambiaron de opinión y ya no quieren intentarlo, mientras que 9 participantes que
# no pensaban en dejar de fumar, ahora lo estaban considerando. ¿Qué se puede decir del impacto de la
# intervención?

# En vista de que se pide determinar el impacto de la intervención, se puede determinar si existe o no 
# cambios en las respuestas de los entrevistados, y además, como se tiene 2 respuestas en distintas instancias
# para los mismos entrevistados (muestras pareadas) se puede utilizar una prueba de McNemar para responder esta pregunta.

# Primero se comprueba si los datos cumplen los requisitos para utilizar esta prueba:

# En vista de que los entrevistados no supera el 10% de personas fumadoras a nivel mundial, 
# y que la muestra proviene de una fuente confiable (escuela de psicología),
# se puede decir que las observaciones de esta muestra son independientes entre sí.

# Las hipótesis a contrastar son:
# H0: No hay cambios significativos en las respuestas.
# H1: Sí hay cambios significativos en las respuestas.

# Se procede a crear la tabla de confusión con las respuestas de los entrevistados

antes_intervencion <- c(rep("Dejar de fumar", 25), rep("No dejar de fumar", 25))
despues_intervencion <- c(rep("Dejar de fumar", 20), rep("No dejar de fumar", 21), rep("Dejar de fumar", 9))
tabla <- table(despues_intervencion, antes_intervencion)
print(tabla)
# Una vez creada la tabla de confusión se aplica la prueba de McNemar
prueba <- mcnemar.test(tabla)
print(prueba)

# Los resultados entregados por la prueba son los siguientes:

# McNemar's Chi-squared test with continuity correction
# data:  tabla
# McNemar's chi-squared = 0.64286, df = 1, p-value = 0.4227

# Si se utiliza un nivel de significación alfa de 0.05, en vista de que el p-value obtenido es de 0.4227
# y este es mayor que el nivel de significación escogido, se puede asegurar con un 95% de confianza que se falla en
# rechazar la hipótesis nula H0, por lo que no hay cambios significativos en las respuestas de los entrevistados.
# Por lo tanto, la intervención propuesta por la escuela de psicología no tuvo un gran impacto en la opinión de los entrevistados.


############################### PREGUNTA 3 ###############################
# Un grupo de activistas ha denunciado racismo en la conformación de los jurados de un pequeño condado
# en Texas, EE.UU. Su denuncia se basa que, según ellos, las proporciones raciales de las personas
# seleccionadas para ser jurado el año pasado (205 blancos, 26 negros, 25 latinos y 19 de otras razas) no
# se corresponde con las proporciones reportadas en el último censo (72% adultos blancos, 7% adultos
# negros, 12% adultos latinos y 9% adultos que se declaran de otras raza). ¿Tienen razón los denunciantes?

# Se pide comprobar si la muestra es representativa del censo, y para ello
# se utiiza una prueba chi-cuadrado de bondad de ajuste, ya que según los denunciantes
# las proporciones raciales de las personas seleccionadas para ser jurado el año pasado 
# no se corresponde con las proporciones reportadas en el último censo.

# Verificamos el cumplimiento de la condicinones para emplear esta prueba:

# Puesto que la muestra representa menos del 10% de la población,
# se asume que las observaciones de la muestra son independientes entre sí.

# Luego, se verifica si se esperan más de 5 observaciones por cada grupo
muestra <- c(205, 26, 25, 19)
n_muestra <- sum(muestra)
censo <- c(0.72*n_muestra, 0.07*n_muestra, 0.12*n_muestra, 0.09*n_muestra)

n_censo <- sum(censo)
proporciones <- round(censo/n_censo, 3)
esperados <- round(proporciones * n_muestra, 3)
print(esperados)

# Con los valores esperados obtenidos, se puede 
# observar que son todos mayores a 5, por lo tanto
# se cumple la segunda condición para poder utilizar la prueba 
# chi-cuadrado de bondad de ajuste.

# Las hipótesis a contrastar son:
# H0: las proporciones raciales de personas seleccionadas para ser jurado son las 
# mismas para el censo y para la muestra del año pasado.

# HA: las proporciones raciales de personas seleccionadas para ser jurado son
# diferentes para el censo y para la muestra del año pasado.

# Matemáticamente, siendo p1 y p2, las proporciones de personas en cada raza seleccionadas
# en el censo y en la muestra respectivamente, se tiene:
# H0: p1 = p2
# HA: p1 ≠ p2 (p1 distinto de p2)

# Se crea la tabla de confusión para realizar la prueba 
tabla <- as.table(rbind(censo,muestra))
dimnames(tabla) <- list(grupo = c("Censo","Muestra"),
                        raza = c("Blancos", "Negros", "Latinos", "Otras razas"))

print(tabla)

# Se realiza una prueba chi-cuadrado de bondad de ajuste
prueba <- chisq.test(tabla, correct = FALSE)
print(prueba)

# RESULTADO DE LA PRUEBA
# Pearson's Chi-squared test

# data:  tabla
# X-squared = 2.9877, df = 3, p-value = 0.3935

# El valor resultante es p = 0.3935, por lo que se falla al rechazar
# la hipótesis nula con un nivel de significación alfa = 0.05.
# Por lo tanto, podemos concluir con un 95% de confianza que las proporciones
# raciales de personas seleccionadas para ser jurado son las mismas
# para el censo y para la muestra del año pasado, por lo que los denunciantes
# no tienen razón. 


############################### PREGUNTA 4 ###############################
# Enuncie un ejemplo novedoso (no discutido en clase ni en las lecturas) relacionado con las preferencias
# de los chilenos de las diferentes candidaturas después de los tres debates presidenciales de octubre 2021
# que requiera utilizar una prueba Q de Cochran. Identifique las variables involucradas y las hipótesis a
# contrastar.

# Juan Pérez, el nuevo jefe de la carrera de Ingeniería Civil en Informática de la Universidad de Santiago, 
# desea determinar si existe alguna diferencia significativa en las 
# propuestas impulsadas por cada candidato en los tres debates presidenciales de octubre 2021. Para ello, 
# la Escuela de Periodismo de la Universidad de Santiago ha entrevistado a 20 personas elegidas al azar preguntando 
# si están o no de acuerdo con las propuestas de los candidatos Eduardo Artés, 
# Gabriel Boric, Marco Enríquez-Ominami, José Antonio Kast, Franco Parisi, Yasna Provoste y Sebastián Sichel. 

# Variables involucradas:
# Variable de respuesta dicotómica: Persona entrevistada que está o no de acuerdo con las propuestas de los candidatos.
# Variable independiente: Candidato a las elecciones presidenciales.

# Hipótesis a contrastar: 
# H0: la proporción de "éxitos" es la misma para todos los candidatos. 
# H1: la proporción de "éxitos" es distinta para al menos un candidato. 

# Este ejemplo se puede resolver mediante la prueba Q de Cochran, ya que 
# la variable de respuesta es dicotómica (De acuerdo o en desacuerdo), y la variable independiente tiene más de 2 observaciones
# pareadas (específicamente 7, ya que hay 7 candidatos en el estudio). Además, las observaciones son independientes entre sí, ya que, según el enunciado
# las personas a entrevistar fueron escogidas al azar.  Y por último, el tamaño de la muestra es lo suficientemente grande, 
# considerando n*k >= 24, donde n = 20, y k = 7. 