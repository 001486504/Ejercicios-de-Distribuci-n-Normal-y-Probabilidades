
#--------------------------------------------------------------------------------------
#Ejercicio 2
# Parámetros de la distribución normal
media <- 40
desviacion_estandar <- 5

# Valores de x para graficar la distribución
x <- seq(20, 60, length = 100)

# Función de densidad de la distribución normal
densidad <- dnorm(x, mean = media, sd = desviacion_estandar)

# Graficar la distribución normal
plot(x, densidad, type = "l", lwd = 2, col = "blue", 
     xlab = "Valor", ylab = "Densidad",
     main = "La Media = 40 // Desviación Estándar = 5")

# Sombrear el área entre 35 y 45
x_sombreado <- seq(35, 45, length = 100)
densidad_sombreada <- dnorm(x_sombreado, mean = media, sd = desviacion_estandar)
polygon(c(35, x_sombreado, 45), c(0, densidad_sombreada, 0), col = rgb(0, 0, 1, 0.3))

# Calcular la probabilidad de que el valor esté entre 35 y 45
prob_45 <- pnorm(45, mean = media, sd = desviacion_estandar)
prob_35 <- pnorm(35, mean = media, sd = desviacion_estandar)
probabilidad <- prob_45 - prob_35

# Agregar el texto de la probabilidad al gráfico
text(40, 0.02, paste("P(35 < X < 45) =", round(probabilidad, 4)), col = "blue")
#---------------------------------------------------------------------------------------#
# Resumen de su Funcionamiento                                                          #
# - Se definen los parámetros de la distribución y se calculan valores para graficar.   #
# - Se dibuja la curva de densidad de la distribución normal.                           #
# - Se sombrean los valores entre 35 y 45 para representar el área de probabilidad.     #
# - Se calcula la probabilidad de estar entre 35 y 45 y se muestra en el gráfico.       #
#                                                                                       #
#---------------------------------------------------------------------------------------#
#################################################################################################
#Ejercicio 4
# Generar las muestras
set.seed(123)  # Para reproducibilidad
muestra_A <- rnorm(1000, mean = 55, sd = 10)
muestra_B <- rnorm(1000, mean = 65, sd = 15)

# Calcular la media y desviación estándar de cada muestra
media_A <- mean(muestra_A)
desviacion_A <- sd(muestra_A)

media_B <- mean(muestra_B)
desviacion_B <- sd(muestra_B)

# Imprimir los resultados
cat("Muestra A - Media:", media_A, "Desviación estándar:", desviacion_A, "\n")
cat("Muestra B - Media:", media_B, "Desviación estándar:", desviacion_B, "\n")

# Crear el histograma comparativo
hist(muestra_A, breaks = 30, col = rgb(0, 0, 1, 0.5), xlim = c(20, 100), ylim = c(0, 120), 
     xlab = "Valor", ylab = "Frecuencia", main = "Comparación de Distribuciones: Muestra A y B")
hist(muestra_B, breaks = 30, col = rgb(1, 0, 0, 0.5), add = TRUE)
legend("topright", legend = c("Muestra A (media=55, sd=10)", "Muestra B (media=65, sd=15)"), 
       fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))
#--------------------------------------------------------------------------------------------------#
# Resumen de su Funcionamiento                                                                     #
# - Generación de muestras: Se crean dos muestras con diferentes medias y desviaciones estándar.   #
# - Cálculo de estadísticas: Se calculan la media y desviación estándar de ambas muestras.         #
# - Gráfico comparativo: Se dibujan los histogramas superpuestos de ambas muestras con diferentes  #
#   colores y una leyenda explicativa.                                                             #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#
#################################################################################################
#Ejercicio 6
# Parámetros de la distribución normal
media <- 50
desviacion_estandar <- 12

# Generar valores de la variable aleatoria para el gráfico
x <- seq(media - 4 * desviacion_estandar, media + 4 * desviacion_estandar, length.out = 1000)
y <- dnorm(x, mean = media, sd = desviacion_estandar)

# Calcular la probabilidad acumulada hasta 70
prob_70 <- pnorm(70, mean = media, sd = desviacion_estandar)
probabilidad_exceder_70 <- 1 - prob_70

# Crear el gráfico
plot(x, y, type = "l", lwd = 2, col = "blue", 
     main = "Distribución Normal (media=50, desviación estándar=12)",
     xlab = "Valor", ylab = "Densidad de probabilidad")
# Rellenar la zona donde X > 70
polygon(c(70, x[x >= 70], max(x)), c(0, y[x >= 70], 0), col = "red", border = NA)
# Añadir una leyenda
legend("topright", legend = c("Distribución normal", "P(X > 70)"), 
       fill = c("blue", "red"), border = NA)
cat("La probabilidad de que un valor sea mayor que 70 es:", probabilidad_exceder_70, "\n")
#----------------------------------------------------------------------------------------------------#
# Resumen de su Funcionamiento                                                                       #
# - Definir parámetros: Especifica la media y desviación estándar de la distribución.                #
# - Generar datos para el gráfico: Calcula valores de la distribución normal para un rango de x.     #
# - Calcular probabilidades: Obtiene la probabilidad acumulada hasta 70 y la probabilidad de valores #
#   mayores que 70.                                                                                  #
# - Graficar la curva de densidad: Muestra la distribución normal y añade una sombra en rojo para    #
#   valores mayores a 70.                                                                            #
# - Añadir leyenda: Identifica la curva principal y el área bajo la curva que representa P(X > 70).  #
#----------------------------------------------------------------------------------------------------#
#################################################################################################
#Ejercicio 8
# Generar una muestra de 200 valores con media = 30 y desviación estándar = 5
set.seed(123)  # Para reproducibilidad
muestra <- rnorm(200, mean = 30, sd = 5)

# Realizar la prueba de normalidad usando shapiro.test
prueba_normalidad <- shapiro.test(muestra)

# Imprimir los resultados de la prueba de normalidad
cat("Resultado de la prueba de normalidad:\n")
print(prueba_normalidad)

# Crear el gráfico
hist(muestra, main = "Histograma de la Muestra", xlab = "Valor", ylab = "Frecuencia", col = "skyblue", border = "black", breaks = 20)

# Añadir el texto con los resultados de la prueba en el gráfico
text(x = 35, y = 20, labels = paste("Estadístico W:", round(prueba_normalidad$statistic, 3)), col = "black", pos = 4)
text(x = 35, y = 18, labels = paste("Valor p:", round(prueba_normalidad$p.value, 3)), col = "black", pos = 4)

# Imprimir los resultados de la prueba de normalidad
cat("Resultado de la prueba de normalidad:\n")
print(prueba_normalidad)
#-----------------------------------------------------------------------------------------#
# Resumen de su Funcionamiento                                                            #
# - Genera la muestra y realiza la prueba de normalidad.                                  #
# - Muestra los resultados en la consola.                                                 #
# - Crea el histograma de la muestra e incluye los resultados de la prueba en el gráfico. #
#                                                                                         #
#-----------------------------------------------------------------------------------------#
#################################################################################################
# Ejercicio 10
# Generar las muestras
set.seed(123)
muestra_X <- rnorm(100, mean = 10, sd = 3)
muestra_Y <- rnorm(100, mean = 15, sd = 4)
muestra_Z <- muestra_X + muestra_Y

# Calcular estadísticos observados y teóricos
media_Z <- mean(muestra_Z)
desviacion_Z <- sd(muestra_Z)
media_teorica_Z <- 10 + 15
desviacion_teorica_Z <- sqrt(3^2 + 4^2)

# Imprimir resultados en la consola
cat("Media de Z:", media_Z, "\n")
cat("Desviación estándar de Z:", desviacion_Z, "\n")
cat("Media teórica de Z:", media_teorica_Z, "\n")
cat("Desviación estándar teórica de Z:", desviacion_teorica_Z, "\n")

# Crear el histograma de muestra_Z
hist(muestra_Z, breaks = 20, col = "skyblue", border = "black", 
     main = "Distribución de la Muestra Z", xlab = "Valores de Z", ylab = "Frecuencia")

# Agregar líneas verticales para la media y desviación estándar observadas y teóricas
abline(v = media_Z, col = "red", lwd = 2, lty = 2)
abline(v = media_teorica_Z, col = "blue", lwd = 2, lty = 2)

# Añadir texto de los resultados en el gráfico
text(x = media_Z, y = 15, labels = paste("Media de Z:", round(media_Z, 2)), col = "red", pos = 3)
text(x = media_teorica_Z, y = 14, labels = paste("Media teórica de Z:", round(media_teorica_Z, 2)), col = "blue", pos = 3)
text(x = media_Z, y = 12, labels = paste("Desv. estándar de Z:", round(desviacion_Z, 2)), col = "red", pos = 3)
text(x = media_teorica_Z, y = 11, labels = paste("Desv. estándar teórica de Z:", round(desviacion_teorica_Z, 2)), col = "blue", pos = 3)


# Imprimir los resultados
cat("Media de Z:", media_Z, "\n")
cat("Desviación estándar de Z:", desviacion_Z, "\n")
cat("Media teórica de Z:", media_teorica_Z, "\n")
cat("Desviación estándar teórica de Z:", desviacion_teorica_Z, "\n")

#----------------------------------------------------------------------------------------------------#
# Resumen de su Funcionamiento                                                                       #
# - Se genera la muestra muestra_Z a partir de la suma de muestra_X y muestra_Y.                     #
# - Se calculan y se imprimen las medias y desviaciones estándar observadas y teóricas.              #
# - En el gráfico, se muestra el histograma de muestra_Z con líneas verticales para la media         #
#   observada (en rojo) y teórica (en azul), con etiquetas de los valores para cada línea en el      #
#   gráfico.                                                                                         #
#                                                                                                    #
#----------------------------------------------------------------------------------------------------#
