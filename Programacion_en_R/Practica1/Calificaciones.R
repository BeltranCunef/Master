# AUTHOR: BELTRAN ALLER LOPEZ
# DATE: 28/09/2019
# PURPOSE: READ AND MODIFICATE A FILE WHICH CONTAINS THE STUDENTS CALIFICATIONS

##########LIBRARIES##########
library(readr)


##########SOURCE##########

# cargamos mediante la herramienta Import Dataset el archivo csv, leemos el csv indicando el nombre de archivo,
# que caracter ha de usar para separar las columnas, que carcater se trata de la coma en los double, etc. Y lo mas importante,
# le decimos que nos quite los NA, para que no nos den problemas a la hora de manejar el dataframe
calificaciones <- read_delim("calificaciones ECO 2019.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ","), trim_ws = TRUE, na = "0")
View(calificaciones)

# obtenemos el numero total de alumnos, independientemente de si se presentaron o no
numAlumnos <- nrow(calificaciones)
cat("Hay un total de",numAlumnos, "alumnos")

# Filtramos el dataframe calificaciones, eliminando a aquellos alumnos que no se han presentado
# al examen de junio, mostramos cuantos lo han hecho y
# creamos un nuevo dataframe en el que se encuentran unicamente los que han asistido al
# examen y posteriormente calculamos la nota media

alumnosPresentados <- subset(calificaciones, calificaciones$`Ex. JUNIO-12P` != "NA")
numAlumnosPresentados <- nrow(alumnosPresentados)
cat("Se han presentado un total de",numAlumnosPresentados, "alumnos")

notaMedia <- mean(alumnosPresentados$`Ex. JUNIO-12P`)
notaMedia
cat("La nota media de los alumnos presentados es de:", notaMedia)

# Filtramos el dataframe de todos aquellos que se han presentado al examen final, creamos un nuevo dataframe
# en el que aparecen unicamente los que han aprobado dicho examen y ademas presentan una nota de
# asistencia inferior a 0.5

aprobadosSinAsistencia <- subset(alumnosPresentados, alumnosPresentados$`Ex. JUNIO-12P` >= 5.00 & alumnosPresentados$`Asistencia -1P` < 0.5)
aprobadosSinAsistencia["NOMBRE ALUMNO"]

# Recorremos todas las filas del dataframe calificaciones y vamos comprobando los valores correspondientes
# a la nota final, si es mayor o igual a 9 sobresaliente, mayor o igual a 7 y menor de 8.9 notable,
# mayor o igual a 5 y menor a 6.9 aprobado y menor a 5 suspenso

for (i in 1:nrow(calificaciones)) {
  if(calificaciones$CALIFICACION[i] == ""){
    if(calificaciones$Nota_FINAL[i]>=9.00){
      calificaciones$CALIFICACION[i] <- c("SOBRESALIENTE")
    }
    if(calificaciones$Nota_FINAL[i]>=7.00 & calificaciones$Nota_FINAL[i]<=8.9){
      calificaciones$CALIFICACION[i] <- c("NOTABLE")
    }
    if(calificaciones$Nota_FINAL[i]>=5.00 & calificaciones$Nota_FINAL[i]<=6.9){
      calificaciones$CALIFICACION[i] <- c("APROBADO")
    }
    if(calificaciones$Nota_FINAL[i] < 5.00) {
      calificaciones$CALIFICACION[i] <- c("SUSPENSO")
    }
  }
}

View(calificaciones)

# Ahora procedemos a guardar la tabla modificada con el nombre exigido
write.csv(calificaciones, file = "Calificaciones2019.csv", row.names = FALSE)
