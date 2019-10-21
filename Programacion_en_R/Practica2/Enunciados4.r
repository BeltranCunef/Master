# 1. Crear un nuevo proyecto denominado practica 4.

#CArgo todas las librerias necesaria spara llevar a cabo la practica
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(TeachingDemos)

# 2. Mediante la libreria readr, o mediante los menus de RStudio, leer los datasets sleep.csv  y activities.csv
# ambos archivos deben estar previamente en la carpeta del proyecto creado

#Leo el archivo y lo guardo en un dataframe
activities <- read_csv("activities.csv")

# 3.Comprobar el contenido  con View y contar cuantos NAs hay en la columna GPS del dataset activities

View(activities)
sum(is.na(activities$GPS)) #Hay 305 Na en la columna GPS

# 4. Crear un objeto R denominado act_new que contenga solo las variables 
# siguientes: 1,2,5-6

#Incluyo las variables desde la 1 hasta l 6 y elimino la 3 y 4
act_new <- select(activities, 1:6, -3, -4)
view(act_new)

# 5. Renombrar la variable 'Activity type' con el nombre 'tipo' y la variable 'Time zone' como 'ciudad'

act_new <- rename(act_new, tipo = 'Activity type') 
act_new <- rename(act_new, ciudad = Timezone)
view(act_new)

# 6. Realizar un recuento de tipo de actividad con summary. Para ello 
# debes transformar previamente la variable tipo a factor con as.factor.
# Crea un grafico de barras con dicha variable par visualizar las frecuencias.
# Haz lo mismo para la variable ciudad

act_new$tipo <- as.factor(act_new$tipo)

#hago el recuento
summary(act_new$tipo)

#dibujo el grafico
plot(x = act_new$tipo, main = 'Actividades')

act_new$ciudad <- as.factor(act_new$ciudad)
summary(act_new$ciudad)
plot(act_new$ciudad, main = "Ciudades")

#7. Filtrar los registros de act_new que correspondan con ciudad Amsterdam en otro objeto
# y lo mismo con Madrid. Con esos nuevos objetos determina los deportes que 
# no se practican en Amsterdam y s? en Madrid y viceversa. Genera graficos para visualizar los resultados

#filtro para coger unicamente madrid y amsterdam
ams_mad <- filter(act_new, act_new$ciudad == 'Europe/Madrid' | act_new$ciudad == 'Europe/Amsterdam')

#selecciono unicamente las variables ciudad y tipo
ams_mad <- select(ams_mad, ciudad:tipo)

#represento una tabla y un grafico de puntos para que se vea que deportes se practican en cada ciudad y
#cuales no
table(as.character(ams_mad$ciudad), as.character(ams_mad$tipo))
ggplot(data = ams_mad) + geom_point(mapping = aes(x = tipo, y = ciudad))

#borro el dataframe creado para este apartado
rm(ams_mad)

#en amsterdan se practica dancing y en madrid no, en madrid se practica hiking, other, ski, swimming y tennis
#mientras que en amsterdam no, el resto se practican en ambas

#8. Encontrar las fechas en las que se ha practicado bicicleta o pilates en Amsterdam en el a?o 2019

#creo un dataframe nuevo, en este genero una variable llamado anno de tipo factor, esta sera igual a 
#los cuatro primeros caracteres de la variable 'de', de esta forma ya tengo el anno
act_new2 <- act_new
act_new2 <- mutate(act_new2, anno = as.character(act_new2$de))
act_new2$anno <- substr(act_new2$de, 1, 4)

#creo un nuevo dataframe cogiendo unicamente lo que me interesa del anterior
ams2019 <- subset(act_new2, anno == '2019' & (tipo == 'Cycling' | tipo == 'Pilates') & ciudad == 'Europe/Amsterdam')
view(ams2019)

#borro los datafram creados para el apartado
rm(act_new2)
rm(ams2019)

#9. Crear una nueva variable dif con los minutos de realizaci?n de cada actividad en Amsterdam
# y realizar una representaci?n gr?fica de los resultados con plot y determinar que deporte o deportes
# se han practicado durante dos horas o mas

#creo la variable diferencia restando los tiempo de 'a' y 'de'
act_new <- mutate(act_new, dif = a - de)

#filtro, agrupo por tipo y calculo el total
act_new2 <- act_new %>%
  filter(ciudad == 'Europe/Amsterdam') %>%
  group_by(tipo) %>%
  summarize(count = sum(dif)) 

#visualizao los minutos de cada actividad
act_new2
ggplot(data = act_new2) + geom_col(mapping = aes(x = tipo, y = count))

#filtro para que solo se vean las de 120 minutos o mas
two_or_more <- filter(act_new2, act_new2$count >= 120)
two_or_more
  
#10. Guardar el nuevo dataset en un archivo llamado  "act_new.csv"

write.csv(act_new, file = "act_new.csv", row.names = FALSE)

#-------------------------------
#-----SEGUNDA PARTE-------------
# 11. Cargar el dataset sleep en un objeto llamado sleep

#leo el archivo y guardo en un dataframe
sleep <- read_csv("sleep.csv")

#12. crear un nuevo data set llamado sleep_new que contenga solo las variables
#que contengan informaci?n, que no sean todo cero.

#sumando las columnas veo que variables son todo ceros y cuales no, me quedo con las que no
sleep_new <- sleep[, colSums(sleep != 0, na.rm = TRUE) > 0] 

#13. Renombrar las variables de sleep_new a nombres cortos:

#cambio los nombres
sleep_new <- rename(sleep_new, ligero = 'ligero (s)', profundo = 'profundo (s)', despierto = 'despierto (s)',
                    DurationToSleep = 'Duration to sleep (s)', DurationToWakeUp = 'Duration to wake up (s)')

#14. Eliminar todas las filas que contengan alg?n NA

sleep_new <- na.omit(sleep_new)

# 15. Calcular cuanto tiempo en total se ha dormido cada noche: ligero+profundo

#creo una nueva variable que sea la suma del tiempo de ambos 
sleep_new <- mutate(sleep_new, TiempoTotal = ligero + profundo)

# 16. Visualizacion de la relacion ligero-profundo-total

ligeroPorcentaje <- (sum(sleep_new$ligero)/sum(sleep_new$TiempoTotal))*100
ligeroPorcentaje

profundoPorcentaje <- (sum(sleep_new$profundo)/sum(sleep_new$TiempoTotal))*100
profundoPorcentaje

valores <- c(ligeroPorcentaje, profundoPorcentaje)
label <- paste(valores, '%', sep = ' ')
pie(valores, labels = label, clockwise = TRUE, main = 'Relación sueño', col = color)

# A la vista de los resultados, que tipo de sue?o es mas relevante?

#Es más relevante el sueño profundo, pero solo un poco mas, ambos presentan porcentajes parecidos
#respecto al sueño total

# 17. Realizar un analisis de diferencias entre los dos tipos de sue?o e interpretar los resultados
# usar la funci?n ICalpha o el 'One sample t-test' de TeachingDemos: t.test()
# nos devuelve un intervalo de confianza, si el cero pertenece es que no hay diferencias significativas
# sino pertecene si que las hay y hay que decir el nivel

ICalpha<-function(ModeloA, ModeloB, alfa=0.05)
{
  n<-length(ModeloA)
  diferencias<-ModeloA-ModeloB
  mediad<-mean(diferencias)
  #mediad2<-mean(diferencias^2)
  s<-sqrt(var(diferencias))
  #s<-sqrt(mediad2-mediad^2)
  valort<-qt(alfa/2,n-1,lower.tail = F)
  valor<-valort*s/sqrt(n)
  cotaInf<-mediad-valor
  cotaSup<-mediad+valor
  df<-data.frame(cotaInf, cotaSup)
  return(df)
}

#primero represento graficamente los modelos, en este caso sueño ligero y sueño profundo

plot(sleep_new$ligero, sleep_new$profundo)

#planteo el contraste de hipotesis:

# H0: mean(sleep_new$ligero) - mean(sleep_new$profundo) = 0 -> no existen diferencias significativas
# H1: mean(sleep_new$ligero) - mean(sleep_new$profundo) != 0 -> existen diferencias significativas

#el contraste de hipotesis se realizara con un nivel de confianza del 95%
# y un nivel de significatividad del 5%

#a continuacion utilizo la funcion ICalpha para resolver el contraste de hipotesis

ICalpha(sleep_new$ligero, sleep_new$profundo)

#el intervalo de confianza es (-1731.623 , -491.9643)
#el cero no esta incluido en el intervalo, por lo tanto con estos datos y
#con un nivel de confianza del 95% y de significatividad del 5%
#se rechaza la hipotesis nula

#18. Crear una nueva variable 'ciudad' en sleep_new con la informacion de act_new.

#en primer lugar creo una nueva variable en la que solo almaceno la fecha, es lo que me importa
#asi luego puedo agrupar la informacion en funcion de esta

fecha <- substr(act_new$de, 1, 10)
act_new$fecha <-as.factor(fecha)

fecha <- substr(sleep_new$de, 1, 10)
sleep_new$fecha <-as.factor(fecha)

#las variable de y a ya no me importan, las borro para que no molesten

act_new <- select(act_new, -de, -a)
sleep_new <- select(sleep_new, -de, -a)

#ahora genero un nuevo dataframe agrupando la info de de act_new por fecha y ciudad
#ademas obtengo la suma de todo el tiempo de deporte

act_new2 <- act_new %>% group_by(fecha, ciudad) %>% summarize(count = sum(dif))

#ahora junto ambos dataframe en funcion de la fecha
sleep_new <- inner_join(act_new2, sleep_new)

#19. Representar la relaci?n totalsleep y profundo usando como facetas el factor ciudad

#agrupo por ciudad y creo agregaciones para el sueno profundo y total, luego a partir de
#estas calculo el porcentaje que supone el sueno profundo sobre el total

sleep_new2 <- sleep_new %>% group_by(ciudad) %>% summarize(profundo = sum(profundo), total = sum(TiempoTotal), relacion_sleep = profundo/total)
sleep_new2

#20. Guardar el dataset sleep_new en un archivo "sleep_new.csv"

write.csv(sleep_new, file = "sleep_new.csv", row.names = FALSE)

#21. Guardar el proyecto completo. Subir la carpeta del proyecto al campus.