###### INTRODUCCION A GGPLOT2 #####
library(ggplot2)

# estandar unicode: UTF8
# si se te destroza el fichero por culpa de los putos acentos -- abrir file, reopen with encoding

mpg
mtcars
ggplot2::aes() # con esto no cargas la libreria entera, accedes solo a lo que quieres de esa libreria

##### PRIMER EJEMPLO ######
ggplot(mpg, aes(class)) + geom_bar()

##### SEGUNDO EJEMPLO #####
ggplot(mpg, aes(class)) + geom_point()

# nos da un error, la geometria de puntos no tiene sentido sino hay un parametro en el eje y

##### EJERCICIO 2 #####
ggplot(mpg, aes( x = class, y = displ)) + geom_point()

##### EJERCICIO 3 #####
#x = cty y = displ size = hwy color = clase
ggplot(mpg, aes( x = cty, y = displ, size = hwy, color = class)) + geom_point()

##### CAPAS #####
#despues de poner la base pongo las geometrias y se pintan en el orden que las pongas

##### EJERCICIO 4 #####
# x = cty y = displ size = hwy color = class
ggplot(mpg, aes( x = cty)) + #aes que son compartidos por ambos
  geom_bar(fill = "grey") + #es una constante el gris, las barras son grises siempre
  geom_point(aes( y = displ, size = hwy, color = class))  
  

##### EJERCICIO 5: PARA CASA ##### 

##### FACET #####

##### EJERCICIO 6 #####
ggplot(mpg, aes( x = cty, y = displ)) +
  geom_point() + 
  facet_grid(.~year) 

##### EJERCICIO 7 #####
ggplot(mpg, aes( x = cty, y = displ)) +
  geom_point() + 
  facet_wrap(.~manufacturer, ncol = 3) 

##### ESTILADO #####
ggplot(mpg, aes( x = cty, y = displ)) +
  geom_point() + 
  facet_wrap(.~manufacturer, ncol = 3) +
  theme_minimal()

##### ESTADISTICAS #####

##### EJERCICIO 9 #####
base <- ggplot(mpg, aes( x = cty, y = displ)) 

base + geom_density2d()

base + geom_hex()

base + stat_smooth()

##### TRUCO: OTRAS LIBRERIAS #####

## plot.ly 
library(plotly)

miGrafica <- ggplot(mpg, aes( x = class, y = displ)) + geom_point()
ggplotly(miGrafica)
