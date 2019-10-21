# AUTHOR: BELTRAN ALLER LOPEZ
# DATE: 02/10/2019
# PURPOSE: CREATE A FUNCTION WHO GENERATES A SPECIFIC NUMBER OF TRIO FROM A FIBONACCI SECUENCE 

##########LIBRARIES##########
library(DescTools)

##########SOURCE##########

# Creamos una funcion denominada ternasn a la cual le pasaremos un numero entero que se 
# correspondera con el numero de ternas pitagoricas a crear
ternasn <- function(numeroDeTernas){
  
  # Tenemos dos contadores, x e y, que se corresponden con los limites del intervalo
  # que pasaremos a la funcion de fibonacci para que genere una secuencia
  contador_x <- 1
  contador_y <- 4
  
  # La variable almacen_secuencias_fib será aquella que almacene las secuencias de fibonacci generadas
  almacen_secuencias_fib <- c()
  
  # Lista_ternas almacenara las ternas creadas
  lista_ternas <- c()
  
  # Con el bucle for realizamos tantas iteraciones como ternas queremos crear
  for (i in 1:numeroDeTernas) {
    
    # Guardamos en almacen_secuencias_fib la secuencia de fibonacci desde x hasta y
    almacen_secuencias_fib <- cbind(almacen_secuencias_fib, Fibonacci(contador_x:contador_y))
    
    # Calculamos los valores de los catetos y la hipotenusa
    cateto_a <- almacen_secuencias_fib[1,i] * almacen_secuencias_fib[4,i]
    cateto_b <- 2 * almacen_secuencias_fib[2,i] * almacen_secuencias_fib[3,i]
    hipotenusa <- (almacen_secuencias_fib[2,i])^2 + (almacen_secuencias_fib[3,i])^2
    
    # Guardamos la terna en la lista
    lista_ternas <- cbind(lista_ternas, c(cateto_a,cateto_b,hipotenusa))
    
    # Incrementamos los contadores x e y en una unidad cada uno para que en la siguiente
    # iteracion la funcion de fibonacci genere una secuencia distinta a la anterior
    contador_x <- contador_x + 1
    contador_y <- contador_y + 1
  }
  print(lista_ternas)
}

##########DOC##########

# Mi funcion ternas genera un dataframe de tantas columnas como ternas se soliciten y de
# tres filas, las dos primeras se corresponden con un cateto cada una respectivamente y la
# tercera con la hipotenusa, de esta forma podemos acceder a cada uno de los valores de
# cada terna en caso de ser necesario

##########RESULTS##########

ternasn(15)


