##########CONJUNTO DE DATOS##########
data("iris")

##########VISUALIZACION##########
View(iris)

##########SOURCE##########

medias <- sapply(iris[1:4], mean)
barplot(medias, main = "Media de cada una de las variables", ylab = "cm")

desviaciones <- sapply(iris[1:4], sd)
barplot(desviaciones, main = "Desviación típica de cada variable", ylab = "cm")

boxplot(iris$Sepal.Length)
boxplot(iris[1:4], xlab = "Variables", ylab = "cm")
