##################################
## Author: Beltrán Aller López
## Date: 19/10/2019
## Title: apartado2
##################################

library(ggplot2)

data("economics")
data("economics_long")

ggplot(economics_long, aes(x = date, y = value01)) + geom_line(aes(colour = variable))

       