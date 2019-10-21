##################################
## Author: Beltrán Aller López
## Date: 19/10/2019
## Title: apartado4
##################################

library(ggplot2)
data("faithful")

ggplot(faithful, aes(x = eruptions, y = waiting)) + geom_point(aes(alpha = I(0.25)))

       