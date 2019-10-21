##################################
## Author: Beltrán Aller López
## Date: 19/10/2019
## Title: apartado5
##################################

library(ggplot2)
data("faithful")

ggplot(faithful, aes(x = eruptions, y = waiting)) + 
  geom_jitter(position = 'jitter')  
  
