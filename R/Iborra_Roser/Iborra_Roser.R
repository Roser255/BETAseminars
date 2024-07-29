library(ggplot2)
library(openxlsx)
b <- read.xlsx("Registre de Temperatures.xlsx")
a <- "HOLA"
print(a)

ggplot(b, aes(x = Temps, y = Temperatura.Pila)) +
  geom_point()