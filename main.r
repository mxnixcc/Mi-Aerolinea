numeros <- c(10, 20, 30, 40, 50)
numeros
View (numeros)
#Medias
numeros_modificados <- numeros + 5
numeros_modificados
print(numeros_modificados)

#Sumas
suma_numeros <- sum(numeros)
suma_numeros_modificados <- sum(numeros_modificados)
suma_numeros
suma_numeros_modificados

cat("Media:", media_numeros, "\nSuma", suma_numeros)

#DF estudiantes y sus calificaciones

estudiantes <- data.frame(
  Nombre = c("Ana","Luis","Carlos","Marta","Sofía"),
  Edad = c(23, 22, 21, 22,34),
  Calificacion = c(95, 98, 92, 85, 97)
)

#Estudiantes calificados sobre 90 (Filtro)

estudiantes_destacados <- estudiantes[estudiantes$Calificacion > 90,]

estudiantes_destacados

#Visualización
install.packages("ggplot2")
library(ggplot2)

data <- data.frame(
  Categoria = c("A", "B", "C", "D"),
  Valores = c(10, 23, 17, -9)
)

data

ggplot(data, aes(x = Categoria, y = Valores)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  labs(title = "Gráfico de barras", x = "Categorías", y = "Valores")

#Analisis de estadisticas simples

grupo_1 <- c(85, 90, 78, 92, 88, 85, 87)
grupo_2 <- c(82, 89, 77, 91, 85, 86, 86, 88)

resultado_ttest <- t.test(grupo_1, grupo_2)

resultado_ttest

#Generar 100 numeros aleatorio con dist normal

datos_aleatorios <- rnorm(100, mean = 50, sd = 10)
datos_aleatorios

#Historiograma
hist(datos_aleatorios,
     main = "Historiograma de datos aleatorios",
     xlab = "Valores",
     col = "lightblue",
     border = "black")

summary(datos_aleatorios)

#Simulación de Lanzamiento

set.seed(123)
lanzamientos <- sample(c("Cara", "Sello"), size = 100, replace = TRUE)
#Contar resultados

resultado <- table(lanzamientos)

resultado

#Secuencia de numeros del 1 al 9
secuencia <- 1 : 9
matriz <- matrix(secuencia, nrow = 3, ncol =3)

matriz
traspuesta <- t(matriz)

traspuesta

inversa <- solve(matriz)
inversa


#install.packages(c("nycflights13", "tidyverse", "lubridate", "maps", "ggrpel", "tidyr"))
#install.packages("ggridges")
#install.packages("ggrepel")

library(nycflights13)
library(tidyverse)
library(lubridate)
library(maps)
library(ggrepel)
library(tidyr)
library(ggridges)
library(dplyr)

#Asignar nombres a tablas

flights <- nycflights13::flights
airlines <- nycflights13::airlines
airports <- nycflights13::airports
planes <- nycflights13::planes
weather <- nycflights13::weather

View(weather)

glimpse(flights)
summary(flights)

#Vuelos por mes
flights %>%
  count(month) %>%
  ggplot(aes(factor(month), n))+
  geom_col()+
  labs(x = "Mes", y = "Numero de vuelos", title = "Cantidad de vuelos por mes")

#Retrasos promedios por aereolinea
flights%>%
  group_by(carrier) %>%
  summarise(mean_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(mean_dep_delay))

flights %>%
  mutate(wday = wday(make_date(year, month, day), label = TRUE)) %>%
  group_by(wday) %>%
  summarise(mean_dep_delay = mean(dep_delay, na.rm =  TRUE)) %>%
  ggplot(aes(x = wday, y = mean_dep_delay, group = 1)) +
  geom_line()+
  labs(x = "Día de la semana", y = "Retraso promedio (minutos)")
  
flights %>%
  filter(!is.na(dep_delay)) %>%
  ggplot(aes(dep_delay))+
  geom_histogram(binwidth = 50, fill ="skyblue", color ="black" )+
  labs(title = "Historiograma de retrasos de salida", x = "Retraso (min)", y ="Frecuencia")

flights %>%
  summarise(
    Media = mean(dep_delay, na.rm = TRUE),
    Mediana = median(dep_delay, na.rm = TRUE),
    q25 = quantile(dep_delay, 0.25, na.rm = TRUE),
    q75 = quantile(dep_delay, 0.75, na.rm = TRUE)
  )

x <- seq(-pi, pi, 0.1)
plot(x, cos(x))