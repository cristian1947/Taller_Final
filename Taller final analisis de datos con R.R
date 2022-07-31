
### TALLER FINAL: ANALISIS DE DATOS CON R
## INTEGRANTES: Cristian Mendez - Lizeth Vasquez - Sebastian Osorio


###  PUNTO_1

# 1.1. Usted está diseñando un programa que devuelva mensajes automatizados a un restaurante, de acuerdo con la calificación de sus 
# clientes en una escala de 0 a 30. Para ello, se le pide implementar un bucle de R que:

# Imprima en consola "¡Hay mucho por mejorar!" si la calificación está entre 0 y 10 (excluyendo el 10).
# Imprima en consola "¡Bien! Pero podría ser excelente." si la calificación está entre 10 y 20 (sin incluir el 20).
# Imprima en consola "¡Excelente Servicio! Sigue así." si la calificación está entre 20 y 30.
install.packages("svDialogs")
library("svDialogs")
nota <- dlg_input(message =  "Califique su experiencia en el restaurante en una escala de 0 a 30: ")$res
nota <- as.integer(nota)
if (nota <= 9){
  print("¡Hay mucho por mejorar!")
} else if (nota >= 10 & nota <20){
  print("¡Bien! Pero podría ser excelente.") 
} else if(nota >= 20 & nota <=30){
  print("¡Excelente Servicio! Sigue así.")
}

      
## Punto 1.2. funciones
#Posteriormente, le indican que hubo un cambio en el sistema de calificaciones, que permite desagregar 
#la calificación del restaurante en 3 elementos: calidad de la comida, decoración y servicio, cada uno en 
#escala de 0 a 30.
      
#Sin embargo, es necesario que se mantenga un indicador de calificación conjunto. Para ello, implemente 
#una función en R que reciba 3 argumentos -calidad, decoración, servicio- y devuelva la suma de los 3.
      
#Además, esta función debe incluir valores por defecto para las variables en caso de que el usuario 
#no las incluya. Los valores por defecto deben ser:
        
#Calidad: 10
#Decoración: 5
#Servicio: 15
      
cal_tot <- function(cal = 10, deco = 5, serv = 15){
  notat <- cal + deco + serv
  return(notat)
}
cal_tot(20,25,30)
cal_tot(1)
cal_tot(,2,)

###    PUNTO_2

# Cargar los paquetes necesarios
library(tidyverse)

##Parte 2. Tidyverse


#Luego de realizar las operaciones anteriores, se le entrega informacion sobre 
#los precios y calificaciones de 168 restaurantes italianos en Manhattan,
#separados en dos archivos: price_ratings.csv y restaurant_locations.csv.

#La primera tabla contiene un identificador ?nico para los restaurantes ( Id), 
#una columna que incluye varias medidas asociadas al restaurante ( Variable) y 
#el valor espec?fico de dichas medidas ( Value).

#La segunda tabla contiene un identificador ?nico para los restaurantes 
#( Id_restaurant), el nombre del Restaurante ( Restaurant), y una variable
#que indica si el restaurante se ubica al este o al oeste de 5th Avenue ( East).

#Punto 2.1. cargar datos
#Usando la funci?n read_csv()cargar ambos archivos en su entorno de trabajo. 


##Cargar la base de datos
DATA1 <-read_csv("C:/Users/Sebastian Osorio/Desktop/Universidad/2022-V/Clase_fundamentosR/DATA/price-ratings.csv")
head(DATA1)
DATA2 <-read_csv("C:/Users/Sebastian Osorio/Desktop/Universidad/2022-V/Clase_fundamentosR/DATA/restaurante-ubicaciones.csv")
head(DATA2)

##Posteriormente imprima en consola la estructura de ambos dataframes y 
##las primeras 8 filas.


head(DATA1, 8)

head(DATA2, 8)

##Ambos dataframes cumplen con que cada fila es una observacion y cada 
##columna es una variable?

### Al verificar en la consola notamos que ambas bases de datos si cumplen con
###que cada fila es una observacion y cada columna es una variable 

## Nota: Para imprimir la estructura no basta con escribir las funciones 
##colnames()o nrows().

#Punto 2.2. Pivote
#Utilizando las funciones vistas en clase y que hacen parte del tidyverse, 
#convierta el dataframe del archivo price_ratings.csv en formato wide. 
#Es decir, que Price, Food, Decor y Service sean columnas individuales.

prueba_wider <- DATA1 %>%
  pivot_wider(names_from = Variable, values_from = Valor)
head(prueba_wider)

#Nota: Esto implica que el dataframe resultante debe tener 168 filas y 5 columnas.

#Punto 2.3. Uniones
#Usando las funciones vistas en clase, una las dos tablas con base en las 
#columnas que identifican los restaurantes: Id y Id_restaurant.

#Nota: Utilice la funciOn inner_join()para resolver el punto.

resultado_inner <- prueba_wider %>%
  inner_join(DATA2, by = c("Id" = "Id_restaurant"))

#Punto 2.4. Seleccione y organice
#Con base en el dataframe completo que resulta del punto anterior, 
#escriba la secuencia de comandos que devuelve un dataframe que contenga las 
#columnas de nombre, precio y servicio, ordenados seg?n la variable precio.


Precios_rest <- select(resultado_inner, Restaurant, Price, Service)
Precios_rest1 <- arrange(Precios_rest, desc(Price))
head(Precios_rest1, 5)
tail(Precios_rest1, 5)

# cual es el restaurante mas caro? Cual es el mas barato?
## Los restaurantes mas caros son Harry Cipriani, Rainbow Grill y San Domenico -- 
## El restaurante mas barato es Lamarca --  

#Punto 2.5. Agrupar_por y Resumir----
#Usando el dataframe completo, construya un dataframe que agrupe los restaurantes
#de acuerdo con la variable East y calcule el precio promedio de la comida.

East_ <- resultado_inner %>%
 group_by(East) %>%
  summarise(precio_promedio = mean(Price)) %>%
  arrange(desc(precio_promedio))
head(East_)
 
## Con un precio promedio de $44 dolares la comida es mas cara en el lado Este de Manhattan, y en el oeste el precio promedio es de $40.4 dolares.---



###   PUNTO_3 ANALISIS ESTADISTICO


##  Punto 3.1. Regresión lineal


# Usando la función lm() de R, construya una regresión lineal de la variable Price contra Food, Decor, Service y East y guarde el resultado en un objeto llamado regresion.
# Posteriormente, ejecute el comando summary(regresion).
#¿Todas las variables son estadísticamente significaticas? ¿Qué variable parece influir más en el precio de la comida?


install.packages('car')
install.packages('lmtest')
library(car)
library(lmtest)

prueba_wider <- DATA2 %>%
  pivot_wider(names_from = Variable, values_from = Valor)
head(prueba_wider)

resultado_inner <- prueba_wider %>%
  inner_join(DATA, by = c("Id" = "Id_restaurant"))

regresion = lm(Price ~ Food +
            Decor +
            Service +
            East, data = resultado_inner)

summary(regresion)

## Segun el modelo de la regresion no todas las variables son significativas ya que la variable 
## service(servicio) no es significativa en el modelo.
## la variable que parece influir mas en el precio es el lugar (east) ya que mas al este aporta el 2.06 mas en el precio



###   PUNTO_4


##Punto 4.1. Gráfico de Densidad ----
##Cree un gráfico con la estimación de la función de densidad para la variable Service, con el atributo linetype asociado a la variable East. Añada un título de su elección y cambie los nombres de los ejes y la leyenda para que aparezcan en español.

##¿Cuál zona de Manhattan tiende a tener mejor calificación de servicio?
  
  #Nota: Para hacer este gráfico, no olvide convertir la variable East a factor.

GF1 <- ggplot(resultado_inner, aes(Service, linetype = factor(East))) +
  geom_density() +
  labs (
    title = "Graph Densidad",
    x = "Calificación de Servicio",
    y = "Densidad"
)
GF1

## La zona del Este de Manhattan tiende a tener mejor calificacion de servicio. 

## PUNTO 4.2----
##Cree un gráfico de dispersión entre las variables Price y Decor, donde el color de los puntos esté asociado a la variable East. Modifique la escala de color de tal forma que:
#El color de 0 sea azul y de 1 sea verde.
##La etiqueta de 0 sea Oeste y de 1 sea Este.
##Según el gráfico, ¿A qué zona de Manhattan pertenece el restaurante con menor precio?
##Nota: Para hacer este gráfico, no olvide convertir la variable East a factor.


GF2 <- ggplot(resultado_inner, aes(Price, Decor, colour = factor(East))) +
  geom_point() +
  labs(
    title = "GRÁFICO DE DISPERSIÓN",
    x = "Precio",
    y = "Decoración",
    colour = "ZONA"
  ) +
  scale_color_manual(
    labels = c("Oeste",
               "Este"),
    values = c("lightblue",
               "green")
  )
GF2

### De acuerdo al gráfico generado, se evidencia que el restaurante 
### más economico de Manhattan está ubicado en la zona Oeste. 
