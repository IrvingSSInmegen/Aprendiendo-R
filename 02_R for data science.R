##################################
##################################
###########            ###########
##########   MÓDULO 2   ##########
###########            ###########
##################################
###################################

library(dplyr)
library(ggplot2)
library(magrittr)
library(reader)
library(data.table)
library(vroom)
library(readxl)
library(janitor)

#####################
## LEER UN ARCHIVO ##
#####################

#Ya sabia cargar un .cvs desde mi ordenador y tambien desde una URL
#ejemplo Iris

read.table(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_01.csv")
#Asi como esta ahora solo nos arroja una columna con todos los datos amontonados.

read.table(file = ,header = ,sep = ,quote = ,row.names =,col.names = ,skip = ,nrows = ,stringsAsFactors = 
           ,dec = ,numerals = ,as.is = ,na.strings = ,colClasses = ,check.names = ,fill = 
           ,strip.white = ,blank.lines.skip = ,comment.char = ,allowEscapes = ,flush = ,fileEncoding = 
           ,encoding = ,text = ,skipNul = )
#Si damos tabulador dentro de la función nos da todas las opciones que ofrece la funcion a usar.
#Hay que poner las condiciones necesarias para que nuestro dataframe salga bien.

my_iris <- read.table(file ="https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_01.csv",
                       header = TRUE,
                       sep = ",",
                       stringsAsFactors = TRUE)
#Tabla de datos con especificaciones, ya nos arroja una tabla de datos ordenada con filas y columnas.

head(my_iris)
#head() muestra los primeros 6 valores de la tabla de datos.

str(object = my_iris)  
# Muestra información sobre la tabla de datos

summary(object = my_iris)
# Arroja información estadistica sobre cada columna que tenga valores numéricos
# y al parecer en los datos que son carácteres nos da el total de datos que
# hay de cada categoria.

boxplot(my_iris)
# Diagrama de caja

pairs(my_iris)
# Otro tipo de gráfica, con puntos. ¿Qué quiere decir?
# Distribución de los datos.

my_iris %>% ggplot(aes(x = Petal.Length,
                       y = Sepal.Length,
                       colour = Species)) +
                      geom_point() + 
              ggtitle('Especies de iris',
                      subtitle = "por longuitud de petalo y sepalo")
# Mismo tipo de gráfica que pairs() pero solo usa dos variables.

####################
## LEER CON READR ##
####################

read_delim() 
#Datos delimitados (cualquier separador)

read_csv()   
#Datos separados por comas

read_tsv()   
#Datos separados por tabuladores 
# Nota: read.tsv() no me aparece en el paquete

read_table() 
#Datos separados por espacio en blanco (esapcios, tabs,...)

#ejemplo iris

my_iris_tab <- readr::read_csv(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_01.csv" )

#Parsed with column specification:
#cols(
#  Sepal.Length = col_double(),
#  Sepal.Width = col_double(),
#  Petal.Length = col_double(),
#  Petal.Width = col_double(),
#  Species = col_character()
# )
# read_csv tambinén nos arroja información sobre el nombre de las columnas y que tipo de datos contienen.
#   Al parecer se llaman "tibble"
# Es una versión mas moderna de los data.frame
#   ventajas: Print mas limpio
#             Solo algunos renglones ( no necesitmaos head())
#             Indica dimensiones y tipos de variables ( no necesitamos str(), dim())
#             No obliga a que los strings sean factores por default
#             No usa rownames (¿y eso es algo bueno?)
# Los tibbles pueden hacer todo lo que los data frames tradicionales

str(my_iris_tab)

# Cuidado los métodos de tibble no adivinan cosas como los de base.

boxplot(my_iris_tab)

# ¿Por qué falló ese boxplot? ¿ Cómo solucionarlo?
#  Fallo porque la columna de Species contiene caracteres, como ya se dijo
#  read_cvs no los convierte en factores.
#  volviendo a utilizar as.factor???

###########################
##  LEER CON DATA.TABLE  ##
###########################

#   El paquete data.table esta orientado a big data
#   Es una filosofía distinta a tydi; inspirada en SQL
#   Bastante más rápido; ¿sintaxis es mas espartana?

my_iris_dt = data.table::fread(input = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_01.csv" )
my_iris_dt
my_iris_dt %>% str()
#   Nota:tampoco convierte la columna de caracteres en factores.

######################
##  LEER CON Vroom  ##
######################

# Rápido
# Eficiente con la memoria
# Devuelve tibbles ---> tydi

my_iris_vroom = vroom::vroom(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_01.csv")
my_iris_vroom

#######################
##  LEER CON readxl  ##
#######################

#   Este paquete puede leer datos de excel en diferentes formatos ( xls,xlsx,...)

readxl::read_xlsx(path = "datasets_clasicos.xlsx")
readxl::read_xlsx(path = "datasets_clasicos.xlsx",sheet = 1)
readxl::read_xlsx(path = "datasets_clasicos.xlsx",sheet = 2)
#   Podemos escoger que página del excel mostrar.

readxl::read_xlsx(path = "datasets_clasicos.xlsx",sheet = "USarrests")
#   Tambien se puede escoger por nombre de la página.

readxl::read_xlsx(path = "datasets_clasicos.xlsx",sheet = 4)
#   Aparece la primera linea de la página, la cual no queremos
#   para que solo se aprecie la tabla de datos.

readxl::read_xlsx(path = "datasets_clasicos.xlsx",sheet = 4,skip = 1)
#   Con la opción skip podemos saltar lineas que no queremos cargar
#   en nuestros datos

####################################
##  LIMPIEZA DE HEADERS: JANITOR  ##
####################################

#   Si tenemos datos con cabezales que no son limpios como por ejemplo
#   espacios, simbolos raros, MaYuscUlas mal usadas,...etc
#   Cuando pasa esto podemos usar janitor::clean_names

my_genes <- vroom::vroom("https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/genes_500.txt")
my_genes

my_genes <- janitor::clean_names(dat = my_genes,case = "snake")
my_genes
#   Parece que simplemente limpia espacios en los cabezales
#   y cambia todo a minúsculas.Además cambia el simbolo % por la palabra percent
#   Nota: vroom te da un tibble.

my_genes <- janitor::clean_names(dat = my_genes,case = "screaming_snake")
my_genes
#   Cambia los cabezales a mayúsculas y espacios por una barra baja

my_genes <- janitor::clean_names(dat = my_genes,case = "lower_camel")
my_genes
#   Quita espacios, junta las palabras pero agrega una mayuscula

my_genes <- janitor::clean_names(dat = my_genes,case = "upper_camel")
my_genes
#   Combinación entre matusculas y minúsculas sin espacios

my_genes <- janitor::clean_names(dat = my_genes,case = "lower_upper")
my_genes
#   Combinacion entre palabras con mayúsculas y minúsculas

my_genes <- janitor::clean_names(dat = my_genes,case = "upper_lower")
my_genes
#   Lo mismo que la anterior pero al revés

##########################
## OTROS TIPOS DE DATOS ##
##########################

#   SUPONGO QUE SON OTROS TIPOS DE ARCHIVOS QUE SE PUEDEN CARGAN EN r
#   Haven, googledrive, googlesheets


#########################
## TIPOS DE DATOS EN R ##
#########################

# Ordenados de menos a mas ¿flexibles?

#Lógicos
my_logic = c(TRUE, TRUE, FALSE)
# Esto crea un vector o una lista 
my_logic
# Muestra los datos dentro de la lista 
typeof(my_logic)
# typeof() dice que tipo de datos contiene la lista
# Nota: si agregamos un número en la lista toma el TRUE o FALSE como 1 y 0 

#Enteros
my_integer = c (7L, 2L, 5L)
my_integer
typeof(my_integer)

#Doble
my_double = c(7.25, 3, 14)
my_double
typeof(my_double)

#Caracter
my_char = c("Esto","es","1","de","muchos","textos","TRUE")
my_char
typeof(my_char)



################
##  FACTORES  ##
################

# Los factores son vectores atómicos que contienen valores predefinidos.
# Permiten almacenar variables categóricas
my_factor <- factor(c("interno","externo","externo","interno"))
my_factor  
View(my_factor)

levels(my_factor)
#levels: creo que son las clases que se encuentran en el vector.

class(my_factor)
#Los factores son en realidad numéricos

typeof(my_factor)
as.numeric(my_factor)
#Parece ser que se les asiga una numeración a los diferentes tipos de clases.

############################
############################
#####                  #####
####  EJERCICIO MTCARS  ####
#####                  #####
############################
############################

my_cars <- read.table(file ="https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/mtcars_01.csv",
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = TRUE)

my_cars %>% head()
#head(my_cars)
#Practicamente gastas mas código si lo usas solo para una función
#pero parece práctico si lo usas con varias.

str(object = my_cars)
summary(object = my_cars)
boxplot(my_cars)
pairs(my_cars)

# ¿Cuántas variables y observaciones tiene el conjunto de datos mtcars?

str(object = my_cars)
# 32 observaciones (marca del auto) y 12 variables (especificaciones del auto)
# Al parecer las variables siempre van a ir en la parte superior de las tablas de datos

dim(my_cars)
nrow(my_cars)
ncol(my_cars)

# Encuentre una pareja de variables tales que los autos se separen por el numero de cilindros (cyl)
# R= las dos variables son nombre del auto (columna 1) y la cyl 

my_cars %>% ggplot(aes(x = cyl,
                       y = X,
                       colour = cyl)) +
  geom_point() + 
  ggtitle('Carros con la misma cilindrada',
          subtitle = "separados por marca")

#Separados por caballos de fuerza
my_cars %>% ggplot(aes(x = hp,
                       y = X,
                       colour = hp)) +
  geom_point() + 
  ggtitle('Carros con la misma cilindrada',
          subtitle = "separados por marca")



###########################
##  ¿MAS COMANDOS DE R?  ##
###########################

#saveRDS() --> RDS --> Guarda un objeto

#save() --> RData --> Guarda objetos en el espacio de trabajo nombrados

#save.image() --> RData --> Guarda todos los objetos en el espacio de trabajo nombrados

#   Creo que ya lo hace Rstdio en windows cada que cierro seción
#   Cuando vuelvo a abrir se quedan guardados los dataset cargados 
#   Lo que no se guarda son las imagenes ploteadas.


#   No encuentro mucha utilidad a estas funciones.

otro_iris_mas <- iris
#   Creo que son datos que ya tiene R precargados.
save(otro_iris_mas,file = "results/iris_forever.RData" )
rm(otro_iris_mas)
otro_iris_mas
load("results/iris_forever.RData")
otro_iris_mas
otro_iris_mas <- iris
save.image(file = "results/sesion_01.RData")
otro_iris_mas <- iris
rm(list =ls())
my_iris
load("results/sesion_01.RData")
otro_iris_mas %>% head()
my_iris_vroom

#   Todos los paquetes de lectura tienen su versión de lectura
#   read.table <--> write.table
#   data.table::fread <--> data.table::fwrite
#   vroom::vroom <--> vroom::vroom_write

write.table(x = my_cars,
            file = "mmtcar.txt",
            sep = "\t",
            quote = FALSE,
            row.names = TRUE,
            col.names = TRUE)
#   Creo que lo que hace esta función es crear un nuevo archivo 
#   en este caso me creo un archivo de texto (bloc de notas)
#   con el nombre mmtcar.txt de manera que ya aparecen los datos ordenados,
#   separados por tabulación, con las filas numeradas y las columnas nombradas.

#   ¿Qué pasa si cambiamos los parámtetros?

write.table(x = my_cars,
            file = "mmtcar1.txt",
            sep = "\t",
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE)
#   No pone las filas numeradas, y quita los nombres de las columnas.

write.table(x = my_cars,
            file = "mmtcar2.txt",
            sep = "\t",
            quote = TRUE,
            row.names = TRUE,
            col.names = TRUE)
#   quote= TRUE pone comillas en los valores que son caracteres.


#####################
#####################
#####           #####
####  EJERCICIO  ####
#####           #####
#####################
#####################

#   Leer y manipular el rarchivo: iris_esp.cvs
#   Lo podemos leer con readr

iris_esp_dr <- readr::read_csv(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_esp.csv"
                               ,comment = "#")
View(iris_esp)

iris_esp_dr <- janitor::clean_names(dat = iris_esp_dr,case = "snake")
iris_esp <- janitor::clean_names(dat = iris_esp_dr,case = "screaming_snake")
iris_esp <- janitor::clean_names(dat = iris_esp_dr,case = "upper_lower")
iris_esp <- janitor::clean_names(dat = iris_esp_dr,case = "lower_upper")
iris_esp <- janitor::clean_names(dat = iris_esp_dr,case = "upper_camel")
iris_esp <- janitor::clean_names(dat = iris_esp_dr,case = "lower_camel")


#   Se necesita saber que case de janitor aplciar primero para que queden bien los cabezales
#   Creo que lo mejor que pude dejar los cabezales es aplicando
#   1:screaming snake
#   2:upper lower
#   3:screaming snake
#   4:upper lower

#   Podemos leer con data.table
iris_esp_dt <- data.table::fread(input = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_esp.csv" )
iris_esp_dt <- janitor::clean_names(dat = iris_esp_dt,case = "snake")
iris_esp_dt <- janitor::clean_names(dat = iris_esp_dt,case = "screaming_snake")
iris_esp_dt <- janitor::clean_names(dat = iris_esp_dt,case = "upper_lower")
#   Nota: parece que de entraba data.table cambia acentos por símbolos extraños
#         y tambien ya omite los comentarios #

#   Leer con vroom

iris_esp_vroom = vroom::vroom(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_esp.csv"
                              ,comment = "#")
iris_esp_vroom <- janitor::clean_names(dat = iris_esp_vroom,case = "snake")

#   Leer y manipular el rarchivo: mundiales_femenil

mun_fem <- readxl::read_xlsx(path = "mundiales_femenil.xlsx",skip = 1)

#   Ya los cargué, no sé que mas hacerles...




