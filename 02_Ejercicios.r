############################
############################
#####                  #####
####     EJERCICIOs     ####
#####                  #####
############################
############################

my_cars <- read.table(file ="https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/mtcars_01.csv",
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = TRUE)
head(my_cars)

# ¿Cuántas variables y observaciones tiene el conjunto de datos mtcars?

str(object = my_cars)
# 32 observaciones (marca del auto) y 12 variables (especificaciones del auto)

#  Encuentre una pareja de variables tales que los autos se separen por el numero de cilindros (cyl)
#  R= las dos variables son: nombre del auto (columna 1) y la cyl (columna 3)

my_cars %>% ggplot(aes(x = cyl,
                       y = X,
                       colour = cyl)) +
  geom_point() + 
  ggtitle('Carros con la misma cilindrada',
          subtitle = "separados por marca")

##################################################################################################################
##################################################################################################################
#ejemplo iris

my_iris_tab <- readr::read_csv(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_01.csv" )

boxplot(my_iris_tab)

# ¿Por qué falló ese bosplot? 
#  Fallo porque la columna de Species contiene caracteres, como ya se dijo
#  read_cvs no los convierte en factores, si bien los determina como clases
#  pero no les establece un valor numérico.
#
#  ¿ Cómo solucionarlo?
#  volviendo a utilizar as.factor???

# Entiendo que as.factor() convierte un vector en factores, osea
# podemos cambiar un vector de caracteres en clases
# pero como aplicarlo para que se cambie en una tabla de datos 
# solo una columna???

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







