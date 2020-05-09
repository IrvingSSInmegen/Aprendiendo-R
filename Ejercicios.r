##############################
##############################
#####                    #####
####     EJERCICIOs M2    ####
#####                    #####
##############################
##############################

my_cars <- read.table(file ="https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/mtcars_01.csv",
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = TRUE)
head(my_cars)

# �Cu�ntas variables y observaciones tiene el conjunto de datos mtcars?

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

my_iris_tab <- readr::read_csv(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_01.csv")

# �Por qu� fall� ese boxplot? 
#  Fallo porque la columna de Species contiene caracteres, como ya se dijo
#  read_cvs no los convierte en factores, si bien los determina como clases
#  pero no les establece un valor num�rico.
#
#  � C�mo solucionarlo?
#  Usando

my_iris_tab$Species = as.factor(my_iris_tab$Species) 

boxplot(my_iris_tab)

# Entiendo que as.factor() convierte un vector en factores, osea
# podemos cambiar un vector de caracteres en clases
# pero como aplicarlo para que se cambie en una tabla de datos 
# solo una columna  �RESUELTO!

##################################################################################################################
##################################################################################################################

#   Leer y manipular el rarchivo: iris_esp.cvs
#   Lo podemos leer con readr

iris_esp_dr <- readr::read_csv(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_esp.csv"
                            ,comment = "#")
View(iris_esp)

iris_esp_dr <- janitor::clean_names(dat = iris_esp_dr,case = "snake")
iris_esp_dr <- janitor::clean_names(dat = iris_esp_dr,case = "screaming_snake")
iris_esp_dr <- janitor::clean_names(dat = iris_esp_dr,case = "upper_lower")
iris_esp_dr <- janitor::clean_names(dat = iris_esp_dr,case = "lower_upper")
iris_esp_dr <- janitor::clean_names(dat = iris_esp_dr,case = "upper_camel")
iris_esp_dr <- janitor::clean_names(dat = iris_esp_dr,case = "lower_camel")


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
#   Nota: parece que de entraba data.table cambia acentos por s�mbolos extra�os
#         y tambien ya omite los comentarios #

#   Leer con vroom

iris_esp_vroom = vroom::vroom(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_esp.csv"
                              ,comment = "#")
iris_esp_vroom <- janitor::clean_names(dat = iris_esp_vroom,case = "snake")

#   Leer y manipular el rarchivo: mundiales_femenil

mun_fem <- readxl::read_xlsx(path = "mundiales_femenil.xlsx",skip = 1)

#   Ya los cargu�, no s� que mas hacerles...limpiar bien los cabezales??

##############################
##############################
#####                    #####
####     EJERCICIOs M3    ####
#####         1          #####
##############################
##############################

#   Queremos saber como se comportaron las aerolineas a lo largo del a�o 2013,
#   mes a mes en t�rminos de tiempo promedio de vuelvo

#   - Genere un gr�fico de puntos que le permita contestar: 
#   1.- �Qu� aerolinea tiene el tiempo de viaje mas largo?�Es la misma todo el a�o?
#     R= UA es la aerolinea con el tiempo de viaje mas largo.
#        Durante los meses 1 , 3 , 7 , 11 y 12
#        En los meses complementarios es HA
#   2.- �Qu� aerolinea tiene el tiempo de viaje mas corto?�Es la misma todo el a�o?
#     R= 
#   Si el gr�fico no es suficiente apoyese en res�menes de an�lisis de datos.

flights %>% group_by(year,month,carrier) %>%
            summarise(max_airtime = max(air_time,na.rm = TRUE)) %>%
            ggplot(mapping = aes(x = max_airtime,
                                 y = month,
                                 colour = as_factor(carrier))) + geom_point(size = 4) 

vmax <- flights %>% group_by(month) %>%
                    summarise(max_airtime = max(air_time,na.rm = TRUE))
                    
                    
vmax

flights %>% group_by(year,month,carrier) %>%
            summarise(min_airtime = min(air_time,na.rm = TRUE)) %>%
            ggplot(mapping = aes(x = min_airtime,
                                 y = month,
                                 colour = as_factor(carrier))) + geom_point(size = 4)

vmin <- flights %>% group_by(month) %>%
                    summarise(min_airtime = min(air_time,na.rm = TRUE))
vmin

##############################
##############################
#####                    #####
####     EJERCICIOS M3    ####
#####         2          #####
##############################
##############################

#   CALCULE LA VELOCIDAD PROMEDIO DE CADA AEROLINEA
#   Suponiendo que la distancia est� en millas o kilometro
#   y el tiempo de vuelo en minutos es la columna air_time
#   Adem�s se muestra la velocidad max y m�nima de cada aerolinea

vel_prom <- flights %>% mutate(vel_m.p.h = distance / (air_time / 60))
view(vel_prom)

v <- vel_prom %>% group_by(carrier) %>%
  summarise(vel_prom = mean(vel_m.p.h, na.rm = TRUE),
            vel_max = max(vel_m.p.h, na.rm = TRUE),
            vel_min = min(vel_m.p.h, na.rm = TRUE ))
view(v)


##############################
##############################
#####                    #####
####     EJERCICIOS M3    ####
#####         3          #####
##############################
##############################

#   Haga el join adecuado para los datos: flights y airlines

#   Problema: airlines contiene los nombres de las aerolineas sin abreviaciones
#            de alguna manera tenemos que juntar los archivos de tal forma que 
#            la nueva tabla de datos contenga los nombres de las aerolineas sin abreviaciones.

#   ��Queremos tener toda la tabla de flights completa y tener una nueva columna de nombres de 
#   las aerolineas sin abreviaciones??

air_name <- vroom::vroom(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/airlines.txt")
view(air_name)

name3_flight <- dplyr::right_join(x =  air_name,
                                  y = flights,
                                  by = c("carrier","carrier"))
view(name3_flight)






























































