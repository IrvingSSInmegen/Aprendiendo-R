##################################
##################################
###########            ###########  
##########   MÓDULO 3   ##########    
###########            ###########
##################################
##################################

 #   " -Manipulando datos- "    #

library(dplyr)
library(ggplot2)
library(magrittr)
library(reader)
library(data.table)
library(vroom)
library(readxl)
library(janitor)
library(tidyr)
library(tidyverse)

#   ¿Datos limpios?
#   - Cada columna es una variable
#   - Cada renglón es una observación
#   - Si tenemos datos limpios, podemos pensar en usar código parecido para diferentes problemas
#   - Código limpio: más legible
#   - Más tiempo para pensar en nuestro análisis

#   ¿QUÉ QUEREMOS MANIPULAR?
#   En un set de datos rectangulares
#   - queremos modificar variables completas
#   - queremos hacer operaciones y sacar nuevas variables
#   - queremos seleccionar observaciones que cumplan ciertas caracteristicas
#   - queremos agrupar
#   - queremos sacar descripciones de los datos
#   - queremos reordenar variables

######################
######################
###                ###
## PAQUETE MAGRITTR ##
###                ###
######################
######################

############################
## " FUNCIONES ANIDADAS " ##
############################

#   "FUNCIONES ANIDADAS"
#   Aplica las operaciones a todos los valores del vector x, como la composición de funciones.

x <- 1:10
# asigna a x los valores de 1 hasta 10, formando un vector de numeros enteros.

View(x)
# lo muestra como una columna.

(exp(sqrt(log(x))))

#   "OBJETOS INTERMEDIO"
#   Aplicando diferente proceso para el mismo resultado
#   se van aginando diferentes objetos para cada operación que se hace.

X <- 1:10
Y <- log(X)
Z <- sqrt(Y)
W <- exp(Z)
W

#   "SOBRE ESCRIBIR OBJETOS"
#   Practicamente a un mismo objeto se le van asignando diferentes operaciones.
#   - Un enviroment mas limpio.

A <- 1:10
A <- log(A)
A <- sqrt(A)
A <- exp(A)
A

########################
## USANDO EL PIPE %>% ##
########################

#   El símbolo %>% es como la composición de funciones.
#   - Se ve con mas orden al no usar varios paréntesis.

a <- 1:10
my_result <- a %>% log() %>% sqrt() %>% exp()
my_result

#   " OTROS PIPE" 

#   " EL TE " 

rnorm(100) %>% matrix(ncol = 2) %T>% plot() %>% str()

# rnorm(100) %>% matrix(ncol = 2) Esto crea 100 valores aleatorios, y lo pone en una matriz de
# 2x50 
# El operador %T>% me regresa el valor de str(rnorm(100) %>% matrix(ncol = 2))
# Creo que lo que hace el operador tee es aplicar lo anterior a las funciones continuas al operador
# rnorm(100) %>% matrix(ncol = 2)
# str(rnorm(100) %>% matrix(ncol = 2))
# plot(rnorm(100) %>% matrix(ncol = 2))

#   "LA EXPLoSIÓN" 

mtcars %$% cor(disp,mpg)
#   Parece que mtcars ya era un archivo pregrabado de R
#   cor <- parece que calcula varianza, covarianza y correlación.(Estadistica)
#   toma del dataset las columnas "disp" y "mpg"
#   Investigar como funciona por separado la funcion cor()

###############
###         ###
## FILTRADOS ##
###         ###
###############

#   "EL PAQUETE DPLYR"
#   Una gramática para la manipulación de datos
#   tidyverse -> Verbos
#   - mutate
#   - select
#   - filter
#   - group_by
#   - summarise
#   - arrange
 
#   Analicemos flights

flights <- vroom::vroom(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/flights.txt")

View(flights)

#  Buscamos observaciones (renglones) que cumplan con una condición lógica

#######################
## " VERBO: FILTER " ##
#######################

# Primera forma de filtrar
filter(.data = flights, dest == "SBN")

# Lo mismo pero usando %>%
flights %>% filter(dest == "HDN")

# Lo mismo pero con una condición mas
flights %>% filter(dest == "HDN",!is.na(dep_time))

# Parece que !is.na(dep_time) solo nos deja las observaciones donde la columna dep_time
# tenga un valor  y no  (NA)  ( ¿¿el simbolo "!" creo que es la condicional if??)

######################
## " VERBO: SLICE " ##
######################

# Nos deja seleccionar renglones por posición

flights %>% slice(14:17)
#  Una submatriz (4x19), sigue siendo tibble toma los renglones 14,15,16,17.

#######################
## " VERBO: SELECT " ##
#######################

# Permite escoger algunas variables (columnas)

flights %>% select(flight,carrier,origin,dest,distance)
#  Una submatriz (336776x5) que solo toma ciertas columnas

flights %>% select(contains("time"))
#  Toma las columnas que contengan dicha palabra? 

flights %>% select_if(is_character)
#  Toma las columnas que solo contienen caracteres
#  Una matriz (336776x4)

######################################
## CONCATENAR VERBOS USANDO " %>% " ##
######################################

flights %>% select("dest",contains("time")) %>%
            filter(dest == "HDN") %>%
             tidyr::drop_na() %>%
             slice(1:7)

#   Se aplican varias funciones a flights
#   -a: Seleccionamos la columna dest y las que contenga la palabra time
#     Nota: dest no es la primera columna de flights sin embrago select la convierte 
#           en la primera columna 
#   -b: Filtramos los renglones que contengan en la columna dest  "HDN"
#   -c: Parece que drop_na() quita las filas que contengan valores perdidos (NA) en cualquier columna
#   -d: Al final solo tomamos los renglones del 1 al 7 

a <- view(flights %>% select("dest",contains("time")))
        
b<- view(flights %>% select("dest",contains("time")) %>%
           filter(dest == "HDN"))
c <- view(flights %>% select("dest",contains("time")) %>%
            filter(dest == "HDN") %>%
            tidyr::drop_na())
d <- view(flights %>% select("dest",contains("time")) %>%
            filter(dest == "HDN") %>%
            tidyr::drop_na() %>%
            slice(1:7))

#####################
## " VERBO: PULL " ##
#####################


#   Permite sacar una variable como verctor

flights %>% pull("dest") %>% head()
#   Nota: recordar que head()solo muestra los primeros 6 o 10 datos

#########################
## " VERBO: GROUP_BY " ##
#########################

#     "AGRUPAMIENTOS"
#   group_by me permite agregar por variables categóricas (o coercible a variables categóricas)
#   Esto servirá para hacer resúmenes después
#   Cuando se aplique con la función de summarise los datos quedan acomodados respecto a las clases
#   que crea group_by

flights %>% group_by(dest)
#   No entiendo que hace::
#   Respuesta, agrupa en clases dependiendo la columna que elijas
#   No cambia la forma de los datos hasta que lo combinas con otros verbos de dplyr

flights %>% group_by(dest,origin)
#   Creo que lo único que hace es reconocer las clases de esa columna, pero
#   no ordena la tabla de datos.

##############################
## " FUNCIONES DE RESUMEN " ##
##############################

#   - Descripciones del conjunto de datos.
#   - Funcionan en el conjunto tanto original como manipulado.
#   - Te regresa la informacion en una tabla_dataframe

flights %>% summarise(mean_distance = mean(distance))

#   Lo mismo pero con mas condiciones

flights %>% select(contains("time")) %>%
            summarise(mean_deptime = mean(dep_time,na.rm = TRUE),
                      mean_sched_deptime = mean(sched_dep_time, na.rm = TRUE),
                      mean_arrtime = mean(arr_time, na.rm = TRUE),
                      mean_sched_arrtime = mean(sched_arr_time, na.rm = TRUE),
                      mean_airtime = mean(air_time, na.rm = TRUE),
                      mean_timehour = mean(time_hour, na.rm = TRUE))
#   Parece que saca la media que cada una de las columnas
#   ¿¿No sé porque la función mean solita no me permite sacar la media por columna??

#   Lo mismo pero con otras condiciones

flights %>% select(contains("time")) %>%
            summarise_all(mean, na.rm=TRUE)
#   Una forma mas facil de aplicar a todas las columnas summarise
#   Creo que na.rm=TRUE omite los valores faltantes (NA)

#   " USARLA SOBRE DATOS AGRUPADOS "

flights %>% group_by(dest) %>%
            select(contains("time"))%>%
            summarise_all(mean, na.rm=TRUE)

#   De alguna manera aqui la columna dest se acomoda alfabeticamente.
#   Res: esto pasa por la combinación de group:by() en combinación con otra función de dplyr.
#   Las demás columnas no tienen un orden numérico.
#   Creo que saca la media de las observaciones que tienen el mismo caracter en dest.
#   Pasó de de ser una tabla de 336776x19 a una tabla de 105x7.

flights %>% group_by(origin,dest) %>%
            select(contains("time")) %>%
            summarise_all(list(med = median,avg = mean), na.rm=TRUE)
#   Primero ordena la columna "origen" en orden alfabético
#   Después hay subcategorias dentro de "origen" y vuelve a orden respecto a dest
#   en orden alfabético
#   Hace dos operaciones a cada una de las "clases", sacar la media y la mediana 
#   Crea una tabla de 224x14

#####################################
##  " CONTAR: n , count , tally "  ##
#####################################

#   " Contar con: n "

flights %>% group_by(month,origin,dest) %>% summarise(cuantos=n())
#   Cuenta cuantos hay de cada clase en las que se agruparon los datos.
#   flights %>% group_by(month,origin,dest) arroja un total de 2313 clases.

#   " Contar con: count "

flights %>% count()
#   Da 336776 me parece que solo cuenta el número de observaciones que hay.

flights %>% count(origin)
#   Cuenta  cuantos hay de cada clase en la columna "origin".
#   Nota: no se le ha aplicado group_by a la tabla de datos 
#         y aun así hizo el conteo.

#   " count() con datos agrupados " 

flights %>% group_by(month) %>% count(origin)
#   Agrupa por month y luego cuenta cuantos hay que cumple la misma condición de origin.

#   " Contar con: tally "

flights %>% group_by(month,origin,dest) %>% tally()
#   Cuenta cuantos hay de cada clase en las que se agruparon los datos.
#   flights %>% group_by(month,origin,dest) arroja un total de 2313 clases.
#   Hace lo mismo que contar con n() pero sin usar summarise().
#   Tampoco nos da una columna nombrada como en summarise() y n().

###########################
###                     ###      
## RANGOS Y DISPERCIONES ##
###                     ###
###########################

#   Sacar máximos, mínimos y desviación standar de una clase.

flights %>% group_by(origin,dest) %>% 
            summarise(min_airtime = min(air_time,na.rm = TRUE),
                      max_airtime = max(air_time,na.rm = TRUE),
                      std_dev_airtime = sd(air_time,na.rm = TRUE),
                      cuantos = n())
#   Agrupa por origen y destino ( total de 224 clases) 
#   min() y max() de todas las clases toman el valor min de vuelo y max de vuelo
#   sd() calcula desviación estandar (que tan lejos estan los datos de la media)
#   Al final nos dice de cuantos datos se hizo el max , min y sd

############################
###                      ###      
## dplyr + pipe + ggplot2 ##
###                      ###
############################

flights %>% group_by(origin,dest) %>% 
            summarise(mean_depdelay = mean(dep_delay,na.rm = TRUE),
                      mean_arrdelay = mean(arr_delay,na.rm = TRUE)) %>%
            ggplot(mapping = aes(x = mean_depdelay,
                                 y = mean_arrdelay,
                                 colour = as_factor(origin))) + geom_point()


##############################
##############################
#####                    #####
####     EJERCICIOS M3    ####
#####         1          #####
##############################
##############################

#   Queremos saber como se comportaron las aerolineas a lo largo del año 2013,
#   mes a mes en términos de tiempo promedio de vuelvo

#   - Genere un gráfico de puntos que le permita contestar: 
#   1.- ¿Qué aerolinea tiene el tiempo de viaje mas largo?¿Es la misma todo el año?
#     R= UA es la aerolinea con el tiempo de viaje mas largo.
#        Durante los meses 1 , 3 , 7 , 11 y 12
#        En los meses complementarios es HA
#   2.- ¿Qué aerolinea tiene el tiempo de viaje ams corto?¿Es la misma todo el año?
#     R= 
#   Si el gráfico no es suficiente apoyese en resúmenes de análisis de datos.

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

################
################
####        ####      
###  MUTATE  ###
####        ####
################
################

#   " Tansformaciones de variables " 
#   - Quiero saber el tiempo que estaba esperado para los vuelos.

flights %>% select(carrier,flight,air_time,dep_delay,arr_delay) %>%
            mutate(expected_airtime = air_time - dep_delay - arr_delay )
#   Nos muestra las columnas seleccionadas y ademas opera con restas 
#   las columnas air_time, dep_delay, arr_delay y junta una nueva columna
#   a nuestra tabla de datos.

#   Tambien se pueden hacer variables intermedias

flights %>% select(carrier,flight,air_time,dep_delay,arr_delay) %>%
            mutate(expected_airtime = air_time - dep_delay - arr_delay,
                   total_delay = dep_delay + arr_delay)
#   Nota: se agregan nuevas variables poniendo "," en mutate()

#   " case_when PARA CONDICIONALES "
#   - Quiero una variable que diga si adelantó o se retrasó

my_flights <- flights %>% mutate(total_delay = dep_delay + arr_delay,
                                 expected_airtime = air_time - total_delay) %>% 
                          mutate(net_gainloss = case_when(total_delay > 0 ~ "retraso",
                                                          total_delay == 0 ~ "a tiempo",
                                                          total_delay < 0 ~ "adelanto"))
#   Usando la condicional cuando suceda algo
view(my_flights)

my_flights33 <- flights %>% mutate(total_delay = dep_delay + arr_delay,
                                 expected_airtime = air_time - total_delay,
                                 net_gainloss = case_when(total_delay > 0 ~ "retraso",
                                                          total_delay == 0 ~ " a tiempo",
                                                          total_delay < 0 ~ " adelantado"))

view(my_flights33)
#   Pero no entiendo porque debería ir en otro mutate separado,parece funcionar igual.

my_flights %>% select(carrier,flight,air_time,expected_airtime,total_delay,net_gainloss)

##############################
##############################
#####                    #####
####     EJERCICIOS M3    ####
#####         2          #####
##############################
##############################

#   CALCULE LA VELOCIDAD PROMEDIO DE CADA AEROLINEA
#   Suponiendo que la distancia está en millas o kilometro
#   y el tiempo de vuelo en minutos es la columna air_time
#   Además se muestra la velocidad max y mínima de cada aerolinea

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
####        JOINS         ####
#####                    #####
##############################
##############################

#   " EL PROBLEMA CLÁSICO DE JUNTAR DICCIONARIOS "

#   Datos sobre películas del hombre araña
#   Nos gustía responer
#   - ¿Qué actores interpretan a cada personaje?
#   - ¿Qué personajes aparecen en la primera serie?
#   - ¿Qué personajes aparecen en la segunda serie?
#   - ¿Qué personajes aparecen en ambas?

spiderman_raimi <- vroom::vroom(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/spiderman_raimi.csv")
View(spiderman_raimi)

spiderman_amazing <- vroom::vroom(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/amazing_spiderman.csv")
View(spiderman_amazing)

#########################
###                   ###      
## JUNTAR DICCIONARIOS ##
###                   ###
#########################

#
#   " LEFT JOIN " 
#     AU(A intersección B)

dplyr::left_join(x = spiderman_raimi, 
                 y = spiderman_amazing, 
                 suffix = c ("_raimi", "_amazing"),
                 by = c("character" = "character"))
#   - Toma los personajes que estan en X y crea una tabla con esos datos (11),si el personaje no se
#     encuentra en Y pone NA
#   - "x" y "y" son los archivos que está tomando en cuenta
#   - suffix: Debe ser un vector de caracteres de longuitud 2
#             Creo que está nombrando las dos columnas en donde van a aparecer los
#             los actores que interpretaron a cada personaje.
#   - by: Toma las columnas que van a ser comparadas.
#   NOTA: ¿¿¿las tablas de datos que se van a poner, siempre tendran que tener 
#         la propiedad de nx2??? 
#         Respuesta: NO, revisar el ejercicio siguiente 
#   NOTA: ¿¿¿El orden de las columnas debe ser igual en ambas tablas de datos???
#         Respuesta: NO, no importa el orden de las columnas.

#   " RIGHT JOIN "
#   BU(B intersección A)

dplyr::right_join(x = spiderman_raimi,
                  y = spiderman_amazing,
                  suffix = c("_pelideTobey","_pelidelGarfield"),
                  by = c("character","character"))
#   - Toma los personajes que están en Y y crea una tabla con esos datos (8) si el personaje no
#     se encuentra en X pone NA
#   - Suffix: podemos nombrar las dos columnas que apareceran como queramos

#   " SEMI JOIN "
#   A intersección B

dplyr::semi_join(x = spiderman_raimi,
                 y = spiderman_amazing,
                 suffix = c("_raimi","_amazing"),
                 by = c("character" = "character"))
#   Toma los personajes que hay en X y los personajes que hay en Y y crea una tabla
#   solo con los que aparecen en ambos ( intersección)
#   NOTA: El número de filas depende del número de datos que se toman en cuenta
#         al apliar left_join , right_join o semi_join

#   " FULL JOIN "
#   A U B

dplyr::full_join(x = spiderman_raimi,
                 y = spiderman_amazing,
                 suffix = c("_raimi","_amazing"),
                 by = c("character" = "character"))
#   Toma en cuenta todos los personajes de X y de Y une todos los datos para crear la tabla

#   " ANTI JOIN "
#   A diferencia simetrica B

dplyr::anti_join(x = spiderman_raimi,
                 y= spiderman_amazing,
                 suffix = c("_raimi","_amazing"),
                 by = c("character" = "character"))
#   Toma los personajes que no tienen en común X y Y
#   Crea una tabla de datos pero solo con dos columnas
#   No se especifica a que saga pertenece cada personaje
#   Pregunta: ¿¿Podemos omitir suffix??

dplyr::anti_join(x = spiderman_raimi,
                 y= spiderman_amazing,
                 by = c("character" = "character"))
#   Respuesta:Si
#   Arroja el mismo resultado.

#   ¿¿Qué pasa si no especificamos suffix en left_join??

dplyr::left_join(x = spiderman_raimi, 
                 y = spiderman_amazing, 
                 by = c("character" = "character"))
#   Respuesta: hace lo mismo, simplemente las columnas se nombran como
#              "cast.x" y "cast.y" pero arroja la misma tabla

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

air_name <- vroom::vroom(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/airlines.txt")
view(air_name)

name_flights <- dplyr::left_join(x = flights,
                                 y = air_name,
                                 by = c("carrier","carrier"))
view(name_flights)
#   Nota: pone el nombre de la aerolinea sin abreviación al final de todas las columnas de flight

name_flights1 <- dplyr::right_join(x = flights,
                             y = air_name,
                             by = c("carrier","carrier"))
#   Nota: pone el nombre de la aerolinea sin abreviación al final de todas las columnas de flight
#         pero ordena los datos como aparecen en air_name

name2_flight <- dplyr::left_join(x = air_name,
                           y = flights,
                           by = c("carrier","carrier"))
#   Nota: cambiar el orden los archivos afecta
#         ahora las primeras dos columnas son "carrier" y "name" (corto, largo)
#         además de estar ordenados los datos respecto a como aparece la columa "carrier"
#         en la tabla de datos air_name

name3_flight <- dplyr::right_join(x =  air_name,
                                  y = flights,
                                  by = c("carrier","carrier"))
#   Nota: primeras dos columnas "carrier" y "name" en el mismo orden que flights

#   Parece que cualquiera de estos 4 casos resuelve el problema de juntar diccionarios


#############################
#############################
#####                   #####
####     MAS TIDYING     ####
#####                   #####
#############################
#############################

calif <- vroom::vroom(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/calificaciones_untidy.txt")
view(calif)

#   Si intentamos hacer el plot

#calif %>% ggplot() %>% geom_point(mapping = aes(x = student???,
#                                                y = calificaciones???
#                                                  Dónde se pondrían las materias))

#   Los datos no están organizados para esta aplicación

#########################
##  " TIDYR::GATHER "  ##
#########################

calif_gather <- calif %>% gather(-"student",key = "materia",value = "calificación")
view(calif_gather)

#   Acomoda los datos tomando como primer columna "student" repite los studiantes
#   tantas veces como columnas haya

calif_gather %>% ggplot(mapping = aes(x = as_factor(student),
                                      y = calificación,
                                      colour = as_factor(materia))) + geom_point(size = 4)

#   Nota: no es la única manera de pivotear datos

#   reshape2::melt
#   tidyr::pivot_long


##########################
##  " RESHAPE2::MELT "  ##
##########################

calif_melt <- calif %>% reshape2::melt(variable.name = "materia",
                                       value.name = "calificación",
                                       id.var = "student")
view(calif_melt)

#############################
##  " tidyr::pivot_long "  ##
#############################

calif_pivotlog <- calif %>% tidyr::pivot_longer(cols = -"student",
                                                names_to = "materia",
                                                values_to = "calificacion")

view(calif_pivotlog)





















































