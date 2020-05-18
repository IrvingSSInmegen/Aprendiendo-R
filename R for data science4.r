##################################
##################################
###########            ###########
##########   M�DULO 4   ##########
###########            ###########
##################################
##################################

#   " VISUALIZANDO DATOS "   #

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

#   " VISUALIZANDO DATOS "   #

#   �Por qu� visualizamos los datos?

#   - Explorar los datos
#   - Reconocer patrones
#   - Transmitir informaci�n
#   - Presentar resultados

#   Diferentes primitivas a alto nivel (plot,barplot,hist,boxplot, etc)
#   Primitivas de bajo nivel (points, lines)
#   Agregaci�n por orden de la llamada a la funci�n (abline,legend,etc)

######################
######################
###                ###
##     GGPLOT2      ##
###                ###
######################
######################

#   La idea es que ggplot() tranforma los datos en la forma 
#   espec�fica que los requieren los geom_function

#   " GGPLOT2: FILOSOF�A "

#   ggplot(data = <DATA>) + <GEOM_FUNCTION>(mapping = aes( <MAPPINGS>),
#                                           stat = <STAT>,
#                                           position = <POSITION>) + 
#                           < COORDINATE_FUNCTION > + 
#                           < FACET_FUNCTION > +
#                           < THEME >

#   Dar de comer los datos que se van a utilizar usando %>% o dandolos dentro de la funci�n

#       nustros_datos %>% ggplot2()

#       ggplot(data = nuestros_datos)

#   " GGPLOT2 : MAPPING "
#      " aes " : definen como las variables son mapeadas a las propiedades visuales de los geom.
#               (nota: est�ticas o aesthetic)

#   " GGPLOT2 : PRIMITIVAS "
#      " geom " : objetos geom�tricos que son la representaci�n visual de las obsevaciones

#   " geom_area() "
#    Gr�fica de �rea
#    ?geom_area() para mas informaci�n


#   Gram�tica de gr�ficos ( Pre-tidyverse )
#   -->  Objetos + Tranforms + Persistencia

###############################
## " Ejemplo de dispersi�n " ##
###############################

#   Hacer gr�fico de dispersi�n usando el archivo iris
#   para petal.lenght vs petal.width en versipon de R-base con ggplot2

iris %>% head()

#   Gr�fico con la vesi�n base de R

plot(x = iris$Petal.Length, 
     y = iris$Petal.Width, 
     col = iris$Species,
     type = "p",
     xlab = "Petal.legth",
     ylab = "Petal.Width")
legend(legend = levels(iris$Species),
       col = seq(levels(iris$Species)),
       x = 1,
       y = 2.5,
       lty = 1)
#   Con la base de R se plotea y despupes se pone la leyenda requerida
#   Adem�s de que podemos darle la escala que queramos al eje "x" y "y"
#   Creo que la legenda estorba en la gr�fica 
#   ���investigar como arreglar???

#   Versi�n con ggplot2

iris %>% ggplot(mapping = aes(x = Petal.Length,
                              y = Petal.Width,
                              colour = Species)) + geom_point()

##############################
##############################
#####                    #####
####     EJERCICIOs M2    ####
#####                    #####
##############################
##############################

#   Ejercicio: cambiar le punto en la gr�fica de sipersi�n de datos.
#   Problema: no puedo hacer que el rombo sea relleno de los colores que originalmente eran.
#   Resuelto: hab�a que usar fill en el ggplot y solo con shape en el geom_point

iris %>% ggplot(mapping = aes(x = Petal.Length,
                              y = Petal.Width,
                              fill = Species,)) +  geom_point(shape = 23,size = 3)

#################################################
## " �QU� MAS LE PODEMOS HACER A LA GR�FICA? " ##
#################################################

#   ggplot2: T�tulos, subt�tulos y leyenda abajo 
#   Esquema

labs(title = "Mi t�tulo",
     subtitle = "Mi subt�tulo",
     caption = " Un pie de figura")

# Siguiendo con el ejemplo

iris %>% ggplot(mapping = aes(x = Petal.Length,
                              y = Petal.Width,
                              fill = Species)) + 
                geom_point(shape = 23) + 
                labs(title = "IRIS",
                     subtitle = "Dispersi�n de dimensi�n de p�talos",
                     caption = "Fuente: IRIS datos de R")

# S�mbolos matem�ticos en los ejes: En lab agregamos lo que queremos en los ejes

labs(x = quote(petal.length = sum(x[i]^2,i == 1,n)),
     y = quote(petal.width = alpha + beta + frac(delta,theta)))

#   Regresando al ejemplo

iris %>% ggplot(mapping = aes(x = Petal.Length,
                              y = Petal.Width,
                              fill = Species)) + 
                geom_point(shape = 23) + 
                labs(title = " IRIS",
                     subtitle = " Dispesi�n de dimensi�n de p�talos",
                     caption = " Fuente: IRIS datos de R",
                     x = quote(sum(x[i]^2, i == 1,n)),
                     y = quote(alpha + beta + frac(delta,theta)))

#   Quitar el fondo gris
#   ggplto: Themes
#   Hay diferentes themes 
#   https://ggplot2.tidyverse.org/reference/ggtheme.html

#   Respecto a la legenda podemos hacer:

# +theme(legend.position = NULL) <--remueve la leyenda
# +theme(legend.position = "left")  "top", "bottom", "right"

#   Regresando al ejemplo

iris %>% ggplot(mapping = aes(x = Petal.Length,
                              y = Petal.Width,
                              fill = Species)) + 
                geom_point(shape = 23, size = 3) + 
                labs(title = "IRIS",
                     subtitle = "Dispersi�n de dimensi�n de p�talos",
                     caption = "Fuente: IRIS, datos de R",
                     x = quote(sum(x[i]^2, i == 1,n)),
                     y = quote( alpha + beta + frac(delta,theta))) + 
                theme_dark() + 
                theme(legend.position = "bottom")

#   Cambiar la paleta de colores
#   ggplot: SCALES ( varias opciones y funciones )

# scale_x / y / colour / fill_manual / continus / discrete / log10
scale_x_continuous()
scale_fill_manual()
scale_x_log10()

scale_x_continuous(limits = range(iris$Petal.Length))
#   Debemos de darle los limites del gr�fico, range toma el rango de la columna
scale_y_continuous(limits = c(min = 0,max = 10))
#   Debemos darle l�mite, aqu� los creamos con un vector c para indicar un max y min
scale_color_brewer(palette = "Set1")
#   Supongo que ya hay varios conjuntos predise�ados
scale_fill_distiller("spectral")

#   Regresando al ejemplo

iris %>% ggplot(mapping = aes(x = Petal.Length,
                              y = Petal.Width,
                              fill = Species)) + 
        geom_point(shape = 23, size = 3) + 
        labs(title = "IRIS",
             subtitle = "Dispersi�n de dimensi�n de p�talos",
             caption = "Fuente: IRIS, datos de R",
             x = quote(sum(x[i]^2, i == 1,n)),
             y = quote( alpha + beta + frac(delta,theta))) + 
        theme_dark() + 
        theme(legend.position = "bottom") + 
        scale_fill_manual(values = c("yellow","blue","green"),
                          name = "Especies")

#   Separar paneles por especie 
#    ggplot:facets <- define como los datos pueden ser agrupados en grillas (filas /columnas)
#           wrap vs grid
#
#   facet_wrap : Para una variable
#   facet_grid : Para una matriz de dos variables

#   Regresando al ejemplo

iris %>% ggplot(mapping = aes(x = Petal.Length,
                              y = Petal.Width,
                              fill = Species)) + 
        geom_point(shape = 23, size = 3) + 
        labs(title = "IRIS",
             subtitle = "Dispersi�n de dimensi�n de p�talos",
             caption = "Fuente: IRIS, datos de R",
             x = quote(sum(x[i]^2, i == 1,n)),
             y = quote( alpha + beta + frac(delta,theta))) + 
        theme_dark() + 
        theme(legend.position = "bottom") + 
        scale_fill_manual(values = c("yellow","blue","green"),
                          name = "Especies") + 
        facet_wrap(~Species)
#   Valores maximon y minimos en las escalas son iguales para los tres grupos

iris %>% ggplot(mapping = aes(x = Petal.Length,
                              y = Petal.Width,
                              fill = Species)) + 
        geom_point(shape = 23, size = 3) + 
        labs(title = "IRIS",
             subtitle = "Dispersi�n de dimensi�n de p�talos",
             caption = "Fuente: IRIS, datos de R",
             x = quote(sum(x[i]^2, i == 1,n)),
             y = quote( alpha + beta + frac(delta,theta))) + 
        theme_dark() + 
        theme(legend.position = "bottom") + 
        scale_fill_manual(values = c("yellow","blue","green"),
                          name = "Especies") + 
        facet_wrap(~Species, scales = "free")
#   Los valorews en los ejes son independientes en cada grupo

##############################
##############################
#####                    #####
####     EJERCICIOs M4    ####
#####         1          #####
##############################
##############################

#   Utilizando mtcars retomar la gr�fica de dispersi�n
#   - Agregar t�tulo a la gr�fica
#   - utilice el theme negro y blanco
#   - genere los p�neles correspondientes a la cantidad de cilindros

mtcars %>% ggplot(mapping = aes(x = hp,
                                y = mpg,
                                fill = as.factor(cyl))) + 
           geom_point(shape = 23, size = 2) + 
           labs(title = "MTCARS",
                subtitle = " caballos de fuerza vs millas por galon",
                caption = " Si hp aumenta entonces mpg disminuye ",
                x = "millas por galon",
                y = "caballos de fuerza" ) + 
           theme_linedraw() + 
           theme(legend.position = "bottom") + 
           scale_fill_manual(values = c("yellow","orange","red"),
                             name = "N�mero de cilindros") + 
           facet_wrap(~as.factor(cyl), scales = "free")

#   Bonus : geom_smooth

mtcars %>% ggplot(mapping = aes(x = wt,
                                y = mpg,
                                fill = as.factor(cyl))) + 
           geom_point(shape = 23, size = 2) + 
           labs(title = "MTCARS",
                subtitle = " Peso vs millas por galon",
                caption = " Si el peso aumenta entonces las millas por galon disminuyen ",
                x = " Peso del Automovil",
                y = "millas por galon" ) + 
          theme_linedraw() + 
          theme(legend.position = "bottom") + 
          scale_fill_manual(values = c("yellow","orange","red"),
                            name = "N�mero de cilindros") + 
          facet_wrap(~as.factor(cyl), scales = "free") +
          geom_smooth()

##################
## " BOXPLOTS " ##
##################

#   " geom_boxplot "
#   - Formato de los datos: " largo "

iris %>% head()


         
iris %>% tidyr::pivot_longer(cols = -Species,
                                       names_to = "variables",
                                       values_to = "valores") %>%
                ggplot(mapping = aes(x = variables,
                                     y = valores,
                                     fill = as.factor(Species))) + 
                geom_boxplot() + 
                facet_wrap(~variables,scales = "free")

##############################
##############################
#####                    #####
####     EJERCICIOs M4    ####
#####         2          #####
##############################
##############################

#   Hacer un ggplot�s boxplot con MTCARs para las variables "dips" y "hp"

#   Variables de disp y hp juntas

mtcars %>% select(cyl,hp,disp) %>% tidyr::pivot_longer(cols = -cyl,
                                                       names_to = "variables",
                                                       values_to = "valores") %>%
                                   ggplot(mapping = aes(x = variables,
                                                        y = valores,
                                                        fill = as.factor(cyl))) + 
                                   geom_boxplot() + 
                                   labs(title = "Desplazamiento y caballos de fuerza",
                                   subtitle = "cyl vs hp / cyl vs disp",
                                   caption = "An�lisis estadistico de automoviles",
                                   x = "despazamiento                   caballos de fuerza",
                                   y = "Valor") +
                                   theme_dark() +
                                   theme(legend.position = "bottom") +
                                   scale_fill_manual(values = c("blue","green","red"),
                                                     name = " N�mero de cilindros") + 
                                   facet_wrap(~variables,scales = "free")

#   Variables por separado.

mtcars %>% ggplot(mapping = aes(x = as.factor(cyl),
                                y = disp,
                                fill = as.factor(cyl))) + 
                 geom_boxplot()+ 
                 theme_dark() +
                 labs(title = " DISP vs CYL",
                      subtitle = "Desplazamiento dependiendo del n�mero de cilindros",
                      x = "n�mero de cilindros",
                      y = " desplzamiento") + 
                 facet_wrap(~as.factor(cyl), scales = "free")

mtcars %>% ggplot(mapping = aes(x = as.factor(cyl),
                                y = hp,
                                fill = as.factor(cyl))) + 
                  geom_boxplot()+ 
                  theme_dark() +
                  labs(title = " HP vs CYL",
                       subtitle = "Caballos de fuerza dependiendo del n�mero de cilindros",
                       x = "n�mero de cilindros",
                       y = " caballos de fuerza") + 
                  facet_wrap(~as.factor(cyl), scales = "free")

#####################
## " GEOM_VIOLIN " ##
#####################

#   Calcula y dibuja la estimaci�n de la densidad del n�cleo,
#   que es una versi�n suavizada del histograma.
#   Esta es una alternativa �til al histograma para datos continuos
#   que provienen de una distribuci�n uniforme subyacente

iris %>% tidyr::pivot_longer(cols = -Species,
                             names_to = "variables",
                             values_to = "valores") %>%
         ggplot(mapping = aes(x = variables,
                              y = valores,
                              fill = as.factor(Species))) +
         geom_violin() + 
         facet_wrap(~as.factor(Species),scales = "free")

######################
## " GEOM_DENSITY " ##
######################

#  Pensemos en datos simulados

tibble(valores = c(rnorm(100),
                   rnorm(n = 100,mean = 10))) %>% 
         ggplot(aes(x = valores))+ 
         geom_density()

##############################
##############################
#####                    #####
####     EJERCICIOs M4    ####
#####         3          #####
##############################
##############################

#   Genere el boxplot y el violin de los datos anteriores
#   �Qu� observa en ambos casos? � Es correcto?

tibble(valores = c(rnorm(100),
                   rnorm(n = 100,mean = 10))) %>%
       ggplot(aes(x = valores)) +
       geom_boxplot() + 
       coord_flip()

tibble(valores = c(rnorm(100),
                   rnorm(n = 100,mean = 10))) %>%
        ggplot(aes(x = valores)) +
        geom_violin() 
#   Boxplot se puede hacer, pero ambos son gr�ficos que necesitan dos variables 

#############################
## " GGPLOT2: GEOM_BAR() " ##
#############################

diamonds %>% head()

#   El mismo gr�fico de barras pero con diferente funci�n

diamonds %>% ggplot() + geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + stat_count(mapping = aes(x = cut))

#   Otro tipo de gr�ficos

ggplot(data = diamonds) + stat_summary(mapping = aes(x = cut,y = depth),
                                       fun.ymin = min,
                                       fun.ymax = max,
                                       fun.y = median)
#   mensaje de erro dice que fun.ymin, y las otras dos son obsoletas que las use sin la "y" 
#   pero generan el mismo gr�fico


###########################
## " GGPLOT2: POSITION " ##
###########################

ggplot(data = diamonds) +
        geom_bar(mapping = aes(x = cut,
                               fill = clarity),
                 position = "dodge")

###################################
## " coord_flip vs coord_polar " ##
###################################

bar <- ggplot(data = diamonds) + 
       geom_bar(mapping = aes(x = cut,
                              fill = cut),
                show.legend = FALSE,
                width = 1) + 
        theme(aspect.ratio = 1) + 
        labs(x = NULL,
             y = NULL)
bar + coord_flip()
bar + coord_polar()

###################
## " geom_tile " ##
###################

#   Falta completar este gr�fico

USArrests %>% rownames_to_column(var = "estado") %>% 
              pivot_longer(cols = -estado,
                           names_to = "type",
                           values_to = "value",
                           #Aqui falta un valor pero no s� cual es) + 
              ggplot(mapping = aes(x = type,
                                   y = estado,
                                   fill = value)) + 
              geom_tile() + 
              scale_fill_distiller("spectral")
        
                           















