#   ¿Qué hace el operador %T>%?

rnorm(100) %>% matrix(ncol = 2) %T>% plot() %>% str()

#   -El operador %T>% me regresa el valor de str(rnorm(100) %>% matrix(ncol = 2))
#    Creo que lo que hace el operador tee es aplicar el resultafo lo anterior a 
#    las funciones siguientes del operador

#   -No entiendo para que funciona el pipe %$% :::

#    En el ejercicios de las aerolineas
#   Logro hacer con summarise que me muestre el tiempo max en el aire de cada mes, pero
#   no sé como hacer para que se muestre la aerolinea que lo tiene
#   osea logro mostrar 2 columnas
#   month|vel_max
#   pero como quisiera tenerlo sería con 3 columnas
#   month | carrer | vel_max |

v <- flights %>% group_by(month) %>%
                 summarise(max_airtime = max(air_time,na.rm = TRUE))
v

#   Como saco el simbolo de virgulilla? (la s acostada)



