            ############################################
            ####                                    ####
            ####   Métodos de standarizar valores   ####
            ####                                    ####
            ############################################

###################################            
###################################
##########   Ejemplo 1   ##########
###################################
###################################
            
dat <- data.frame(x = rnorm(10, 30, .2), y = runif(10, 3, 5)) 
View(dat)

#   Lo anterior crea una matriz de 2x10
#   la columna x : rnorm <- da 10 valores aleatorios con distribución normal
#                          alrededor del 30 con una desviación standar de .2
#   la columna y : runif <- da 10 valores aleatorios con distribución uniforme 
#                            con min de 3 y max de 5

scaled.dat <- scale(dat) 
View(scaled.dat)

#   scale <- centra o escala los valores de las columnas de una matriz numérica.
#            pero con media 0 y desviación estandar de 1
#   Nota: En el ejemplo cambió el nombre de la primera columna a scaled.dat.
#   usar ?scale para ver el método que se utiliza para escalar o centrar.

# check that we get mean of 0 and sd of 1 
colMeans(scaled.dat)  
apply(scaled.dat, 2, sd) 
View(scaled.dat)

#   Parece que suma los datos de las columnas ¿Para...?   

##########################################
####   Lo que quiero estandarizar    #####
##########################################

cajita.stn <- scale(t(cajita))                                                  
View(cajita.stn)

#   Nota: arroja la matriz con valores entre [-1,2] .                             
#   Nota: no cambia los nombres de columnas y filas.

cajita.stn1 <- scale(cajita)                                                  
View(cajita.stn1)  

#   Nota: depende de como este acomodada escala los valores de diferente manera  
#         hay columnas que quedan con diferentes rangos.                          
#   Nota: Para los heatmaps ¿requiero todos los valores de la matriz dentro del   
#         mismo rango?.   

EnfySan.stn <- scale(EandS)                                                   
View(EnfySan.stn)  

#   Nota: columna de SAN2 queda fuera del mismo rango que las otras.             
#   Nota: Diferencia entre data.frame y Matrix los tipos de datos                
#         que pueden tener.                                                      

Enfysan.stn.t <- scale(t(EandS))
View(Enfysan.stn.t)

#   Nota: ¿Necesito los genes como columna o fila?
#         Depende de como los pidan heatmap.2

###################################            
###################################
##########   Ejemplo 2   ##########
###################################
###################################

library(caret)
preObj <- preProcess(dat, method=c("center", "scale"),rangeBounds = c(-1, 1))
newData <- predict(preObj, dat)
View(newData)
#   Nota: no logro poner los datos en un mismo rango
#         ¿rangeBounds?

#########################################
####   Lo que quiero standarizar    #####
#########################################

preObj1 <- preProcess(t(cajita), method=c("center", "scale"),rangeBounds = c(-1, 1))
newData1 <- predict(preObj1, t(cajita),)
View(newData1)


preObj2 <- preProcess(cajita, method=c("center", "scale"))
newData2 <- predict(preObj2, cajita)
View(newData2)
#   Casi todas las coumnas tienen el mismo rango

preObj3 <- preProcess(EandS, method=c("center", "scale"))
newData3 <- predict(preObj3, EandS)
View(newData3)
#   Columna de SAN2 [-1.5,2] contiene los demas rangos [-1,2]

###################################            
###################################
##########   Ejemplo 3   ##########
###################################
###################################

library(dplyr) 

set.seed(1234) 
dat.1 <- data.frame(x = rnorm(10, 30, .2), 
                  y = runif(10, 3, 5), 
                  z = runif(10, 10, 20)) 
#   Crea una matriz de 3x10

View(dat.1) 

dat.2 <- dat.1 %>% mutate_each_(funs(scale(.) %>% as.vector), 
                             vars=c("y","z")) 
View(dat.2) 

#   Nota: Solo normaliza las columnas seleccionadas 

#########################################
####   Lo que quiero standarizar    #####
#########################################

EandS.3 <- EandS %>% mutate_each_(funs(scale(.) %>% as.vector), 
                                vars=c("ENF1","ENF2","ENF3","SAN1","SAN2","SAN3")) 
View(EandS.3) 

#   Nota: Falta investigar como dejarlos en el mismo rango

###################################            
###################################
##########   Ejemplo 4   ##########
###################################
###################################

library(clusterSim)
dat.4 <- data.Normalization (dat,type="n5",normalization="column")
View(dat.4)

#   Nota: Usar ?data.Normalization para ver todas las opciones que tiene nn
#   Nota: n5 normaliza y deja en un rango de [-1,1]

#########################################
####   Lo que quiero standarizar    #####
#########################################

EandS.4 <- data.Normalization(EandS,type = "n5",normalization = "column")
View(EandS.4)

#   Nota: todas las columnas caen dentro del rango [-1,1]


###################################            
###################################
##########   Ejemplo 5   ##########
###################################
###################################

for(i in 1:length(colnames(EandS))) { 
  if(class(EandS[,i]) == "numeric" || class(EandS[,i]) == "integer") { 
    EandS[,i] <- as.vector(scale(EandS[,i])) } 
} 

View(EandS)

#   Nota: Faltaría encontrar la forma de limitar el rango []
#   No es tan útil, pierdes el dataframe original ya que no se le asiga
#   nuevo nombre al dataframe normalizado





