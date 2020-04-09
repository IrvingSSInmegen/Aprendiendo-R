##############################
#########           ########## 
########   HEATMAP   #########
#########           ##########
##############################

#install.packages("gplots")
#install.packages("RColorBrewer")
library(dplyr)
library(DESeq2)
library(gplots)
library(RColorBrewer)

#   Preparación del dataset.

#   Preparamos los datos ya que para los heatmaps necesitamos
#   ingresar una matriz solo con valores numéricos.
#   El dataset se carga desde una URL de github.

data <- read.csv("https://raw.githubusercontent.com/rasbt/R_snippets/master/heatmaps/dataset.csv", 
                 comment.char="#")  
#   comment.char: ignora las lineas que empiecen con el símbolo "#"
#   data es una tabla de datos de 5x10 .

rnames <- data[,1] 
#   Guardamos la columna 1 para usarlo como los nombres
#   de las filas.

mat_data <- data.matrix(data[,2:ncol(data)])  
#   tranformamos data en una matriz tomando de la columna 2 hasta ncol(data)
#   que nos arroja hasta la última columna que existe en la tabla de datos.
#   Nota: es preferible usar ncol() para cuando se trabaja con dataset grandes.

rownames(mat_data) <- rnames                
#   nombramos las filas con rnames ya guardado anteriormente.

View(mat_data)
#   Aprendiendo a customizar los colores de heatmaps.


my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
#   Cambio de ver a rojo pasando por amarillo.
#   n=299 nnúmero de colores individuales.

col_breaks = c(seq(-1,0.0000,length=100), # for red
               seq(0.0001,0.8000,length=100),  # for yellow
               seq(0.8001,1,length=100)) # for green
#   Podemos decidir el rango de valor para los cuales se ocupe cada color.
#   Esto se ocupa si quieres resltar algunos valores en un rango 
#   en especial.
#   Nota: Se definen my_palette y col_breaks antes para que el codigo en heatmap.2() 
#         se vea de una manera mas elegante.

#Opcional: guardar el heatmap como PNG

png("Nombre del archivo",width = 5*300,height = 5*300,res = 300,pointsize = 8)        

#   Crea archivo PNG del heatmap
#   5x300 pixels
#   300 pixels por pulgada
#   tamaño de la fuente
#   [1500 pixels] / [300 pixels/inch] = 5 inches (Fórmula para determinar tamaño de la imagen)

heatmap.2(mat_data,
          cellnote = mat_data,  # same data set for cell labels
          main = "Correlation", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks = col_breaks,  # enable color transition at specified limits
          dendrogram="row",     # only draw a row dendrogram
          Colv="NA")            # turn off column clustering
#   Casi todo bien, me tira el mensaje: unsorted 'breaks' will be sorted before use
#   (los 'descansos' sin clasificar se clasificarán antes de su uso)
#   supongo que se refiere a los saltos que doy [-1,0], [0.001,0.8],[0.801,1]
#   y los posibles valos que no entren en los intervalos.
#   Si los breaks no se definen heatmap.2() lo hace automaticamente y no tira el mensaje anterior.











