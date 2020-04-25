#  La expresión génica diferencial es el proceso mediante el cual 
#  las células son capaces de decodificar la información contenida
#  dentro del material genético (ácidos nucleicos) para la elaboración
#  del producto génico necesario para el buen funcionamiento del organismo.
#  El proceso más utilizado para revelar este producto génico es el 
#  RNA-Seq, a partir del cual se puede realizar el análisis de la 
#  expresión diferencial que nos permite detectar aquellos genes que 
#  son diferencialmente expresados con respecto a una condición de control.
#  ENSG--> ¿genes?
#  SRR--> ¿muestras(personas)?

#  library() sirve para invocar librerias que va a usar el programa.

library(dplyr)
library(DESeq2)
  
#  carga los datos que estamos usando y los coloca en una matriz

#  read.csv(file, header = TRUE, sep = ",", quote = "\"",dec = ".",
#           fill = TRUE, comment.char = "", ...)
#  file= nombre del archivo que debe estar dentro del directorio donde se trabaja getwd()
#  header= TRUE, si la primera fila contiene el nombre de las variables
#          el encabezado se establece en VERDADERO si y solo si la primera fila
#          contiene un campo menos que el número de columnas.
#  sep= "??" El simboloso que se usa para separar los datos en la base de datos.
#  dec="." el caracter utilizado en el archivo para puntos decimales.
#  fill=TRUE si las filas tienen logitud desigual se rellenan automatico.
#  row.names=1 proporciona la columna de la tabla que contiene los nombres de las filas.
 
countData = read.csv(file = "GSE37704_featurecounts.csv", row.names= 1) %>% 
  dplyr::select(-length) %>% 
  as.matrix()

#filtra las filas que en sus datos suman mayor que 1.

countData = countData[rowSums(countData)>1, ]
head(countData)

colData = read.csv(file = "GSE37704_metadata.csv", row.names=1)
colData

# Set up the DESeqDataSet Object and run the DESeq pipeline
#Convierte en una clase de matriz específica.

dds = DESeqDataSetFromMatrix(countData=countData,
                             colData=colData,
                             design=~condition)
dds = DESeq(dds)
dds

#  Resultado: HoxA1 vs siRNA (dos tipos de muestras)
#  HoxA1--> (proteína)
#  siRNA-->(ARN de silenciamiento)
#  p-value--->se define como la probabilidad de que un valor 
#             estadístico calculado sea posible dada una hipótesis nula cierta.
#             En términos simples, el valor p ayuda a diferenciar resultados
#             que son producto del azar del muestreo, de resultados que son 
#             estadísticamente significativos.
#             Si el valor p cumple con la condición de ser menor que un nivel 
#             de significancia impuesto arbitrariamente, este se considera 
#             como un resultado estadisticamente significativo y, por lo tanto,
#             permite rechazar la hipótesis nula.
#             p-value=P(resultado extremo|Hip-null)
#  genes are up o down (¿apagados o prendidos?). Mas expresados unos que otros.

res = results(dds, contrast=c("condition", "hoxa1_kd", "control_sirna"))
res = res[order(res$pvalue),]
summary(res)

###########################################################################################

#  Dado que mapeamos y contamos contra la anotación Ensembl,
#  nuestros resultados solo tienen información sobre las ID de genes Ensembl.
#  Agregamos una base de datos.

library("AnnotationDbi")
library("org.Hs.eg.db")
columns(org.Hs.eg.db)

# el simbolo $ toma de res una columna en específico.
res$symbol = mapIds(org.Hs.eg.db,
                    keys=row.names(res), 
                    column="SYMBOL",
                    keytype="ENSEMBL",
                    multiVals="first")
res$entrez = mapIds(org.Hs.eg.db,
                    keys=row.names(res), 
                    column="ENTREZID",
                    keytype="ENSEMBL",
                    multiVals="first")
res$name =   mapIds(org.Hs.eg.db,
                    keys=row.names(res), 
                    column="GENENAME",
                    keytype="ENSEMBL",
                    multiVals="first")

head(res, 10)

#  Interpretación de resultados?? 
#  Solo muestras 10 genes mas expresados?

###########################################################################################

#  Pathway analysis

#step 0: setup (also need to map the reads outside R)
#if (!requireNamespace("BiocManager", quietly=TRUE))
#  install.packages("BiocManager")
#BiocManager::install(c("pathview", "gage", "gageData", "GenomicAlignments",
#                           "TxDb.Hsapiens.UCSC.hg19.knownGene"))

library(pathview)
library(gage)
library(gageData)
data(kegg.sets.hs)
data(sigmet.idx.hs)
kegg.sets.hs = kegg.sets.hs[sigmet.idx.hs]
head(kegg.sets.hs, 3)

foldchanges = res$log2FoldChange
names(foldchanges) = res$entrez
head(foldchanges)
  
# Get the results
keggres = gage(foldchanges, gsets=kegg.sets.hs, same.dir=TRUE)
  
# Look at both up (greater), down (less), and statatistics.
lapply(keggres, head)
 
# Get the pathways
keggrespathways = data.frame(id=rownames(keggres$greater), keggres$greater) %>% 
  tbl_df() %>% 
  filter(row_number()<=5) %>% 
  .$id %>% 
  as.character()
keggrespathways  

# Get the IDs.
keggresids = substr(keggrespathways, start=1, stop=8)
keggresids  

# Define plotting function for applying later
plot_pathway = function(pid) pathview(gene.data=foldchanges, pathway.id=pid, species="hsa", new.signature=FALSE)
  
# plot multiple pathways (plots saved to disk and returns a throwaway list object)
tmp = sapply(keggresids, function(pid) pathview(gene.data=foldchanges, pathway.id=pid, species="hsa"))  
  

# Gene Ontology (GO)
  
data(go.sets.hs)
data(go.subs.hs)
gobpsets = go.sets.hs[go.subs.hs$BP]
  
gobpres = gage(foldchanges, gsets=gobpsets, same.dir=TRUE)
  
lapply(gobpres, head)

#################################
###########           ###########
##########   BOXPLOT   ##########
###########           ###########
#################################

#   Boxplot separados enfermos/ Sanos

EvsS <- read.csv("GSE37704_featurecounts.csv", stringsAsFactors = FALSE)

colnames(EvsS) <- c("GEN","LONG","ENF1","ENF2","ENF3","SAN1","SAN2","SAN3")
View(EvsS)

caja1 <- subset(EvsS, GEN == "ENSG00000183508" , select = c("ENF1","ENF2","ENF3"))
caja2 <- subset(EvsS, GEN == "ENSG00000117519" , select = c("ENF1","ENF2","ENF3"))
caja3 <- subset(EvsS, GEN == "ENSG00000159176" , select = c("ENF1","ENF2","ENF3"))
caja4 <- subset(EvsS, GEN == "ENSG00000150938" , select = c("ENF1","ENF2","ENF3"))
caja5 <- subset(EvsS, GEN == "ENSG00000116016" , select = c("ENF1","ENF2","ENF3"))
caja6 <- subset(EvsS, GEN == "ENSG00000166507" , select = c("ENF1","ENF2","ENF3"))
caja7 <- subset(EvsS, GEN == "ENSG00000175279" , select = c("ENF1","ENF2","ENF3"))
caja8 <- subset(EvsS, GEN == "ENSG00000133997" , select = c("ENF1","ENF2","ENF3"))
caja9 <- subset(EvsS, GEN == "ENSG00000160948" , select = c("ENF1","ENF2","ENF3"))
caja10 <- subset(EvsS, GEN == "ENSG00000140600" , select = c("ENF1","ENF2","ENF3"))
cajita <- data.frame(t(caja1),t(caja2),t(caja3),t(caja4),
                     t(caja5),t(caja6),t(caja7),t(caja8),
                     t(caja9),t(caja10))
colnames(cajita) <- c("ENSG00000183508","ENSG00000117519","ENSG00000159176","ENSG00000150938",
                      "ENSG00000116016","ENSG00000166507","ENSG00000175279","ENSG00000133997",
                      "ENSG00000160948","ENSG00000140600")

View(cajita)  # Matriz de 10x3 donde ewstan los genes expresados en lso enfermos.
boxplot(cajita,las= 2,xlab= "GEN",main="Enfermos")


caja1.1 <- subset(EvsS, GEN == "ENSG00000183508" , select = c("SAN1","SAN2","SAN3"))
caja2.1 <- subset(EvsS, GEN == "ENSG00000117519" , select = c("SAN1","SAN2","SAN3"))
caja3.1 <- subset(EvsS, GEN == "ENSG00000159176" , select = c("SAN1","SAN2","SAN3"))
caja4.1 <- subset(EvsS, GEN == "ENSG00000150938" , select = c("SAN1","SAN2","SAN3"))
caja5.1 <- subset(EvsS, GEN == "ENSG00000116016" , select = c("SAN1","SAN2","SAN3"))
caja6.1 <- subset(EvsS, GEN == "ENSG00000166507" , select = c("SAN1","SAN2","SAN3"))
caja7.1 <- subset(EvsS, GEN == "ENSG00000175279" , select = c("SAN1","SAN2","SAN3"))
caja8.1 <- subset(EvsS, GEN == "ENSG00000133997" , select = c("SAN1","SAN2","SAN3"))
caja9.1 <- subset(EvsS, GEN == "ENSG00000160948" , select = c("SAN1","SAN2","SAN3"))
caja10.1 <- subset(EvsS, GEN == "ENSG00000140600" , select = c("SAN1","SAN2","SAN3"))
cajita.1 <- data.frame(t(caja1.1),t(caja2.1),t(caja3.1),t(caja4.1),
                     t(caja5.1),t(caja6.1),t(caja7.1),t(caja8.1),
                     t(caja9.1),t(caja10.1))
colnames(cajita.1) <- c("ENSG00000183508","ENSG00000117519","ENSG00000159176","ENSG00000150938",
                      "ENSG00000116016","ENSG00000166507","ENSG00000175279","ENSG00000133997",
                      "ENSG00000160948","ENSG00000140600")

View(cajita.1)
boxplot(cajita.1,las = 2,main="Sanos")


EandS <- data.frame(t(cajita),t(cajita.1))
View(EandS)



########################################################################################################
# Poner boxplot juntos para comparar

EvsS <- read.csv("GSE37704_featurecounts.csv", stringsAsFactors = FALSE)
colnames(EvsS) <- c("GEN","LONG","ENF1","ENF2","ENF3","SAN1","SAN2","SAN3")
View(EvsS)

#  comparación entre los genes 

#  Gen ENSG00000183508
En1 <- subset(EvsS, GEN == "ENSG00000183508" , select = c("ENF1","ENF2","ENF3"))
En11 <- data.frame(t(En1))
San1<- subset(EvsS, GEN == "ENSG00000183508" , select = c("SAN1","SAN2","SAN3"))
San11 <- data.frame(t(San1))
# 
#  Gen ENSG00000117519
En2 <- subset(EvsS, GEN == "ENSG00000117519" , select = c("ENF1","ENF2","ENF3"))
En22 <- data.frame(t(En2))
San2<- subset(EvsS, GEN == "ENSG00000117519" , select = c("SAN1","SAN2","SAN3"))
San22 <- data.frame(t(San2))
#
#  Gen ENSG00000159176
En3 <- subset(EvsS, GEN == "ENSG00000159176" , select = c("ENF1","ENF2","ENF3"))
En33 <- data.frame(t(En3))
San3 <- subset(EvsS, GEN == "ENSG00000159176" , select = c("SAN1","SAN2","SAN3"))
San33 <- data.frame(t(San3))
#
#  Gen ENSG00000150938
En4 <- subset(EvsS, GEN == "ENSG00000150938" , select = c("ENF1","ENF2","ENF3"))
En44 <- data.frame(t(En4))
San4 <- subset(EvsS, GEN == "ENSG00000150938" , select = c("SAN1","SAN2","SAN3"))
San44 <- data.frame(t(San4))
#
# Gen ENSG00000116016
En5 <- subset(EvsS, GEN == "ENSG00000116016" , select = c("ENF1","ENF2","ENF3"))
En55 <- data.frame(t(En5))
San5 <- subset(EvsS, GEN == "ENSG00000116016" , select = c("SAN1","SAN2","SAN3"))
San55 <- data.frame(t(San5))

#  Gen ENSG00000166507
En6 <- subset(EvsS, GEN == "ENSG00000166507" , select = c("ENF1","ENF2","ENF3"))
En66 <- data.frame(t(En6))
San6 <- subset(EvsS, GEN == "ENSG00000166507" , select = c("SAN1","SAN2","SAN3"))
San66 <- data.frame(t(San6))
# 
#  Gen ENSG00000175279
En7 <- subset(EvsS, GEN == "ENSG00000175279" , select = c("ENF1","ENF2","ENF3"))
En77 <- data.frame(t(En7))
San7 <- subset(EvsS, GEN == "ENSG00000175279" , select = c("SAN1","SAN2","SAN3"))
San77 <- data.frame(t(San7))
#
#  Gen ENSG00000133997
En8 <- subset(EvsS, GEN == "ENSG00000133997" , select = c("ENF1","ENF2","ENF3"))
En88 <- data.frame(t(En8))
San8 <- subset(EvsS, GEN == "ENSG00000133997" , select = c("SAN1","SAN2","SAN3"))
San88 <- data.frame(t(San8))
#
#  Gen ENSG00000160948
En9 <- subset(EvsS, GEN == "ENSG00000160948" , select = c("ENF1","ENF2","ENF3"))
En99 <- data.frame(t(En9))
San9 <- subset(EvsS, GEN == "ENSG00000160948" , select = c("SAN1","SAN2","SAN3"))
San99 <- data.frame(t(San9))
#
# Gen ENSG00000140600
En10 <- subset(EvsS, GEN == "ENSG00000140600" , select = c("ENF1","ENF2","ENF3"))
En1010 <- data.frame(t(En10))
San10 <- subset(EvsS, GEN == "ENSG00000140600" , select = c("SAN1","SAN2","SAN3"))
San1010 <- data.frame(t(San10))


x11()
Conf5x2 = matrix(c(1:10), nrow=2, byrow=TRUE)
Conf5x2
layout(Conf5x2)
layout.show(10)
boxplot(En11,las= 2,xlab= "ENSG00000183508",main="Enfermo",col = "red")
boxplot(En22,las= 2,xlab= "ENSG00000117519",main="Enfermo",col = "red")
boxplot(En33,las= 2,xlab= "ENSG00000159176",main="Enfermo",col = "red")
boxplot(En44,las= 2,xlab= "ENSG00000150938",main="Enfermo",col = "red")
boxplot(En55,las= 2,xlab= "ENSG00000116016",main="Enfermo",col = "red")
boxplot(San11,las= 2,xlab= "ENSG00000183508",main="Sano",col = "green" )
boxplot(San22,las= 2,xlab= "ENSG00000117519",main="Sano",col = "green" )
boxplot(San33,las= 2,xlab= "ENSG00000159176",main="Sano",col = "green" )
boxplot(San44,las= 2,xlab= "ENSG00000150938",main="Sano",col = "green" )
boxplot(San55,las= 2,xlab= "ENSG00000116016",main="Sano",col = "green" )

x11()
Conf5x2 = matrix(c(1:10), nrow=2, byrow=TRUE)
Conf5x2
layout(Conf5x2)
layout.show(10)
boxplot(En66,las= 2,xlab= "ENSG00000166507",main="Enfermo",col = "red")
boxplot(En77,las= 2,xlab= "ENSG00000175279",main="Enfermo",col = "red")
boxplot(En88,las= 2,xlab= "ENSG00000133997",main="Enfermo",col = "red")
boxplot(En99,las= 2,xlab= "ENSG00000160948",main="Enfermo",col = "red")
boxplot(En1010,las= 2,xlab= "ENSG00000140600",main="Enfermo",col = "red")
boxplot(San66,las= 2,xlab= "ENSG00000166507",main="Sano",col = "green" )
boxplot(San77,las= 2,xlab= "ENSG00000175279",main="Sano",col = "green" )
boxplot(San88,las= 2,xlab= "ENSG00000133997",main="Sano",col = "green" )
boxplot(San99,las= 2,xlab= "ENSG00000160948",main="Sano",col = "green" )
boxplot(San1010,las= 2,xlab= "ENSG00000140600",main="Sano",col = "green" )

##################################
###########            ###########
##########   HEATMAPS   ##########
###########            ###########
##################################

#   Genes expresados en enfermos

library(dplyr)
library(DESeq2)
library(gplots)
library(RColorBrewer)
library(clusterSim)

colortransc <- colorRampPalette(c("red", "black", "green"))(n = 299)

#   Genes expresados en enfermos

hmenf <-t(cajita)
View(hmenf)

hmenf.stn <- data.Normalization (hmenf,type="n5",normalization="column")
View(hmenf.stn)

heatmap.2(hmenf.stn,
          cellnote = hmenf.stn,  
          main = "Enfermos", 
          notecol="black",      
          density.info="none",  
          trace="none",         
          margins =c(12,9),     
          col=colortransc,      
          dendrogram="row",     
          Colv="NA")           


#   Genes expresados en Sanos

hmsan <-t(cajita.1)
View(hmsan)

hmsan.stn <- data.Normalization (hmsan,type="n5",normalization="column")
View(hmsan.stn)

heatmap.2(hmsan.stn,
          cellnote = hmsan.stn,  
          main = "Sanos", 
          notecol="black",      
          density.info="none",  
          trace="none",         
          margins =c(12,9),     
          col=colortransc,      
          dendrogram="row",     
          Colv="NA")  

#   Genes expresados con ambos casos

hmevss <- data.Normalization (EandS,type="n5",normalization="column")
View(hmevss)

mat_hmevss <- data.matrix(hmevss[,1:6]) 
View(mat_hmevss)

heatmap.2(mat_hmevss,
          cellnote = mat_hmevss,  
          main = "Enfermos", 
          notecol="black",      
          density.info="none",  
          trace="none",         
          margins =c(12,9),     
          col=colortransc,      
          dendrogram="row",     
          Colv="NA")  

