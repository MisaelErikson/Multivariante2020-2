#Análisis Discriminate
library(tidyverse)

#accediendo al directorio de trabajo.
org="D:\\DocDoctorado\\DocMExDoc\\Borrador\\ClasesUNMSM\\Curso Analisis Multivariante\\BaseDatos"
setwd(org)

#Lectura de la base datos 
bdvino=read.delim("wine.data",sep=",",
                  header=TRUE)

#Transformar la data tibble
bdvino=tbl_df(bdvino)
glimpse(bdvino)
bdvino

#explorando la base de datos.
plot(bdvino[c(4,6)],
     col=bdvino$Cultivador)

#estadisticas descriptivas
summary(bdvino)


# diagrama de cajas
boxplot(bdvino$V1,main="V1")
boxplot(bdvino$V2,main="V2")
boxplot(bdvino$V3,main="V3")
boxplot(bdvino$V4,main="V4")

#calculando los promedios
#tapply(vector, index, function)
tapply(bdvino$V1,bdvino$Cultivador,
       mean)
tapply(bdvino$V2,bdvino$Cultivador,mean)
tapply(bdvino$V3,bdvino$Cultivador,mean)
tapply(bdvino$V4,bdvino$Cultivador,mean)

# cajas según la variable respuesta 
boxplot(bdvino$V1~bdvino$Cultivador,
        col="green",main="V1") 

boxplot(bdvino$V2~bdvino$Cultivador,
        col="green",main="V2")

boxplot(bdvino$V3~bdvino$Cultivador,
        col="green",main="V3")

# Normalidad Univariante, normalidad multivariada
library(MVN)
mvn(data=bdvino[,-1],mvnTest ="mardia",
    multivariateOutlierMethod = "quan",
    set.seed(1234))

mvn(data=bdvino[,-1],mvnTest ="mardia",
    multivariatePlot = "qq",
    set.seed(1234))


mvn(data=bdvino[,-1],mvnTest ="hz",
    multivariateOutlierMethod = "quan",
    set.seed(1234))

mvn(data=bdvino[,-1],mvnTest ="royston",
    multivariateOutlierMethod = "quan",
    ,set.seed(1234))

# homogeneidad de las varianzas
Ho: R=I  (DL) (D=b1x+b2x)
H1: R<>I (QDA) 

library(biotools)
boxM(data=bdvino[,-1],
     grouping =factor(bdvino$Cultivador))


library(klaR)
greedy.wilks(Cultivador~.,data=bdvino)

# Modelo discriminate 

library(MASS)
modelo=lda(Cultivador~.,data=bdvino,
           prior=c(1,1,1)/3)
modelo

# ojo para fines labores se recomienda
# dividir la data en dos proporciones 
# testing (70 - 80)  prueba (30 - 20)

# clases predichas
clase.pred=predict(modelo,bdvino[,-1])$class

head(clase.pred)

# propabilidad predición
proba.pred=predict(modelo,bdvino[,-1])$posterior

proba.pred1=proba.pred[,1]

ggplot(bdvino,aes(x=proba.pred1))+
  geom_density(aes(fill=Cultivador),alpha=0.25)+
  theme_bw()

# tablas de clasificación
library(gmodels)
CrossTable(x=bdvino$Cultivador,
           y=clase.pred,
           prop.r =FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,size=0.3)

table(bdvino$Cultivador,
      clase.pred)


# calculo de accuracy

accuracy=mean(bdvino$Cultivador==clase.pred)
accuracy

#calculo del error
error=mean(bdvino$Cultivador!=clase.pred)
error

# matriz de confución
library(caret)
confusionMatrix(data=clase.pred,
                factor(bdvino$Cultivador))

# curva ROC
# es mejor si ROC es mayor que 0.5
library(caTools)
colAUC(proba.pred,
       factor(bdvino$Cultivador),
       plotROC = TRUE)
abline(0,1,col="red",tly=3)


# visualizar la clasificación
library(klaR)
partimat(factor(bdvino$Cultivador)~V1+V2+V3,
         data=bdvino,method="lda",prec=200)

#Pronosticos
V1=round(runif(1,min(bdvino$V1),
               max(bdvino$V1)),2)
V2=round(runif(1,min(bdvino$V2),
               max(bdvino$V2)),2)
V3=round(runif(1,min(bdvino$V3),
               max(bdvino$V3)),2)
V4=round(runif(1,min(bdvino$V4),
               max(bdvino$V4)),2)
V5=round(runif(1,min(bdvino$V5),
               max(bdvino$V5)),2)
V6=round(runif(1,min(bdvino$V6),
               max(bdvino$V6)),2)
V7=round(runif(1,min(bdvino$V7),
               max(bdvino$V7)),2)
V8=round(runif(1,min(bdvino$V8),
               max(bdvino$V8)),2)
V9=round(runif(1,min(bdvino$V9),
               max(bdvino$V9)),2)
V10=round(runif(1,min(bdvino$V10),
                max(bdvino$V10)),2)
V11=round(runif(1,min(bdvino$V11),
                max(bdvino$V11)),2)
V12=round(runif(1,min(bdvino$V12),
                max(bdvino$V12)),2)
V13=round(runif(1,min(bdvino$V13),
                max(bdvino$V13)),2)

#contruyendo un data frame
data1=data.frame(V1,V2,V3,V4,V5,V6,V7,V8,
                 V9,V10,V11,V12,V13)
predict(modelo,data1)$class
predict(modelo,data1)$posterior

# referencia de consulta.

# http://www.est.uc3m.es/esp/nueva_docencia/getafe/estadistica/analisis_multivariante/doc_generica/archivos/metodos.pdf
