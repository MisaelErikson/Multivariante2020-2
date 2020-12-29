# Análisis de correspondencia 
library(tidyverse)
library(foreign)

# Ingresando a directorio de trabajo
org="D:\\DocDoctorado\\DocMExDoc\\Borrador\\ClasesUNMSM\\Curso Analisis Multivariante\\BaseDatos"
setwd(org)

#lecturando la base de datos
eese=read.spss("EESEADULTO_2014.sav",
               to.data.frame=T)

#covertir tibble
eese=tbl_df(eese)
dim(eese)

# E4: Convivencia en pareja
# G21: Estado de salud en los ultimos 12 meses
# T112: Frecuencia con la que realiza una actividad fisica
# V121: ¿Fuma actualmente?
# W127: Frecuencia de consumo de alcohol en los ultimos 12 meses

datos=eese %>% 
  select(convivencia=E4,
         estado.salud=G21,
         actividad=T112,
         fuma=V121,
         alcohol=W127)
table(datos$alcohol)

# no sabe  y no contesta= NA
levels(datos$alcohol)=c("diario","semanal","semanal",
                        "semana","mensual","mensual","anual","ex","nunca",NA,NA)

#ACS

# consumo de alcohol vs estado.salud

freqs=table(datos$estado.salud,datos$alcohol)
freqs

# ver una tabla de frecuncias graficamente

library(gplots)
balloonplot(freqs,label=FALSE,
            show.margins = FALSE,
            main="tabla de frec.")

# determinar la relación entre las variables 
ct1=chisq.test(freqs)
ct1

# verificando los valos esperados
ct1$expected

# contrate multiple

for(atributo in c("convivencia","actividad","fuma","alcohol")){}
sapply(datos[,c(1,3:5)], function(x){
  atributo=factor(x)
  chisq.test(table(datos$estado.salud,atributo))$p.value
})

# n>50 ( Ho o H1)  y cuando n=grande es casi H1

# Analisis de correspondeincia simple

library(FactoMineR)
corres1=CA(freqs)

# resumen del modelo
summary(corres1)


max.porc=max(1/(dim(freqs)-1)*100)

# grafico de sedimentación
library(factoextra)

fviz_screeplot(corres1)+
  geom_hline(yintercept =max.porc,color="red",linetype=2)

fviz_ca_row(corres1,col.row="cos2")

fviz_ca_col(corres1,col.col="cos2")


# contribuciones 
fviz_contrib(corres1,choice = "row", axe=1:2)

fviz_contrib(corres1,choice = "col", axe=1:2)

# grafico correlaciones 
library(corrplot)
corrplot(corres1$row$contrib, is.corr=FALSE)

corrplot(corres1$col$contrib, is.corr=FALSE)

# solapamiento de las categorias 
ellipseCA(corres1)

# Correpondica multiple 
# recodificar 

levels(datos$convivencia)=c("cónyuge",
                            "pareja",
                            "no",NA,NA)
levels(datos$actividad)=c("ninguno",
                          "ocasional",
                          "mensual",
                          "semanal",
                          NA,NA)
levels(datos$fuma)=c("fuma_mucho",
                     "fuma_poco",
                     "fuma_ex",
                     "fuma_nunca",
                     NA,NA)
levels(datos$estado.salud)

corres2=MCA(datos,method="burt",na.method ="averange")

summary(corres2)


# el grafico de sedimentación
fviz_screeplot(corres2,addlabels=TRUE)

colSums(corres2$var$contrib)

corrplot(corres2$var$contrib, is.corr=FALSE)

# analisis de las correspondicas Multiples conjuntas.
library(ca)
res=mjca(datos,lambda="Burt")
summary(res)

fviz_mca_biplot(corres2)

fviz_mca_var(corres2,col.var="contrib")

#fin
