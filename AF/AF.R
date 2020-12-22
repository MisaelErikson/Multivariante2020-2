# Analisis Factorial
# Exploratorio 
library(tidyverse)
library(foreign)
library(psych) # Factorial Exploratorio
org="D:\\DocDoctorado\\DocMExDoc\\Borrador\\ClasesUNMSM\\Curso Analisis Multivariante\\BaseDatos"
setwd(org)
bafx=read.spss("BAFEX.sav",to.data.frame = T)
bafx=tbl_df(bafx)
bafx

bafxst=bafx %>% 
  select(-ID,-BRAND)

bafxst

# Estudio de Condiciones previas 
r=bafxst %>% 
  cor() %>% 
  round(3)
r

# grafico de correlaciones 
library(PerformanceAnalytics)
bafxst %>% 
  chart.Correlation(histogram=F,pch=19)

# Test de Esfericidad de barlett
bafxst %>% 
  bartlett.test()

# determinate
det(r)

# criterio KMO

bafxst %>% 
  KMO()


# AFE
afex=fa(bafxst,nfactors = 3,fm="ml",
        rotate = "none",cor="poly")
afex

print(afex,digits =2,cut=.30,sort=TRUE)

# Numero de factores a extraer

library(parameters)
nfact=bafxst %>% 
  n_factors(rotate="none",fm="ml",
            n=NULL)
plot(nfact)
# tablas 
nfact

# Vemos sugerencias por diferentes metodos 
as.data.frame(nfact)
summary(nfact)

# verificación terorica con eigen valores
bafxst %>% 
  cor() %>% 
  eigen()

afex=fa(bafxst,nfactors = 3,fm="ml",
        rotate = "varimax",cor="poly",
        scores="regression")
afex

print(afex,digits =2,cut=.30,sort=TRUE)


# ordenado las cargas factoriales po factor y tamaño

fa.sort(afex)

plot(afex,labels=row.names(r))

# grafico de facotes
library(GPArotation)
diagram(afex,e.size=0.1)

#puntuaciones factoriales 
fisco=afex$scores

# graficos de puntuiaciones facotoriales 

dgraf=data.frame(fisco,eti=bafx$BRAND)
dgraf=tbl_df(dgraf)

dgraf=dgraf %>%
  mutate(labels=as.numeric(eti))
dgraf

dgraf


dgraf=dgraf %>% 
  mutate(tbebida=case_when((labels=="1") | (labels=="2")~ "Colas",
                           (labels=="3") | (labels=="4")~ "Deportivas",
                           (labels=="5") | (labels=="6")~ "Te"))

dgraf
# ML1 VS ML3
ggplot(dgraf)+
  geom_point(aes(x=ML1, y=ML3,color=tbebida,
                 shape=tbebida),size=3)+
  #geom_text_repel(aes(x=Dim.1, y=Dim.2),label=rownames(datos.grafico2))+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="1.Apaga sed (32%)", y="3.Funcional (35%)")+
  theme(legend.title=element_blank())

# ML1 VS ML2
ggplot(dgraf)+
  geom_point(aes(x=ML1, y=ML2,color=tbebida,
                 shape=tbebida),size=3)+
  #geom_text_repel(aes(x=Dim.1, y=Dim.2),label=rownames(datos.grafico2))+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="1.Apaga sed (32%)", y="2.Sabor (33%)")+
  theme(legend.title=element_blank())

# ML2 VS ML3
ggplot(dgraf)+
  geom_point(aes(x=ML2, y=ML3,color=tbebida,
                 shape=tbebida),size=3)+
  #geom_text_repel(aes(x=Dim.1, y=Dim.2),label=rownames(datos.grafico2))+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="2.Sabor (33%)", y="3.Funcional (32%)")+
  theme(legend.title=element_blank())

# Análisis Factorial Confirmatorio
library(semPlot)
library(lavaan)
library(semTools)

# Especificación del modelo conceptual (primero unidimensional, luego bifactorial)

Onefactor='cobebi=~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10'


nfactors='ML_3=~ X7 + X3 + X10 
ML_2=~ X4 + X8 + X1
ML_1=~ X2 + X9 + X5'

#realización del AFC para la primera estructura
CFAone = cfa(Onefactor,orthogonal=TRUE,
             data=bafxst, estimator="WLSMV",ordered =names(bafxst))
summary(CFAone, fit.measures=TRUE)

#Análisis Factorial Confirmatorio para la segunda dimensionalidad.
modelo= cfa(nfactors,orthogonal=FALSE, 
            data=bafxst, estimator="WLSMV",ordered =names(bafxst))
summary(modelo, fit.measures=TRUE)

# todos los indices de ajuste
fitMeasures(modelo)

# grafico SEM
semPaths(modelo, intercepts = FALSE,edge.label.cex=1.5,
         optimizeLatRes = TRUE, groups = "lat",pastel = TRUE,
         exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6,
         label.prop=2,sizeLat = 6,"std", layout="circle2")

