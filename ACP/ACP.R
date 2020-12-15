# Analisis de Componetes Principales 

library(tidyverse)
#install.packages("factoextra")
library(factoextra)

# Lectura de base de datos 

bacp=read_csv(file.choose()) # Lectura de ruta 
bacp

## detecion de datos perdidos 
which(is.na(bacp))

## analasis exploratorio de datos 

p=bacp %>% 
  select(-Channel,-Region)
p

## Diagrama de dispersión
p %>% 
  pairs()

## Diagrama de cajas
p %>% 
  boxplot()

# Estadisticas descriptivas 
p %>% 
  psych::describe() %>% 
  select(mean,sd,median,skew)

# histogramas
p %>% 
  MVN::mvn(univariatePlot = "histogram",
           desc=FALSE)

# Estadisticos de adecuación muestra

p %>% 
  cor() %>% 
  round(3)

library(PerformanceAnalytics)
chart.Correlation(p,histogram=F,pch=19)

# r<>0 por lo que podemos realizar un ACP
psych::cortest(cor(p))

det(cor(p))
# r<>0 por lo que podemos realizar un ACP

# Prueba de Esfericidad de Bartlet
# Ho: Mcor=I
# H1: Mcor<>I
psych::cortest.bartlett(cor(p),n=440)

# prueba KMO
psych::KMO(p)

# grafico  de sedimentación
library(psych)
dev.off()
scree(bacp,
      main="Grafico de sedimentación")

fa.parallel(bacp,
            fa="pc")

# N=(x-u)/s ----  X----N(u,s)  Normalizar 
# z=(x-ux)/sx ----  X----P(ux,sx) Estandarizar

# Estandarizar 
estd=scale(bacp)

# generando ACP

acp=prcomp(estd)
acp

desv=acp$sdev
varianza=desv^2

summary(acp)

# contribuciones de variables a PC1
fviz_contrib(acp,
             choice = "var",axes=1)

fviz_contrib(acp,
             choice = "var",axes=2)

fviz_contrib(acp,
             choice = "var",axes=3)

#
fviz_contrib(acp,
             choice = "var",axes=4)


# Graficando las puntuaciones individuales 
fviz_eig(acp,addlabels = TRUE)

plot(acp)

# circulos de correlaciones 
fviz_pca_var(acp, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)


fviz_pca_var(acp,
             axes=c(1,2),
             col.var = "cos2")

fviz_pca_ind(acp,
             axes=c(1,2),
             col.ind = "cos2")

fviz_pca_biplot(acp,
             axes=c(1,2))
