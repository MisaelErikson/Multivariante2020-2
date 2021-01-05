# análisis de conglomerados
library(tidyverse)
library(maps) # sirve para graficar mapas
library(mapdata)

# lectura de ruta de base datos
org="D:\\DocDoctorado\\DocMExDoc\\Borrador\\ClasesUNMSM\\Curso Analisis Multivariante\\BaseDatos"
setwd(org)
datos=read_csv("ClusterJera.csv")

# convertir a tibble
datos=tbl_df(datos)
datos

#Extraer datos 
nombres=datos$Name

Datos=datos[-1]
Datos

# Calculemos las distancias ecludianas

euclid=dist(Datos, method = "euclidean")
euclid

# Agrupamiento jerarquico hclust

#agrupamiento jerarquico aglometativo
jera=hclust(euclid,method = "ward.D")
jera
View(jera)

# crear un dendograma
plot(jera,labels=nombres)

# digamos queremos formar 4  clusters
rect.hclust(jera,k=4,border="blue")

rect.hclust(jera,k=3,border="red")

# establecer los coortes 
clusterResults=cutree(jera,k=3)
clusterResults

dataN=cbind(nombres,Datos,clusterResults)
dataN

# calculemos lo centroides del cluster
dataN %>% 
  group_by(clusterResults) %>% 
  summarise(mean(Action),mean(Comedy),
            mean(Drama),mean(Horror),
            mean(Sci))

# Graficado en un diagrama de dispersión
library(factoextra)
add_rownames(Datos,var="nombres")
fviz_cluster(list(data=Datos,cluster=clusterResults))

# agrupamiento jerarquico divisional
library(cluster)
ch4=diana(Datos)
pltree(ch4,cex=0.6,hang=-1,main="dendograma",labels = nombres)

rect.hclust(ch4,k=3)

clust=cutree(ch4,k=3)
clust

fviz_cluster(list(data=Datos,cluster=clust))


# agrupamientos no jerarquicos 

# [k means] análisis de Clúster
library(NbClust)

# lectura de datos web

df=USArrests
df

# normalizar la data

df=scale(df)

#centroide 0
head(df)

#calulamos la distancia 

m.distancia=get_dist(df,method = "euclidean")

fviz_dist(m.distancia,gradient = list(low="blue",
                                      mid="white",
                                      high="red"))
# estimar el número de clústers

fviz_nbclust(df,kmeans,method ="wss")

fviz_nbclust(df,kmeans,method ="silhouette")

fviz_nbclust(df,kmeans,method ="gap_stat")

# determinado por varios estadisticos el numero de clusters
rnumclu=NbClust(df,distance="euclidean",
                min.nc=2,max.nc=10,
                method = "kmeans",
                index="alllong")

#graficando el numero de clusters
fviz_nbclust(rnumclu)

# determinemos los clusters k means

k2=kmeans(df,centers=2,nstart=25)
k2

# graficando los cluster

fviz_cluster(k2,data=df)

# centroides
fviz_cluster(k2,data=df,ellipse.type ="euclid" )

# jeraquico 2 clusters

res2=hcut(df,k=2,stand=TRUE)
fviz_dend(res2,rect=TRUE,cex=0.5,
          k_colors =c("red","blue"))

res3=hcut(df,k=3,stand=TRUE)
fviz_dend(res3,rect=TRUE,cex=0.5,
          k_colors =c("red","blue","green"))

# kmedoids con PAM --- (explorar este modelo mas robusto)
res4=hcut(df, k = 4, stand = TRUE)
fviz_dend(res4, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black"))
# pasar los cluster a mi df inicial para trabajar con ellos
# resultados las medidas de cada uno de ellos
# medias originales no estandarizados
USArrests %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
# añadiendo el cluster a la base de datos
df=USArrests
df
df$clus=as.factor(k2$cluster)
df
df=USArrests
df=scale(df)
df=as.data.frame(df)
df$clus=as.factor(k2$cluster)
df
# transformar a vertical la base de datos
df$clus=factor(df$clus)
data_long=gather(df, caracteristica, valor, Murder:Rape, factor_key=TRUE)
data_long
# los mas altos mayor aporte o mayor peligro
ggplot(data_long, aes(as.factor(x = caracteristica), y = valor,group=clus, colour = clus)) +
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")+
  geom_point(aes(shape=clus))
# Kmedoids con PAM
data("USArrests")
df=scale(USArrests)
head(df)
#estimar el número de clústers
#Elbow, silhouette o gap_stat method
fviz_nbclust(df, pam, method = "wss")
fviz_nbclust(df, pam, method = "silhouette")
fviz_nbclust(df, pam, method = "gap_stat")
resnumclust=NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "median", index = "alllong")
#resnumclust
fviz_nbclust(resnumclust)
#calculamos los dos clústers
library(fpc)
pam9=pam(df, 9)
print(pam9)
pam2=pam(df, 2)
print(pam2)
#probamos algunas visualizaciones
fviz_cluster(pam2, data = df, ellipse.type = "norm")
fviz_cluster(pam9, data = df)
res9=hcut(df, k = 9, stand = TRUE, method = "median")
fviz_dend(res9, rect = TRUE, cex = 0.5,
          k_colors = "simpsons")
#le pedimos un pequeño resumen
USArrests %>%
  mutate(Cluster = pam9$clustering) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
#lo pegamos en nuestro data frame y lo dibujamos ;)
df=USArrests
df
df=scale(USArrests)
df
df=as.data.frame(df)
df
df$clus=as.factor(pam9$clustering)
df
df$clus=factor(df$clus)
data_long=gather(df, caracteristica, valor, Murder:Rape, factor_key=TRUE)
data_long
ggplot(data_long, aes(as.factor(x = caracteristica), y = valor,group=clus, colour = clus)) +
  stat_summary(fun = mean, geom="pointrange", size = 1, aes(shape = clus))+
  stat_summary(geom="line")
#geom_point(aes(shape=clus))
sessionInfo()

packageVersion("nombre del paquete")