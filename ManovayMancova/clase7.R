# análisis de la varianza multivariante (MANOVA y MANCOVA)

# descripción de la base de datos 
# los efectos de la terapia cognitivo-conductual (TCC)
# sobre el trastorno obsesivo compulsivo (TOC).
# Se consideran dos variables dependientes (DV1 y DV2):
# la ocurrencia de conductas relacionadas con la obsesión
# ( Acciones ) y la ocurrencia de cogniciones relacionadas
# con la obsesión ( Pensamientos ). Los pacientes con TOC
# se agrupan en tres condiciones: con TCC, con terapia
# conductual (BT) y sin tratamiento (NT).

# Base(OCD.dat) #leer

#ruta de acceso a base datos 
org="D:\\DocDoctorado\\DocMExDoc\\Borrador\\ClasesUNMSM\\Curso Analisis Multivariante\\BaseDatos"
setwd(org)
getwd()

library(tidyverse)
library(rstatix) # estadisticas
library(ggpubr) # graficos 

# Lectura de datos 

bmanova=read.delim("OCD.dat")
head(bmanova)

names(bmanova)=c("grupo",
                 "Comportamiento",
                 "pensamientos")

#convertimos tibble
bmanova=tbl_df(bmanova)
glimpse(bmanova)
bmanova

#Mostrado tabla de frecuencias 
table(bmanova[1])

## transformar la varible grupo
# y transformalo a factor

bmanova$grupo=factor(bmanova$grupo,
                     levels=c("CBT","BT","No Treatment Control"),
                     labels=c("CBT","BT","NT"))
glimpse(bmanova)

# Modelo de regresión 
# Y(Cualitativa)=x(cuant)B+error (anova) univariante
#  multivariado
# Y=y1----- y=y1+y2----

# identificar datos perdidos 

bmanova %>% 
  group_by(grupo) %>% 
  identify_outliers(Comportamiento)

bmanova %>% 
  group_by(grupo) %>% 
  identify_outliers(pensamientos)

table(bmanova$Comportamiento)
table(bmanova$pensamientos)

# utilizado distancias de mahalalobis
bmanova %>% 
  group_by(grupo) %>% 
  mahalanobis_distance() %>% 
  filter(is.outlier==TRUE) %>% 
  as.data.frame()

# Dispersión  y diagrama 

bmanova %>% 
  ggplot(aes(Comportamiento,pensamientos))+
  geom_point()+
  theme_bw()

# visualmente no existe Colinealidad
p=bmanova %>% 
  gather(key="variables",value="pntj",-grupo)   

p %>% 
  ggplot(aes(x=grupo,y=pntj, color=variables))+
  geom_boxplot()+
  theme_bw()

bmanova %>% 
  ggplot(aes(x=grupo,y=Comportamiento))+
  geom_boxplot()+
  theme_bw()

# extrayendo estadisticas descriptivas
varind=bmanova %>% 
  select(-grupo)

varind %>% 
  psych::describe() %>% 
  select(mean,sd,median)


# contrate hipotesis 

# contraste de hipotes de normalidad
# univariante  y multivariante
varind %>% 
  MVN::mvn(mvnTest ="mardia",
           univariateTest ="SF",desc =FALSE)

ggqqplot(bmanova,"Comportamiento",facet.by ="grupo",
         ylab="comportamiento",ggtheme =theme_bw())

ggqqplot(bmanova,"pensamientos",facet.by ="grupo",
         ylab="pensamientos",ggtheme =theme_bw())

# hipotesis de multicolinealidad
# rstatix 
# cor_test()---- 2 variables independentes
# cor_mat()--- > más de 2
bmanova %>% 
  cor_test(Comportamiento,pensamientos)

# Contrastando la hipotesis de linealidad 
# se puede hacer visualmente.
library(GGally)
results=bmanova %>% 
  select(Comportamiento,pensamientos,grupo) %>% 
  group_by(grupo) %>% 
  doo(~ggpairs(.)+theme_bw(),result="plots")

# analisis de linealidad por grupo CBT=1, BT=2, NT=3
results$plots[2] 

# contraste de hipotesis de homocedasticidas R=I

library(biotools)
boxM(bmanova[c(2,3)],bmanova$grupo) 

# posible continuar con el MANOVA 

# Ho: no hay diferencia entre los tratamientos

vectind=cbind(bmanova$Comportamiento,
              bmanova$pensamientos)
y=cbind(bmanova$grupo)

modelo=manova(vectind~y,bmanova)
summary(modelo,intercept=TRUE) #--- pillai

summary(modelo,intercept=TRUE,test="Wilks")

summary(modelo,intercept=TRUE,test="Hotelling") 

summary(modelo,intercept=TRUE,test="Roy")

#y=a+Comportamiento*b+error
#y=a+pesamiento*b+error
summary.aov(modelo)

# t-student---- sc^2-- varianzas iguales  

# pruebas post-hoc
# rstatix
# anova_test()----- cuando cumplen todos los supuestos
# welch_anova_test()---- cuando no cumple homogenidad de varianzas
bmanova %>% 
  gather(key="variables",value="pntj",-grupo) %>% 
  group_by(variables) %>% 
  anova_test(pntj~grupo)

bmanova %>% 
  gather(key="variables",value="pntj",-grupo) %>% 
  group_by(variables) %>% 
  welch_anova_test(pntj~grupo)

bmanova %>% 
  gather(key="variables",value="pntj",-grupo) %>% 
  group_by(variables) %>% 
  tukey_hsd(pntj~grupo)

# Group the data by variable
grouped.data = bmanova %>% 
  gather(key="variables",value="pntj",-grupo) %>% 
  group_by(variables)


# Calcular ANOVA univariante unidireccional

library(rstatix)

# Realice una prueba de anova unidireccional de Welch
grouped.data %>% welch_anova_test(pntj ~ grupo)

# o hacer la prueba de Kruskal-Wallis
grouped.data %>% kruskal_test(pntj ~ grupo)

# or usar aov()
grouped.data %>% anova_test(pntj ~ grupo)

# Calcule m?ltiples comparaciones por pares

# Las funciones R tukey_hsd()[paquete rstatix] se pueden utilizar 
# para calcular las pruebas post-hoc de Tukey si se cumple el 
# supuesto de homogeneidad de la varianza.

# Si ha violado el supuesto de homogeneidad de las variaciones,
# post-hoc de Games-Howell "games_howell_test()".
# Tambi?n es posible utilizar la funci?n pairwise_t_test()[rstatix]
# con la opci?n pool.sd = FALSEy var.equal = FALSE.

pwc = bmanova %>% 
  gather(key="variables",value="pntj",-grupo) %>% 
  group_by(variables) %>%
  games_howell_test(pntj ~ grupo) %>%
  select(-estimate, -conf.low, -conf.high) # Remover detalles
pwc

pwc %>% add_xy_position(x = "grupo")
test.label = create_test_label(
  description = "MANOVA", statistic.text = quote(italic("F")),
  statistic = 745.23, p= "<0.0001", parameter = "4,54",
  type = "expression", detailed = TRUE
)


library(ggpubr)
ggboxplot(
  bmanova, x = "grupo", y = c("comportamiento", "pensamientos"), 
  merge = TRUE) + 
  stat_pvalue_manual(
    pwc, step.group.by = "variables", y.position = 20,
    color = "variables")


ggplot() + 
  stat_pvalue_manual(
    pwc, step.group.by = "variables", y.position = 20,
    color = "variables")+
  labs(
    subtitle = test.label,
    caption = get_pwc_label(pwc, type = "expression"))+
  theme_bw()


## mancova
#mancova(
#data,
#deps,
#factors = NULL,
#covs = NULL,
#multivar = list("pillai", "wilks", "hotel", "roy"),
#boxM = FALSE,
#shapiro = FALSE,
#qqPlot = FALSE
#)


jmv::mancova(data = bmanova,
             deps = vars(comportamiento, pensamientos),
             factors = grupo)