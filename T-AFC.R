#================================================================================================================================================================================================================================================#
#                       CONFIRMATORY FACTOR ANALYSIS (CFA)
#================================================================================================================================================================================================================================================#
#Funcion Para Instalar Varios Paquetes.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# Paquetes a Usar
packages <- c("readxl","parameters","apa", "apaTables","haven","ggplot2","ggpubr","gridExtra","apaTables", "reshape", "GPArotation", "mvtnorm", "psych", "psychometric", "lavaan", "nFactors", "semPlot", "lavaan", "MVN", "semTools")
ipak(packages)

#Importar la Base de Datos
BDT <- read_excel("G:/OLID JP/GitHub Repositorio/Practicando-R/BDTesis-DLA.xlsx")
View(BDT)

#Especificación del modelo conceptual
SixFactor<-'EA=~ ITEM08 + ITEM09 + ITEM14 + ITEM16 + ITEM18 + ITEM19 + ITEM25 + ITEM40 + ITEM45
EC=~ ITEM15 + ITEM21 + ITEM22 + ITEM23 + ITEM24 + ITEM26 + ITEM27 + ITEM31 + ITEM33 + ITEM37 + ITEM38
EPE=~ ITEM20 + ITEM29 + ITEM34 + ITEM35 + ITEM36 + ITEM42 
EPt=~ ITEM01 + ITEM02 + ITEM04 + ITEM28 + ITEM30 + ITEM41
ECp=~ ITEM03 + ITEM05 + ITEM06 + ITEM07 + ITEM11 + ITEM13 + ITEM32 + ITEM43
EP=~ ITEM10 + ITEM12 + ITEM17 + ITEM39 + ITEM44'

#Análisis Factorial Confirmatorio para seis dimensiones. Estimadores: ML: máxima verosimilitud, GLS: mínimos cuadrados ponderados, Usaremos:WLSMV

AFCSixFactor<-cfa(SixFactor,orthogonal=FALSE, data = BDT, estimator="WLSMV", ordered = names(BDT))
lavInspect(SixFactor, "cov.lv")
summary(AFCSixFactor, fit.measures=TRUE, standardized=TRUE)


#Mostar todos los Indices de Ajuste
fitMeasures(AFCSixFactor)

#Graficar el AFC del Modelo
semPaths(AFCSixFactor, intercepts = FALSE,edge.label.cex=1, optimizeLatRes = TRUE, groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=1,sizeLat = 7,"std", layout="circle2")

