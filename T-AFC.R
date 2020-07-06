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
packages <- c("dplyr","readxl","parameters","apa", "apaTables","haven","ggplot2","ggpubr","gridExtra",
              "apaTables", "reshape", "GPArotation", "mvtnorm", "psych", "psychometric", "lavaan", "nFactors",
              "semPlot", "lavaan", "MVN", "semTools")
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
semPaths(AFCSixFactor, intercepts = FALSE,edge.label.cex=1, optimizeLatRes = TRUE,
         groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",
         esize = 6, label.prop=1,sizeLat = 7,"std", layout="circle2")

#================================================================================================================================================================================================================================================#
#                       ANALISIS DESCRIPTIVOS
#================================================================================================================================================================================================================================================#

#Grafica de la Genero

PGenero<-BDT %>% 
  group_by(Genero) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Porcentaje=`n`/sum(`n`) * 100)

ggplot(PGenero, aes(x=1, y=Porcentaje, fill=Genero)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(round(Porcentaje,1),"%")), position = position_stack(vjust = 0.5)) + #Etiqueta de datos
  coord_polar(theta = "y") + 
  theme_void() + 
  scale_fill_manual(values = c("Orange2","steelblue")) + 
  ggtitle("Porcentaje de Genero de los Estudiantes Encunestados del Área de Derecho y Humanidades de la UNASAM")


# Grafica Edad 

PEdad<- BDT %>% 
  group_by(Edad) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Porcentaje=`n`/sum(`n`) * 100)

ggplot(PEdad, aes(x=Edad, y=Porcentaje)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  theme_cleveland() + 
  coord_flip() + 
  geom_text(aes(label = paste0(round(Porcentaje,1),"%")), position = position_stack(vjust = 0.5)) + 
  ggtitle("Edad de los Estudiantes Encuestados")
# NOTA: CORREGIR EL EJE X

#Grafica para Ciclo

PCiclo<- BDT %>% 
  group_by(Ciclo) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Porcentaje=`n`/sum(`n`) * 100)

ggplot(PCiclo, aes(x=Ciclo, y=Porcentaje)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  theme_cleveland() + 
  coord_flip() + #Grafica Horizontal
  geom_text(aes(label = paste0(round(Porcentaje,1),"%")), position = position_stack(vjust = 0.5)) + 
  ggtitle("Procentaje de Estudiantes Encuestados segun el Ciclo el que cruzan")

#Grafica de Escuela Academica Profecional

PEscuelaP<- BDT %>% 
  group_by(`Escuela Profesional`) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Porcentaje=`n`/sum(`n`) * 100)

ggplot(PEscuelaP, aes(x=`Escuela Profesional`, y=Porcentaje)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  theme_cleveland() + 
  coord_flip() +
  geom_text(aes(label = paste0(round(Porcentaje,1),"%")), position = position_stack(vjust = 0.5)) + 
  ggtitle("Estudiantes encuestados sengun sus Escuelas Academicas Profesionales")




ggplot(BDT, aes(x=`Escuela Profesional`), aes(y = (..count..)/sum(..count..))) + 
  geom_bar(aes(fill=Genero)) + 
  theme_cleveland() +
  coord_flip() +
  #geom_text(aes(label = paste0(round(Porcentaje,1),"%")), position = position_stack(vjust = 0.5)) + 
  ggtitle("Estudiantes encuestados sengun sus Escuelas Academicas Profesionales y Genero") +  
  scale_y_continuous("Porcentaje",labels=scales::percent)




#================================================================================================================================================================================================================================================#
#                       AYUDAS
#================================================================================================================================================================================================================================================#

# https://germangfeler.github.io/datascience/barras-y-tortas/                               ++++++++
# https://www.youtube.com/watch?v=aJBiXcjQZiA                                               ++ Barras
# https://www.youtube.com/watch?v=EQNm0Dcte3Y                                               ++ Pastel
# http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html    +++ Colores
# https://es.r4ds.hadley.nz/comunicar-con-gr%C3%A1ficos.html#escalas                        +++ Colores y Mas

#================================================================================================================================================================================================================================================#
#                       Guias GGPLOP2
#================================================================================================================================================================================================================================================#

# https://bookdown.org/gboccardo/manual-ED-UCH/construccion-de-graficos-usando-rstudio-funcionalidades-basicas-y-uso-del-paquete-ggplot2.html       +++Guia Ggplop2
# https://arcruz0.github.io/libroadp/dataviz.html

