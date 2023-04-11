#ANÁLISIS MULTIVARIADO - Encuesta de Carga Financiera y Educación Financiera de los Hogares - IEFIC-2017 -2018
#Fecha: 05/04/2023
#Autor: Diego Sanabria
#Nota: Este script carga un conjunto de datos provenieintes de DANE,
#realiza un análisis exploratorio de los datos y genera un archivo RMarkdow.


#cargar librerias
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("data.table", "readxl", "dplyr","readr","esquisse", "scales", "ggplot2", "RColorBrewer", "devtools", "corrplot", "GGally", "pheatmap", "PerformanceAnalytics", "plotly")
ipak(packages)

options(scipen = 999)

#cargar los datos Cuantitativos
df_Ct <- fread("Data/IEFIC_2017.csv") %>% 
  select(INGTOTOB,P2478_1,P2478_2,P2478_8,DEPARTAMENTO) %>%
  mutate(DEPARTAMENTO = as.character(DEPARTAMENTO)) %>% 
  filter(INGTOTOB != "0", INGTOTOB != "97",P2478_1 !="0",P2478_1 !="99", P2478_1 !="98",P2478_2 !="98", P2478_2 !="0", P2478_2 !="99",P2478_8 !="99", P2478_8 !="98",P2478_8!="0",
         !is.na(INGTOTOB),!is.na(P2478_1),!is.na(P2478_2),!is.na(P2478_8))

colnames(df_Ct) <- c("INGRESO", "G_ALIMENTACIÓN", "G_VESTUARIO", "G_RECREACIÓN", "DEPARTAMENTO")
str(df_Ct)
saveRDS(df_Ct, "Outputs/df_Ct.rds")


# "DICCIONARIO DE HOMOLOGACIÓN"

TD <- read_excel("Data/Diccionario IEFIC.xlsx", sheet = "Hoja2")
str(TD)

#cargar los datos Cuanlitativos
df_Cl <- fread("Data/IEFIC_2017.csv") %>%
  select(P35,P3045,P2439,DEPARTAMENTO) %>%
  filter(DEPARTAMENTO <= 99) %>% 
  mutate(DEPARTAMENTO = as.character(DEPARTAMENTO), P35 = as.character(P35),
         P3045 = as.character(P3045), P2439 = as.character(P2439))

colnames(df_Cl) <- c("SEXO", "PER. PAG. ELECTRÓNICOS", "VIVIENDA PROPIA", "DEPARTAMENTO")
str(df_Cl)
saveRDS(df_Cl, "Outputs/df_Cl.rds")

#Matriz de varianzas y covarianzas
Mdf_Ct <- df_Ct %>%
  select(-DEPARTAMENTO)
str(Mdf_Ct)
s <- cov(Mdf_Ct)
det(s)
eigen(s)


#matriz de correlación
mcor <- cor(Mdf_Ct)
mcor1 <- data.frame(mcor)

# #plotear la correlación
corrplot::corrplot(mcor)


p <- ggplotly(ggpairs(df_Ct, aes(color = DEPARTAMENTO)) +
                scale_color_brewer(palette = "Set1")+
                scale_y_continuous(labels = dollar_format()))


saveRDS(p, "Outputs/corCuanti.rds")
p



pheatmap(mcor1,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)

p1 <-ggplotly(chart.Correlation(Mdf_Ct, histogram = TRUE, method = "pearson"))

p1
#vector de medias
vec.men <- Mdf_Ct%>% 
  summarise_all(.funs = mean) %>%
  as.matrix() %>% 
  as.vector()
vec.men

#Distancia de mahalanobis
d2 <- mahalanobis(Mdf_Ct, vec.men, s)
d2

#boxplot
summary(d2)
boxplot(d2)
hist(d2)

#varianza total 
diag(s)
sum(diag(s))
boxplot(scale(Mdf_Ct))

#varianza promedio 
sum(diag(s))/ncol(Mdf_Ct)
ncol(Mdf_Ct)

#varianza generalizada 
det(s)

#desviación típica generalizada
sqrt(det(s))

#desviación promedio
(det(s)^(1/(2*ncol(Mdf_Ct))))

#Coeficientes de asimetría 

#datos atípicos con chí-cuadrado
chi095 <- qchisq(0.95,3)
chi095
