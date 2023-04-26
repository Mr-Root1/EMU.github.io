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

packages <- c("data.table", "readxl", "dplyr","readr","esquisse", "scales", "ggplot2", "RColorBrewer", "devtools", "corrplot", "GGally", "pheatmap", "PerformanceAnalytics", "plotly", "MVN")
ipak(packages)

options(scipen = 999)

#cargar los datos Cuantitativos
df_MULT <- fread("Data/IEFIC_2017.csv") %>% 
  select(INGTOTOB,P2478_1,P2478_2,P2478_8,P35,P3045,P2439, DEPARTAMENTO) %>%
  filter(INGTOTOB != "0", INGTOTOB != "97",P2478_1 !="0",P2478_1 !="99", P2478_1 !="98",P2478_2 !="98", P2478_2 !="0", P2478_2 !="99",P2478_8 !="99", P2478_8 !="98",P2478_8!="0",
         !is.na(INGTOTOB),!is.na(P2478_1),!is.na(P2478_2),!is.na(P2478_8),DEPARTAMENTO <= 99,!is.na(P35),!is.na(P3045),!is.na(P2439),!is.na(DEPARTAMENTO),P3045 >= 1, P3045 <= 6)
str(df_MULT)
saveRDS(df_MULT, "Outputs/df_MULT.rds")

#Resumen estadístico
R_Ingreso <- summary(df_MULT$INGTOTOB)
R_G.Alim <- summary(df_MULT$P2478_1)
R_G.Vest <- summary(df_MULT$P2478_2)
R_G.Recr <- summary(df_MULT$P2478_8)
str(R_Ingreso)

#Dispersograma 
pairs(df_MULT, upper.panel=NULL, col="#8B0000")

boxplot(scale(df_Ct))
# Establecer los límites del eje Y
ylim <- c(-2, 2)
# Mostrar el gráfico
boxplot(scale(df_Ct), col=rainbow(4), main="Diagrama de caja para cada variable")

###MEDIDAS GLOBALES DE VARIABILIDAD
# Cálculo del vector de medias 
vec.med <- (1/9515)*t(df_Ct)%*%rep(1,9515)
vec.med

#Matriz de varianzas y covarianzas
cov_DANE <- cov(df_Ct)
#varianza total
Dcov_DANE <- diag(cov_DANE)
#varianza promedio
var_prom <- sum(diag(cov_DANE))/4
#varianza generalizada
determinante <- det(cov_DANE)
#desviación típica generalizada
Des_tipG <- sqrt(det(cov_DANE))
#variabilidad promedio
Variab_prom <- (det(cov_DANE))^(1/4)
#Desviación Promedio 
Desv_prom <- (det(cov_DANE))^(1/8)

#DISTANCIAS
dis.DANE <- mahalanobis(df_Ct,vec.med,cov_DANE)
Disp.distan.Dane <- plot(dis.DANE, main = "Distancias de Mahalanobis DANE")

##ASIMETRÍA Y KURTOSIS
Asi_Kur<-function(x){ 
  
  n<-dim(x)[1] 
  p<-dim(x)[2]
  vec.medias<-(1/n)*t(x)%*%rep(1,n)
  S<-cov(x)*(n-1)/n 
  
  b1.ma<-matrix(NA,n,n)
  b2.ma<-rep(NA,n) 
  
  for (i in 1:n) {
    
    for (j in 1:n) {
      b1.ma[i,j]<-(sum(t(x[i,]-vec.med)%*%solve(S)%*%(x[j,]-vec.med)))^3 
    } 
    b2.ma[i]<-(sum(t(x[i,]-vec.med)%*%solve(S)%*%(x[i,]-vec.med)))^2 
  } 
  Asimetria.multi<-sum(b1.ma)/(n^2) 
  Kurtosis.multi<-sum(b2.ma)/n 
  list("Filas"=n,"columnas"=p,"vector de medias"=vec.med, "S"=S,"Asimetria.mul"=Asimetria.multi,"Kurtosis.mul"=Kurtosis.multi) 
}

##ASIMETRÍA Y KURTOSIS
Asi_Kur(df_Ct)


#DISTRIBUCIÓN NORMAL MULTIVARIANTE
vec.med
cov_DANE


#PASO 1: Calcular las distancias de mahalanobis y ordenarlas en forma 
# ascendente
dis.DANE.O <- sort(dis.DANE)
#PASO 2: hallar el percentil q((i-0.5/)n) de la chi-cuadrado
qchi.x<-qchisq((1:9515-0.5)/9515, df=4)
qchi.x
# PASO 3: graficar los pares ordenados (qchi.x, di.X.ord).
plot(qchi.x, dis.DANE.O, main = "CHI SQUARE PLOT",
     xlab = "Cuantiles de la Chi-cuadrado con v = 2gl",
     ylab = "Distancias de Mahalanobis", col="red")
abline(0,1, col="blue")

# PRUEBA DE MULTINORMALIDAD 
# Primero revisión caso univariado 
# Algunos gráficos: cuantil-cuantil e histogramas 
mvn(df_Ct, univariatePlot = "qqplot")

# HACIENDO LA PRUEBA PARA JUZGAR NORMALIDAD:

mvn(df_Ct,univariateTest = "Lillie")

# prueba de multinormalidad 
mvn(df_Ct, mvnTest = c("mardia"))

# GRÁFICO CHI CUADRADO - Chi square plot
par(mfrow=c(2,2)) 
mvn(df_Ct, multivariatePlot = "qq")

#ATÍPICOS
chi0.95<-qchisq(0.95,4) 
chi0.95
par(mfrow=c(1,1))
plot(dis.DANE, ylabel = "Distancia de Mahalanobis", xlabel ="Indice")
abline(chi0.95,0, col="red")

faces(df_Ct)










valores <- eigen(s)

#matriz de correlación
mcor <- cor(df_Ct)
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
# Gráfico de las distancias de mahalanobis 
plot(d2, col="blue")


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






