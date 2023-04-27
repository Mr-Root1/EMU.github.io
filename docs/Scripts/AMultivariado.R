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

packages <- c("partykit", "data.table", "readxl", "dplyr","readr","esquisse", "scales", "ggplot2", "RColorBrewer", "devtools", "corrplot", "GGally", "pheatmap", "PerformanceAnalytics", "plotly", "MVN", "vcd", "lattice")
ipak(packages)
library(mvpart)
options(scipen = 999)

#cargar los datos Cuantitativos
df_MULT <- fread("Data/IEFIC_2017.csv") %>% 
  select(INGTOTOB,P2478_1,P2478_2,P2478_8) %>%
  filter(INGTOTOB != "0", INGTOTOB != "97",P2478_1 !="0",P2478_1 !="99", P2478_1 !="98",P2478_2 !="98", P2478_2 !="0", P2478_2 !="99",P2478_8 !="99", P2478_8 !="98",P2478_8!="0",
         !is.na(INGTOTOB),!is.na(P2478_1),!is.na(P2478_2),!is.na(P2478_8))
str(df_MULT)
saveRDS(df_MULT, "Outputs/df_MULT.rds")

#Resumen estadístico
R_Ingreso <- summary(df_MULT$INGTOTOB)
saveRDS(R_Ingreso, "Outputs/R_Ingreso.rds")
R_G.Alim <- summary(df_MULT$P2478_1)
saveRDS(R_G.Alim, "Outputs/R_G.Alim.rds")
R_G.Vest <- summary(df_MULT$P2478_2)
saveRDS(R_G.Vest, "Outputs/R_G.Vest.rds")
R_G.Recr <- summary(df_MULT$P2478_8)
saveRDS(R_G.Recr, "Outputs/R_G.Recr.rds")

#Dispersograma 
pairs(df_MULT, upper.panel=NULL, col="#8B0000")

boxplot(scale(df_MULT))
# Establecer los límites del eje Y
ylim <- c(-2, 2)
# Mostrar el gráfico
boxplot(scale(df_MULT), col=rainbow(4), main="Diagrama de caja para cada variable")

###MEDIDAS GLOBALES DE VARIABILIDAD
# Cálculo del vector de medias 
vec.med <- (1/9515)*t(df_MULT)%*%rep(1,9515)
vec.med
#Matriz de varianzas y covarianzas
cov_DANE <- cov(df_MULT)
saveRDS(cov_DANE, "Outputs/cov_DANE.rds")
#varianza total
Dcov_DANE <- diag(cov_DANE)
saveRDS(Dcov_DANE, "Outputs/Dcov_DANE.rds")
#varianza promedio
var_prom <- sum(diag(cov_DANE))/4
var_prom
#varianza generalizada
determinante <- det(cov_DANE)
determinante
#desviación típica generalizada
Des_tipG <- sqrt(det(cov_DANE))
Des_tipG
#variabilidad promedio
Variab_prom <- (det(cov_DANE))^(1/4)
Variab_prom
#Desviación Promedio 
Desv_prom <- (det(cov_DANE))^(1/8)
Desv_prom

#DISTANCIAS
dis.DANE <- mahalanobis(df_MULT,vec.med,cov_DANE)
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
Asi_Kur(df_MULT)

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
mvn(df_MULT, univariatePlot = "qqplot")

# HACIENDO LA PRUEBA PARA JUZGAR NORMALIDAD:

mvn(df_MULT,univariateTest = "Lillie")

# prueba de multinormalidad 
mvn(df_MULT, mvnTest = c("mardia"))

# GRÁFICO CHI CUADRADO - Chi square plot
par(mfrow=c(2,2)) 
mvn(df_MULT, multivariatePlot = "qq")

#ATÍPICOS
chi0.95<-qchisq(0.95,4)
par(mfrow=c(1,1))
plot(dis.DANE, ylab = "Distancia de Mahalanobis", xlab = "Indice")
abline(chi0.95,0, col="red")


df_Ct <- fread("Data/IEFIC_2017.csv") %>% 
  select(INGTOTOB,P2478_1,P2478_2,P2478_8,DEPARTAMENTO) %>%
  mutate(DEPARTAMENTO = as.character(DEPARTAMENTO)) %>%
  filter(INGTOTOB != "0", INGTOTOB != "97",P2478_1 !="0",P2478_1 !="99", 
         P2478_1 !="98",P2478_2 !="98", P2478_2 !="0", P2478_2 !="99",P2478_8 !="99", P2478_8 !="98",P2478_8!="0",
         !is.na(INGTOTOB),!is.na(P2478_1),!is.na(P2478_2),!is.na(P2478_8))

colnames(df_Ct) <- c("INGRESO", "G_ALIMENTACIÓN", "G_VESTUARIO", "G_RECREACIÓN", "DEPARTAMENTO")
str(df_Ct)


valores <- eigen(cov_DANE)
df_Ct1 <- df_Ct %>%  select(-DEPARTAMENTO)
#matriz de correlación
mcor <- cor(df_Ct1)
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

p1 <-ggplotly(chart.Correlation(df_Ct1, histogram = TRUE, method = "pearson"))

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


####################################################
#Multivariado Cualitativo
#DICCIONARIOS
Departamentos <- read_excel("Data/Diccionario IEFIC.xlsx", sheet = "Hoja2") %>%
  data.frame()
Sexos <- read_excel("Data/Diccionario IEFIC.xlsx", sheet = "Hoja4") %>% 
  mutate_all(as.character) %>% 
  data.frame()
Percepciónes <- read_excel("Data/Diccionario IEFIC.xlsx", sheet = "Hoja3") %>% 
  mutate_all(as.character) %>% 
  data.frame()
Vivienda <- read_excel("Data/Diccionario IEFIC.xlsx", sheet = "Hoja5") %>% 
  mutate_all(as.character) %>% 
  data.frame()

#cargar los datos Cuanlitativos
df_Cl <- fread("Data/IEFIC_2017.csv") %>%
  select(P35,P3045,P2439,DEPARTAMENTO) %>%
  filter(P35 <= 2) %>%
  filter(DEPARTAMENTO <= 99,!is.na(P35),!is.na(P3045),!is.na(P2439),!is.na(DEPARTAMENTO),P3045 >= 1, P3045 <= 6) %>% 
  mutate(P35 = factor(P35, levels = Sexos$P35, labels = Sexos$SEXO)) %>% 
  mutate(DEPARTAMENTO = factor(DEPARTAMENTO, levels = Departamentos$DEPARTAMENTO, labels = Departamentos$Nombre.del.departamento)) %>% 
  mutate(P3045 = factor(P3045, levels = Percepciónes$P3045, labels = Percepciónes$PERCEPCIÓN.PAGOS.ELECTRÓNICOS)) %>%
  mutate(P2439 = factor(P2439, levels = Vivienda$P2439, labels = Vivienda$Vivienda.Propia))
str(df_Cl)
saveRDS(df_Cl, "Outputs/df_MCl.rds")
#DICCIONARIOS
colnames(df_Cl) <- c("SEXO", "PERCEPCIÓN PAGOS ELECT.", "VIVIENDA PROPIA", "DEPARTAMENTO")

#Tabla Sexo vs Percepción de pagos electrónicos
tabla.S.Per <- table(df_Cl$SEXO,df_Cl$`PERCEPCIÓN PAGOS ELECT.`)
tabla.S.Per <- round(addmargins(prop.table(tabla.S.Per)*100),2)
tabla.S.Per
saveRDS(tabla.S.Per, "Outputs/tabla.S.Per.rds")

#Tabla Sexo vs Vivienda Propia
tabla.S.Viv <- table(df_Cl$SEXO,df_Cl$`VIVIENDA PROPIA`)
tabla.S.Viv <- round(addmargins(prop.table(tabla.S.Viv)*100),2)
tabla.S.Viv
saveRDS(tabla.S.Viv, "Outputs/tabla.S.Viv.rds")


#Graficos tablas de contingencia
colores <- c("#80FFFF", "#FFFFFF")
barplot(tabla.S.Per, col = colores, cex.names = 0.8)
# Añadimos una leyenda
legend("topright", legend = c("Hombre", "Mujer"), fill = colores)


colores <- c("#800FFF", "#AAAFFF")
barplot(tabla.S.Viv, col = colores, cex.names = 0.8)
# Añadimos una leyenda
legend("topleft", legend = c("Hombre", "Mujer"), fill = colores)



###MULTIVARIADO MIXTO
Violín1 <- bwplot(df_MULT$INGTOTOB ~ df_Cl$P35, 
                  ylim=c(-100000, 6000000), 
                  scales=list(cex=1.2, font=2), 
                  ylab = list("Ingreso", cex=2), 
                  panel = function(..., box.ratio) {
                    panel.violin(..., col = "transparent", 
                                 varwidth = FALSE, box.ratio = box.ratio)
                    panel.bwplot(..., fill = NULL, box.ratio = 0.1)
                  } 
)

# Aplicar rotación al eje x
Violín1 <- update(Violín1, scales=list(x=list(rot=30)))



Violín2 <- bwplot(df_MULT$INGTOTOB ~ df_Cl$P3045, ylim=c(-100000, 6000000), scales=list(cex=1.2, font=2), ylab = list
                  ("Ingreso", cex=2), panel = function(..., box.ratio)
                  {
                    panel.violin(..., col = "transparent",
                                 varwidth = FALSE, box.ratio = box.ratio)
                    panel.bwplot(..., fill = NULL, box.ratio = 0.1)
                  } )
Violín2 <- update(Violín2, scales=list(x=list(rot=30)))


##Análisis COMPONENTES PRINCIPALES
str(df_MULT)
colnames(df_MULT) <- c("INGRESO", "G_ALIMENTACIÓN", "G_VESTUARIO", "G_RECREACIÓN")
#Centrar los datos
datos_centrados <- df_MULT
datos_centrados$INGTOTOB <- df_MULT$INGTOTOB - mean(df_MULT$INGTOTOB)
datos_centrados$P2478_1 <- df_MULT$P2478_1 - mean(df_MULT$P2478_1)
datos_centrados$P2478_2 <- df_MULT$P2478_2 - mean(df_MULT$P2478_2)
datos_centrados$P2478_8 <- df_MULT$P2478_8 - mean(df_MULT$P2478_8)
datos_centrados


#cálculo de matriz de correlación
matriz_cov <- cov(datos_centrados)
matriz_cov

eigen <- eigen(matriz_cov)
# Valores propios
eigen$values
# Vectore propios
eigen$vectors

t_eigenvectors <- t(eigen$vectors)
t_datos_centrados <- t(datos_centrados)

# Producto matricial
pc_scores <- t_eigenvectors %*% t_datos_centrados
rownames(pc_scores) <- c("PC1", "PC2", "PC3", "PC4")

# Se vuelve a transponer para que los datos estén en modo tabla
t(pc_scores)

datos_recuperados <- t(eigen$vectors %*% pc_scores)
datos_recuperados[, 1] <- datos_recuperados[, 1] + mean(df_MULT$INGTOTOB)
datos_recuperados[, 2] <- datos_recuperados[, 2] + mean(df_MULT$P2478_1)
datos_recuperados[, 3] <- datos_recuperados[, 3] + mean(df_MULT$P2478_2)
datos_recuperados[, 4] <- datos_recuperados[, 4] + mean(df_MULT$P2478_8)
datos_recuperados

pca <- prcomp(df_MULT, scale = TRUE)
names(pca)

# Promedios
pca$center

# Varianzas
pca$scale

pca$rotation

dim(pca$x)

biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

pca$rotation <- -pca$rotation
pca$x        <- -pca$x
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

#Varianza acumulada
prop_varianza <- pca$sdev^2 / sum(pca$sdev^2)
prop_varianza

ggplot(data = data.frame(prop_varianza, pc = 1:4),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")

prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum

ggplot(data = data.frame(prop_varianza_acum, pc = 1:4),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")


##Análisis Conglomerados

fit <- ctree(INGTOTOB ~ herbs + reft + moss + sand + twigs + water, data = df_MULT)

# Graficar el árbol
plot(as.party(fit))












