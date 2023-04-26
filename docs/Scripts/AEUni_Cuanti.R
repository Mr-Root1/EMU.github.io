#ANÁLISIS UNIVARIADO - Encuesta de Carga Financiera y Educación Financiera de los Hogares - IEFIC-2017 -2018
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

packages <- c("data.table","readxl", "dplyr","readr","esquisse", "ggplot2", "scales", "stringr", "bslib")
ipak(packages)

options(scipen = 999)


# cargar los datos Cuantitativos para el análisis exploratorio de los datos

df_Ct <- fread("Data/IEFIC_2017.csv") %>% 
  select(INGTOTOB,P2478_1,P2478_2,P2478_8,DEPARTAMENTO) %>%
  # mutate(DEPARTAMENTO = as.character(DEPARTAMENTO)) %>%
  filter(INGTOTOB != "0", INGTOTOB != "97",P2478_1 !="0",P2478_1 !="99", 
         P2478_1 !="98",P2478_2 !="98", P2478_2 !="0", P2478_2 !="99",P2478_8 !="99", P2478_8 !="98",P2478_8!="0",
         !is.na(INGTOTOB),!is.na(P2478_1),!is.na(P2478_2),!is.na(P2478_8))

colnames(df_Ct) <- c("INGRESO", "G_ALIMENTACIÓN", "G_VESTUARIO", "G_RECREACIÓN", "DEPARTAMENTO")
str(df_Ct)

# write.csv(df_Ct, file = "Outputs/df_Ct.csv")
# saveRDS(df_Ct, "Outputs/df_Ct.rds")


# "DICCIONARIO DE HOMOLOGACIÓN"

TD <- read_excel("Data/Diccionario IEFIC.xlsx", sheet = "Hoja2")
str(TD)


#################################################### AE UNIVARIADO INGRESO


# Recuento de valores ingreso

rec_ingreso <- df_Ct %>% 
  filter(!is.na(INGRESO)) %>% 
  select(INGRESO)
str(rec_ingreso)


k = round(1 + 3.3*log10(length(rec_ingreso$INGRESO)))
k

mini <-min(rec_ingreso$INGRESO, na.rm = T)
maxi <-max(rec_ingreso$INGRESO, na.rm = T)

# Construimos los límites

Rango = max(rec_ingreso$INGRESO, na.rm = T) - min(rec_ingreso$INGRESO, na.rm = T)
Rango

longitud = Rango/15
longitud

cortes_ingreso <- c(min(rec_ingreso$INGRESO), 
            min(rec_ingreso$INGRESO) + longitud,
            min(rec_ingreso$INGRESO) + 2*longitud,
            min(rec_ingreso$INGRESO) + 3*longitud,
            min(rec_ingreso$INGRESO) + 4*longitud,
            min(rec_ingreso$INGRESO) + 5*longitud,
            min(rec_ingreso$INGRESO) + 6*longitud,
            min(rec_ingreso$INGRESO) + 7*longitud,
            min(rec_ingreso$INGRESO) + 8*longitud,
            min(rec_ingreso$INGRESO) + 9*longitud,
            min(rec_ingreso$INGRESO) + 10*longitud,
            min(rec_ingreso$INGRESO) + 11*longitud,
            min(rec_ingreso$INGRESO) + 12*longitud,
            min(rec_ingreso$INGRESO) + 13*longitud,
            min(rec_ingreso$INGRESO) + 14*longitud,
            min(rec_ingreso$INGRESO) + 15*longitud)
cortes_ingreso

# saveRDS(cortes2, "Outputs/cortes.rds")
# cortes2 <- min(rec_ingreso$INGRESO) + c(seq(mini,k,1)) * longitud
# cortes2


tf_ingreso <- rec_ingreso %>% 
  mutate(intervalos = as.factor(cut(INGRESO, breaks = cortes_ingreso, include.lowest = TRUE, dig.lab=9))) %>% 
  group_by(intervalos, .drop = F, add = F) %>% 
  summarise(n_i = n()) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_i = cumsum(f_i),
         x_i = cortes_ingreso[1:15] + longitud/2,
         c_i = abs(cortes_ingreso[1:15] - cortes_ingreso[2:16]),
         d_i = n_i/c_i
  )

tf_ingreso

saveRDS(tf_ingreso, "Outputs/tf_ingreso.rds")


# Mostrar el resultado
tf_ingreso

sum(tf_ingreso$n_i)
sum(tf_ingreso$f_i)


#################################
# Graficos para variables cuanti 

# Histograma
Hist_ingreso <- hist(rec_ingreso$INGRESO, breaks = cortes_ingreso, freq = T,
              main = "Ingresos del Hogar", xlab = "Ingresos", ylab = "Frecuencia")
Hist_ingreso
saveRDS(Hist_ingreso, "Outputs/Hist_ingreso.rds")


# Diagrama de caja y bigotes
#creación de los cuartiles 
q1 <- quantile(rec_ingreso$INGRESO, probs = 0.25, type = 6)

q1# el 25% de la población gana menos de $400.000 por conceptos de arrendamientos

q2 <- quantile(rec_ingreso$INGRESO, probs = 0.50, type = 6)

q2# el 50% de la población gana menos de $600.000 por concepto de arrendamientos

q3 <- quantile(rec_ingreso$INGRESO, probs = 0.75, type = 6)

q3# el 75% de la población gana menos de $1´000.000 por concepto de arrendamientos 

quantile(rec_ingreso$INGRESO, probs = 0.9, type = 6)
# el 90% de la poblaciín gana menos de $2´000.000 por concepto de arrendamientos.
# el 10% de la población gana más de $2´000.000 por concepto de arrendamientos.

quantile(rec_ingreso$INGRESO, probs = 0.97, type = 6)
# el 97% de la poblaciín gana menos de $4´000.000 por concepto de arrendamientos.
# el 3% de la población gana más de $4´000.000 por concepto de arrendamientos.
g1 <- boxplot(rec_ingreso$INGRESO,horizontal = T, xlab = "Ingresos")

ls <- min(max(rec_ingreso$INGRESO), q3 + 1.5*(q3-q1))
ls

li <- max(min(rec_ingreso$INGRESO), q1 - 1.5*(q3-q1))
li

p <- ggplot(rec_ingreso, aes(x = "", y = INGRESO))
BOX_ingreso <- p + geom_boxplot() + ylab("Ingreso del Hogar") +
  scale_y_continuous(labels = scales::dollar_format())

BOX_ingreso
saveRDS(BOX_ingreso, "Outputs/BOX_ingreso.rds")

#promedio ponderado
potencia <- 2
pesos <- df_Ct$DEPARTAMENTO^potencia
media_ponderada <- weighted.mean(rec_ingreso$INGRESO, pesos)
media_ponderada


###TABLA DE RESULTADOS DE LOS 5 NÚMEROS
NUM_5_INGRESO1 <- data.frame(
  Minimo = min(rec_ingreso$INGRESO, na.rm = T), 
  Q1 = q1,
  media_ponderada,
  Mediana = median(rec_ingreso$INGRESO),
  Q2 = q2,
  Q3 = q3) %>% 
  mutate(Coef.Asimetría = (q1+q3-2*q2)/(q3-q1))
NUM_5_INGRESO1

saveRDS(NUM_5_INGRESO1, "Outputs/NUM_5_INGRESO1.rds")

NUM_5_INGRESO2 <- data.frame(
  Máximo = max(rec_ingreso$INGRESO, na.rm = T), 
  Varianza = var(rec_ingreso$INGRESO),
  Des.Estandar = sd(rec_ingreso$INGRESO),
  Coef.Variación = sd(rec_ingreso$INGRESO)/abs(media_ponderada))
NUM_5_INGRESO2

saveRDS(NUM_5_INGRESO2, "Outputs/NUM_5_INGRESO2.rds")


################################################### AE UNI GASTOS ALIMENTACIÓN


# Recuento de valores ingreso

rec_aliment <- df_Ct %>% 
  filter(!is.na(G_ALIMENTACIÓN)) %>% 
  select(G_ALIMENTACIÓN)
str(rec_aliment)


k1 = round(1 + 3.3*log10(length(rec_aliment$G_ALIMENTACIÓN)))
k1

mini1 <-min(rec_aliment$G_ALIMENTACIÓN, na.rm = T)
maxi1 <-max(rec_aliment$G_ALIMENTACIÓN, na.rm = T)

# Construimos los límites

Rango1 = max(rec_aliment$G_ALIMENTACIÓN, na.rm = T) - min(rec_aliment$G_ALIMENTACIÓN, na.rm = T)
Rango1

longitud1 = Rango1/15
longitud1

cortes_aliment <- c(min(rec_aliment$G_ALIMENTACIÓN), 
                    min(rec_aliment$G_ALIMENTACIÓN) + longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 2*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 3*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 4*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 5*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 6*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 7*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 8*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 9*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 10*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 11*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 12*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 13*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 14*longitud1,
                    min(rec_aliment$G_ALIMENTACIÓN) + 15*longitud1)
cortes_aliment

# saveRDS(cortes2, "Outputs/cortes.rds")
# cortes2 <- min(rec_ingreso$INGRESO) + c(seq(mini,k,1)) * longitud
# cortes2


tf_aliment <- rec_aliment %>% 
  mutate(intervalos1 = as.factor(cut(G_ALIMENTACIÓN, breaks = cortes_aliment, include.lowest = TRUE, dig.lab=9))) %>% 
  group_by(intervalos1, .drop = F, add = F) %>% 
  summarise(n_i = n()) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_i = cumsum(f_i),
         x_i = cortes_aliment[1:15] + longitud1/2,
         c_i = abs(cortes_aliment[1:15] - cortes_aliment[2:16]),
         d_i = n_i/c_i
  )

tf_aliment

# saveRDS(tf_aliment, "Outputs/tf_aliment.rds")


# Mostrar el resultado
sum(tf_aliment$n_i)
sum(tf_aliment$f_i)


#################################
# Graficos para variables cuanti 

# Histograma
Hist_aliment <- hist(rec_aliment$G_ALIMENTACIÓN, breaks = cortes_aliment, freq = T,
                     main = "Gastos en Alimentación", xlab = "Gasto", ylab = "Frecuencia")
Hist_aliment
saveRDS(Hist_aliment, "Outputs/Hist_aliment.rds")


# Diagrama de caja y bigotes
#creación de los cuartiles 
q11 <- quantile(rec_aliment$G_ALIMENTACIÓN, probs = 0.25, type = 6)

q11# el 25% de la población gana menos de $400.000 por conceptos de arrendamientos

q21 <- quantile(rec_aliment$G_ALIMENTACIÓN, probs = 0.50, type = 6)

q21# el 50% de la población gana menos de $600.000 por concepto de arrendamientos

q31 <- quantile(rec_aliment$G_ALIMENTACIÓN, probs = 0.75, type = 6)

q31# el 75% de la población gana menos de $1´000.000 por concepto de arrendamientos 

quantile(rec_aliment$G_ALIMENTACIÓN, probs = 0.9, type = 6)
# el 90% de la poblaciín gana menos de $2´000.000 por concepto de arrendamientos.
# el 10% de la población gana más de $2´000.000 por concepto de arrendamientos.

quantile(rec_aliment$G_ALIMENTACIÓN, probs = 0.97, type = 6)
# el 97% de la poblaciín gana menos de $4´000.000 por concepto de arrendamientos.
# el 3% de la población gana más de $4´000.000 por concepto de arrendamientos.
g11 <- boxplot(rec_aliment$G_ALIMENTACIÓN,horizontal = T, xlab = "Gastos en Alimentación")

ls1 <- min(max(rec_aliment$G_ALIMENTACIÓN), q31 + 1.5*(q31-q11))
ls1

li1 <- max(min(rec_aliment$G_ALIMENTACIÓN), q11 - 1.5*(q31-q11))
li1

# Crear el objeto de la gráfica
p1 <- ggplot(rec_aliment, aes(x = "", y = G_ALIMENTACIÓN))

# Agregar la capa del box plot
BOX_aliment <- p1 + geom_boxplot() + ylab("Gastos en Alimentación") +
  scale_y_continuous(labels = scales::dollar_format())
saveRDS(BOX_aliment, "Outputs/BOX_aliment.rds")


#promedio ponderado
potencia <- 2
pesos <- df_Ct$DEPARTAMENTO^potencia
media_ponderada_aliment <- weighted.mean(rec_aliment$G_ALIMENTACIÓN, pesos)
media_ponderada_aliment


###TABLA DE RESULTADOS DE LOS 5 NÚMEROS
NUM_5_ALIMENT1 <- data.frame(
  Minimo = min(rec_aliment$G_ALIMENTACIÓN, na.rm = T), 
  Q1 = q11,
  media_ponderada_aliment,
  Mediana = median(rec_aliment$G_ALIMENTACIÓN),
  Q2 = q21,
  Q3 = q31) %>% 
  mutate(Coef.Asimetría = (q11+q31-2*q21)/(q31-q11))
NUM_5_ALIMENT1

saveRDS(NUM_5_ALIMENT1, "Outputs/NUM_5_ALIMENT1.rds")

NUM_5_ALIMENT2 <- data.frame(
  Máximo = max(rec_aliment$G_ALIMENTACIÓN, na.rm = T), 
  Varianza = var(rec_aliment$G_ALIMENTACIÓN),
  Des.Estandar = sd(rec_aliment$G_ALIMENTACIÓN),
  Coef.Variación = sd(rec_aliment$G_ALIMENTACIÓN)/abs(media_ponderada_aliment))
NUM_5_ALIMENT2

saveRDS(NUM_5_ALIMENT2, "Outputs/NUM_5_ALIMENT2.rds")



################################################### AE UNI GASTOS VESTUARIO


# Recuento de valores ingreso

rec_vest <- df_Ct %>% 
  filter(!is.na(G_VESTUARIO)) %>% 
  select(G_VESTUARIO)
str(rec_vest)


k2 = round(1 + 3.3*log10(length(rec_vest$G_VESTUARIO)))
k2

mini1 <-min(rec_vest$G_VESTUARIO, na.rm = T)
maxi1 <-max(rec_vest$G_VESTUARIO, na.rm = T)

# Construimos los límites

Rango2 = max(rec_vest$G_VESTUARIO, na.rm = T) - min(rec_vest$G_VESTUARIO, na.rm = T)
Rango2

longitud2 = Rango2/14
longitud2

cortes_vest <- c(min(rec_vest$G_VESTUARIO), 
                    min(rec_vest$G_VESTUARIO) + longitud2,
                    min(rec_vest$G_VESTUARIO) + 2*longitud2,
                    min(rec_vest$G_VESTUARIO) + 3*longitud2,
                    min(rec_vest$G_VESTUARIO) + 4*longitud2,
                    min(rec_vest$G_VESTUARIO) + 5*longitud2,
                    min(rec_vest$G_VESTUARIO) + 6*longitud2,
                    min(rec_vest$G_VESTUARIO) + 7*longitud2,
                    min(rec_vest$G_VESTUARIO) + 8*longitud2,
                    min(rec_vest$G_VESTUARIO) + 9*longitud2,
                    min(rec_vest$G_VESTUARIO) + 10*longitud2,
                    min(rec_vest$G_VESTUARIO) + 11*longitud2,
                    min(rec_vest$G_VESTUARIO) + 12*longitud2,
                    min(rec_vest$G_VESTUARIO) + 13*longitud2,
                    min(rec_vest$G_VESTUARIO) + 14*longitud2)
cortes_vest

# saveRDS(cortes2, "Outputs/cortes.rds")
# cortes2 <- min(rec_ingreso$INGRESO) + c(seq(mini,k2,1)) * longitud2
# cortes2


tf_vest <- rec_vest %>% 
  mutate(intervalos2 = as.factor(cut(G_VESTUARIO, breaks = cortes_vest, include.lowest = TRUE, dig.lab=9))) %>% 
  group_by(intervalos2, .drop = F, add = F) %>% 
  summarise(n_i = n()) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_i = cumsum(f_i),
         x_i = cortes_vest[1:14] + longitud2/2,
         c_i = abs(cortes_vest[1:14] - cortes_vest[2:15]),
         d_i = n_i/c_i
  )

tf_vest

saveRDS(tf_vest, "Outputs/tf_vest.rds")


# Mostrar el resultado
sum(tf_vest$n_i)
sum(tf_vest$f_i)


#################################
# Graficos para variables cuanti 

# Histograma
Hist_vest <- hist(rec_vest$G_VESTUARIO, breaks = cortes_vest, freq = T,
                     main = "Gastos en Vestuario", xlab = "Gasto", ylab = "Frecuencia")
Hist_vest
saveRDS(Hist_vest, "Outputs/Hist_vest.rds")


# Diagrama de caja y bigotes
#creación de los cuartiles 
q12 <- quantile(rec_vest$G_VESTUARIO, probs = 0.25, type = 6)

q12# el 25% de la población gana menos de $400.000 por conceptos de arrendamientos

q22 <- quantile(rec_vest$G_VESTUARIO, probs = 0.50, type = 6)

q22# el 50% de la población gana menos de $600.000 por concepto de arrendamientos

q32 <- quantile(rec_vest$G_VESTUARIO, probs = 0.75, type = 6)

q32# el 75% de la población gana menos de $1´000.000 por concepto de arrendamientos 

quantile(rec_vest$G_VESTUARIO, probs = 0.9, type = 6)
# el 90% de la poblaciín gana menos de $2´000.000 por concepto de arrendamientos.
# el 10% de la población gana más de $2´000.000 por concepto de arrendamientos.

quantile(rec_vest$G_VESTUARIO, probs = 0.97, type = 6)
# el 97% de la poblaciín gana menos de $4´000.000 por concepto de arrendamientos.
# el 3% de la población gana más de $4´000.000 por concepto de arrendamientos.
g12 <- boxplot(rec_vest$G_VESTUARIO,horizontal = T, xlab = "Gastos en Vestuario")

ls2 <- min(max(rec_vest$G_VESTUARIO), q32 + 1.5*(q32-q12))
ls2

li2 <- max(min(rec_vest$G_VESTUARIO), q12 - 1.5*(q32-q12))
li2

# Crear el objeto de la gráfica
p2 <- ggplot(rec_vest, aes(x = "", y = G_VESTUARIO))

# Agregar la capa del box plot
BOX_vest <- p2 + geom_boxplot() + ylab("Gastos en Vestuario")  +
  scale_y_continuous(labels = scales::dollar_format())
saveRDS(BOX_vest, "Outputs/BOX_vest.rds")


#promedio ponderado
potencia <- 2
pesos <- df_Ct$DEPARTAMENTO^potencia
media_ponderada_vest <- weighted.mean(rec_vest$G_VESTUARIO, pesos)
media_ponderada_vest


###TABLA DE RESULTADOS DE LOS 5 NÚMEROS
NUM_5_VEST1 <- data.frame(
  Minimo = min(rec_vest$G_VESTUARIO, na.rm = T), 
  Q1 = q12,
  media_ponderada_vest,
  Mediana = median(rec_vest$G_VESTUARIO),
  Q2 = q22,
  Q3 = q32) %>% 
  mutate(Coef.Asimetría = (q12+q32-2*q22)/(q32-q12))
NUM_5_VEST1

saveRDS(NUM_5_VEST1, "Outputs/NUM_5_VEST1.rds")

NUM_5_VEST2 <- data.frame(
  Máximo = max(rec_vest$G_VESTUARIO, na.rm = T), 
  Varianza = var(rec_vest$G_VESTUARIO),
  Des.Estandar = sd(rec_vest$G_VESTUARIO),
  Coef.Variación = sd(rec_vest$G_VESTUARIO)/abs(media_ponderada_vest))
NUM_5_VEST2

saveRDS(NUM_5_VEST2, "Outputs/NUM_5_VEST2.rds")





################################################### AE UNI GASTOS RECREACIÓN


# Recuento de valores ingreso

rec_recr <- df_Ct %>% 
  filter(!is.na(G_RECREACIÓN)) %>% 
  select(G_RECREACIÓN)
str(rec_recr)


k3 = round(1 + 3.3*log10(length(rec_recr$G_RECREACIÓN)))
k3

mini1 <-min(rec_recr$G_RECREACIÓN, na.rm = T)
maxi1 <-max(rec_recr$G_RECREACIÓN, na.rm = T)

# Construimos los límites

Rango3 = max(rec_recr$G_RECREACIÓN, na.rm = T) - min(rec_recr$G_RECREACIÓN, na.rm = T)
Rango3


longitud3 = Rango3/15
longitud3

cortes_recr <- c(min(rec_recr$G_RECREACIÓN), 
                 min(rec_recr$G_RECREACIÓN) + longitud3,
                 min(rec_recr$G_RECREACIÓN) + 2*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 3*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 4*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 5*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 6*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 7*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 8*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 9*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 10*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 11*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 12*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 13*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 14*longitud3,
                 min(rec_recr$G_RECREACIÓN) + 15*longitud3)
cortes_recr

# saveRDS(cortes2, "Outputs/cortes.rds")
# cortes2 <- min(rec_ingreso$INGRESO) + c(seq(mini,k,1)) * longitud
# cortes2


tf_recr <- rec_recr %>% 
  mutate(intervalos3 = as.factor(cut(G_RECREACIÓN, breaks = cortes_recr, include.lowest = TRUE, dig.lab=9))) %>% 
  group_by(intervalos3, .drop = F, add = F) %>% 
  summarise(n_i = n()) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_i = cumsum(f_i),
         x_i = cortes_recr[1:15] + longitud3/2,
         c_i = abs(cortes_recr[1:15] - cortes_recr[2:16]),
         d_i = n_i/c_i
  )

tf_recr

saveRDS(tf_recr, "Outputs/tf_recr.rds")


# Mostrar el resultado
sum(tf_recr$n_i)
sum(tf_recr$f_i)


#################################
# Graficos para variables cuanti 

# Histograma
Hist_recr <- hist(rec_recr$G_RECREACIÓN, breaks = cortes_recr, freq = T,
                  main = "Gastos en Recreación", xlab = "Gasto", ylab = "Frecuencia")
Hist_recr
saveRDS(Hist_recr, "Outputs/Hist_recr.rds")


# Diagrama de caja y bigotes
#creación de los cuartiles 
q13 <- quantile(rec_recr$G_RECREACIÓN, probs = 0.25, type = 6)

q13# el 25% de la población gana menos de $400.000 por conceptos de arrendamientos

q23 <- quantile(rec_recr$G_RECREACIÓN, probs = 0.50, type = 6)

q23# el 50% de la población gana menos de $600.000 por concepto de arrendamientos

q33 <- quantile(rec_recr$G_RECREACIÓN, probs = 0.75, type = 6)

q33# el 75% de la población gana menos de $1´000.000 por concepto de arrendamientos 

quantile(rec_recr$G_RECREACIÓN, probs = 0.9, type = 6)
# el 90% de la poblaciín gana menos de $2´000.000 por concepto de arrendamientos.
# el 10% de la población gana más de $2´000.000 por concepto de arrendamientos.

quantile(rec_recr$G_RECREACIÓN, probs = 0.97, type = 6)
# el 97% de la poblaciín gana menos de $4´000.000 por concepto de arrendamientos.
# el 3% de la población gana más de $4´000.000 por concepto de arrendamientos.
g13 <- boxplot(rec_recr$G_RECREACIÓN,horizontal = T, xlab = "Gastos en Recreación")

ls3 <- min(max(rec_recr$G_RECREACIÓN), q33 + 1.5*(q33-q13))
ls3

li3 <- max(min(rec_recr$G_RECREACIÓN), q13 - 1.5*(q33-q13))
li3

# Crear el objeto de la gráfica
p3 <- ggplot(rec_vest, aes(x = "", y = G_VESTUARIO))

# Agregar la capa del box plot
BOX_recr <- p3 + geom_boxplot() + ylab("Gastos en Recreación")  +
  scale_y_continuous(labels = scales::dollar_format())
saveRDS(BOX_recr, "Outputs/BOX_recr.rds")


#promedio ponderado
potencia <- 2
pesos <- df_Ct$DEPARTAMENTO^potencia
media_ponderada_recr <- weighted.mean(rec_recr$G_RECREACIÓN, pesos)
media_ponderada_recr


###TABLA DE RESULTADOS DE LOS 5 NÚMEROS
NUM_5_RECR1 <- data.frame(
  Minimo = min(rec_recr$G_RECREACIÓN, na.rm = T), 
  Q1 = q13,
  media_ponderada_vest,
  Mediana = median(rec_recr$G_RECREACIÓN),
  Q2 = q23,
  Q3 = q33) %>% 
  mutate(Coef.Asimetría = (q13+q33-2*q23)/(q33-q13))
NUM_5_RECR1

saveRDS(NUM_5_RECR1, "Outputs/NUM_5_RECR1.rds")

NUM_5_RECR2 <- data.frame(
  Máximo = max(rec_recr$G_RECREACIÓN, na.rm = T), 
  Varianza = var(rec_recr$G_RECREACIÓN),
  Des.Estandar = sd(rec_recr$G_RECREACIÓN),
  Coef.Variación = sd(rec_recr$G_RECREACIÓN)/abs(media_ponderada_recr))
NUM_5_RECR2

saveRDS(NUM_5_RECR2, "Outputs/NUM_5_RECR2.rds")

