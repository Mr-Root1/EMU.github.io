#ANÁLISIS UNIVARIADO - Encuesta de Carga Financiera y Educación Financiera de los Hogares - IEFIC-2017
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

#cargar los datos Cuanlitativos
df_Cl <- fread("Data/IEFIC_2017.csv") %>%
  select(P35,P3045,P2439,DEPARTAMENTO) %>%
  filter(DEPARTAMENTO <= 99,!is.na(P35),!is.na(P3045),!is.na(P2439),!is.na(DEPARTAMENTO),P3045 >= 1, P3045 <= 6)
str(df_Cl)
saveRDS(df_Cl, "Outputs/df_Cl.rds")
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

#TABLA DE FRECUENCIAS VAR. ##SEXO##
TDF_SEXO <- df_Cl %>%
  select(P35) %>%
  filter(P35 <= 2) %>% 
  mutate(P35 = as.character(P35)) %>%
  left_join(Sexos, by = "P35") %>%
  group_by(SEXO) %>%
  summarise(n_i = n()) %>%
  mutate(N_i= cumsum(n_i),
         f_i=n_i/sum(n_i),
         F_i=cumsum(f_i))
str(TDF_SEXO)
saveRDS(TDF_SEXO, "Outputs/TDF_SEXO.rds")

#grafico de barras para variables cualitativas actividad en la que ocupa el tiempo
Histo_Sexo <- ggplot(TDF_SEXO) +
  aes(x = reorder(SEXO, -n_i), y = n_i) +
  geom_col(fill = "#8B0000") +
  labs(x = "Sexo", 
       y = "Frecuencia", title = "Proporción por sexo IEFIC-2017", 
       subtitle = "Figura 1. Encuesta de Carga Financiera y Educación Financiera de los Hogares - IEFIC-2017") +
  coord_flip() +
  theme_minimal() +
  scale_y_reverse()

Histo_Sexo

#gráfico de sectores 
# Calcular los porcentajes correspondientes
conteo_AFT10p <- TDF_SEXO %>%
  mutate(pct = n_i / sum(n_i) * 100)
saveRDS(conteo_AFT10p, "outputs/conteo_AF10p.rds")
# Generar el gráfico de torta
Sect_Sexo <- ggplot(conteo_AFT10p, aes(x="", y=n_i, fill=reorder(SEXO, -n_i))) +
  geom_bar(stat="identity", width=50, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Set2") +
  labs(title="",
       fill="Sexos",
       x=NULL,
       y=NULL) +
  theme_void() +
  theme(legend.position="right") +
  # Agregar el porcentaje a cada categoría
  geom_text(aes(label = paste0(round(pct), "%")), 
            position = position_stack(vjust = 0.5))

Sect_Sexo

#TABLA DE FRECUENCIAS VAR. ##PERCEPCIÓN PAGOS##
TDF_PERCEP <- df_Cl %>%
  select(P3045) %>%
  filter(!is.na(P3045)) %>% 
  mutate(P3045 = as.character(P3045)) %>%
  left_join(Percepciónes, by = "P3045") %>%
  group_by(PERCEPCIÓN.PAGOS.ELECTRÓNICOS) %>%
  summarise(n_i = n()) %>%
  mutate(N_i= cumsum(n_i),
         f_i=n_i/sum(n_i),
         F_i=cumsum(f_i))
str(TDF_PERCEP)
saveRDS(TDF_PERCEP, "Outputs/TDF_PERCEP.rds")

#grafico de barras para variables cualitativas actividad en la que ocupa el tiempo
Histo_Percep <- ggplot(TDF_PERCEP) +
  aes(x = reorder(PERCEPCIÓN.PAGOS.ELECTRÓNICOS, -n_i), y = n_i) +
  geom_col(fill = "#458B74") +
    labs(x = "Percepción", 
       y = "Frecuencia", title = "Percepción a los págos electrónicos", 
       subtitle = "Figura 1. Percepción a los págos electrónicos") +
  coord_flip() +
  theme_minimal() +
  scale_y_reverse()

Histo_Percep

#gráfico de sectores 
# Calcular los porcentajes correspondientes
PorTDF_PERCEP <- TDF_PERCEP %>%
  mutate(pct = n_i / sum(n_i) * 100)
saveRDS(PorTDF_PERCEP, "outputs/conteo_AF10p.rds")
# Generar el gráfico de torta
Sect_Percep <- ggplot(PorTDF_PERCEP, aes(x="", y=n_i, fill=reorder(PERCEPCIÓN.PAGOS.ELECTRÓNICOS, -n_i))) +
  geom_bar(stat="identity", width=50, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Set2") +
  labs(title="",
       fill="¿Cual es su percepción hacia los págos electrónicos?",
       x=NULL,
       y=NULL) +
  theme_void() +
  theme(legend.position="right") +
  # Agregar el porcentaje a cada categoría
  geom_text(aes(label = paste0(round(pct), "%")), 
            position = position_stack(vjust = 0.5))

Sect_Percep


#TABLA DE FRECUENCIAS VAR. ##VIVIENDA##
TDF_VIVIENDA <- df_Cl %>%
  select(P2439) %>%
  filter(!is.na(P2439)) %>% 
  mutate(P2439 = as.character(P2439)) %>%
  left_join(Vivienda, by = "P2439") %>%
  group_by(Vivienda.Propia) %>%
  summarise(n_i = n()) %>%
  mutate(N_i= cumsum(n_i),
         f_i=n_i/sum(n_i),
         F_i=cumsum(f_i))
str(TDF_VIVIENDA)
saveRDS(TDF_VIVIENDA, "Outputs/TDF_VIVIENDA.rds")

#grafico de barras para variables cualitativas actividad en la que ocupa el tiempo
Histo_Viv <- ggplot(TDF_VIVIENDA) +
  aes(x = reorder(Vivienda.Propia, -n_i), y = n_i) +
  geom_col(fill = "#8B4500") +
  labs(x = "¿Tiene vivienda propia?", 
       y = "Frecuencia", title = "Poseción de Vivienda propia", 
       subtitle = "Figura 1. Poseción de Vivienda propia") +
  coord_flip() +
  theme_minimal() +
  scale_y_reverse()

Histo_Viv

#gráfico de sectores 
# Calcular los porcentajes correspondientes
PorTDF_Viv <- TDF_VIVIENDA %>%
  mutate(pct = n_i / sum(n_i) * 100)
saveRDS(PorTDF_PERCEP, "outputs/conteo_AF10p.rds")
# Generar el gráfico de torta
Sect_Viv <- ggplot(PorTDF_Viv, aes(x="", y=n_i, fill=reorder(Vivienda.Propia, -n_i))) +
  geom_bar(stat="identity", width=50, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Set2") +
  labs(title="",
       fill="¿Tiene vivienda propia?",
       x=NULL,
       y=NULL) +
  theme_void() +
  theme(legend.position="right") +
  # Agregar el porcentaje a cada categoría
  geom_text(aes(label = paste0(round(pct), "%")), 
            position = position_stack(vjust = 0.5))

Sect_Viv
