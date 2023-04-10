

#cargar los datos Cuanlitativos
df_Cl <- fread("Data/IEFIC_2017.csv") %>%
  select(P35,P3045,P2439,DEPARTAMENTO) %>%
  filter(DEPARTAMENTO <= 99) %>% 
  mutate(DEPARTAMENTO = as.character(DEPARTAMENTO), P35 = as.character(P35),
         P3045 = as.character(P3045), P2439 = as.character(P2439))

colnames(df_Cl) <- c("SEXO", "PER. PAG. ELECTRÓNICOS", "VIVIENDA PROPIA", "DEPARTAMENTO")
str(df_Cl)
# saveRDS(df_Cl, "Outputs/df_Cl.rds")

#seleccionar el diccionario para homologación
# setwd("/home/ds/Diego/ESP. ESTADÍSTICA/EST. UNIVARIADA/TRABAJO FINAL")
actividades <- read_excel("Data/Diccionario GEIH.xlsx", sheet = 3) %>%
  mutate(P6240 = as.character(P6240)) %>%
  data.frame()
str(actividades)
saveRDS(actividades, "Outputs/actividades.rds")



#Nota: Cunado se hacen uniones de tablas, las llaves deben estar como caracter
datos_agr <- dff_select %>%
  group_by(P6240) %>%
  summarise(Total = sum(DPTO, na.rm = T)) %>%
  arrange(desc(Total)) %>%
  mutate(P6240 = as.character(P6240)) %>%
  left_join(actividades, by = c("P6240"="P6240"))
str(datos_agr)
###############################################TABLA DE FRECUENCIAS PARA VARIABLE CUALITATIVA
#####################################################
###########################################################
####################################################################### 



#Tabla de frecuencias para datos cualitativos 
conteo_AFT <- dff_select %>%
  select(P6240) %>%
  mutate(P6240 = as.character(P6240)) %>%
  left_join(actividades, by = "P6240") %>%
  group_by(ACTIVIDAD) %>%
  summarise(n_i = n()) %>%
  mutate(N_i= cumsum(n_i),
         f_i=n_i/sum(n_i),   #n_i lotes por municipio
         F_i=cumsum(f_i))
str(conteo_AFT)

saveRDS(conteo_AFT, "Outputs/TDF_cuali.rds")
#tabla de frecuencias 

saveRDS(datos_agr, "outputs/conteo_AFT.rds")

conteo_AFT10 <- conteo_AFT %>%
  arrange(desc(n_i)) %>%
  slice(1:10)

conteoaf10 <- saveRDS(conteo_AFT10, "outputs/conteo_AF10.rds")

#lanzador de esquisse
#esquisser(view = "browser")


#grafico de barras para variables cualitativas actividad en la que ocupa el tiempo
Gbarras <- ggplot(conteo_AFT10) +
  aes(x = reorder(ACTIVIDAD, -n_i), y = n_i) +
  geom_col(fill = "#2CA279") +
  labs(x = "Ocupación del Tiempo", 
       y = "Frecuencia", title = "¿En que actividad ocupó la mayor parte del tiempo la semana pasada?", 
       subtitle = "Figura 1. Frecuencia de actividades en las que la pobalación Colombiana ocupa más el tiempo.") +
  coord_flip() +
  theme_minimal() +
  scale_y_reverse()

Gbarras
saveRDS(Gbarras, "outputs/graf_barr_cuali.rds")
#gráfico de sectores 

library(ggplot2)

GTgraf <- ggplot(conteo_AFT10, aes(x="", y=n_i, fill=reorder(ACTIVIDAD, -n_i))) +
  geom_bar(stat="identity", width=50, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Set2") +
  labs(title="Diagrama de Torta",
       fill="Actividades",
       x=NULL,
       y=NULL) +
  theme_void() +
  theme(legend.position="right")

GTgraf



# Calcular los porcentajes correspondientes
conteo_AFT10p <- conteo_AFT10 %>%
  mutate(pct = n_i / sum(n_i) * 100)
saveRDS(conteo_AFT10p, "outputs/conteo_AF10p.rds")
# Generar el gráfico de torta
GTgraf <- ggplot(conteo_AFT10p, aes(x="", y=n_i, fill=reorder(ACTIVIDAD, -n_i))) +
  geom_bar(stat="identity", width=50, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Set2") +
  labs(title="Diagrama de Torta",
       fill="Actividades",
       x=NULL,
       y=NULL) +
  theme_void() +
  theme(legend.position="right") +
  # Agregar el porcentaje a cada categoría
  geom_text(aes(label = paste0(round(pct), "%")), 
            position = position_stack(vjust = 0.5))

GTgraf
