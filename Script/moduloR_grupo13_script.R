#-----------------------------------PROYECTO FINAL-----------------------------
#INTEGRANTES: Lizeth Moreno, Carolina Briones, Tamara Quijano

#LIBRERIAS
library(readxl)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(dplyr)

#CARGAR DATOS
balances_2014 <- read_excel("Data/balances_2014.xlsx")
cias_codebook <- read_excel("Data/cias_codebook.xlsx")
ciiu <- read_excel("Data/ciiu.xlsx")
Formulario_102 <- read_excel("Data/Formulario 102.xls")

#Parte 1 - Data: En esta sección deberás describir y mostrar cómo está conformado el dataset balance_2014.xlsx.
str(balances_2014)
head(balances_2014)
#1.1 Utilizando los datos en el archivo llamado balance_2014.xlsx genera un tibble que denomines empresas y contenga las siguientes variables:
empresas<-tibble::as_tibble(balances_2014) 
empresas_1<- empresas %>%  mutate(LiquidezCorriente= (v345/v539), 
                                  EndeudamientoActivo= (v599/v499),
                                  EndeudamientoPatrimonial= (v599/v698),
                                  EndeudamientoActivoFijo= (v698/v498),
                                  Apalancamiento= (v499/v698))  %>%
                          select(Empresas=nombre_cia, 
                                 Status=situacion,
                                 TipoEmpresa = tipo, 
                                 Pais=pais, 
                                 Provincia=provincia, 
                                 Canton=canton, 
                                 Ciudad=ciudad,
                                 Actividad_economica=ciiu4_nivel1,
                                 SubActividad=ciiu4_nivel6,
                                 LiquidezCorriente,
                                 EndeudamientoActivo,
                                 EndeudamientoPatrimonial,
                                 EndeudamientoActivoFijo,
                                 Apalancamiento,
                                 fecha_const,
                                 tamanio,
                                 trabajadores_directo=trab_direc,
                                 trabajadores_administrativos=trab_admin)





#ANÁLISIS EXPLORATORIO DE DATOS
glimpse(empresas_1)
empresas_1$Status<- as.factor(empresas_1$Status) 
empresas_1$TipoEmpresa<- as.factor(empresas_1$TipoEmpresa)
empresas_1$Actividad_economica<- as.factor(empresas_1$Actividad_economica)
empresas_1$SubActividad<- as.factor(empresas_1$SubActividad)#convertir a factor


#quitar datos -Inf y +Inf
quitar_inf <- function(dataframe, columnas_numericas) {
  # Iterar sobre las columnas numéricas y reemplazar los -Inf y +Inf con NA
  for (col in columnas_numericas) {
    dataframe[[col]][dataframe[[col]] == -Inf | dataframe[[col]] == Inf] <- NA
  }
  
  return(dataframe)
}
columnas_a_limpiar <- c("LiquidezCorriente", "EndeudamientoActivo","EndeudamientoPatrimonial",
                        "EndeudamientoActivoFijo", "Apalancamiento")
empresas_2 <- quitar_inf(empresas_1, columnas_a_limpiar)

#borrar NAs
empresas_3 <- empresas_2 %>% drop_na()

#datos atípicos
# Función para imputar outliers
impute_outliers <- function(dataframe, variables) {
  for (var in variables) {
    dataframe[[var]][dataframe[[var]] < quantile(dataframe[[var]], 0.05, na.rm = TRUE)] <- mean(dataframe[[var]], na.rm = TRUE)
    dataframe[[var]][dataframe[[var]] > quantile(dataframe[[var]], 0.95, na.rm = TRUE)] <- median(dataframe[[var]], na.rm = TRUE)
  }
  return(dataframe)
}
columnas_a_imputar <- c("LiquidezCorriente", "EndeudamientoActivo","EndeudamientoPatrimonial",
                        "EndeudamientoActivoFijo", "Apalancamiento")

empresas_4 <- impute_outliers(empresas_3, columnas_a_imputar)
plot(empresas_4$EndeudamientoActivo) #verificar que las variables no tengan outliers en gráfica

#total empresas por actividad económica y por cantón
# Resumen del número total de empresas por actividad económica
resumen_actividad_economica <- empresas_4 %>%
  group_by(Actividad_economica) %>%
  summarise(Total_Empresas = n()) %>% View("resumen_actividad_economica")
# Resumen del número total de empresas por actividad económica y por cantón
resumen_actividad_canton <- empresas_4 %>%
  group_by(Actividad_economica, Canton) %>%
  summarise(Total_Empresas = n()) %>% View("resumen_actividad_canton")

#Graficar liquidez por status y provincia
#solo Status=ACTIVA
#liquidez
empresas_activa<- empresas_4 %>% filter(Status=="ACTIVA")
ggplot(empresas_activa, aes(x=fecha_const,  y = LiquidezCorriente, color= Provincia)) +
  geom_line() +
  labs(title = "Gráfico comparativo",
       subtitle = "Gráfico comparativo entre Liquidez Corriente y provincia")+
  facet_wrap(~Provincia)+
  theme_grey()+
  theme(legend.position = "none")
#solvencia
ggplot(empresas_activa, aes(x=fecha_const,  y = Apalancamiento, color= Provincia)) +
  geom_line() +
  labs(title = "Gráfico comparativo",
       subtitle = "Gráfico comparativo entre Solvencia y provincia")+
  facet_wrap(~Provincia)+
  theme_grey()+
  theme(legend.position = "none")


#Graficar liquidez por status y tipo de empresa
#liquidez
ggplot(empresas_activa, aes(x=fecha_const,  y = LiquidezCorriente, color= TipoEmpresa, group=TipoEmpresa)) +
  geom_line() +
  labs(title = "Gráfico comparativo",
       subtitle = "Gráfico comparativo entre Liquidez Corriente y Tipo de Empresa")+
  facet_wrap(~TipoEmpresa)+
  theme_grey()+
  theme(legend.position = "none")                            
#solvencia
ggplot(empresas_activa, aes(x=fecha_const,  y = Apalancamiento, color= TipoEmpresa, group=TipoEmpresa)) +
  geom_line() +
  labs(title = "Gráfico comparativo",
       subtitle = "Gráfico comparativo entre Liquidez Corriente y Tipo de Empresa")+
  facet_wrap(~TipoEmpresa)+
  theme_grey()+
  theme(legend.position = "none")   


#Preguntas de Investigación
#¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?
pregunta1 <- empresas_4 %>%
  group_by(tamanio) %>%
  summarise(Promedio_Endeudamiento = mean(EndeudamientoActivo, na.rm = FALSE)) #no ignora na porque no hay
pregunta1
grafico_endeudamiento <- ggplot(pregunta1, aes(x = tamanio, y = Promedio_Endeudamiento, fill = tamanio)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Promedio del Endeudamiento Activo por Tamaño de Empresa",
       x = "Tamaño de Empresa",
       y = "Promedio del Endeudamiento Activo") +
  theme_minimal()
grafico_endeudamiento
#¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más de
#60 trabajadores directos y que cuenta con 100 a 800 trabajadores administrativos?
empresas_filtradas <- empresas_4 %>%
  filter(trabajadores_administrativos >= 100, trabajadores_administrativos <= 800,trabajadores_directo > 60) # Filtrar las empresas que tienen más de 60 trabajadores directos y que cuentan con 100 a 800 trabajadores administrativos
empresas_filtradas
pregunta2 <- empresas_filtradas %>%
  group_by(TipoEmpresa) %>%
  summarise(Promedio_Liquidez = mean(LiquidezCorriente, na.rm = FALSE))#liquidez por tipo de empresa
pregunta2
grafico_trabajadores <- ggplot(pregunta2, aes(x = TipoEmpresa, y = Promedio_Liquidez, fill = TipoEmpresa)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Promedio de Liquidez por Tipo de Empresa",
       x = "Tipo de Empresa",
       y = "Promedio de Liquidez") +
  theme_minimal()
grafico_trabajadores
#Describe el top 10 de empresas con mayor apalancamiento.
top_empresas_apalancamiento <- empresas_4 %>%
  arrange(desc(Apalancamiento)) %>%
  head(10) 
top_empresas_apalancamiento
grafico_apalancamiento <- ggplot(top_empresas_apalancamiento, aes(x = Empresas, y = Apalancamiento, fill = Empresas)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Empresas con Mayor Apalancamiento",
       x = "Empresas",
       y = "Apalancamiento") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Empresas")
grafico_apalancamiento
