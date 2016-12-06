library(tidyverse)
library(readxl)
library(stringr)

Sys.setlocale("LC_ALL","es_ES.UTF-8")

Ingresos <- read_excel("Datos/Energia/Corredor Energia.xlsx",sheet="Ingresos")
Costos <- read_excel("Datos/Energia/Corredor Energia.xlsx",sheet="Costos")
Inversiones <- read_excel("Datos/Energia/Corredor Energia.xlsx",sheet="Inversiones")

Ingresos <- Ingresos%>%
  gather(Year,Valor,3:length(Ingresos))%>%
  rename(Infraestructura=Sistema)
Ingresos <- Ingresos%>%
  select(Year,Infraestructura,Ingresos.Operación,Valor)%>%
  rename(Ingresos.Operacion=Ingresos.Operación)

Costos <- Costos%>%
  rename(Costos.Operacion=Costos.Operación)%>%
  gather(Year,Valor,-Infraestructura,-Costos.Operacion,-Categoria)%>%
  select(-Categoria)%>%
  group_by(Year,Infraestructura,Costos.Operacion)%>%
  summarise(Valor=sum(Valor))

Costos$Costos.Operacion<-str_trim(Costos$Costos.Operacion)

Costos <- Costos%>% 
  spread(Costos.Operacion,Valor)%>%
  filter(!is.na(Mantenimiento))

Inversiones <- Inversiones%>%
  rename(Infraestructura=Sistema,Categoria=Categoría)%>%
  gather(Year,Valor,-Infraestructura,-Tipo,-Categoria,-Fase)%>%
  select(-Categoria,-Fase)%>%
  group_by(Year,Infraestructura,Tipo)%>%
  summarise(Valor=sum(Valor))

Inversiones <- Inversiones%>%
  spread(Tipo,Valor)

names(Inversiones)[3:4] <- c("Inversion.Infraestructura","Inversion.Superestructura")
