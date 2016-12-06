library(tidyverse)
library(readxl)
library(stringr)

Sys.setlocale("LC_ALL","es_ES.UTF-8")

Ingresos <- read_excel("Datos/Energia/Corredor Energia.xlsx",sheet="Ingresos")
Costos <- read_excel("Datos/Energia/Corredor Energia.xlsx",sheet="Costos")
Inversiones <- read_excel("Datos/Energia/Corredor Energia.xlsx",sheet="Inversiones")

Ingresos <- Ingresos%>%
  gather(Year,Ingresos.Brutos,3:length(Ingresos))%>%
  select(-Ingresos.Operación)


Costos <- Costos%>%
  gather(Year,Valor,-Infraestructura,-Costos.Operacion,-Categoria)%>%
  select(-Categoria)%>%
  group_by(Year,Infraestructura,Costos.Operacion)%>%
  summarise(Valor=sum(Valor))%>%
  filter(!is.na(Valor))


Costos <- Costos%>% 
  spread(Costos.Operacion,Valor)

Inversiones <- Inversiones%>%
  gather(Year,Valor,-Infraestructura,-Tipo,-Categoria,-Fase,-Sistema)%>%
  select(-Categoria,-Fase)%>%
  group_by(Year,Infraestructura,Tipo)%>%
  summarise(Valor=sum(Valor))

Inversiones <- Inversiones%>%
  spread(Tipo,Valor)

names(Inversiones)[3:4] <- c("Inversion.Infraestructura","Inversion.Superestructura")

