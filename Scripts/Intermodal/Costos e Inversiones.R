library(tidyverse)
library(readxl)
library(stringr)

Sys.setlocale("LC_ALL","es_ES.UTF-8")

Costos <- read_excel("Datos/Intermodal/Costos e Inversiones.xlsx",sheet="Costos")

Costos <- Costos%>%
  gather(Year,Valor,-Infraestructura,-Costos.Operación,-Categoria)%>%
  select(-Categoria)%>%
  group_by(Year,Infraestructura,Costos.Operación)%>%
  summarise(Valor=sum(Valor))

Costos$Costos.Operación<-str_trim(Costos$Costos.Operación)

Costos <- Costos %>% spread(Costos.Operación,Valor)

Inversiones <- read_excel("Datos/Intermodal/Costos e Inversiones.xlsx",sheet="Inversiones")

Inversiones <- Inversiones%>%
  gather(Year, Valor,-Sistema,-Tipo,-Caterogoría,-Fase)%>%
  select(-Caterogoría,-Fase)%>%
  group_by(Sistema,Year,Tipo)%>%
  summarise(Valor=sum(Valor))%>%
  spread(Sistema,Valor)%>%
  mutate(Puertos = `San Luis`+`San Jorge`)%>%
  select(-`San Luis`,-`San Jorge`)%>%
  gather(Infraestructura,Valor,-Year,-Tipo)%>%
  spread(Tipo,Valor)

names(Inversiones)[3:4] <- c("Inversion.Infraestructura","Inversion.Superestructura")
  