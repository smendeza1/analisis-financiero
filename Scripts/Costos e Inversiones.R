library(tidyverse)
library(readxl)
library(stringr)

Sys.setlocale("LC_ALL","es_ES.UTF-8")

Costos <- read_excel("Modelo/Intermodal/Costos e Inversiones.xlsx",sheet="Costos")

Costos <- Costos%>%
  gather(Year,Valor,-Infraestructura,-Costos.Operacion,-Categoria)%>%
  select(-Categoria)%>%
  group_by(Year,Infraestructura,Costos.Operacion)%>%
  summarise(Valor=sum(Valor))

Costos$Costos.Operacion<-str_trim(Costos$Costos.Operacion)

Costos <- Costos %>% spread(Costos.Operacion,Valor)

Inversiones <- read_excel("Modelo/Intermodal/Costos e Inversiones.xlsx",sheet="Inversiones")

Inversiones <- Inversiones%>%
  gather(Year, Valor,-Sistema,-Tipo,-Categoria,-Fase)%>%
  select(-Categoria,-Fase)%>%
  group_by(Sistema,Year,Tipo)%>%
  summarise(Valor=sum(Valor))%>%
  spread(Sistema,Valor)%>%
  mutate(Puertos = `San Luis`+`San Jorge`)%>%
  select(-`San Luis`,-`San Jorge`)%>%
  gather(Infraestructura,Valor,-Year,-Tipo)%>%
  spread(Tipo,Valor)

names(Inversiones)[3:4] <- c("Inversion.Infraestructura","Inversion.Superestructura")
  