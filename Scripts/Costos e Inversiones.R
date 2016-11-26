library(tidyverse)
library(readxl)
library(stringr)

Sys.setlocale("LC_ALL","es_ES.UTF-8")

Costos <- read_excel("Modelo/Intermodal/Costos e Inversiones.xlsx",sheet="Costos")

Costos <- Costos%>%
  gather(Year,Valor,-Infraestructura,-Costos.Operación,-Categoria)%>%
  select(-Categoria)%>%
  group_by(Year,Infraestructura,Costos.Operación)%>%
  summarise(Valor=sum(Valor))

Costos$Costos.Operación<-str_trim(Costos$Costos.Operación)

Costos <- Costos %>% spread(Costos.Operación,Valor)
