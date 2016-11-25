library(tidyverse)
library(readxl)

Sys.setlocale("LC_ALL","es_ES.UTF-8")

Costos <- read_excel("Modelo/Intermodal/Costos e Inversiones.xlsx",sheet="Costos")

Costos <- Costos%>%
  gather(Year,Valor,-Infraestructura,-`Costos Operación`)%>%
  spread(`Costos Operación`,Valor)