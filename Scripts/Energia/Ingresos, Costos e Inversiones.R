library(tidyverse)
library(readxl)
library(stringr)

Sys.setlocale("LC_ALL","es_ES.UTF-8")

Ingresos <- read_excel("Datos/Energia/Corredor Energia.xlsx",sheet="Ingresos")
Costos <- read_excel("Datos/Energia/Corredor Energia.xlsx",sheet="Costos")
Inversiones <- read_excel("Datos/Energia/Corredor Energia.xlsx",sheet="Inversiones")

Ingresos <- Ingresos%>%
  gather(Year,Valor,3:length(Ingresos))
Ingresos <- Ingresos%>%
  select(Year,Ingresos.Operación,Valor)
  

  