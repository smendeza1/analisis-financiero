### En este script se arregla la informacion de demanda y tarifas a un formato de tabla long
library(tidyverse)
library(readxl)

excel_sheets("Modelo/Intermodal/Ingresos.xlsx")

demanda <- read_excel("Modelo/Intermodal/Ingresos.xlsx",sheet = "Demanda") %>%
  gather(Year,TEUS,-Escenario) %>%
  replace_na(demanda,replace = list(TEUS=0)) %>%
  spread(Escenario,TEUS)

names(demanda)[2] <- "Ramp.Up" # Utilizo un punto para hacer mas facil la referencia a esta columna

tarifas <- read_excel("Modelo/Intermodal/Ingresos.xlsx", sheet= "Tarifas")
