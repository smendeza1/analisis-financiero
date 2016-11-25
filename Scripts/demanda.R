### En este script se arregla la informacion de demanda a un formato de tabla long

library(tidyverse)
library(readxl)

excel_sheets("Modelo/Intermodal/Ingresos.xlsx")

demanda <- read_excel("Modelo/Intermodal/Ingresos.xlsx",sheet = "Demanda") %>%
  gather(Year,TEUS,-Escenario) %>%
  replace_na(demanda,replace = list(TEUS=0)) %>%
  spread(Escenario,TEUS)

names(demanda)[2] <- "Ramp.Up"
