### En este script se arregla la informacion de demanda y tarifas a un formato de tabla long


# Demanda -----------------------------------------------------------------

library(tidyverse)
library(readxl)

excel_sheets("Datos/Intermodal/Ingresos.xlsx")

demanda <- read_excel("Datos/Intermodal/Ingresos.xlsx",sheet = "Demanda") %>%
  gather(Year,TEUS,-Escenario) %>%
  replace_na(demanda,replace = list(TEUS=0)) %>%
  spread(Escenario,TEUS)

names(demanda)[2] <- "Ramp.Up" # Utilizo un punto para hacer mas facil la referencia a esta columna



# Tarifas -----------------------------------------------------------------

tarifas <- read_excel("Datos/Intermodal/Ingresos.xlsx", sheet= "Tarifas") %>%
  gather(Year,Tarifa.dols, -TARIFAS)

names(tarifas)[1] <- "Sistema"
