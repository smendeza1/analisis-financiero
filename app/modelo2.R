# # Información utilizada por la función para poder funcionar
source("app/famortizacion.R") # funciones para calculos financieros
Demanda <- read_excel("app/Datos/Intermodal/Ingresos.xlsx", sheet = "Demanda")
Tarifas <- read_excel("app/Datos/Intermodal/Ingresos.xlsx", sheet = "Tarifas")
Costos <- read_excel("app/Datos/Intermodal/Costos e Inversiones.xlsx", sheet = "Costos")
Inversiones <- read_excel("app/Datos/Intermodal/Costos e Inversiones.xlsx", sheet = "Inversiones")
Ingresos.poliducto <- read_excel("app/Datos/Intermodal/Ingresos.xlsx", sheet = "Ingresos")

require(shiny)
require(tidyverse)
require(readxl)
require(stringr)
require(FinCal)
require(scales)



# ##Datos para realizar modificaciones a la función
n=30
r=0.05 ## Regimen sobre ingresos
r2=0.25 ## Regimen sobre utilidades
tasa.descuento = 0.08
pct.financiado=0.6
tipo.demanda="min"
demanda=demanda2
tarifas=tarifas2
Costos=Costos2
Inversiones=Inversiones2
isr=0.07
isr2=0.25
horizonte=2080
regimen.fiscal = FALSE ## en donde FALSE indica regimen sobre ingresos
split.puertos  = 0.5
pct.royalty = 0.05


# Leyendas data frame -----------------------------------------------------

## ___@@@xx <- leyenda primaria
## por ejemplo ingresos.M11 se refiere a los ingresos para la version de
## inversion en infra y super por parte de sistema

## ___ <- Es el nombre del df principal
## @@ <- Se refiere a la parte evaluada, esta puede ser Sistema o Elemento
## xx <- La primera x indica si sistema realiza inversion en infra y la segunda
## si la realiza en super

### Combinaciones de escenarios de negociación
## 11 <- inversion en infra y superestructura por parte de sistema
## 10 <- inversion en infra por parte de sistema y super por un tercero
## 00 <- inversion en infra y super por parte de un tercero

### Sistemas
## M = multimodal completo
## E = energia completo
### Completo
## Sistema =  multimodal + energia
### Elementos
## SJ = san jorge
## SL = san luis
## F = ferrocarril
### Tipos de negociacion
## SJf = san jorge + 1/2 ferrocarril
## SLF = san luis + 1/2 ferrocarril


# Demanda -----------------------------------------------------------------

  
Demanda <-   Demanda %>%
  gather(Año,TEUS,-Escenario) %>%
  replace_na(demanda,replace = list(TEUS = 0)) %>%
  spread(Escenario,TEUS)

names(Demanda)[2] <- "Ramp.Up" # Utilizo un punto en el nombre de la columna para hacer mas facil la referencia a esta columna


# Tarifas -----------------------------------------------------------------

Tarifas <- Tarifas %>%
  gather(Año,Tarifa.dols, -Sistema,-Elemento) %>%
  filter(!is.na(Tarifa.dols))



# Ingresos ----------------------------------------------------------------
# En el caso del ferrocarril, se asume que el 100% de los TEUS disponibles seran movilizados
Tarifas$pct.mercado <- ifelse(Tarifas$Elemento != "Ferrocarril",split.puertos,1)
Tarifas$pct.mercado[Tarifas$Elemento == "San Jorge"] <- 1 - split.puertos


# Tabla principal para modelar los ingresos
Ingresos <- left_join(Tarifas, Demanda) %>%
  mutate(Año = as.numeric(Año))


# Seleccion de tipo de demanda para modelar los ingresos del sistema y elementos multimodales

if (tipo.demanda == "min") {
  Ingresos <- Ingresos %>%
    mutate(Ingresos.Brutos = if_else(Elemento != "Ferrocarril", 
                                     Tarifa.dols*pct.mercado*Ramp.Up*MIN*2, 
                                     Tarifa.dols*pct.mercado*Ramp.Up*MIN),
           Mercado = pct.mercado*Ramp.Up*MIN,
           Royalty = Ingresos.Brutos*pct.royalty) 
  
}else{
  Ingresos <- Ingresos %>%
    mutate(Ingresos.Brutos = if_else(Elemento != "Ferrocarril",
                                     Tarifa.dols*pct.mercado*Ramp.Up*MAX*2,
                                     Tarifa.dols*pct.mercado*Ramp.Up*MAX),
           Mercado = pct.mercado*Ramp.Up*MAX,
           Royalty = Ingresos.Brutos*pct.royalty) 
  
}

Ingresos <- Ingresos %>%
  mutate(Elemento = as.factor(Elemento),
         Sistema = as.factor(Sistema))


## Funcion que sirve para obtener los data frame para los distintos escenarios de negociación
segmentar_inversion <- function(df = Inversiones, type) {
  require(dplyr)
  require(purrr)
  
  switch(type,
         SF = df %>% 
           mutate(Sistema = as.factor(Sistema)) %>%
           split(df$Sistema) %>%
           map(gather, Año, Valor, -c(Sistema:Fase)) %>%
           map(mutate, Año = as.factor(Año)) %>%
           map(group_by, Año) %>%
           map(summarise, Inversion = sum(Valor)),
         ST = df %>% 
           mutate(Sistema = as.factor(Sistema)) %>%
           split(df$Sistema) %>%
           map(gather, Año, Valor, -c(Sistema:Fase)) %>%
           map(mutate, Año = as.factor(Año)) %>%
           map(group_by, Año, Componente) %>%
           map(summarise, Inversion = sum(Valor)),
         EF = df %>% 
           mutate(Elemento = as.factor(Elemento)) %>%
           split(df$Elemento) %>%
           map(gather, Año, Valor, -c(Sistema:Fase)) %>%
           map(mutate, Año = as.factor(Año)) %>%
           map(group_by, Año) %>%
           map(summarise, Inversion = sum(Valor)),
         ET = df %>% 
           mutate(Elemento = as.factor(Elemento)) %>%
           split(df$Elemento) %>%
           map(gather, Año, Valor, -c(Sistema:Fase)) %>%
           map(mutate, Año = as.factor(Año)) %>%
           map(group_by, Año, Componente) %>%
           map(summarise, Inversion = sum(Valor)))
  
}
inversiones.sistema11 <- segmentar_inversion(Inversiones, type = "SF")
inversiones.sistema10 <- segmentar_inversion(Inversiones, type = "ST")
inversiones.elemento11 <- segmentar_inversion(Inversiones, type = "EF")
inversiones.elemento10 <- segmentar_inversion(Inversiones, type = "ET")

## Funcion que sirve para calcular los anticipos y las anualidades de cada canon
## en donde canon puede referirse al valor de renta de infraestructura o concesión del corredor
canon.infraestructura <- function(df, 
                                  tasa.descuento.ice = 0.04, 
                                  n.canon = 30, 
                                  pct.pago.anticipado = 0.1){
  
  inversion.tmp <- npv(r = tasa.descuento.ice, 
                       c(0,df$Infraestructura))
  
  pago.anticipado <-  -pmt(r = tasa.descuento.ice,
                                   n = n.canon,
                                   pv = inversion.tmp,
                                   fv = 0)*1.05*n.canon*pct.pago.anticipado
  
  inversion.tmp <- inversion.tmp*(1-pct.pago.anticipado)
  
  pago <- -pmt(r = tasa.descuento.ice,
       n = n.canon,
       pv = inversion.tmp,
       fv = 0)*1.05

  
  res <- list(pago = pago, pago.anticipado = pago.anticipado)
  res
  }

canon.concesion <- function(valor = 5500000,
                            tasa.descuento.ice = 0.04,
                            n.canon = 30,
                            pct.pago.anticipado = 0.1){
  
  
  pago.anticipado <- -pmt(r = tasa.descuento.ice,
               n = n.canon,
               pv = valor,
               fv = 0)*n.canon*pct.pago.anticipado

  valor <- valor *(1-pct.pago.anticipado)
  pago <- -pmt(r = tasa.descuento.ice,
               n = n.canon,
               pv = valor,
               fv = 0)
  res <- list(pago = pago, pago.anticipado = pago.anticipado)
  res
}



valor.canon.infraestructura.elemento <- map(inversiones.elemento10, 
                                            spread, 
                                            Componente, 
                                            Inversion) %>% 
  map(canon.infraestructura)


valor.canon.infraestructura.sistema <- map(inversiones.sistema10, 
                                            spread, 
                                            Componente, 
                                            Inversion) %>% 
  map(canon.infraestructura)



Ingresos

# 
# tmp <- Ingresos %>% mutate(Año = factor(Año)) %>% left_join(Ingresos.poliducto)
# 
# Ingresos.M11 <- Ingresos %>%
#   group_by(Año) %>%
#   summarise(Ingresos.Brutos = sum(Ingresos.Brutos))
# 
# Ingresos.M10 <- Ingresos %>%
#   group_by(Año) %>%
#   summarise(Ingresos.Brutos = sum(Ingresos.Brutos),
#             Royalty = sum(Royalty)) %>%
#   mutate(Royalty = lag(Royalty)) ## Se debe de agregar los canones por el valor de la infraestructura y de la tierra
#   
# 
# Ingresos.M10 %>% mutate(Royalty = lag(Royalty))
# 
# Ingresos.M10 <- Ingresos %>%
#   group_by(Año) %>%
#   summarise(Ingresos.Brutos = sum(Ingresos.Brutos),
#             Royalty = sum(Royalty)) ## Se debe de agregar los canones por el valor de la tierra
# 
# Ingresos.poliducto <- Ingresos.poliducto %>%
#   gather(Año, Valor, -Sistema,-Elemento,-Categoria.Ingresos ) %>%
#   mutate(Año = as.factor(Año))
# 
# Ingresos.E11 <- Ingresos.poliducto %>%
#   group_by(Año) %>%
#   summarise(Ingresos.Brutos = sum(Valor))
# # 
# # tmp <- split(Ingresos,Ingresos$Elemento)
# # 
# # tmp$
  