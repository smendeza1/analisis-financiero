# # Información utilizada por la función para poder funcionar
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
n                 = 30
r                 = 0.05 ## Regimen sobre ingresos
r2                = 0.25 ## Regimen sobre utilidades
tasa.descuento    = 0.08
pct.financiado    = 0.6
tipo.demanda      = "min"
isr               = 0.07
isr2              = 0.25
horizonte         = 2080
regimen.fiscal    = FALSE ## en donde FALSE indica regimen sobre ingresos
split.puertos     = 0.5
pct.royalty       = 0.05


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
  mutate(Año = as.factor(Año))


# Seleccion de tipo de demanda para modelar los ingresos del sistema y elementos multimodales

if (tipo.demanda == "min") {
  Ingresos <- Ingresos %>%
    mutate(Ingresos.Brutos = if_else(Elemento != "Ferrocarril", 
                                     Tarifa.dols*pct.mercado*Ramp.Up*MIN*2, 
                                     Tarifa.dols*pct.mercado*Ramp.Up*MIN),
           Mercado = pct.mercado*Ramp.Up*MIN,
           Royalty = Ingresos.Brutos*pct.royalty,
           IVAxPagar = Ingresos.Brutos*0.12) 
  
}else{
  Ingresos <- Ingresos %>%
    mutate(Ingresos.Brutos = if_else(Elemento != "Ferrocarril",
                                     Tarifa.dols*pct.mercado*Ramp.Up*MAX*2,
                                     Tarifa.dols*pct.mercado*Ramp.Up*MAX),
           Mercado = pct.mercado*Ramp.Up*MAX,
           Royalty = Ingresos.Brutos*pct.royalty,
           IVAxPagar = Ingresos.Brutos*0.12) 
  
}

Ingresos <- Ingresos %>%
  mutate(Elemento = as.factor(Elemento),
         Sistema = as.factor(Sistema)) %>%
  select(Sistema, Elemento, Año, Ingresos.Brutos, Royalty, IVAxPagar)


 Ingresos.poliducto <- Ingresos.poliducto %>%
   gather(Año, Ingresos.Brutos, -Sistema,-Elemento ) %>%
   mutate(Año = as.factor(Año),
          Sistema = as.factor(Sistema),
          Elemento = as.factor(Elemento),
          Royalty = Ingresos.Brutos*pct.royalty,
          IVAxPagar = Ingresos.Brutos*0.12)

Ingresos <- Ingresos %>% 
  bind_rows(Ingresos.poliducto) %>%
  mutate(Año = as.factor(Año),
         Sistema = as.factor(Sistema),
         Elemento = as.factor(Elemento))


segmentar_ingreso <- function(df = Ingresos, type = "Sistema"){
  
  switch(type,
  Sistema = Ingresos %>%
    split(Ingresos$Sistema) %>%
    map(group_by, Año) %>%
    map(select, -Elemento, -Sistema) %>%
    map(summarise_all, sum) %>% 
    map(mutate, ISR = Ingresos.Brutos*isr),
  Elemento = Ingresos %>%
    split(Ingresos$Elemento) %>%
    map(group_by, Año) %>%
    map(select, -Elemento, -Sistema) %>%
    map(summarise_all, sum)%>% 
    map(mutate, ISR = Ingresos.Brutos*isr))
  }
  
ingresos.sistema11 <- segmentar_ingreso(type = "Sistema")
ingresos.elemento11 <- segmentar_ingreso(type = "Elemento")

# Inversiones y canones de infraestructura y concesión -------------------------------------------------------------


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
           map(summarise, Inversion = sum(Valor)) %>% 
           map(mutate, IVAxCobrar = Inversion*0.12),
         ST = df %>% 
           mutate(Sistema = as.factor(Sistema)) %>%
           split(df$Sistema) %>%
           map(gather, Año, Valor, -c(Sistema:Fase)) %>%
           map(mutate, Año = as.factor(Año)) %>%
           map(group_by, Año, Componente) %>%
           map(summarise, Inversion = sum(Valor)) %>% 
           map(mutate, IVAxCobrar = Inversion*0.12),
         EF = df %>% 
           mutate(Elemento = as.factor(Elemento)) %>%
           split(df$Elemento) %>%
           map(gather, Año, Valor, -c(Sistema:Fase)) %>%
           map(mutate, Año = as.factor(Año)) %>%
           map(group_by, Año) %>%
           map(summarise, Inversion = sum(Valor)) %>% 
           map(mutate, IVAxCobrar = Inversion*0.12),
         ET = df %>% 
           mutate(Elemento = as.factor(Elemento)) %>%
           split(df$Elemento) %>%
           map(gather, Año, Valor, -c(Sistema:Fase)) %>%
           map(mutate, Año = as.factor(Año)) %>%
           map(group_by, Año, Componente) %>%
           map(summarise, Inversion = sum(Valor)) %>% 
           map(mutate, IVAxCobrar = Inversion*0.12))
  
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
                                  pct.pago.anticipado = 0.1,
                                  año.primer.ingreso = 2018){
  
  
  res <- data.frame(Año = numeric(n.canon + 1),
                    Canon.infr = numeric(n.canon + 1))
  
  res[1] <- año.primer.ingreso - 3
  res[1] <- seq_len(n.canon + 1) + res[1] - 1
  
  inversion.tmp <- npv(r = tasa.descuento.ice, 
                       c(0,df$Infraestructura))
  
  pago.anticipado <-  -pmt(r = tasa.descuento.ice,
                                   n = n.canon,
                                   pv = inversion.tmp,
                                   fv = 0)*1.05*n.canon*pct.pago.anticipado
  
  res[1,2] <- pago.anticipado
  
  inversion.tmp <- inversion.tmp*(1 - pct.pago.anticipado)
  
  pago <- -pmt(r = tasa.descuento.ice,
       n = n.canon,
       pv = inversion.tmp,
       fv = 0)*1.05
  
  res[2:nrow(res),2] <- pago
  res$Año <- as.factor(res$Año)
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

## estos valores de canon me van a servir para los escenario 10 y 00

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


# Costos ------------------------------------------------------------------

Costos
segmentar_costos <- function(df = Costos, type) {
  require(dplyr)
  require(purrr)

  switch(type,
         SF = df %>% 
           mutate(Sistema = as.factor(Sistema)) %>%
           split(df$Sistema) %>%
           map(gather, Año, Valor, -c(Sistema:Componente)) %>%
           map(mutate, Año = as.factor(Año)) %>%
           map(group_by, Año) %>%
           map(summarise, Costos = sum(Valor)) %>% 
           map(mutate, IVAxCobrar = Costos*0.12),
         ST = df %>% 
           mutate(Sistema = as.factor(Sistema)) %>%
           split(df$Sistema) %>%
           map(gather, Año, Valor, -c(Sistema:Componente)) %>%
           map(mutate, Año = as.factor(Año)) %>%
           map(group_by, Año, Componente) %>%
           map(summarise, Costos = sum(Valor)) %>% 
           map(mutate, IVAxCobrar = Costos*0.12),
         EF = df %>% 
           mutate(Elemento = as.factor(Elemento)) %>%
           split(df$Elemento) %>%
           map(gather, Año, Valor, -c(Sistema:Componente)) %>%
           map(mutate, Año = as.factor(Año)) %>%
           map(group_by, Año) %>%
           map(summarise, Costos = sum(Valor)) %>% 
           map(mutate, IVAxCobrar = Costos*0.12),
         ET = df %>% 
           mutate(Elemento = as.factor(Elemento)) %>%
           split(df$Elemento) %>%
           map(gather, Año, Valor, -c(Sistema:Componente)) %>%
           map(mutate, Año = as.factor(Año)) %>%
           map(group_by, Año, Componente) %>%
           map(summarise, Costos = sum(Valor)) %>% 
           map(mutate, IVAxCobrar = Costos*0.12))
  
}

costos.sistema11 <- segmentar_costos(type = "SF")
costos.sistema10 <- segmentar_costos(type = "ST")
costos.elemento11 <- segmentar_costos(type = "EF")
costos.elemento10 <- segmentar_costos(type = "ET")
# IVA e ISR ---------------------------------------------------------------------

Impuestos11 <- map2(ingresos.sistema11,inversiones.sistema11,left_join) %>%
  map2(costos.sistema11, left_join, by = "Año") %>%
  map(mutate, IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y) %>%
  map(select, -Inversion, -Royalty, -Ingresos.Brutos, -IVAxCobrar.x, -IVAxCobrar.y, -Costos ) %>% 
  map(mutate, IVA.aux1 = cumsum(IVAxCobrar - IVAxPagar),
              IVA.aux2 = IVAxCobrar - IVAxPagar,
              IVA.Neto = if_else(lag(IVA.aux1) < 0 ,IVA.aux2, IVA.aux1)) %>% 
  map(select, -contains("aux"))



# Financiamiento ----------------------------------------------------------

df <- inversiones.sistema11 %>%
  map2(ingresos_sistema11,left_join)
df <- df$Multimodal

## aqui empezaria la funcion

amortizacion <- function(df,pct.financiado = 1, n = 30, r = 0.07 ){
require(tidyverse)
    
n.inversiones <- df %>% filter(Inversion != 0) %>% nrow()

primer.ingreso <- filter(df, Ingresos.Brutos != 0)
primer.ingreso <- primer.ingreso$Año %>%  as.character() %>% as.numeric() %>% min()

df <- df %>% filter(Inversion != 0)
inversion.input <- df[1,]
inversion.input$Inversion <- inversion.input$Inversion*pct.financiado
año.inversion <- inversion.input$Año %>% as.character() %>% as.numeric()

### Definicion periodo de gracia
if (año.inversion < primer.ingreso ) {
  pg = primer.ingreso - año.inversion
}else{
  pg = 0 
}

# creacion de escenario inversión -----------------------------------------
## al momento de crear el escenario sumo n+pg por el periodo de gracia de cada pago
escenario <- data_frame(Año = numeric(n + pg), 
                        Saldo = numeric(n + pg),
                        Intereses = numeric(n + pg),
                        Amortizacion = numeric(n + pg))
escenario$Año <- seq_len(n + pg) + año.inversion - 1 # Llenado de los años correspondientes al escenario
escenario$Amortizacion <- inversion.input$Inversion/n # Calculo de los pagos de amortización anuales
 

if (pg == 0) {
  
  escenario <- escenario %>% 
    mutate(Saldo = inversion.input$Inversion - cumsum(escenario$Amortizacion),
           Intereses = (Saldo + Amortizacion)*r)
  
}else {
  escenario$Amortizacion[1:pg] <- 0 # Set de periodo de gracia
  escenario <- escenario %>% 
    mutate(Saldo = inversion.input$Inversion - cumsum(escenario$Amortizacion),
           Intereses = (Saldo + Amortizacion)*r)
}


if(exists("Componente",df)) {
  escenario  <- escenario %>% mutate(Pago = Intereses + Amortizacion)
  escenario$Componente <- inversion.input$Componente
} else {
  escenario  <- escenario %>% mutate(Pago = Intereses + Amortizacion)
}

for (i in 2:n.inversiones) {
  inversion.input <- df[i,]
  inversion.input$Inversion <- inversion.input$Inversion*pct.financiado
  año.inversion <- inversion.input$Año %>% as.character() %>% as.numeric()
  
  ### Definicion periodo de gracia
  if (año.inversion < primer.ingreso ) {
    pg = primer.ingreso - año.inversion
  }else{
    pg = 0 
  }
  
  # creacion de escenario inversión -----------------------------------------
  ## al momento de crear el escenario sumo n+pg por el periodo de gracia de cada pago
  tmp <- data_frame(Año = numeric(n + pg), 
                          Saldo = numeric(n + pg),
                          Intereses = numeric(n + pg),
                          Amortizacion = numeric(n + pg))
  tmp$Año <- seq_len(n + pg) + año.inversion - 1 # Llenado de los años correspondientes al escenario
  tmp$Amortizacion <- inversion.input$Inversion/n # Calculo de los pagos de amortización anuales
  
  
  if (pg == 0) {
    
    tmp <- tmp %>% 
      mutate(Saldo = inversion.input$Inversion - cumsum(tmp$Amortizacion),
             Intereses = (Saldo + Amortizacion)*r)
    
  }else {
    tmp$Amortizacion[1:pg] <- 0 # Set de periodo de gracia
    tmp <- tmp %>% 
      mutate(Saldo = inversion.input$Inversion - cumsum(tmp$Amortizacion),
             Intereses = (Saldo + Amortizacion)*r)
  }
  
  if (exists("Componente",df)) {
    tmp  <- tmp %>% mutate(Pago = Intereses + Amortizacion)
    tmp$Componente <- inversion.input$Componente
  } else {
    tmp  <- tmp %>% mutate(Pago = Intereses + Amortizacion)
  }
  
  escenario <- rbind(escenario, tmp)
}

if (exists("Componente",df)) {
  escenario <- escenario %>% 
    group_by(Año, Componente) %>% 
    summarise_all(sum)
  escenario 
}else{
  escenario <- escenario %>% 
    group_by(Año) %>% 
    summarise_all(sum)
  escenario$Año <- as.factor(escenario$Año)
  escenario
}
escenario$Año <- as.factor(escenario$Año)
escenario
}

financiamiento.sistema11 <- inversiones.sistema11 %>% 
  map2(ingresos.sistema11, left_join) %>% 
  map(amortizacion,pct.financiado = pct.financiado, n = n, r = r)




inversiones.sistema11 <- inversiones.sistema11 %>% 
  map( function(df) df %>% mutate(Inversion = Inversion * (1 - pct.financiado)))

df <- ingresos.sistema11 %>%
  map2(costos.sistema11, full_join) %>%
  map(select, -contains("IVA")) %>% 
  map2(inversiones.sistema11, full_join) %>% 
  map(select, -contains("IVA")) %>% 
  map2(financiamiento.sistema11, full_join) %>%
  map(select, -Saldo, -Intereses, -Amortizacion) %>% 
  map2(Impuestos11, full_join) %>% 
  map(select, -contains("IVAx")) %>% 
  map(replace_na, replace = list(Ingresos.Brutos = 0,
                                 Royalty = 0,
                                 ISR = 0,
                                 Costos = 0,
                                 Inversion = 0,
                                 Pago = 0,
                                 IVA.Neto = 0)) 

df <- df$Multimodal

# calcular_fen <- function(df,r,)
  
df <-  df %>% 
  mutate(IVA.Neto = if_else(IVA.Neto > 0,0,IVA.Neto),
         FEN      =  Ingresos.Brutos
         - ISR
         - Costos
         - Inversion
         - Pago
         - IVA.Neto)

npv(r = r, cf = df$FEN)
irr2(df$FEN)
