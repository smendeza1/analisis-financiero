
require(shiny)
require(tidyverse)
require(readxl)
require(stringr)
require(FinCal)
require(scales)
require(purrr)
# # Información utilizada por la función para poder funcionar
Demanda <- read_excel("app/Datos/Intermodal/Ingresos.xlsx", sheet = "Demanda")
Tarifas <- read_excel("app/Datos/Intermodal/Ingresos.xlsx", sheet = "Tarifas")
Costos <- read_excel("app/Datos/Intermodal/Costos e Inversiones.xlsx", sheet = "Costos")
Inversiones <- read_excel("app/Datos/Intermodal/Costos e Inversiones.xlsx", sheet = "Inversiones")
Ingresos.poliducto <- read_excel("app/Datos/Intermodal/Ingresos.xlsx", sheet = "Ingresos")


# ##Datos para realizar modificaciones a la función
n                 = 30 # Tiempo financiamiento
n.concesion.multi = 50
n.concesion.poli  = 30
r                 = 0.05 ## Tasa financiamiento
r.ice             = 0.04 ## Tasa Canon Concesión
tasa.retorno.manual = FALSE
tasa.descuento    = 0.08
pct.financiado    = 0.6
tipo.demanda      = "min"
isr               = 0.07
isr2              = 0.25
horizonte         = 2062
regimen.fiscal    = TRUE ## en donde FALSE indica regimen sobre ingresos
split.puertos     = 0.5
pct.royalty       = 0.05
pct.gastos.comercializacion = 0.025


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



# modelo.base <- function(n                 = 30,
#                         r                 = 0.05, ## Tasa financiamiento
#                         tasa.descuento    = 0.08,
#                         pct.financiado    = 0.6,
#                         tipo.demanda      = "min",
#                         isr               = 0.07,
#                         isr2              = 0.25,
#                         horizonte         = 2062,
#                         regimen.fiscal    = TRUE, ## en donde FALSE indica regimen sobre ingresos
#                         split.puertos     = 0.5,
#                         pct.royalty       = 0.05,
#                         pct.gastos.comercializacion = 0.025,
#                         tasa.retorno.manual = FALSE) {
#   

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
      mutate( Mercado = round(pct.mercado*Ramp.Up*MIN,0),
              Mercado = if_else(Elemento == "Ferrocarril", 
                                lag(Mercado,1) + lag(Mercado,2),
                                Mercado),
              Ingresos.Brutos = if_else(Elemento != "Ferrocarril", 
                                       Tarifa.dols*Mercado*2, 
                                       Tarifa.dols*Mercado),
             Royalty = Ingresos.Brutos*pct.royalty,
             IVAxPagar = Ingresos.Brutos*0.12) 
    
  }else{
    Ingresos <- Ingresos %>%
      mutate( Mercado = round(pct.mercado*Ramp.Up*MAX,0),
              Mercado = if_else(Elemento == "Ferrocarril", 
                                lag(Mercado,1) + lag(Mercado,2),
                                Mercado),
             Ingresos.Brutos = if_else(Elemento != "Ferrocarril",
                                       Tarifa.dols*Mercado*2,
                                       Tarifa.dols*Mercado),
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
      map(select, -Elemento, -Sistema) %>%
      map(group_by, Año) %>%
      map(summarise_all, sum) %>% 
      map(mutate, ISR = Ingresos.Brutos*isr),
    Elemento = Ingresos %>%
      split(Ingresos$Elemento) %>%
      map(select, -Elemento, -Sistema) %>%
      map(group_by, Año) %>%
      map(summarise_all, sum) %>% 
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
  ## en donde el primer canon se refiere al valor de renta de infraestructura  y
  ## el segundo canon al valor de la concesión del corredor
  
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
  
    valor <- valor *(1 - pct.pago.anticipado)
    pago <- -pmt(r = tasa.descuento.ice,
                 n = n.canon,
                 pv = valor,
                 fv = 0)
    res <- data.frame(pago = pago, pago.anticipado = pago.anticipado)
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
  
  
  # Financiamiento ----------------------------------------------------------
  
  
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
  
  financiamiento.elemento11 <- inversiones.elemento11 %>%
    map2(ingresos.elemento11, left_join) %>% 
    map(amortizacion,pct.financiado = pct.financiado, n = n, r = r)
  
  # IVA e ISR ---------------------------------------------------------------------
  
  Impuestos.sistema11 <- map2(ingresos.sistema11,inversiones.sistema11,left_join) %>%
    map2(costos.sistema11, left_join, by = "Año") %>%
    map(mutate, IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y) %>%
    map(select, -Inversion, -Royalty, -Ingresos.Brutos, -IVAxCobrar.x, -IVAxCobrar.y, -Costos ) %>% 
    map(mutate, IVA.aux1 = cumsum(IVAxCobrar - IVAxPagar),
        IVA.aux2 = IVAxCobrar - IVAxPagar,
        IVA.Neto = if_else(lag(IVA.aux1) < 0 ,IVA.aux2, IVA.aux1)) 
  
  
  
  
  impuestos.elemento11 <- map2(ingresos.elemento11,inversiones.elemento11,left_join) %>%
    map2(costos.elemento11, left_join, by = "Año") %>%
    map(mutate, IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y) %>%
    map(select, -Inversion, -Royalty, -Ingresos.Brutos, -IVAxCobrar.x, -IVAxCobrar.y, -Costos ) %>% 
    map(mutate, IVA.aux1 = cumsum(IVAxCobrar - IVAxPagar),
        IVA.aux2 = IVAxCobrar - IVAxPagar,
        IVA.Neto = if_else(lag(IVA.aux1) < 0 ,IVA.aux2, IVA.aux1)) %>% 
    map(select, -contains("aux"))
  
  
  # Valorizacion ------------------------------------------------------------
  
  ### Calculo de la tasa de retorno esperada
  
  
    if (tasa.retorno.manual == TRUE) {
      expected.return <- tasa.descuento
    } else{
      require(rvest)
      url <- "http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/totalbeta.html"
      url2 <- "http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/histretSP.html"
      
      safe_read <- safely(read_html)
      prueba <- safe_read(url)
      
      
        if (is_null(prueba$result)) {
        betas <- readRDS("app/Datos/Intermodal/betas2017-01-25.rds")
        tasas <- readRDS("app/Datos/Intermodal/tasas2017-01-25.rds")
        
      } else{
        betas <- read_html(url) %>% 
          html_nodes(xpath = "/html/body/table") %>% 
          html_table()
        
        fecha.consulta <- Sys.Date()
        flname <- paste0("app/Datos/Intermodal/betas",fecha.consulta,".rds")
        saveRDS(object = betas ,file = flname)
        
        tasas <- read_html(url2) %>% 
          html_table()
        
        flname <- paste0("app/Datos/Intermodal/tasas",fecha.consulta,".rds")
        saveRDS(object = tasas ,file = flname)
      }
      
      
      betas <- as.data.frame(betas)
      names(betas) <- betas[1,]
      betas <- betas[c(91,92),c(1,3)]
      beta <- betas$`Average Unlevered Beta` %>% 
        as.numeric() %>% 
        mean()
      
      tasas <- as.data.frame(tasas)
      names(tasas) <- tasas[2,]
      
      tasas  <- tasas[3:nrow(tasas),] 
      tasas.promedio <- tail(tasas,9) %>% head(4)
      nombres <- c("periodo","SP500","3month.bill","10year.bond")
      tasas <- tasas.promedio[1:4] 
      names(tasas) <- nombres
      tasas <- tasas[2:nrow(tasas),]
      
      rm <- tasas[3,2]
      rf <- tasas[3,4]
      rm <- rm %>% str_replace("%","")
      rm <- as.numeric(rm)/100
      
      rf <- rf %>% str_replace("%","")
      rf <- as.numeric(rf)/100
      expected.return <- rf + beta*(rm - rf)
      financial.data <- data.frame(expected.return, rf, rm,beta)
      financial.data
    }
  
  expected.return <- financial.data[["expected.return"]]
  
  ### Creación de data frames que sirven de input para el calculo del VP base del 
  ### proyecto y VP del financiamiento en las modalidades: 
  ### Sistema completo, portipo de sistema y por elemento del sistema
  df.sistema11 <- ingresos.sistema11 %>%
    map2(costos.sistema11, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(inversiones.sistema11, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(financiamiento.sistema11, full_join) %>%
    map(select, -Saldo, -Amortizacion) %>%
    map2(Impuestos.sistema11, full_join) %>%
    map(select, -contains("IVAx")) %>%
    map(replace_na, replace = list(Ingresos.Brutos = 0,
                                   Royalty = 0,
                                   ISR = 0,
                                   Costos = 0,
                                   Inversion = 0,
                                   Pago = 0,
                                   IVA.Neto = 0,
                                   Intereses = 0))
  
  df.sistema.completo11 <- df.sistema11 %>% 
    bind_rows() %>% 
    group_by(Año) %>% 
    summarise_each("sum") 
  
  df.elemento11 <- ingresos.elemento11 %>%
    map2(costos.elemento11, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(inversiones.elemento11, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(financiamiento.elemento11, full_join) %>%
    map(select, -Saldo, -Amortizacion) %>%
    map2(impuestos.elemento11, full_join) %>%
    map(select, -contains("IVAx")) %>%
    map(replace_na, replace = list(Ingresos.Brutos = 0,
                                   Royalty = 0,
                                   ISR = 0,
                                   Costos = 0,
                                   Inversion = 0,
                                   Pago = 0,
                                   IVA.Neto = 0,
                                   Intereses = 0))
  
  ### Funciones que calculan vpn base y de financiamiento respectivamente
  calcular_fen <- function(df,r,horizonte = 2062, res = 1, regimen.fiscal = FALSE){
    
    df$regimen.fiscal <- regimen.fiscal
    df <-  df %>% 
      mutate(IVA.Neto = if_else(IVA.Neto > 0,0,IVA.Neto),
             ISR2 = if_else(regimen.fiscal == FALSE, ISR, (Ingresos.Brutos - Costos)*isr2),
             FEN      =  Ingresos.Brutos
             - ISR2
             - Costos
             - Inversion
             # - Pago
             + IVA.Neto,
             Año = as.numeric(Año)) %>% 
      filter(Año <= horizonte)
    
    flujo.base <- sum(df$FEN)
    vp.base <- npv(r = r, cf = df$FEN)
    tir <- irr(df$FEN)
    
    if (res == 1) {
    res <- data.frame(flujo.base, vp.base, tir)
    } else {
    df
  }
  }
  calcular_vpnf <- function(df,r,horizonte = 2062, res = 1, regimen.fiscal = FALSE,
                            pct.financiado = pct.financiado){
    
    df$regimen.fiscal <- regimen.fiscal
    
    
    df <-  df %>% 
      mutate(FEN = Inversion*pct.financiado
                 - Pago
                 + Intereses*isr2,
             Año = as.numeric(Año)) %>% 
      filter(Año <= horizonte) %>% 
      arrange(Año)
    
    flujo.fin <- sum(df$FEN)
    vp.fin <- npv(r = r, cf = df$FEN)
    
    if (res == 1) {
      res <- data.frame(flujo.fin, vp.fin)
    } else {
      df
    }
  }
  
  ### Calculo de vp base para cada modalidad
  valor.sistema.completo <- calcular_fen(df.sistema.completo11, r = expected.return, horizonte = horizonte, res = 1, regimen.fiscal = regimen.fiscal )
  valor.sistema11 <- map(df.sistema11, calcular_fen,  r = expected.return, horizonte = horizonte, res = 1, regimen.fiscal = regimen.fiscal)
  valor.elemento11 <- map(df.elemento11, calcular_fen,  r = expected.return, horizonte = horizonte, res = 1, regimen.fiscal = regimen.fiscal)
  valor.sistema11 <- bind_rows(valor.sistema11, .id = "Sistema")
  valor.elemento11 <- bind_rows(valor.elemento11, .id = "Elemento")
  
  ### Calculo de vp del financiamiento para cada modalidad
  valor.sistema.completo.fin <- calcular_vpnf(df.sistema.completo11, r = expected.return, horizonte = horizonte, res = 1, regimen.fiscal = regimen.fiscal, pct.financiado = pct.financiado)
  valor.sistema11.fin <- map(df.sistema11, calcular_vpnf,  r = expected.return, horizonte = horizonte, res = 1, regimen.fiscal = regimen.fiscal, pct.financiado = pct.financiado)
  valor.elemento11.fin <- map(df.elemento11, calcular_vpnf,  r = expected.return, horizonte = horizonte, res = 1, regimen.fiscal = regimen.fiscal, pct.financiado = pct.financiado)
  valor.sistema11.fin <- bind_rows(valor.sistema11.fin, .id = "Sistema")
  valor.elemento11.fin <- bind_rows(valor.elemento11.fin, .id = "Elemento")
  
  ### DF para cada variedad
  valor.completo <- bind_cols(list(valor.sistema.completo,
                                   valor.sistema.completo.fin)) %>% 
                    mutate(vp.total = vp.base + vp.fin)
  
  valor.sistema <- left_join(valor.sistema11, valor.sistema11.fin) %>% 
                   mutate(vp.total = vp.base + vp.fin)

  valor.elemento <- left_join(valor.elemento11, valor.elemento11.fin) %>% 
    mutate(vp.total = vp.base + vp.fin)
  


  
  resultado <- list(Completo = valor.completo, 
                    Sistema  = valor.sistema, 
                    Elemento = valor.elemento)
  
  return(resultado)

# }

res <- modelo.base()
  
  
res[[1]]$vp.total
