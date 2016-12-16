# # Información utilizada por la función para poder funcionar
# source("app/famortizacion.R") # funciones para calculos financieros
# demanda2 <- read_excel("Datos/Intermodal/Ingresos.xlsx",sheet = "Demanda")
# tarifas2 <- read_excel("Datos/Intermodal/Ingresos.xlsx", sheet= "Tarifas")
# Costos2 <- read_excel("Datos/Intermodal/Costos e Inversiones.xlsx",sheet="Costos")
# Inversiones2 <- read_excel("Datos/Intermodal/Costos e Inversiones.xlsx",sheet="Inversiones")

require(shiny)
require(tidyverse)
require(readxl)
require(stringr)
require(FinCal)
require(scales)

modelo <- function(n=30,r=0.05,pct.financiado=0.6,tipo.demanda="min",demanda=demanda2,
                   tarifas=tarifas2,Costos=Costos2,Inversiones=Inversiones2,isr=0.07,isr2=0.25,
                   tasa.descuento=0.08,horizonte=2080,regimen.fiscal = FALSE){

# ##Datos para realizar modificaciones a la función
# n=30
# r=0.05 ## Regimen sobre ingresos
# r2=0.25 ## Regimen sobre utilidades
# tasa.descuento = 0.08
# pct.financiado=0.6
# tipo.demanda="min"
# demanda=demanda2
# tarifas=tarifas2
# Costos=Costos2
# Inversiones=Inversiones2
# isr=0.07
# isr2=0.25
# horizonte=2080
# regimen.fiscal = FALSE ## en donde FALSE indica regimen sobre ingresos

 # Demanda -----------------------------------------------------------------
  demanda <- demanda %>%
    gather(Year,TEUS,-Escenario) %>%
    replace_na(demanda,replace = list(TEUS=0)) %>%
    spread(Escenario,TEUS)
  
  names(demanda)[2] <- "Ramp.Up" # Utilizo un punto en el nombre de la columna para hacer mas facil la referencia a esta columna
  
  
  # Tarifas -----------------------------------------------------------------
  
  tarifas <- tarifas %>%
    gather(Year,Tarifa.dols, -TARIFAS)
  
  names(tarifas)[1] <- "Sistema"
  
  # Ingresos ----------------------------------------------------------------
  # Se asume que la cantidad de TEUS movidos por cada puerto sera exactamente la mitad de la demanda disponible para cada puerto.
  # En el caso del ferrocarril, se asume que el 100% de los TEUS disponibles seran movilizados
  tarifas$pct.mercado <- ifelse(tarifas$Sistema!="Ferrocarril",0.5,1)
  # Creao una tabla distinta para modelar los ingresos
  ingresos <- left_join(tarifas,demanda)
  ingresos$tipo.demanda <- tipo.demanda
  
  # Desde este momento se utilizara la demanda minima para modelar los ingresos
  # Considerar que luego debo de tener una forma de cambiar la columna de demanda, la cual debera de ser influenciada por el modelo a crear.

  ##Seleccion de tipo de demanda
  
  if(tipo.demanda=="min"){
    ingresos <- ingresos %>%
      mutate(Ingresos.Brutos = if_else(Sistema!="Ferrocarril",Tarifa.dols*pct.mercado*Ramp.Up*MIN*2,Tarifa.dols*pct.mercado*Ramp.Up*MIN),
             Mercado=pct.mercado*Ramp.Up*MIN) 

  }else{
    ingresos <- ingresos %>%
      mutate(Ingresos.Brutos = if_else(Sistema!="Ferrocarril",Tarifa.dols*pct.mercado*Ramp.Up*MAX*2,Tarifa.dols*pct.mercado*Ramp.Up*MAX),
             Mercado=pct.mercado*Ramp.Up*MAX) 

  }
  


  ingresos$Infraestructura <- ifelse(ingresos$Sistema!="Ferrocarril","Puertos","Ferrocarril")
  
  ingresos <- ingresos %>% 
    select(Ingresos.Brutos,Year,Infraestructura) %>%
    group_by(Year,Infraestructura)%>%
    summarise_all(sum)
  
  ingresos <- replace_na(ingresos, replace = list(Ingresos.Brutos=0))
  
  
  # Costos e inversiones ----------------------------------------------------
  
  Costos <- Costos%>%
    gather(Year,Valor,-Infraestructura,-Costos.Operacion,-Categoria)%>%
    select(-Categoria)%>%
    group_by(Year,Infraestructura,Costos.Operacion)%>%
    summarise(Valor=sum(Valor))
  
  Costos$Costos.Operacion<-str_trim(Costos$Costos.Operacion)
  
  Costos <- Costos %>% spread(Costos.Operacion,Valor)
  
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
  
  
  # Financiamiento ----------------------------------------------------------
  
  i.infr <- Inversiones %>% #Realizo la separacion para poder dividir el problema en pequeños problemas y luego juntarlo
    filter(Inversion.Infraestructura!=0) %>%
    select(-Inversion.Superestructura) 
  
  i.super <- Inversiones %>% 
    filter(Inversion.Superestructura!=0) %>%
    select(-Inversion.Infraestructura)
  
  # Calculo de amortizaciones para cada inversión 
  
  inversion = i.infr[1,]
  escenario <- amortizacion(inversion=inversion,pct.financiado = pct.financiado,n = n,r = r,ingresos = ingresos)
  escenario$tipo.inversion <- "Inversion.Infraestructura"
  
  for(i in 2:nrow(i.infr)){
    inversion = i.infr[i,]
    tmp <- amortizacion(inversion=inversion,pct.financiado = pct.financiado,ingresos=ingresos)
    tmp$tipo.inversion <- "Inversion.Infraestructura"
    escenario <- rbind(escenario,tmp)
  }
  
  for(i in 1:nrow(i.super)){
    inversion = i.super[i,]
    tmp <- amortizacion(inversion=inversion,pct.financiado = pct.financiado,ingresos=ingresos)
    tmp$tipo.inversion <- "Inversion.superaestructura"
    escenario <- rbind(escenario,tmp)
  }
  
  
  #Este data.frame es el que se usa para el modelo del cashflow
  pago.financiamiento <- escenario %>% mutate(tipo.inversion=as.factor(tipo.inversion)) %>%
    group_by(Year) %>%
    summarise(Saldo=sum(Saldo),Intereses=sum(Intereses),Amortizacion=sum(Amortizacion),Pago=sum(Pago))
  
  tmp<-Inversiones %>% 
    select(-Infraestructura)%>%
    group_by(Year)%>%
    summarise(credito.necesario=sum(Inversion.Infraestructura+Inversion.Superestructura)*pct.financiado)
  tmp$Year <- as.numeric(tmp$Year)
  
  pago.financiamiento <- left_join(pago.financiamiento,tmp)
  
  # Calculo de primer año de inversion
  min.infr <-min(i.infr$Year)
  min.super <-min(i.super$Year)
  
  year.inversion <- as.numeric(if_else(min.infr<=min.super,min.infr,min.super))
  
  rm(tmp,i.infr,i.super,i,min.infr,min.super)
  
  
  # Creación de FEN ----------------------------------------------------------
  
  cashflow <- full_join(full_join(ingresos,Costos),Inversiones)
  
  # Para estimar el monto de inversiones de capital se utiliza 1-pct.financiado
  
  cashflow$Inversion.Infraestructura <- cashflow$Inversion.Infraestructura*(1-pct.financiado)
  cashflow$Inversion.Superestructura <- cashflow$Inversion.Superestructura*(1-pct.financiado)
  
  cashflow <- cashflow %>%
    filter(Year <= 2080) %>%
    mutate(Year = as.numeric(Year)) %>%
    select(-Infraestructura) %>%
    group_by(Year) %>%
    summarise_all(sum)
  
  cashflow <- full_join(cashflow, pago.financiamiento[c(1, 5)], by = "Year")
  
  cashflow <- cashflow %>% replace_na(list(Pago = 0))
  
  cashflow <- cashflow %>%
    mutate(
      fen = Ingresos.Brutos * (1 - 0.12-isr) # El 1-0.17 refleja el pago de impuestos por concepto de 12% de IVA y 5% de ISR
      - Explotación - Mantenimiento
      - Reposición
      - Inversion.Infraestructura
      - Inversion.Superestructura
      - Pago,
      fen = if_else(Year < year.inversion, 0, fen)
    )
  
  vpn <- round(npv(tasa.descuento,cashflow$fen)/1e+3,0)

  irr <- irr(cashflow$fen)
  resultados <- c(`VPN en B $US`=scales::dollar(vpn),TIR=scales::percent(irr))
  resultados <- paste("El vpn es de: ",resultados[1]," millones y la TIR de:",resultados[2])
  
  p <- qplot(data = cashflow, Year, fen/1e+3, geom = "line") +
    geom_hline(yintercept = 0,col="grey")+
    scale_x_continuous(breaks = pretty_breaks(10),name = "Año")+
    scale_y_continuous(breaks = pretty_breaks(10),name = "$",labels = dollar)+
    theme_light(base_family = "Open Sans")
  
  resultados <- list(resultados,p)
    return(resultados)
}

modelo()
