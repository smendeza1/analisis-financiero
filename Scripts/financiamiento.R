# Amortizaciones
source("Scripts/Costos e Inversiones.R")


# Creacion de funcion de amortización -------------------------------------

# Tengo que considerar que pueden existir distintas modalidades de realizar el financiamiento, por ejemplo
    # Podrian solicitarse creditos por tipo de inversion (Infraestructura/Superestrucutra)
    # Podría interesarnos desglosar las inversiones por tipo de sistema principal/secundario (Ej: Intermodal, compuesto por puertos y ferrocarril)
# En base a lo anterior se van a realizar los escenarios de inversion para cada desembolso que podemos visualizar con sus coordenadas. 
# En el caso de que necesitemos mas visibilidad, podremos visualizar esas composiciones


i.infr <- Inversiones %>% #Realizo la separacion para poder dividir el problema en pequeños problemas y luego juntarlo
  filter(Inversion.Infraestructura!=0) %>%
  select(-Inversion.Superestructura) 

i.super <- Inversiones %>% 
  filter(Inversion.Superestructura!=0) %>%
  select(-Inversion.Infraestructura)

# n <- 30 # Años de financiamiento
# r <- 0.05 # Tasa a la que se trabaja
# pg <- 3 # Años de gracia para poder construir los sistemas

amortizacion <- function(n=30,r=0.05,inversion,pct.financiado=1){

  names(inversion)[3] <- "Inversion"
  inversion$Inversion <-  inversion$Inversion*pct.financiado 
# Prueba logica para determinar periodo de gracia -------------------------

  year.inversion <- as.numeric(inversion$Year) # año en que se realiza la inversion
  source("Scripts/ingresos.R")
  
  primer.ingreso <- ingresos %>% filter(Ingresos.Brutos!=0) %>%
    group_by(Year)%>%
    summarise(Ingresos.Brutos=sum(Ingresos.Brutos))
  
  primer.ingreso <- min(as.numeric(primer.ingreso$Year)) # año en el que existe el primer ingreso
  
  if(year.inversion< primer.ingreso  ){
    pg = primer.ingreso - year.inversion
  }else{
    pg = 0 
  }
  
  
# creacion de escenario inversión -----------------------------------------


# al momento de crear el escenario sumo n+pg por el periodo de gracia de cada pago
escenario <- data_frame(Year=numeric(n+pg), 
                        Saldo=numeric(n+pg),
                        Intereses =numeric(n+pg),
                        Amortizacion=numeric(n+pg))
escenario$Year <- seq_len(n+pg) + as.numeric(inversion$Year)-1 # Llenado de los años correspondientes al escenario
escenario$Amortizacion <- inversion$Inversion/n # Calculo de los pagos de amortización anuales

if(pg==0){
  
  escenario <- escenario %>% 
    mutate(Saldo = inversion$Inversion-cumsum(escenario$Amortizacion),
           Intereses = (Saldo+Amortizacion)*r)
  
}else {
  escenario$Amortizacion[1:pg] <- 0 # Set de periodo de gracia
  escenario <- escenario %>% 
    mutate(Saldo = inversion$Inversion-cumsum(escenario$Amortizacion),
           Intereses = (Saldo+Amortizacion)*r)
}

escenario  <- escenario %>% mutate(Pago=ifelse(Amortizacion==0,0,Intereses+Amortizacion))
escenario$Infraestructura <-inversion$Infraestructura
return(escenario)


}

# Calculo de amortizaciones para cada inversión ---------------------------
pct.financiado = 0.6
inversion = i.infr[1,]
escenario <- amortizacion(inversion=inversion,pct.financiado = pct.financiado)
escenario$tipo.inversion <- "Inversion.Infraestructura"
escenario

for(i in 2:nrow(i.infr)){
  inversion = i.infr[i,]
  tmp <- amortizacion(inversion=inversion,pct.financiado = pct.financiado)
  tmp$tipo.inversion <- "Inversion.Infraestructura"
  escenario <- rbind(escenario,tmp)
  }

for(i in 1:nrow(i.super)){
  inversion = i.super[i,]
  tmp <- amortizacion(inversion=inversion,pct.financiado = pct.financiado)
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