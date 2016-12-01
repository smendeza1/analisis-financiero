# Amortizaciones
source("Scripts/Costos e Inversiones.R")
source("Scripts/fin101.R")


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

n <- 30 # Años de financiamiento
r <- 0.05 # Tasa a la que se trabaja
pg <- 3 # Años de gracia

inversion <- i.infr[1,]
inversion[,3] <- 29414

amortizacion <- function(n=30,r=0.05,pg=3,inversion){

# al momento de crear el escenario sumo n+pg por el periodo de gracia de cada pago
escenario <- data_frame(Year=numeric(n+pg), 
                        Saldo=numeric(n+pg),
                        Intereses =numeric(n+pg),
                        Amortizacion=numeric(n+pg))
escenario$Year <- seq_len(n+pg) + as.numeric(inversion$Year) # Llenado de los años correspondientes al escenario
escenario$Amortizacion <- inversion$Inversion.Infraestructura/n # Calculo de los pagos de amortización anuales

if(pg==0){
  
  escenario <- escenario %>% 
    mutate(Saldo = inversion$Inversion.Infraestructura-cumsum(escenario$Amortizacion),
           Intereses = (Saldo+Amortizacion)*r)
  
}else {
  escenario$Amortizacion[1:pg] <- 0 # Seteo de periodo de gracia
  escenario <- escenario %>% 
    mutate(Saldo = inversion$Inversion.Infraestructura-cumsum(escenario$Amortizacion),
           Intereses = (Saldo+Amortizacion)*r)
  escenario$Intereses[1:pg] <- 0 # Seteo de periodo de gracia
}

escenario  <- escenario %>% mutate(Pago=Intereses+Amortizacion)
return(escenario)
}

amortizacion(inversion = i.infr[3,])
