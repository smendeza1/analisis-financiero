
# funcion amortizacion ----------------------------------------------------

amortizacion <- function(n=30,r=0.05,inversion,pct.financiado=1,ingresos=ingresos){
  
  names(inversion)[3] <- "Inversion"
  inversion$Inversion <-  inversion$Inversion*pct.financiado 
  # Prueba logica para determinar periodo de gracia -------------------------
  
  year.inversion <- as.numeric(inversion$Year) # año en que se realiza la inversion

  
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

