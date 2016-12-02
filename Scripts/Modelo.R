source("Scripts/financiamiento.R")
source("Scripts/ingresos.R")
source("Scripts/fin101.R")
#Función de Financiamiento y Amortizaciones

cashflow <- full_join(full_join(ingresos,Costos),Inversiones)

# Para estimar el monto de inversiones de capital se utiliza 1-pct.financiado

cashflow$Inversion.Infraestructura <- cashflow$Inversion.Infraestructura*(1-pct.financiado)
cashflow$Inversion.Superestructura <- cashflow$Inversion.Superestructura*(1-pct.financiado)

cashflow <- cashflow %>%
  filter(Year<=2080)%>%
  mutate(Year=as.numeric(Year))%>%
  select(-Infraestructura)%>%
  group_by(Year) %>%
  summarise_all(sum)
  
cashflow <- full_join(cashflow,pago.financiamiento[c(1,5)],by="Year")


cashflow <- cashflow %>%
  mutate(fen=Ingresos.Brutos*(1-0.17)-Explotación-Mantenimiento-Reposición-Inversion.Infraestructura-Inversion.Superestructura-Pago)%>%
  replace_na(list(fen=0))

cashflow$fen <- cashflow$fen*(1-0)
scales::dollar(npv_f(cash_flows = cashflow$fen,0.08)/1e+6)

cashflow %>%
  group_by(Infraestructura)%>%
  summarise(vpn=npv2_f(fen,0.08)*1000/1e+9) # Multiplico por 1000 porque los datos vienen divididos por un factor de 1000



#Impuestos

rm(list = ls())

