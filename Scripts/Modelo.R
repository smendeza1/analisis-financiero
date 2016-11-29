source("Scripts/Costos e Inversiones.R")
source("Scripts/ingresos.R")
source("Scripts/fin101.R")
#Función de Financiamiento y Amortizaciones

cashflow <- full_join(full_join(ingresos,Costos),Inversiones)

cashflow <- mutate(cashflow,fen=Ingresos.Brutos-Explotación-Mantenimiento-Reposición-Inversion.Infraestructura-Inversion.Superestructura) %>%
  filter(Year<=2080)

cashflow %>%
  group_by(Infraestructura)%>%
  summarise(vpn=scales::dollar(npv_f(fen,0.08)*1000))



#Impuestos



