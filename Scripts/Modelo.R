source("Scripts/Costos e Inversiones.R")
source("Scripts/ingresos.R")
source("Scripts/fin101.R")
#Función de Financiamiento y Amortizaciones

cashflow <- full_join(full_join(ingresos,Costos),Inversiones)

cashflow <- mutate(cashflow,fen=Ingresos.Brutos-Explotación-Mantenimiento-Reposición-Inversion.Infraestructura-Inversion.Superestructura) %>%
  filter(Year<=2050)

cashflow %>%
  group_by(Infraestructura)%>%
  summarise(vpn=npv2_f(fen,0.08)*1000/1e+9) # Multiplico por 1000 porque los datos vienen divididos por un factor de 1000



#Impuestos



