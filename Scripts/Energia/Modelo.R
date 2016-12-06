source("Scripts/Energia/financiamiento.R")
source("Scripts/Energia/Ingresos, Costos e Inversiones.R")
source("Scripts/Source/fin101.R")
#Función de Financiamiento y Amortizaciones

cashflow <- full_join(full_join(Ingresos,Costos),Inversiones)

# Para estimar el monto de inversiones de capital se utiliza 1-pct.financiado

cashflow$Inversion.Infraestructura <- cashflow$Inversion.Infraestructura*(1-pct.financiado)
cashflow$Inversion.Superestructura <- cashflow$Inversion.Superestructura*(1-pct.financiado)

cashflow$Year <- as.numeric(cashflow$Year)

cashflow <- cashflow %>%
  filter(Year <= 2080) %>%
  select(-Infraestructura,-Ingresos.Operacion)%>%
  group_by(Year) %>%
  summarise_all(sum)

cashflow <- full_join(cashflow, pago.financiamiento[c(1, 5)], by = "Year")

cashflow <- cashflow %>% replace_na(list(Pago = 0))

cashflow <- cashflow %>%
  mutate(
    fen = Ingresos.Brutos * (1 - 0.12) # El 1-0.17 refleja el pago de impuestos por concepto de 12% de IVA y 5% de ISR
    - Explotacion - Mantenimiento
    - Reposición
    - Inversion.Infraestructura
    - Inversion.Superestructura
    - Pago,
    fen = if_else(Year < year.inversion, 0, fen)
  )

scales::dollar(npv_f(cash_flows = cashflow$fen,0.08)/1e+6)

qplot(data = cashflow, Year, fen, geom = "col") +
  geom_abline(intercept = 0, col = "grey")


