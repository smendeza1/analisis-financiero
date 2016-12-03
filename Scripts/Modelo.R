source("Scripts/financiamiento.R")
source("Scripts/ingresos.R")
source("Scripts/fin101.R")
#Función de Financiamiento y Amortizaciones

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
    fen = Ingresos.Brutos * (1 - 0.17) # El 1-0.17 refleja el pago de impuestos por concepto de 12% de IVA y 5% de ISR
    - Explotación - Mantenimiento
    - Reposición
    - Inversion.Infraestructura
    - Inversion.Superestructura
    - Pago,
    fen = if_else(Year < year.inversion, 0, fen)
  )



scales::dollar(npv_f(cash_flows = cashflow$fen,0.08)/1e+6)



qplot(data = cashflow, Year, fen, geom = "line") +
  geom_abline(intercept = 0, col = "grey")


