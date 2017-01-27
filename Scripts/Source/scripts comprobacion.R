ingresos.jordi <- read_excel(path = "app/Datos/Intermodal/IngresosModeloJordi.xlsx") %>%
  gather(Año, Valor.Jordi, -Sistema)

ingresos.jordi %>% 
  left_join(ingresos.sistema11$Multimodal) %>% 
  replace_na(replace = list(Ingresos.Brutos = 0)) %>%
  mutate(Diferencia = (Ingresos.Brutos - Valor.Jordi)/Ingresos.Brutos,
         Año = as.numeric(Año)) %>%
  ggplot(aes(x=Año, y = Diferencia)) +
    # geom_line( aes(y=Ingresos.Brutos), col = "red") +
    # geom_line( aes(y=Valor.Jordi), col = "blue")
    geom_point() +
    geom_hline(yintercept = 0, col = "red")


comparacion <- ingresos.jordi %>% 
  left_join(ingresos.sistema11$Multimodal) %>% 
  replace_na(replace = list(Ingresos.Brutos = 0)) %>%
  mutate(Diferencia = (Ingresos.Brutos - Valor.Jordi)/Ingresos.Brutos,
         Diferencia = Diferencia*100,
         Año = as.numeric(Año))


Ingresos %>%
  mutate( Mercado = round(pct.mercado*Ramp.Up*MIN,0),
          Mercado = if_else(Elemento == "Ferrocarril", 
                            lag(Mercado,1) + lag(Mercado,2),
                            Mercado),
          Ingresos.Brutos = if_else(Elemento != "Ferrocarril", 
                                    Tarifa.dols*Mercado*2, 
                                    Tarifa.dols*Mercado),
          Royalty = Ingresos.Brutos*pct.royalty,
          IVAxPagar = Ingresos.Brutos*0.12) %>% View()


ingresos.jordi2 <- read_excel("app/Datos/Intermodal/IngresosModeloJordi.xlsx", 2) %>%
  gather(Año, Valor.Jordi, -Sistema)


comparacion <- ingresos.jordi2 %>% 
  left_join(ingresos.sistema11$Energia) %>% 
  replace_na(replace = list(Ingresos.Brutos = 0)) %>%
  mutate(Diferencia = (Ingresos.Brutos - Valor.Jordi)/Ingresos.Brutos,
         Diferencia = round(Diferencia,2),
         Año = as.numeric(Año))


costos.jordi <- read_excel("app/Datos/Intermodal/IngresosModeloJordi.xlsx",3) %>% 
  gather(Año, Valor.Jordi, -Sistema, -Cuentas) %>% 
  filter(Cuentas == "- Gastos de gestión (sin amortización)") %>% 
  left_join(costos.sistema11$Multimodal, by = "Año") %>%
  left_join(ingresos.sistema11$Multimodal) %>% 
  mutate(Costos = Costos + Gastos.Comercializacion,
         Diferencia = Costos + Valor.Jordi) 

costos.jordi %>% 
  ggplot(aes(x=as.numeric(Año), y = Diferencia/abs(Costos))) + geom_point()


flujo.jordi <- read_excel("app/Datos/Intermodal/IngresosModeloJordi.xlsx",4) %>% 
  gather(Año, Valor, -Sistema,-Cuenta) %>%
  spread(Cuenta, Valor) %>% 
  mutate(Año = as.numeric(Año)) %>% 
  left_join(valor.sistema11$Multimodal) %>% 
  select(-regimen.fiscal, -Royalty) %>% 
  mutate(diferencia = FENj - FEN,
         dif.por = diferencia/FENj) %>% 
  filter(Año <= 2062)

flujo.jordi %>% 
  filter(Año <= 2062) %>% 
  ggplot(aes(x = Año, y = dif.por)) +
    geom_point() +
    geom_hline(yintercept = 0, col = "red") +
    scale_y_continuous(label = percent) 

diferencias <- flujo.jordi %>% 
  mutate(diff.Ingresos = Ingresosj -Ingresos.Brutos,
         diff.Costos = Costosj + Costos,
         diff.ISR = Impuestosj + ISR2,
         diff.Inversiones = Inversionesj + Inversion,
         diff.IVA = abs(IVAj) - abs(IVA.Neto),
         diff.FEN = FENj - FEN,
         sum.diff = diff.Ingresos + diff.Costos + diff.ISR + diff.Inversiones + diff.IVA + diff.FEN) %>% 
  select(Año,contains("diff"))

diferencias %>% View()
