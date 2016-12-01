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

n <- 30 #Años de financiamiento
r <- 0.05 #Tasa a la que se trabaja



