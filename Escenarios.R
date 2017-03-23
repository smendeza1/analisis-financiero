
# Simulacion --------------------------------------------------------------

# Codigo utilizado para realizar la simulacion
# params <- expand.grid(pct.royalty = c(0, 0.01, 0.02),
#                       pct.pago.anticipado = c(0, 0.01, 0.02),
#                       n.concesion.multi = c(30, 50),
#                       n.concesion.poli = c(20, 30),
#                       r.ice = seq(0:10)*0.01 - 0.0099999,
#                       tipo.demanda = c("min", "max"),
#                       riesgo.pais = c(0.04, 0.05)) %>% 
#   mutate(tipo.demanda = as.character(tipo.demanda))
# 
# start.time <- proc.time()
# tmp4 <- params %>% 
#   pmap(modelo.base,
#        Demanda = Demanda,
#        Tarifas = Tarifas,
#        Costos  = Costos,
#        Inversiones = Inversiones,
#        Ingresos.poliducto = Ingresos.poliducto)
# 
# duration <- proc.time() - start.time
# 
# saveRDS("app/Datos/simulacion2.rds", object = tmp4)
# 
# tmp <- readRDS("app/Datos/simulacion.rds")
# 
# wacc <- transpose(tmp) %>% .$Tasas %>% bind_rows() %>% .[1, 2]
# expected.return <- transpose(tmp) %>% .$Tasas %>% bind_rows() %>% .[1, 4]
# 
# tmp2 <- transpose(tmp) %>%
#   .$dfs %>% 
#   bind_rows()
# 
# tmp2 <- tmp2 %>% 
#   select(-n) %>% 
#   mutate(riesgo.pais = 0.0355,
#          expected.return = expected.return,
#          wacc = wacc)
# 
# 
# tmp3 <- readRDS("app/Datos/simulacion2.rds")
# tmp4 <- transpose(tmp3)
# tmp4 <- tmp4$dfs %>% bind_rows() 
# 
# resultados <- bind_rows(tmp2, tmp4)
# 
# saveRDS(object = resultados, file = "app/Datos/resultados.rds")


# Analisis Resultados -----------------------------------------------------

library(tidyverse)
library(scales)
library(ggbeeswarm)
library(modelr)
resultados <- readRDS("app/Datos/resultados.rds")


resultados %>% 
  filter(Nivel == "Sistema") %>% 
  ggplot(aes( x = vp.base, fill = Interesado)) +
  geom_density(alpha = 0.8, adjust = 3) +
  facet_grid(Escenario~tipo.demanda) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = dollar) +
  scale_fill_manual(values = c("#c51b7d", "#2166ac"))


resultados %>% 
  filter(Nivel == "Elemento") %>% 
  ggplot(aes(x = r.ice, y = vp.base)) +
  geom_quasirandom(alpha = 0.1, varwidth = TRUE, method = "tukey", col = "grey") +
  geom_smooth() +
  facet_grid(Interesado ~ Escenario) +
  ggthemes::theme_igray()


resultados %>% 
  filter(Nivel == "Sistema",
         Nombre == "Multimodal",
         Escenario == "Escn2",
         n.concesion.multi == 50,
         tipo.demanda == "min") %>% 
  ggplot(aes(x = r.ice, y = vp.base, col = as.factor(riesgo.pais))) +
  geom_point(alpha = 0.5 ) +
  facet_grid(Interesado~Escenario) +
  geom_smooth() +
  ggthemes::theme_igray()
    

modelo_energia    <- function(df)   lm(vp.base~. -n.concesion.multi -tir -tipo.demanda -riesgo.pais - wacc, data = df )
# modelo_energia2    <- function(df)   lm(vp.base~. -n.concesion.multi -tir -pct.royalty -pct.pago.anticipado -r.ice -tipo.demanda, data = df )
modelo_multimodal <- function(df)   lm(vp.base~. -n.concesion.poli -tir -riesgo.pais - wacc, data = df )
# modelo_multimodal2 <- function(df)   lm(vp.base~. -n.concesion.poli -tir -pct.royalty -pct.pago.anticipado -r.ice, data = df )

por_escenario <- resultados %>% 
  group_by(Nivel, Escenario ,Interesado, Nombre) %>% 
  nest()


por_escenario <- por_escenario %>% 
  mutate(Modelo = if_else(Nombre %in% c("Energia", "Poliductos"), 
                                  map(data, modelo_energia),
                                  map(data, modelo_multimodal)),
         Glance = map(Modelo, broom::glance),
         Tidy   = map(Modelo, broom::tidy),
         Augment = map(Modelo, broom::augment))

# por_escenario <- por_escenario %>% 
#   mutate(Modelo = if_else(Nombre %in% c("Energia", "Poliductos"), 
#                           if_else(Escenario != "Escn1",
#                                   map(data, modelo_energia),
#                                   map(data, modelo_energia2)),
#                           if_else(Escenario != "Escn1",
#                                   map(data, modelo_multimodal),
#                                   map(data, modelo_multimodal2))),
#          Glance = map(Modelo, broom::glance),
#          Tidy   = map(Modelo, broom::tidy))


resids <- por_escenario %>% 
  mutate(resids = map2(data, Modelo, add_residuals)) %>% 
  unnest(resids)
  
por_escenario %>%
  unnest(Glance) %>% 
  ggplot(aes(x = Interesado, y = r.squared, col = cut_number(adj.r.squared,5))) +
  geom_quasirandom() +
  facet_grid(Nombre~Escenario) +
  coord_cartesian(ylim = c(0, 1))

por_escenario %>%
  unnest(Glance) %>% 
  ggplot(aes(x = r.squared)) +
  geom_density(adjust = 2)

pvalues <- por_escenario %>% 
  unnest(Tidy)

por_escenario %>% 
  unnest(Augment) %>% 
  View()
