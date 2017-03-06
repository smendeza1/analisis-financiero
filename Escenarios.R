params <- expand.grid(pct.royalty = c(0, 0.01, 0.02),
                      pct.pago.anticipado = c(0, 0.02),
                      n.concesion.multi = c(10,20,30),
                      n.concesion.poli = c(20, 30),
                      r.ice = seq(0:10)*0.01-0.01,
                      tipo.demanda = c("min", "max") )


# pct.royalty <-  list(params$pct.royalty)
# pct.pago.anticipado = list(params$pct.pago.anticipado)
# n.concesion.multi   = list(params$n.concesion.multi)
# n.concesion.poli    = list(params$n.concesion.poli)
# r.ice               = list(params$r.ice)
# tipo.demanda        = list(params$tipo.demanda)
# 
# params2 <- list(pct.royalty),
#                 pct.pago.anticipado,
#                 n.concesion.multi,
#                 n.concesion.poli,
#                 r.ice,
#                 tipo.demanda)
# 
# 
# params <- expand.grid(pct.royalty = c(0),
#                    pct.pago.anticipado = c(0),
#                    n.concesion.multi = c(10),
#                    n.concesion.poli = c(20),
#                    r.ice = 0,
#                    tipo.demanda = "min" ) %>% as.list()




tmp <- params %>% 
  pmap(modelo.base,
       Demanda = Demanda,
       Tarifas = Tarifas,
       Costos  = Costos,
       Inversiones = Inversiones,
       Ingresos.poliducto = Ingresos.poliducto)


