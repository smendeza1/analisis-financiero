

# Interfaz gráfica --------------------------------------------------------

ui <- navbarPage(
  title = "Analisis Financiero",
  inverse = FALSE,
  footer = div("  Elaborado por:  ", img(src = "axon2.png")),
  tabPanel("Valor Base",
           sidebarLayout(
             sidebarPanel(
               sliderInput(
                 "años.financiamiento",
                 label = "Años financiamiento",
                 value = 30,
                 max = 30,
                 min = 5
               ),
               sliderInput(
                 "horizonte",
                 "Año final evaluación VPN",
                 value = 2062,
                 max = 2112,
                 min = 2050,
                 step = 2
               ),
               sliderInput(
                 "tasa.financiamiento",
                 "Tasa financiamiento",
                 value = 0.05,
                 max = 0.1,
                 min = 0.01,
                 step = 0.005
               ),
               sliderInput(
                 "pct.financiado",
                 "Porcentaje Inversion Financiada",
                 min   = 0,
                 max = 1,
                 value = 0.6
               ),
               sliderInput(
                 "tasa.descuento",
                 "Tasa de Descuento",
                 value = 0.08,
                 max = 0.2,
                 min = 0.05,
                 step = 0.005
               ),
               helpText(
                 "Esta es la tasa a la que se evalúa el proyecto para el promotor y el operador"
               ),
               radioButtons(
                 "tipo.demanda",
                 "Tipo demanda",
                 choices = list("Demanda Minima" = "min", "Demanda Maxima" = "max"),
                 selected = "min"
               ),
               radioButtons(
                 "regimen.fiscal",
                 "Regimen Fiscal",
                 choices = list("Sobre Ingresos" = FALSE, "Sobre Utilidades" =
                                  TRUE),
                 selected = TRUE
               ),
               sliderInput(
                 "isr",
                 "Impuesto sobre la renta: Sobre ingresos",
                 min = 0.01,
                 max = 0.1,
                 value = 0.07,
                 step = 0.005
               ),
               sliderInput(
                 "isr2",
                 "Impuesto sobre la renta: Sobre utilidades",
                 min = 0.25,
                 max = 0.4,
                 value = 0.25,
                 step = 0.05
               )
             ),
             mainPanel(tabsetPanel(
               tabPanel("VPN Proyecto", dataTableOutput("total")),
               tabPanel("VPN por Sistema", dataTableOutput("sistema")),
               tabPanel("VPN por Elemento", dataTableOutput("elemento"))
             ))
           ))
)
