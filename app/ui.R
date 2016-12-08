
# Interfaz gráfica --------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("años.financiamiento",label = "Años financiamiento",value = 30,max=50,min=5),
      sliderInput("tasa.financiamiento","Tasa financiamiento",value=0.05,max=0.1,min=0.01,step = 0.005),
      sliderInput("pct.financiado","Porcentaje Inversion Financiada",min = 0,max=1,value=0.6),
      sliderInput("isr","Impuesto sobre la renta: Sobre ingresos",min=0.01,max=0.1,value=0.07,step=0.005),
      sliderInput("tasa.descuento","Tasa de Descuento",value=0.08,max = 0.2,min=0.05,step=0.005),
      helpText("Esta es la tasa a la que se evalúa el proyecto para el promotor y el operador"),
      radioButtons("tipo.demanda","Tipo demanda",
                   choices = list("Demanda Minima"="min","Demanda Maxima"="max"),
                   selected = "min")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Resultado analisis",textOutput("modelo")),
      tabPanel("Flujo Efectivo",plotOutput("fen")))
  )
  )
)

