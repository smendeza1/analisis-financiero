library(shiny)


# Interfaz gráfica --------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("años.financiamiento",label = "Años financiamiento",value = 30,max=50,min=20),
      sliderInput("tasa.financiamiento","Tasa financiamiento",value=0.05,max=0.1,min=0.01,step = 0.005),
      sliderInput("pct.financiado","Porcentaje Inversion Financiada",min = 0,max=1,value=0.6),
      sliderInput("isr","Impuesto sobre la renta: Sobre ingresos",min=0.01,max=0.1,value=0.07,step=0.005),
      radioButtons("tipo.demanda","Tipo demanda",
                   choices = list("Demanda Minima"="min","Demanda Maxima"="max"),
                   selected = "Demanda Minima"),
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(textOutput("modelo"))
  )
)



# Server ------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)
library(FinCal)

source("famortizacion.R",local = TRUE) # funciones para calculos financieros
source("funcionmodelo.R",local=TRUE)
demanda2 <- read_excel("Datos/Intermodal/Ingresos.xlsx",sheet = "Demanda")
tarifas2 <- read_excel("Datos/Intermodal/Ingresos.xlsx", sheet= "Tarifas")
Costos2 <- read_excel("Datos/Intermodal/Costos e Inversiones.xlsx",sheet="Costos")
Inversiones2 <- read_excel("Datos/Intermodal/Costos e Inversiones.xlsx",sheet="Inversiones")



server <- function(input, output) {

  output$modelo <- renderText({
    
    modelo(n=input$años.financiamiento,
           r=input$tasa.financiamiento,
           pct.financiado=input$pct.financiado,
           tipo.demanda = input$tipo.demanda,
           isr=input$isr)
  })
}


shinyApp(ui = ui, server = server)
