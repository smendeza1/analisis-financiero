library(shiny)
# Server ------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)
library(FinCal)

source("famortizacion.R") # funciones para calculos financieros
source("funcionmodelo.R")


server <- function(input, output) {
  
  output$modelo <- renderText({
    
    modelo(n=input$años.financiamiento,
           r=input$tasa.financiamiento,
           pct.financiado=input$pct.financiado,
           tipo.demanda = input$tipo.demanda,
           isr=input$isr)
  })
}
