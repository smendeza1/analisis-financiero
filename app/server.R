# Server ------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(readxl)
library(stringr)
library(FinCal)

source("famortizacion.R") # funciones para calculos financieros
source("funcionmodelo.R")

demanda2 <- read_excel("Datos/Intermodal/Ingresos.xlsx",sheet = "Demanda")
tarifas2 <- read_excel("Datos/Intermodal/Ingresos.xlsx", sheet= "Tarifas")
Costos2 <- read_excel("Datos/Intermodal/Costos e Inversiones.xlsx",sheet="Costos")
Inversiones2 <- read_excel("Datos/Intermodal/Costos e Inversiones.xlsx",sheet="Inversiones")

server <- function(input, output) {
  output$modelo <- renderText({

    r <- modelo(n=input$años.financiamiento,
           r=input$tasa.financiamiento,
           pct.financiado=input$pct.financiado,
           tipo.demanda = input$tipo.demanda,
           isr=input$isr,
           demanda=demanda2,
           tarifas = tarifas2,
           Costos = Costos2,
           Inversiones = Inversiones2)
    r[[1]]
  })
  
  output$fen <- renderPlot({
    
    r <- modelo(n=input$años.financiamiento,
                r=input$tasa.financiamiento,
                pct.financiado=input$pct.financiado,
                tipo.demanda = input$tipo.demanda,
                isr=input$isr,
                demanda=demanda2,
                tarifas = tarifas2,
                Costos = Costos2,
                Inversiones = Inversiones2)
    r[[2]]
  })
  
  
  
}
