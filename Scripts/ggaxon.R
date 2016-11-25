ggaxon <- function(grafico,fuente) {
  require(artyfarty)
  require(png)
  require(grid)
  require(gridExtra)
  
  img <-  readPNG(source = "Informacion de Apoyo/Logos/axon.png")
  
  texto <- paste0("Fuente: ",fuente)
  my_g <- grobTree(rectGrob(gp=gpar(fill="white",col="white")),
                   textGrob(texto, x=0.99, hjust=1,vjust=0.5,
                            gp=gpar(col="#737373",
                                    fontfamily="Open Sans",
                                    fontsize=9)),
                   rasterGrob(img, x=0.01, hjust=0))
  
  grid.arrange(grafico,my_g,heights=c(1,0.05))
  
}


