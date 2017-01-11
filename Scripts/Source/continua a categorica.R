
convertir_continua_categorica <- function(x, n = 6, style = "quantile" ){
  require(classInt)
  require(Hmisc)
  brks <- classIntervals(x, n = 6, style = style)
  p <- cut2(x, cuts = p$brks)
  p
}
