library(comtrade)
# Obtener lista de paises
paises<-jsonlite::fromJSON("http://comtrade.un.org/data/cache/partnerAreas.json")
paises <-paises$results
paises <- paises[3:nrow(paises),]
# Primeros 40 paises
ls_reporters <- paises$id[1:40]
comtrade_data <- lapply(ls_reporters, function(i) {
  Sys.sleep(1)
  tmp <- get.Comtrade(r=i,fmt="csv",ps="2015")
  df <- as.data.frame(do.call(rbind, tmp))
  return(df)
})
# EVEN ROW BIND TO DATAFRAME (IF ELEMENTS HAVE SAME EXACT ONE-LEVEL DATA STRUCTURE)
# Primer data frame, lo utilizo para adjuntar las siguientes iteraciones de paises 
df <- dplyr::bind_rows(comtrade_data)

ls_reporters <- paises$id[41:80]
comtrade_data <- lapply(ls_reporters, function(i) {
  Sys.sleep(1)
  tmp <- get.Comtrade(r=i,fmt="csv",ps="2015")
  df <- as.data.frame(do.call(rbind, tmp))
  return(df)
})
# EVEN ROW BIND TO DATAFRAME (IF ELEMENTS HAVE SAME EXACT ONE-LEVEL DATA STRUCTURE)
# Primer data frame, lo utilizo para adjuntar las siguientes iteraciones de paises 
tmp <- dplyr::bind_rows(comtrade_data)
df <- bind_rows(df,tmp)
Sys.sleep(10)
ls_reporters <- paises$id[81:120]
comtrade_data <- lapply(ls_reporters, function(i) {
  Sys.sleep(1)
  tmp <- get.Comtrade(r=i,fmt="csv",ps="2015")
  df <- as.data.frame(do.call(rbind, tmp))
  return(df)
})
# EVEN ROW BIND TO DATAFRAME (IF ELEMENTS HAVE SAME EXACT ONE-LEVEL DATA STRUCTURE)
# Primer data frame, lo utilizo para adjuntar las siguientes iteraciones de paises 
tmp <- dplyr::bind_rows(comtrade_data)
df <- bind_rows(df,tmp)
Sys.sleep(10)
ls_reporters <- paises$id[121:140]
comtrade_data <- lapply(ls_reporters, function(i) {
  Sys.sleep(1)
  tmp <- get.Comtrade(r=i,fmt="csv",ps="2015")
  df <- as.data.frame(do.call(rbind, tmp))
  return(df)
})
# EVEN ROW BIND TO DATAFRAME (IF ELEMENTS HAVE SAME EXACT ONE-LEVEL DATA STRUCTURE)
# Primer data frame, lo utilizo para adjuntar las siguientes iteraciones de paises 
tmp <- dplyr::bind_rows(comtrade_data)
df <- bind_rows(df,tmp)
Sys.sleep(10)
ls_reporters <- paises$id[141:180]
comtrade_data <- lapply(ls_reporters, function(i) {
  Sys.sleep(1)
  tmp <- get.Comtrade(r=i,fmt="csv",ps="2015")
  df <- as.data.frame(do.call(rbind, tmp))
  return(df)
})
# EVEN ROW BIND TO DATAFRAME (IF ELEMENTS HAVE SAME EXACT ONE-LEVEL DATA STRUCTURE)
# Primer data frame, lo utilizo para adjuntar las siguientes iteraciones de paises 
tmp <- dplyr::bind_rows(comtrade_data)
df <- bind_rows(df,tmp)
Sys.sleep(10)
ls_reporters <- paises$id[181:220]
comtrade_data <- lapply(ls_reporters, function(i) {
  Sys.sleep(1)
  tmp <- get.Comtrade(r=i,fmt="csv",ps="2015")
  df <- as.data.frame(do.call(rbind, tmp))
  return(df)
})
# EVEN ROW BIND TO DATAFRAME (IF ELEMENTS HAVE SAME EXACT ONE-LEVEL DATA STRUCTURE)
# Primer data frame, lo utilizo para adjuntar las siguientes iteraciones de paises 
tmp <- dplyr::bind_rows(comtrade_data)
df <- bind_rows(df,tmp)
Sys.sleep(10)
ls_reporters <- paises$id[221:240]
comtrade_data <- lapply(ls_reporters, function(i) {
  Sys.sleep(1)
  tmp <- get.Comtrade(r=i,fmt="csv",ps="2015")
  df <- as.data.frame(do.call(rbind, tmp))
  return(df)
})
# EVEN ROW BIND TO DATAFRAME (IF ELEMENTS HAVE SAME EXACT ONE-LEVEL DATA STRUCTURE)
# Primer data frame, lo utilizo para adjuntar las siguientes iteraciones de paises 
tmp <- dplyr::bind_rows(comtrade_data)
df <- bind_rows(df,tmp)
Sys.sleep(10)
ls_reporters <- paises$id[241:280]
comtrade_data <- lapply(ls_reporters, function(i) {
  Sys.sleep(1)
  tmp <- get.Comtrade(r=i,fmt="csv",ps="2015")
  df <- as.data.frame(do.call(rbind, tmp))
  return(df)
})
# EVEN ROW BIND TO DATAFRAME (IF ELEMENTS HAVE SAME EXACT ONE-LEVEL DATA STRUCTURE)
# Primer data frame, lo utilizo para adjuntar las siguientes iteraciones de paises 
tmp <- dplyr::bind_rows(comtrade_data)
df <- bind_rows(df,tmp)

Sys.sleep(10)

ls_reporters <- paises$id[281:291]
comtrade_data <- lapply(ls_reporters, function(i) {
  Sys.sleep(1)
  tmp <- get.Comtrade(r=i,fmt="csv",ps="2015")
  df <- as.data.frame(do.call(rbind, tmp))
  return(df)
})
# EVEN ROW BIND TO DATAFRAME (IF ELEMENTS HAVE SAME EXACT ONE-LEVEL DATA STRUCTURE)
# Primer data frame, lo utilizo para adjuntar las siguientes iteraciones de paises 
tmp <- dplyr::bind_rows(comtrade_data)
df <- bind_rows(df,tmp)

write_csv(df,"Informacion de Apoyo/Datos/trade2015.csv")

