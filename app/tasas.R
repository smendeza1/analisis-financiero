library(rvest)
url <- "http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/totalbeta.html"
url2 <- "http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/histretSP.html"

safe_read <- safely(read_html)
prueba <- safe_read(url)

if (is_null(prueba$result)) {
  betas <- readRDS("app/Datos/Intermodal/betas2017-01-25.rds")
  tasas <- readRDS("app/Datos/Intermodal/tasas2017-01-25.rds")
  
} else{
  betas <- read_html(url) %>% 
    html_nodes(xpath = "/html/body/table") %>% 
    html_table()
  
  fecha.consulta <- Sys.Date()
  flname <- paste0("app/Datos/Intermodal/betas",fecha.consulta,".rds")
  saveRDS(object = betas ,file = flname)
  
  tasas <- read_html(url2) %>% 
    html_table()
  
  flname <- paste0("app/Datos/Intermodal/tasas",fecha.consulta,".rds")
  saveRDS(object = tasas ,file = flname)
}


betas <- as.data.frame(betas)
class(betas)
names(betas) <- betas[1,]
betas <- betas[c(91,92),c(1,3)]
beta <- betas$`Average Unlevered Beta` %>% 
  as.numeric() %>% 
  mean()




tasas <- as.data.frame(tasas)
names(tasas) <- tasas[2,]

tasas  <- tasas[3:nrow(tasas),] 
tasas.promedio <- tail(tasas,9) %>% head(4)
nombres <- c("periodo","SP500","3month.bill","10year.bond")
tasas <- tasas.promedio[1:4] 
names(tasas) <- nombres
tasas <- tasas[2:nrow(tasas),]

rm <- tasas[3,2]
rf <- tasas[3,4]
rm <- rm %>% str_replace("%","")
rm <- as.numeric(rm)/100

rf <- rf %>% str_replace("%","")
rf <- as.numeric(rf)/100




