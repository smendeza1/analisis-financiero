library(rvest)
library(tidyverse)
url <-  "http://www.polb.com/economics/stats/teus_archive.asp"
longbeachpage <- html(url) %>% 
  html_table(fill = TRUE)

longbeach_teus <- longbeachpage[[11]] 
titles <- longbeach_teus[1 ,] %>% 
  stringr::str_replace("\r","") %>% 
  stringr::str_replace("\n","") %>% 
  stringr::str_replace("Loaded","") %>% 
  stringr::str_trim() 
titles[1] <- "Date"
titles[4] <- "Loaded"
names(longbeach_teus) <- titles
longbeach_teus <- longbeach_teus[-1, ] %>% 
  as_tibble() 


limpiar_comas <- function(x){
  x <- x %>% 
    stringr::str_replace(",", "") %>% as.numeric()
  x
}

longbeach_teus <- longbeach_teus %>% 
  group_by(Date) %>% 
  mutate_each(funs(limpiar_comas)) %>% 
  separate(Date, into = c("Month", "Year"))

write_rds(x = longbeach_teus,"Demanda/Ports/longbeach.rds")
read_rds("Demanda/Ports/longbeach.rds")
