#EL LOOP AUN NO FUNCION, HAY QUE ENCONTRAR BIEN COMO SE INDENTAN LAS PAGINAS
install.packages("rvest")
library(rvest) 

for (i in 1:2){
  test<-cat(sep="","http://www.seloger.com/list.htm?tri=initial&idtypebien=2,1&idtt=2&ci=750056&LISTING-LISTpg=",i,"&naturebien=1,2,4")
  link <-str_sub(test,1,-4)
  my_page <- read_html(link) 
  print(i)
}

#ASI SE TIENEN LOS VECTORES DE CARACTERITICAS PARA LOS APARTAMENTOS EN LA LISTA DE UNA PAGINA
WEB_PAGE <- read_html("http://www.seloger.com/list.htm?tri=initial&idtypebien=2,1&idtt=2&ci=750056&naturebien=1,2,4")
PROPERTY <- WEB_PAGE %>% html_nodes(".c-pa-link") %>% html_text()
CARTIER  <- WEB_PAGE %>% html_nodes(".c-pa-city") %>% html_text()
PRICE    <- WEB_PAGE %>% html_nodes(".c-pa-price") %>% html_text()
CARACT   <- WEB_PAGE %>% html_nodes(".c-pa-criterion") %>% html_text()