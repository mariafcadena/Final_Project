#EL LOOP AUN NO FUNCION, HAY QUE ENCONTRAR BIEN COMO SE INDENTAN LAS PAGINAS
install.packages("rvest")
library(rvest) 

for (i in 1:3)
{
  test<-paste("http://www.seloger.com/list.htm?tri=initial&idtypebien=2,1&idtt=2&ci=750056&naturebien=1,2,4&LISTING-LISTpg=",i, sep="")
  WEB_PAGE <- read_html(test)
  PRE_CARTIER  <- WEB_PAGE %>% html_nodes(".c-pa-city") %>% html_text()
  PRE_PROPERTY <- WEB_PAGE %>% html_nodes(".c-pa-link") %>% html_text()
  PRE_PRICE    <- WEB_PAGE %>% html_nodes(".c-pa-price") %>% html_text()
  PRE_CARACT   <- WEB_PAGE %>% html_nodes(".c-pa-criterion") %>% html_text()
  
  CARTIER  <- CleanCartier(PRE_CARTIER)
  PROPERTY <- CleanProperties(PRE_PROPERTY)
  PRICE    <- CleanPrice(PRE_PRICE)
  AREA     <- CleanArea(PRE_CARACT)
  PIECES   <- CleanPiece(PRE_CARACT)
  CHAMBRES <- CleanChambre(PRE_CARACT)
  
  data<-CreateDataBase(CARTIER2,PROPERTY2,PRICE2,AREA,PIECES,CHAMBRES)
  
  if (i==1){results<-data}
  else {results<-rbind(results, data)}
  
}

#ASI SE TIENEN LOS VECTORES DE CARACTERITICAS PARA LOS APARTAMENTOS EN LA LISTA DE UNA PAGINA
WEB_PAGE <- read_html("http://www.seloger.com/list.htm?tri=initial&idtypebien=2,1&idtt=2&ci=750056&naturebien=1,2,4")
PROPERTY <- WEB_PAGE %>% html_nodes(".c-pa-link") %>% html_text()
CARTIER  <- WEB_PAGE %>% html_nodes(".c-pa-city") %>% html_text()
PRICE    <- WEB_PAGE %>% html_nodes(".c-pa-price") %>% html_text()
CARACT   <- WEB_PAGE %>% html_nodes(".c-pa-criterion") %>% html_text()