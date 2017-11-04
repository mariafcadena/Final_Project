#IDENTIFIES THE TYPE OF PROPERTY IN EACH ELEMENT OF THE VECTOR
library(stringi)
library(stringr)
library(reshape2)

FixProperty <- function(Property)
{
  a<-unlist(str_locate_all(pattern ='Appartement', Property)) 
  if (length(a)>0){NewProperty<-'Appartement'} else { NewProperty<-'Maison' }
  return(NewProperty)
}

CleanProperties <- function(Vector)
{
  NewVec<-lapply(Vector,FixProperty)
  return(NewVec)
}
PROPERTY2<-CleanProperties(PROPERTY)

#IDENTIFIES THE NEIGHBOURHOOD OF PROPERTY IN EACH ELEMENT OF THE VECTOR
CleanCartier <- function(Vector)
{
  NewVec<-lapply(Vector,function(x){stri_trans_general(x,"Latin-ASCII")})
  return(NewVec)
}

#FIXES THE PRICE ON ONE ELEMENT
FixPrice <- function(Price)
{
  P1<-stri_trans_general(trimws(gsub("^\\s*<U\\+\\w+>|-", " ", Price)),"Latin-ASCII")
  P2<-regmatches(P1,gregexpr(" ", P1),TRUE)
  FinalPrice=""
  for (i in 1:(length(P2[[1L]])-1))
  {
    if (grepl("[^a-zA-Z]", P2[[1L]][i])==TRUE){FinalPrice=paste(FinalPrice,P2[[1L]][i],sep="")}
  }
  return(as.numeric(FinalPrice))
}

#CLEANS THE PRICE ON EVERY ITEM OF THE VECTOR
CleanPrice <- function(Vector)
{
  NewVec<-lapply(Vector,FixPrice)
  return(NewVec)
}
PRICE2<-CleanPrice(PRICE)
#FIXES THE VALUES OF ALL THE CHARACTERISTICS OF THE PROPERTY
FixCaract <- function(Caract, Type)
{
  P1  <-  stri_trans_general(trimws(gsub("^\\s*<U\\+\\w+>|-", " ", Caract)),"Latin-ASCII")
  if (Type=="piece")
  {
    P2  <-  regmatches(P1,gregexpr("p", P1),TRUE)
    NewCaract <- trimws(P2[[1L]][1], "both")
  }
  if (Type=="chambre")
  {
    P2  <-  regmatches(P1,gregexpr("p", P1),TRUE)
    P3  <-  regmatches(P2[[1L]][2],gregexpr("chb", P2[[1L]][2]),TRUE)
    if (length(P3[[1L]])==1){NewCaract<-0}
    else {NewCaract<-stri_trans_general(trimws(gsub("^\\s*<U\\+\\w+>|-", " ", P3[[1L]][1])),"Latin-ASCII")}
  }
  if (Type=="area")
  {
    P2  <-  regmatches(P1,gregexpr("chb", P1),TRUE)
    if (length(P2[[1L]])==1){P2  <-  regmatches(P1,gregexpr("p", P1),TRUE)}
    P3  <-  regmatches(P2[[1L]][2],gregexpr("m", P2[[1L]][2]),TRUE)
    NewCaract<-stri_trans_general(trimws(gsub("^\\s*<U\\+\\w+>|-", " ", P3[[1L]][1])),"Latin-ASCII")
  }
  if (grepl(",",NewCaract)==TRUE){NewCaract<-gsub(",", ".", NewCaract)}
  options(digits=5)
  return(as.numeric(NewCaract))
}

#CLEANS THE PIECES ON EVERY ITEM OF THE VECTOR
CleanPiece <- function(Vector)
{
  NewVec<-lapply(Vector, FixCaract, Type="piece")
  return(NewVec)
}

#CLEANS THE NUMBER OF ROOMS ON EVERY ITEM OF THE VECTOR
CleanChambre <- function(Vector)
{
  NewVec<-lapply(Vector, FixCaract, Type="chambre")
  return(NewVec)
}

#CLEANS THE AREA ON EVERY ITEM OF THE VECTOR
CleanArea <- function(Vector)
{
  NewVec<-lapply(Vector, FixCaract, Type="area")
  return(NewVec)
}

GetPages <-function(WEB_PAGE)
{
  PRE_PAGES  <- WEB_PAGE %>% html_nodes(".u-500") %>% html_text()
  PRE_PAGES2<-stri_trans_general(gsub("^\\s*<U\\+\\w+>|-", " ", PRE_PAGES[[1L]][1]),"Latin-ASCII")
  if (length(PRE_PAGES2[[1L]])>1)
  {PRE_PAGES3<-paste(PRE_PAGES2[[1L]][1],PRE_PAGES2[[1L]][2],sep="")}
  else
  {PRE_PAGES3<-PRE_PAGES2[[1L]][1]}
  return(floor(as.numeric(PRE_PAGES3)/20))
}

#CREATE THE DATAFRAME
CreateDataBase <- function(Cartier, Property, Price, Area, Pieces, Chambres)
{
  DataBase <- do.call(rbind.data.frame, Map('c', Cartier,Property, Price, Area, Pieces, Chambres))
  colnames(DataBase) <- c("CARTIER","PROPERTY", "PRICE","AREA", "PIECES", "CAHMBRES")
  return(DataBase)
}

ScrapsInfo <-function(link)
{
  WEB_PAGE <- read_html(link)
  NO_PAGES <- GetPages(WEB_PAGE)
  link<-paste(link,"&LISTING-LISTpg=",sep="")
  for (i in 1:NO_PAGES)
  {
    test<-paste(link,i,sep="")
    WEB_PAGE <- read_html(test)
    PRE_CARTIER  <- WEB_PAGE %>% html_nodes(".c-pa-city") %>% html_text()
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
  return(results)
}

