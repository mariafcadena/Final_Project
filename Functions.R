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

#CREATE THE DATAFRAME
CreateDataBase <- function(Cartier, Property, Price, Area, Pieces, Chambres)
{
  DataBase <- do.call(rbind.data.frame, Map('c', Cartier,Property, Price, Area, Pieces, Chambres))
  colnames(DataBase) <- c("CARTIER","PROPERTY", "PRICE","AREA", "PIECES", "CAHMBRES")
  return(DataBase)
}

data<-CreateDataBase(CARTIER2,PROPERTY2,PRICE2,AREA,PIECES,CHAMBRES)
