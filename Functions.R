#IDENTIFIES THE TYPE OF PROPERTY IN EACH ELEMENT OF THE VECTOR
CleanProperties <- function(Vector)
{
  NewVec<- Vector
  for (i in 1:length(Vector))
  {
    a<-unlist(str_locate_all(pattern ='Appartement', Vector[i])) 
    if (length(a)>0){NewVec[i]<-'Appartement'} else { NewVec[i]<-'Maison' }
  }
  return(NewVec)
}

CleanCartier <- function(Vector)
{
  NewVec<-lapply(Vector,function(x){stri_trans_general(x,"Latin-ASCII")})
  return(NewVec)
}

#FIXES THE PRICE ON ONE ELEMENT
FixedPrice <- function(Price)
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

#CLEANS DE PRICE ON EVERY ITEM OF THE VECTOR
CleanPrice <- function(Vector)
{
  NewVec<-lapply(Vector,FixedPrice)
  return(NewVec)
}