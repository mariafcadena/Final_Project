#EL LOOP AUN NO FUNCION, HAY QUE ENCONTRAR BIEN COMO SE INDENTAN LAS PAGINAS
install.packages("rvest")
library(rvest) 

#Create the matrix that gives the min and max of surface to create pages with less than 
#2000 properties, limit to be able to scrapp information in Selonger.com

m = matrix(c(5,10,20,30,40,50,60,70,80,100,150,200,9,19,29,39,49,59,69,79,99,149,199,400), nrow=12, ncol=2)
links =NULL
for (i in 1:12)
{
  p1 <-"http://www.seloger.com/list.htm?idtt=2&naturebien=1,2,4&idtypebien=1,2&ci=750056&tri=initial&"
  p2 <- paste("surfacemin=",as.character(m[i,1]), sep="")
  p3 <- paste("&surfacemax=",as.character(m[i,2]), sep="")
  link=paste(paste(p1,p2, sep=""),p3, sep="")
  links=append(links, link)
}

#Loop through Areas
for (i in 1:12)
{
  Data <-ScrapsInfo(links[i])
  if (i==1){ParisData<-Data}
  else {ParisData<-rbind(ParisData, Data)}
}

links[2]

x_var <- rnorm( n = 15, mean = 5, sd = 2)
y_var <- x_var + rnorm(n = 15, mean = 5, sd =4)
size_var <- runif(15, 1,10)

df.test_data <- data.frame(x_var, y_var, size_var)
library(ggplot2)

# PLOT THE DATA USING GGPLOT2
ggplot(data=Data, aes(x=CARTIER, y=AREA_GROUP)) +
  geom_point(aes(size=PRICE)) 