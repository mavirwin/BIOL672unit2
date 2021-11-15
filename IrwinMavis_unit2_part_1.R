print("Assignment 2, Part 1")

library("ggplot2")
library("palmerpenguins")
library("dplyr")
library("caret")

data=read.csv("https://raw.githubusercontent.com/netuohcs/BiBC_essentials_20200916/master/data/penguins_lter.csv")
data.int=read.csv("https://raw.githubusercontent.com/mavirwin/raw-data/main/penguins_lter.integer.csv")
#data=read.table("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguins_lter.txt", header=TRUE, sep="\t", )
#setwd("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive")
getwd()

#remove NA observations
data.na.out2=na.omit(data.int)

#listing of columns
Sp=as.factor(data.na.out2$Species) #using names
Is=as.factor(data.na.out2$Island)
CL=data.na.out2$Culmen.Length..mm.
CD=data.na.out2$Culmen.Depth..mm.
FL= data.na.out2$Flipper.Length..mm.
BM= data.na.out2$Body.Mass..g.
Sex=as.factor(data.na.out2$Sex)

#species as integar 
Sp.int=data.na.out2$Sp.as.int
print(data.na.out2$Sp.as.int)

#species as factor, but are ID numbers
Sp.int.fac=as.factor(data.na.out2$Sp.as.int)
print(Sp.int.fac)

#multvariance with Specie IDs as factor
penguin.data=data.frame(
  Sp.int.fac,#categorical variable
  CL,
  CD,
  FL,
  BM
)

#multvariance with Specie IDs as integer
penguin.data.int=data.frame(
  Sp.int, #categorical variable
  CL,
  CD,
  FL,
  BM
)

print(penguin.data)
sub.pen.data=data.frame(CL,CD,FL,BM)  #categorical Variance not included
print(sub.pen.data)

#settings
rnd <- sample(1:150, 1)
set.seed(rnd)

thesample = sample_n(penguin.data.int, 20, replace=FALSE)
print(thesample)
print(dim(thesample))
print(length(thesample))

# make sure class is a vector matching length of a single data column in training data frame
Sp.sample = thesample[,5]
print(thespecies)
print(dim(thespecies))
print(length(thespecies))


#start knn function, with Species as integers for distance based algorithm
mypred <- knn(mysample, dataframe, myspecies, k=3, l = 0, prob = FALSE, use.all = TRUE)
#mypred <- predict(mytest, dataframe, probability = FALSE, decision.values = TRUE)
myplot1 <-ggplot(mysample, aes(PL, PW, colour = as.factor(Sp))) + geom_point()
myplot2 <-ggplot(mysample, aes(SL, SW, colour = as.factor(Sp))) + geom_point()                                 
myplot3 <-ggplot(subframe, aes(PL, PW, colour = as.factor(Sp))) + geom_point()
myplot4 <-ggplot(subframe, aes(SL, SW, colour = as.factor(Sp))) + geom_point()                                 
myKNN <- summary(mytest)
print(myKNN)
myplot5 <-ggplot(subframe, aes(PL, PW, colour = mypred)) + geom_point()                                 
myplot6 <-ggplot(subframe, aes(SL, SW, colour = mypred)) + geom_point()                                 
library('grid')
pushViewport(viewport(layout = grid.layout(3, 2)))
print(myplot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(myplot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(myplot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(myplot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(myplot5, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(myplot6, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
# confusion matrix
print(as.factor(as.integer(mypred)))
print(as.factor(Sp))
mymatrix <- confusionMatrix(as.factor(as.integer(mypred)), as.factor(Sp))
print(mymatrix)



