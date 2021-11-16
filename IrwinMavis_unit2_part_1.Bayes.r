print("Assignment 2, Part 1, KNN")

library("ggplot2")
library("palmerpenguins")
library("dplyr")
library("caret")
library("class")

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

#species as integar with ID numbers
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
rnd = sample(1:150, 1)  #132L??
set.seed(rnd)

#naive Bayes, using the e1071 library
library("e1071")

thesample = sample_n(penguin.data.int, 50, replace=FALSE)
print(thesample)

thespecies = thesample[5:5]
print(thespecies)
#start naivebayes function, with species as integers
test <-  naiveBayes(Sp.int~CD+CL+FL+BM, thesample, laplace = 0) # naive Bayes classifier
pred.bayes <- predict(test, sub.pen.data, probability = FALSE, decision.values = TRUE)






