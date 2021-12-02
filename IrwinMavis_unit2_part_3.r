print("Assignment 2, Part 3, neural network")

library("ggplot2")
library("palmerpenguins")
library("dplyr")
library("caret")
#library("class")
library("e1071")
library("kernlab")
library("neuralnet")
library("keras")
library("tensorflow")


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
CL=as.integer(data.na.out2$Culmen.Length..mm.)
CD=as.integer(data.na.out2$Culmen.Depth..mm.)
FL= as.integer(data.na.out2$Flipper.Length..mm.)
BM= as.integer(data.na.out2$Body.Mass..g.)
Sex=as.factor(data.na.out2$Sex)

#species as integar with ID numbers
Sp.int=data.na.out2$Sp.as.int
print(data.na.out2$Sp.as.int)
typeof(Sp.int)

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

#set the random shuffle of the dataframe
rnd = sample(1:330, 1)  
set.seed(rnd)

thesample = sample_n(penguin.data.int, 80, replace=FALSE) 
print(thesample)
thespecies= thesample[1:1]
print(thespecies)

#run neural network classifier
thetest = neuralnet(Sp.int~CL+CD+FL+BM, thesample, hidden = 3, linear.output = FALSE)
print(thetest)
print(plot(thetest))
thepred = predict(thetest, sub.pen.data, rep=1, all.units = FALSE)
print(round(thepred))

#return column with maximum value
library(data.table)
print(max.col(thepred)) 

#plot of thesample vs full data with integer Species as factor
plot231 =ggplot(thesample, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot232 =ggplot(thesample, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point() 
#plot of subset penguin data, which doesn't have species, but yet here's the integer Species as factor? 
plot233 =ggplot(sub.pen.data, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot234 =ggplot(sub.pen.data, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point()                                 

#plot of subset of penguin data
plot235 =ggplot(sub.pen.data, aes(CD, CL, colour = max.col(thepred))) + geom_point()                                 
plot236 =ggplot(sub.pen.data, aes(FL, BM, colour = max.col(thepred))) + geom_point()                                 

#print plots
library('grid')
pushViewport(viewport(layout = grid.layout(3, 2)))
print(plot231, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot232, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

print(plot233, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot234, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

print(plot235, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(plot236, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
# confusion matrix
print(max.col(thepred))
print(as.integer(Sp))
thematrix = confusionMatrix(as.factor(max.col(thepred)), as.factor(as.integer(Sp)))
print(thematrix)
print(plot(thetest))

note1=cat("I am not sure if the max.col results are supposed to be all 1 or not.")

#save summary
sink("C:/Users/Videosystem/Documents/GitHub/BIOL672unit2/simple.neural.net.txt")
print(thematrix)
sink()




