print("Assignment 2, Part 4, adaboost")

library("ggplot2")
library("palmerpenguins")
library("dplyr")
library("caret")
library("ada")


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
thespecies= thesample[5:5]
print(thespecies)

#adaptive boosting classifier
#install.packages("rpart")
library("rpart")

thetest = boosting(Sp.int~CD+CL+FL+BM, thesample, boos=TRUE)
thepred= predict.bagging(thetest, data=sub.pen.data)
theada = summary(thetest)
print(theada)

#plot of thesample vs full data with integer Species as factor
plot241 =ggplot(thesample, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot242 =ggplot(thesample, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point() 
#plot of subset penguin data, which doesn't have species, but yet here's the integer Species as factor? 
plot243 =ggplot(sub.pen.data, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot244 =ggplot(sub.pen.data, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point()                                 

#plot of subset of penguin data
plot245 =ggplot(sub.pen.data, aes(CD, CL, colour = thepred)) + geom_point()                                 
plot246 =ggplot(sub.pen.data, aes(FL, BM, colour = thepred)) + geom_point()                                 

#print plots
library('grid')
pushViewport(viewport(layout = grid.layout(3, 2)))
print(plot241, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot242, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

print(plot243, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot244, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

print(plot245, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(plot246, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))

# confusion matrix
print(as.factor(thepred))
print(as.factor(Sp.int.fac))

thematrix = confusionMatrix(as.factor(thepred), as.factor(Sp.int.fac))
print(thematrix)

note1=cat("")