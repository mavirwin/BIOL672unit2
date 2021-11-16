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

thesample = sample_n(penguin.data.int, 20, replace=FALSE) #20 samples from data, no value replacements
print(thesample)
print(dim(thesample)) #20 samples in 5 columns
print(length(thesample)) #5...columns?

# make sure class is a vector matching length of a single data column in training data frame
sam.species= thesample[,1] #got 20 samples from 1st column, which is species
print(sam.species)
print(dim(sam.species)) #NULL
print(length(sam.species)) #20...rows?


#start knn function, with Species as integers for distance based algorithm 
pred.knn = knn(thesample, penguin.data.int, sam.species, k=3, l = 0, prob = FALSE, use.all = TRUE)
#plot of thesample vs full data
plot211 =ggplot(thesample, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot212 =ggplot(thesample, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point()                                 
plot213 =ggplot(sub.pen.data, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot214 =ggplot(sub.pen.data, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point()                                 
theKNN = summary(pred.knn)
print(theKNN) 

#plot of knn 
plot215 =ggplot(sub.pen.data, aes(CD, CL, colour = pred.knn)) + geom_point()                                 
plot216 =ggplot(sub.pen.data, aes(FL, BM, colour = pred.knn)) + geom_point()                                 
library('grid')
pushViewport(viewport(layout = grid.layout(3, 2)))
print(plot211, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))

print(plot212, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot213, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot214, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(plot215, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(plot216, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
# confusion matrix
print(as.factor(as.integer(pred.knn)))
print(as.factor(Sp.int.fac))
matrix.knn = confusionMatrix(as.factor(as.integer(pred.knn)), as.factor(Sp.int.fac))
print(matrix.knn)

note1=cat("The #2 ID specie had only one individual data selected for the 20 samples in this current run, \n
possibly causing the underrepresentation of \n
          that specie.")

#save results
sink("unit2.1.knn.txt")
print(thesample)
print(matrix.knn)
print(note1)
sink()



