print("Assignment 2, Part 2")
cat("Compare support vector machine mehtods for classification.")

library("ggplot2")
library("palmerpenguins")
library("dplyr")
library("caret")
#library("class")
library("e1071")
library("kernalb")
library("liquidSVM")

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

#start the kernel tunings of the support vector machine 
svmtest = ksvm(as.matrix(thesample), thespecies, kernel= 'polydot') # tuned polynomial kernel
# svmtest = ksvm(as.matrix(thesample), thespecies, kernel= 'vanilladot') # tuned linear kernel
# svmtest = ksvm(as.matrix(thesample), thespecies, kernel= 'rbfdot') # tuned radial basis function
pred.svm = predict(svmtest, thesample, type='response')
#plot of thesample vs full data with integer Species as factor
plot211 =ggplot(thesample, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot212 =ggplot(thesample, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point() 
#plot of subset penguin data, which doesn't have species, but yet here's the integer Species as factor? 
plot213 =ggplot(sub.pen.data, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot214 =ggplot(sub.pen.data, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point()                                 

theSVM = summary(svmtest) 
print(theSVM) 

#plot of subset of penguin data
plot215 =ggplot(sub.pen.data, aes(CD, CL, colour = pred.svm)) + geom_point()                                 
plot216 =ggplot(sub.pen.data, aes(FL, BM, colour = pred.svm)) + geom_point()                                 

#print plots
library('grid')
pushViewport(viewport(layout = grid.layout(3, 2)))
print(plot211, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot212, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

print(plot213, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot214, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

print(plot215, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(plot216, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
# confusion matrix
pred.svm= round(pred.svm) #round to integer
print(as.factor(pred.svm))
print(as.factor(Sp.int.fac))

matrix.svm = confusionMatrix(as.factor(pred.svm), as.factor(Sp.int.fac))
print(matrix.svm)

cat("That's the kernal for us.")