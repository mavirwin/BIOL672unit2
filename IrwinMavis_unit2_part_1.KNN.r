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

#settings for selecting rows of data using "random numbers distribution" (rnd)
rnd = sample(1:330, 1)  #one row select from the full 330 rows dataframe
set.seed(rnd) #used to generate same set of random numbers/samples of dataframe

#Question: Unclear why and how set.seed affect the sampling of data in line 63
#20 samples from data with no value replacements
thesample = sample_n(penguin.data.int, 20, replace=FALSE) 
print(thesample)
print(dim(thesample)) #20 samples in 5 columns
print(length(thesample)) #5 columns

# make sure class is a vector matching length of a single data column in training data frame
sam.species= thesample[,1] #got 20 samples from 1st column, which is species
print(sam.species)
print(dim(sam.species)) #assume result is NULL, as species is in just one column
print(length(sam.species)) #20 rows


#start knn function, with Species (three types, k=3) as integers for distance based algorithm
#l=0 is minimum vote for definite decision, otherwise doubt, even if k is increased by ties.
pred.knn = knn(thesample, penguin.data.int, sam.species, k=3, l = 0, prob = FALSE, use.all = TRUE)

#plot of thesample vs full data with integer Species as factor
plot211 =ggplot(thesample, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot212 =ggplot(thesample, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point() 
#plot of subset penguin data, which doesn't have species, but yet here's the integer Species as factor? 
plot213 =ggplot(sub.pen.data, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot214 =ggplot(sub.pen.data, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point()                                 
#not sure about this theKNN result because...
theKNN = summary(pred.knn) #example has the "mytest" thing, but it appeared to be an afterthought deletion

print(theKNN) 

#plot of subset of penguin data
plot215 =ggplot(sub.pen.data, aes(CD, CL, colour = pred.knn)) + geom_point()                                 
plot216 =ggplot(sub.pen.data, aes(FL, BM, colour = pred.knn)) + geom_point()                                 

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
print(as.factor(as.integer(pred.knn)))
print(as.factor(Sp.int.fac))
matrix.knn = confusionMatrix(as.factor(as.integer(pred.knn)), as.factor(Sp.int.fac))
print(matrix.knn)

note1=cat("The #2 ID specie had only one individual data selected for the 20 samples in the run last week, \n
possibly causing the underrepresentation of that specie. The code is re-run again today (Nov 23), randomly selecting more species ID 2 \n
          to work with. With species as the factor, both subdata (20 samples) and full data (330 samples)
          are showing dots as correlated by species. Now, when using KNN for CL vs CD and BM vs FL, there is an non-surprising mixture of species ID 1 and 2, \n
          because they have similar measurements in three of four variances. \n
          Overall, the matrix.knn summary shows that the species 2's measured data classification is less certain than for species 1 and 3,\n
          suggesting that maybe more re-run trainings (I am thinking bootstrapping), hopefully making the separation clearer. \n 
          as for species 1 and 3.")

#save results
sink("unit2.1.knn.txt")
print(thesample)
print(matrix.knn)
print(note1)
sink()



