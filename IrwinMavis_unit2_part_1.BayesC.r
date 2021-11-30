print("Assignment 2, Part 1, bayes")

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
rnd = sample(1:330, 1)  #132L
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

#plot of thesample vs full data with integer Species as factor
plot211 =ggplot(thesample, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot212 =ggplot(thesample, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point() 
#plot of subset penguin data, which doesn't have species, but yet here's the integer Species as factor? 
plot213 =ggplot(sub.pen.data, aes(CD, CL, colour = as.factor(Sp.int))) + geom_point()
plot214 =ggplot(sub.pen.data, aes(FL, BM, colour = as.factor(Sp.int))) + geom_point()                                 
#Bayes summary
thebayes = summary(test) 

print(thebayes) 

#plot of subset of penguin data
plot215 =ggplot(sub.pen.data, aes(CD, CL, colour = pred.bayes)) + geom_point()                                 
plot216 =ggplot(sub.pen.data, aes(FL, BM, colour = pred.bayes)) + geom_point()                                 

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
print(as.factor(as.integer(pred.bayes)))
print(as.factor(Sp.int.fac))
matrix.bayes = confusionMatrix(as.factor(as.integer(pred.bayes)), as.factor(Sp.int.fac))
print(matrix.bayes)

note1=cat("Comparing to KNN, the Bayes approach was able to discern between species 1 and 2 better.\n
          Let's use bayes for the 5-groupings procedure.")

#save results
sink("unit2.1.bayes.txt")
print(thesample)
print(matrix.bayes)
print(note1)
sink()

#Start the 5 grouping procedure
#A--shuffle the dataset randomly--
note2= cat("The set.seed(rnd) is on line 57.")

#split the dataset into k groups, using the 5-fold cross-validation method
#reference: https://www.geeksforgeeks.org/cross-validation-in-r-programming/

#make 5-fold cross validation by random sampling
#train.con.bayes= trainControl(method="nb", number=5) #"nb" is not a recognized resampling method? 
train.con.bayes= trainControl(method="cv", number=5, repeats=5) 
print(train.con.bayes)


#create four groups of temporary train sets using 20% of dataset (p=0.2)
sub.train <- 1
for (sub.train in 1:4)
{
  random.select=createDataPartition(penguin.data.int$Sp.int, p=0.2, list= TRUE)
  sub.train= penguin.data.int[random.select,]
  sub.train.group <-rename(sub.train ???????????)
  
  sub.train <- sub.train + 1
}

#create testing set
sub.test= penguin.data.int[-random.select,]
#x1= subset(penguin.data.int, select=-Sp.int, header= TRUE)
#x2=subset(train.set.bayes, select=-Sp.int, header=TRUE)
x2=sub.test, select=-Sp.int, header=TRUE
#print(x1)
#typeof(x1)
print(x2)
#typeof(x2)
y=as.factor(sub.test$Sp.int)

#training the model 
library("klaR")
model.bayes= train(x2,y, 
                   method="nb", #native Bayes
                   trControl= train.con.bayes)
print(model.bayes)









