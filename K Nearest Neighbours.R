#########################################################
## data Mining I HW2
## Subhashchandra Babu Madineni   UBIT = 50373860
## Created on 28th Sept
## Edited: 
#########################################################
rm(list = ls())

#install.packages("ggplot2")
#install.packages("class")
library("ggplot2")
require(class)
library(class)

#########################################################
# Loading th dataset
#########################################################
#setwd("setwd("C:/University At Buffalo Fall 2020 Classes/EAS 506-CDA 541Stastical Data Mining/R for STA/HW2")")
load("C:/University At Buffalo Fall 2020 Classes/EAS 506-CDA 541Stastical Data Mining/R for STA/HW2/zip.test.RData") 
load("C:/University At Buffalo Fall 2020 Classes/EAS 506-CDA 541Stastical Data Mining/R for STA/HW2/zip.train.RData",verbose = TRUE) 

dim(zip.train)
dim(zip.test)

################################################
# Splitting the dataset into X & Y
###############################################
dats = as.data.frame(zip.train)
dats_test = as.data.frame(zip.test)
X_train = data.frame(zip.train[,2:257])
Y_train = data.frame(zip.train[,1])


X_train = zip.train[,2:257]
Y_train = zip.train[,1]


X_test = zip.test[,2:257]
Y_test = zip.test[,1]


################################################
# Bulding the linear regression Model
###############################################

regressor = lm(formula = dats$V1  ~., data = dats)
summary(regressor)


#install.packages('Metrics')
library(Metrics)

mse(dats_test$V1,predict(regressor, newdata = dats_test))     # Test_MSE = 3.754905

################################################
# Building the model for knn
###############################################

error.knn=c()
for (n in c(1,3,5,7,9,11,13,15))
{
  knn.predict=knn(X_train,X_test,Y_train,k=n)
  e=mean(knn.predict!=Y_test)
  error.knn=c(error.knn,e)

}

knn.x <- c(1,3,5,7,9,11,13,15)
knn <- data.frame(x=knn.x,y=error.knn)

prediction = as.matrix(knn.predict=knn(X_train,X_test,Y_train,k=1))


as.matrix()

e=mean(knn.predict!=Y_train)

confusion_matrix = table(knn.predict,Y_train)


