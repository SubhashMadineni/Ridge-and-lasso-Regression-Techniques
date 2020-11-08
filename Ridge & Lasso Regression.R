#########################################################
## data Mining I HW2
## Subhashchandra Babu Madineni   UBIT = 50373860
## Created on 25th Sept
## Edited: 
#########################################################
rm(list = ls())


#########################################################
# Installing the Packages
#########################################################
#setwd("setwd("C:/University At Buffalo Fall 2020 Classes/EAS 506-CDA 541Stastical Data Mining/R for STA/HW2")")
#install.packages("ISLR")
#install.packages("glmnet")
#install.packages("tidyverse")
library("ISLR")
library("tidyverse")
library(glmnet)
#########################################################
# Loading th dataset
#########################################################
#install.packages("ISLR")
#install.packages("tidyverse")

?College

dats <- College

#########################################################
# Removing the Name Variable and assigning numbers to the data
#########################################################
dats <- dats %>% mutate(id = row_number())


#########################################################
# Encoding the Categorical Variable "Private"
#########################################################
dats$Private = factor(dats$Private,
                      levels = c("Yes","No"),
                      labels = c(1,0))

#########################################################
# Density Plotting the dataset for
#           checking the normal Distrubution of the data 
#########################################################

# Before Transformation
par(mfrow = c(3,4))
hist( dats$Accept ,main ="Accept")
hist( dats$Apps,main ="Apps")
hist( dats$Enroll,main ="Enroll")
hist( dats$F.Undergrad,main ="F.Undergrad")
hist( dats$P.Undergrad,main ="P.Undergrad")
hist( dats$Room.Board,main ="Room.Board")
hist( dats$Books,main ="Books")
hist( dats$Personal,main ="Personal")
hist( dats$PhD,main ="PhD")
hist( dats$Terminal,main ="Terminal")
hist( dats$Expend,main ="Expend")


#########################################################
# Tranforming the Variables that are not normally Distrubuted 
#########################################################
dats$Accept <- replace(dats$Accept,dats$Accept>0,log(dats$Accept+1))
dats$Apps <- replace(dats$Apps,dats$Apps>0,log(dats$Apps+1))
dats$Enroll <- replace(dats$Enroll,dats$Enroll>0,log(dats$Enroll+1))
dats$F.Undergrad <- replace(dats$F.Undergrad,dats$F.Undergrad>0,log(dats$F.Undergrad+1))
dats$P.Undergrad <- replace(dats$P.Undergrad,dats$P.Undergrad>0,log(dats$P.Undergrad+1))
dats$Books <- replace(dats$Books,dats$Books>0,log(dats$Books+1))
dats$Personal <- replace(dats$Personal,dats$Personal>0,log(dats$Personal+1))
dats$Expend <- replace(dats$Expend,dats$Expend>0,log(dats$Expend+1))

################################################
# After Transformation
###############################################
par(mfrow = c(3,4))
hist( dats$Accept ,main ="Accept")
hist( dats$Apps,main ="Apps")
hist( dats$Enroll,main ="Enroll")
hist( dats$F.Undergrad,main ="F.Undergrad")
hist( dats$P.Undergrad,main ="P.Undergrad")
hist( dats$Room.Board,main ="Room.Board")
hist( dats$Books,main ="Books")
hist( dats$Personal,main ="Personal")
hist( dats$PhD,main ="PhD")
hist( dats$Terminal,main ="Terminal")
hist( dats$Expend,main ="Expend")

################################################
# Splitting the dataset into Train and Test
###############################################
set.seed(234)
independent_dataset <-sample(2, nrow(dats),replace = TRUE, prob = c(0.8, 0.2))

train_set <- dats[independent_dataset == 1,]
test_set <- dats[independent_dataset == 2,]


#########################################################
# 3)A Fitting a Linear model to The dataset
#########################################################

linear_regresson = lm(formula = train_set$Apps~ .  , data = train_set)
summary(linear_regresson)

#install.packages('Metrics')
library(Metrics)

mse(test_set$Apps,predict(linear_regresson, newdata = test_set))      # Test_MSE =   0.03704768

#########################################################
# 3)Spliting the dataset into X and Y
#########################################################


X_train <- data.matrix(train_set[,-2])
Y_train<- (train_set[,2])


X_test <- data.matrix(test_set[,-2])
Y_test<- (test_set[,2])

#########################################################
# 3)A Fitting a Ridge REGRESSION to The dataset
#########################################################

set.seed(12345)
ridge.fit = glmnet(X_train,Y_train,type.measure = "mse", alpha = 0,family = "gaussian")
ridge.predicted <- as.matrix( predict(ridge.fit, s=ridge.fit$lambda.min, newx=X_test))
par(mfrow = c(1,1))
plot(ridge.fit)
mean((Y_test - ridge.predicted))                                          #test MSE = 0.0280269
mse(Y_test,ridge.predicted)                                            #test MSE = 0.5990396


#########################################################
# 3)A Fitting a Lasso REGRESSION to The dataset
#########################################################

set.seed(12345)
lasso.fit = glmnet(X_train,Y_train,type.measure = "mse", alpha = 1,family = "gaussian")

##########################################################
#finding the minium value of lambda
#########################################################
train <- sample(1:nrow(X_train), round(nrow(X_train)/2))
cv.out <- cv.glmnet(X_train[train,], Y_train[train], alpha = 0)
plot(cv.out)

names(cv.out)
bestlam <- cv.out$lambda.min
bestlam

lasso.predicted <- as.matrix( predict(lasso.fit, s=bestlam, newx=X_test))

mean((Y_test - lasso.predicted))                                          #test Mean = 0.001751347
mse(Y_test,lasso.predicted)                                               #test MSE = 0.06127575

par(mfrow = c(1,1))
plot(lasso.fit)









