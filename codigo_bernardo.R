#### Codigo do Bernardo 
# logreg
# LogitBoost
# regLogistic  Regularized Logistic Regression	
# http://topepo.github.io/caret/train-models-by-tag.html#logic-regression

# K-Means
library(caret)
library(MLmetrics)
dataset.train<-read.table("train_data_new_classifier.txt",header = FALSE)
dataset.test<-read.table("test_data_new_classifier.txt",header = FALSE)

dataset.edited.train<-read.table("train_data_removed",header = FALSE)
dataset.edited.test<-read.table("test_data_removed.txt",header = FALSE)

dataset.edited.train2<-read.table("train_data_removed_2.txt",header = FALSE)
dataset.edited.test2<-read.table("test_data_removed_2.txt",header = FALSE)

dataset.pca.train<-read.csv("dataset_pca.csv",header = TRUE)
dataset.pca.test<-read.csv("dataset_pca_test.csv",header = TRUE)

dataset.pca.train['X'] = NULL
dataset.pca.test['X'] = NULL


# with library class
require("class")
library(class)

# using train dataset
y_train<-dataset.train[,37] # 37 column is the class
x_train<-dataset.train[,1:36] # 36 first columns are the features

# using test dataset
y_test<-dataset.test[,37] # 37 column is the class
x_test<-dataset.test[,1:36] # 36 first columns are the features

################## Dataset 2 - dados normalizados ########################## 
#################  usar: x_train2 y_train / x_test2 y_test

# Normalized dataset
x_train2 = cbind(x_train[1:6],scale(x_train[7:36], center = TRUE, scale = TRUE))
x_test2 = cbind(x_test[1:6],scale(x_test[7:36], center = TRUE, scale = TRUE))

################## Dataset 3 - o dataset q construimos ########################## 
#################  usar: x_train3 y_train3 / x_test3 y_test3

# Removed and new features
x_train3 <-dataset.edited.train[,1:26]
y_train3 <- dataset.edited.train[,27]

x_test3 <-dataset.edited.test[,1:26]
y_test3 <- dataset.edited.test[,27]

x_train3_2 <-dataset.edited.train2[,1:27]
y_train3_2 <- dataset.edited.train2[,28]

x_test3_2 <-dataset.edited.test2[,1:27]
y_test3_2 <- dataset.edited.test2[,28]


## Pca
y_train4<-dataset.pca.train[,16] # 37 column is the class
x_train4<-dataset.pca.train[,1:15] # 36 first columns are the features

y_test4<-dataset.pca.train[,16] # 37 column is the class
x_test4<-dataset.pca.train[,1:15] # 36 first columns are the features


# Create function in order to apply knn for various values of k
classify.knn.leave.on.out<-function(train,test,cl_train,cl_test,vk)
{
  erro<-NULL
  accuracy<-NULL
  recall<-NULL
  f1score<-NULL

    for(k in 1:vk)
  {
    # In order to choose the best value for K, we are going to do grid search
    
    fit<-knn(train,test,cl_train,k=k,prob=TRUE)
    
    erro[k]<-1-sum(cl_test==fit)/length(cl_test)
    accuracy[k]<-sum(cl_test==fit)/length(cl_test)
    f1score[k] <- F1_Score(fit, cl_test, positive = NULL)
    recall[k] <- Sensitivity(fit, cl_test, positive = NULL)

    cat("k=",k,"Misclassification error=",erro[k],"\n")
    cat("k=",k,"Accuracy=",accuracy[k],"\n")
    cat("k=",k,"F1 Score=",f1score[k],"\n")
    cat("k=",k,"Recall=",recall[k],"\n")
    
    print(table(fit,cl_test))
    plot(1:k,erro,type="b",xlab="k",ylab="Misclassification error",
         main="k-Nearest Neighbour - Hold-Out")
    grid(10,10)
  }
}

set.seed(2020)
# Vamos fazer gridsearch com k [1,10]
classify.knn.leave.on.out(train=x_train,test=x_test,cl_train=y_train,cl_test=y_test,vk=10)

pred_train<-knn(x_train,x_train,y_train,k=1,prob=TRUE)
#Confusion matrix
table(y_train,pred_train)


# Using test=train dataset

# Making the classification of the training dataset
pred_train<-knn(x_train,x_train,y_train,k=1,prob=TRUE)

#Confusion matrix
table(y_train,pred_train)

# Accuracy calculation
sum(y_train==pred_train)/length(y_train)

### For test dataset
pred_test<-knn(x_train,x_train,y_train,k=2,prob=TRUE)

#Confusion matrix
table(y_test,pred_test)

# Accuracy calculation
sum(y_test==pred_test)/length(y_test)


##################### Repetead K-Folds Cross Validation ###################

set.seed(2020)
#Dados normalizados
pred.knn.m2 <- train(CL~.,
                     method     = "knn",
                     tuneGrid   = expand.grid(k = 1:20),
                     trControl  = trainControl(method = "repeatedcv", number = 5, repeats = 4),
                     metric     = "Accuracy",
                     data       = cbind(CL = as.factor(y_train), x_train))
# Summarize the results
print(pred.knn.m2)
plot(pred.knn.m2)

fitm2 <- predict(pred.knn.m2, newdata = x_test)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(as.factor(y_test), fitm2)


##################### Bootstrap ###################

# Train the model
pred.knn.m3 <- train(CL~.,
                     method     = "knn",
                     tuneGrid   = expand.grid(k = 1:20),
                     trControl  = trainControl(method="boot", number=100),
                     metric     = "Accuracy", 
                     data       = cbind(CL = as.factor(y_train), x_train))

# Summarize the results
print(pred.knn.m3)

plot(pred.knn.m3)

fitm4 <- predict(pred.knn.m3, newdata = x_test)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(as.factor(y_test), fitm3)


##################### Leave One Out Cross Validation ###################

# Train the model
pred.knn.m4 <- train(CL~.,
                     method     = "LogitBoost",
                     tuneGrid   = expand.grid(nIter = 1:20),
                     trControl  = trainControl(method = "LOOCV"),
                     metric     = "Accuracy", 
                     data       = cbind(CL = as.factor(y_train), x_train))

# Summarize the results
print(pred.knn.m4)

plot(pred.knn.m4)

fitm4 <- predict(pred.knn.m4, newdata = x_test)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(as.factor(y_test), fitm4)


################### K Folds Cross Validation #####################
pred.knn.m5 <- train(CL~.,
                     method     = "knn",
                     tuneGrid   = expand.grid(k = 1:20),
                     trControl  = trainControl(method = "cv",number = 10),
                     metric     = "Accuracy",
                     data       = cbind(CL = as.factor(y_train3_2), x_train3_2))

# Summarize the results
print(pred.knn.m5)
plot(pred.knn.m5)
fitm5 <- predict(pred.knn.m5, newdata = x_test3_2)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(as.factor(y_test3_2), fitm5)



################### Logistic Regression #####################
pred.knn.m6 <- train(CL~.,
                     method     = "LogitBoost",
                     tuneGrid   = expand.grid(nIter = 49),
                     trControl  = trainControl(method = "cv",number = 10),
                     metric     = "Accuracy",
                     data       = cbind(CL = as.factor(y_train3_2), x_train3_2))

# Summarize the results
print(pred.knn.m6)
plot(pred.knn.m6)
fitm6 <- predict(pred.knn.m6, newdata = x_test3_2)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(as.factor(y_test3_2), fitm6)

############################# Random Forests ###################################
pred.knn.m7 <- train(CL~.,
                     method     = "rf",
                     tuneGrid   = expand.grid(mtry = 3),
                     trControl  = trainControl(method = "cv",number = 10),
                     metric     = "Accuracy",
                     data       = cbind(CL = as.factor(y_train4), x_train4))

# Summarize the results
print(pred.knn.m7)
plot(pred.knn.m7)
fitm7 <- predict(pred.knn.m7, newdata = x_test4)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(as.factor(y_test4), fitm7)

################################################################################






