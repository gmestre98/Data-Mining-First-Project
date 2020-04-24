#DATA MINING PROJECT
#Naive Bayes
library(caret)
library(e1071)

#Get the train and test sets
train_data = train_data_new
test_data = test_data_new

#Save categorical variables as factors
train_data[,37] = as.factor(as.numeric(train_data[,37]))
train_data[1] = as.factor(as.numeric(train_data[,1]))
train_data[2] = as.factor(as.numeric(train_data[,2]))
train_data[3] = as.factor(as.numeric(train_data[,3]))
train_data[4] = as.factor(as.numeric(train_data[,4]))
train_data[5] = as.factor(as.numeric(train_data[,5]))
train_data[6] = as.factor(as.numeric(train_data[,6]))

test_data[,37] = as.factor(as.numeric(test_data[,37]))
test_data[1] = as.factor(as.numeric(test_data[,1]))
test_data[2] = as.factor(as.numeric(test_data[,2]))
test_data[3] = as.factor(as.numeric(test_data[,3]))
test_data[4] = as.factor(as.numeric(test_data[,4]))
test_data[5] = as.factor(as.numeric(test_data[,5]))
test_data[6] = as.factor(as.numeric(test_data[,6]))

#Get the same number of factor on train and test set
levels(train_data$V3) <- levels(test_data$V3)
levels(train_data$V1) <- levels(test_data$V1)
levels(train_data$V2) <- levels(test_data$V2)
levels(train_data$V4) <- levels(test_data$V4)
levels(train_data$V5) <- levels(test_data$V5)
levels(train_data$V6) <- levels(test_data$V6)

#Create object x which holds the predictor variables and y which holds the response variables
x_bayes = train_data[,-37]
y_bayes = train_data$V37

#Bayes classifier
#Tuning the smoothing
search_grid = expand.grid(usekernel = c(TRUE, FALSE), fL = 0:1, adjust = 1)
#Trainig the model
model = train(x_bayes,y_bayes,'nb',trControl=trainControl(method='cv'), tuneGrid = search_grid)

#Model Evaluation
#Predict the test set
pred_bayes <- predict(model,newdata = test_data)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(pred_bayes, test_data$V37 )

#Plot variables performance
var_importance <- varImp(model)
plot(var_importance, xlab = "Importance", ylab = "Variables", main = "Variables Importance on the Naive Bayes Classifier")



