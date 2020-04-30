#DATA MINING PROJECT
#Support Vector Machines
library(e1071)
library(caTools)
library(caret)

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

#Get the same number of factors on train and test set
levels(train_data$V3) <- levels(test_data$V3)
levels(train_data$V1) <- levels(test_data$V1)
levels(train_data$V2) <- levels(test_data$V2)
levels(train_data$V4) <- levels(test_data$V4)
levels(train_data$V5) <- levels(test_data$V5)
levels(train_data$V6) <- levels(test_data$V6)

#Split the dataset into training and validation
set.seed(123)
split = sample.split(train_data$V25, SplitRatio = 0.7)
train_svm = subset(train_data, split == TRUE)
valid_svm = subset(train_data, split == FALSE)

#Original range
c_range = cbind(1, 10, 100, 1000, 10000, 100000, 1000000)
g_range = cbind(0.001, 0.01, 0.1, 1, 10, 100, 1000)

#Function to Apply the SVM to different values of the parameters
for(c in c_range){
  for(g in g_range){
    svm_bank = svm(formula = V25 ~., data = train_svm, type = 'C-classification', kernel = 'radial', gamma = g, cost = c)
    y_pred_bank = predict(svm_bank, newdata = valid_svm[,1:23])
    conf_matrix_bank = table(valid_svm[,24], y_pred_bank)
    accuracy = (conf_matrix_bank[1,1]+conf_matrix_bank[2,2])/sum(conf_matrix_bank)
    cat("The accuracy for cost",c,"and gamma",g,"was",accuracy,"\n")
  }
}

#Radial Kernel range (after running the previous code)
c_range = cbind(10, 100, 1000, 10000, 100000, 10000)
g_range = cbind(0.001, 0.01, 0.1, 1, 10)


#Function to do Cross Validation to find the best pair of hyper parameters discovered previously
accuracy = 0
folds = createFolds(train_data$V37, k=10)
cross_v_bank = lapply(folds, function(x){
  training_fold = train_data[-x,];
  test_fold = train_data[x,];
  
    svm_cross = svm(formula = V37 ~., 
                    data = training_fold, 
                    type = 'C-classification', 
                    kernel = 'radial',
                    cost = 1000,
                    gamma = 0.1);
    
    y_cross = predict(svm_cross, newdata = test_fold[,1:36]);
    
    conf_matrix = table(test_fold[,37], y_cross);
    accuracy= ((conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix));
  return(accuracy)
})

#Get the final accuracy
mean_cv = array(as.numeric(unlist(cross_v_bank)), dim=c(10,1))
mean(mean_cv)


