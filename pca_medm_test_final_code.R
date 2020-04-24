#DATA MINING PROJECT
#Principal Component Analysis on the Test Set
library(rrcov)
library(factoextra)
#Get the test set
test_data = test_data_new

#Save categorical variables as factors
test_data[,37] = as.factor(as.numeric(test_data[,37]))
test_data[1] = as.factor(as.numeric(test_data[,1]))
test_data[2] = as.factor(as.numeric(test_data[,2]))
test_data[3] = as.factor(as.numeric(test_data[,3]))
test_data[4] = as.factor(as.numeric(test_data[,4]))
test_data[5] = as.factor(as.numeric(test_data[,5]))
test_data[6] = as.factor(as.numeric(test_data[,6]))

#Apply standardized PCA to the continuous variables
pca_st <- prcomp(test_data[,7:36], scale = TRUE)

#Scree Plot
fviz_eig(pca_st)
#9 Components

# Eigenvalues
eig.val <- get_eigenvalue(pca_st)
eig.val
#Only at the 10th dimension we get 80% variability
#9 Components with variability above 1

#Lets keep 9 components
combinacoes = pca_st$rotation[,1:9]
combinacoes

#Lets ignore the categorical variables when creating the new dataset
train_data_pca = test_data[7:36]

#Get the 1st PC
final <- 1:12000;
for (i in 1:12000){
  final[i] = 0;
  for(c in 1:30){
    final[i] = final[i] + combinacoes[c,1] * train_data_pca[i,c];
  }
}

#Get the 2nd PC
final2 <- 1:12000;
for (i in 1:12000){
  final2[i] = 0;
  for(c in 1:30){
    final2[i] = final2[i] + combinacoes[c,2] * train_data_pca[i,c];
  }
}

#Get the 3rd PC
final3 <- 1:12000;
for (i in 1:12000){
  final3[i] = 0;
  for(c in 1:30){
    final3[i] = final3[i] + combinacoes[c,3] * train_data_pca[i,c];
  }
}

#Get the 4th PC
final4 <- 1:12000;
for (i in 1:12000){
  final4[i] = 0;
  for(c in 1:30){
    final4[i] = final4[i] + combinacoes[c,4] * train_data_pca[i,c];
  }
}

#Get the 5th PC
final5 <- 1:12000;
for (i in 1:12000){
  final5[i] = 0;
  for(c in 1:30){
    final5[i] = final5[i] + combinacoes[c,5] * train_data_pca[i,c];
  }
}

#Get the 6th PC
final6 <- 1:12000;
for (i in 1:12000){
  final6[i] = 0;
  for(c in 1:30){
    final6[i] = final6[i] + combinacoes[c,6] * train_data_pca[i,c];
  }
}

#Get the 7th PC
final7 <- 1:12000;
for (i in 1:12000){
  final7[i] = 0;
  for(c in 1:30){
    final7[i] = final7[i] + combinacoes[c,7] * train_data_pca[i,c];
  }
}

#Get the 8th PC
final8 <- 1:12000;
for (i in 1:12000){
  final8[i] = 0;
  for(c in 1:30){
    final8[i] = final8[i] + combinacoes[c,8] * train_data_pca[i,c];
  }
}

#Get the 9th PC
final9 <- 1:12000;
for (i in 1:12000){
  final9[i] = 0;
  for(c in 1:30){
    final9[i] = final9[i] + combinacoes[c,9] * train_data_pca[i,c];
  }
}

#Get the final dataset
dataset_pca_test = matrix(c(final,final2,final3, final4, final5, final6,final7,final8,final9), nrow = 12000, ncol = 9)
dataset_pca_test = cbind(test_data[1:6], dataset_pca_test) #bind with the categorical variables
dataset_pca_test = cbind(dataset_pca_test, test_data$V37) #bind with the class variable

#Save the dataset
write.csv(dataset_pca_test, "~/Desktop/dataset_pca_test")


