#DATA MINING PROJECT
#Principal Components Analysis on the Train Set
library(rrcov)
library(factoextra)

#Get the train set
train_data = train_data_new

#Save categorical variables as factors
train_data[,37] = as.factor(as.numeric(train_data[,37]))
train_data[1] = as.factor(as.numeric(train_data[,1]))
train_data[2] = as.factor(as.numeric(train_data[,2]))
train_data[3] = as.factor(as.numeric(train_data[,3]))
train_data[4] = as.factor(as.numeric(train_data[,4]))
train_data[5] = as.factor(as.numeric(train_data[,5]))
train_data[6] = as.factor(as.numeric(train_data[,6]))

#Apply standardized PCA to the continuous variables
pca_st <- prcomp(train_data[,7:36], scale = TRUE)

#Lets keep 9 components
combinacoes = pca_st$rotation[,1:9]
combinacoes

#Lets ignore the categorical variables when creating the new dataset
train_data_pca = train_data[7:36]

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
dataset_pca = matrix(c(final,final2,final3, final4, final5, final6,final7,final8,final9), nrow = 12000, ncol = 9)
dataset_pca = cbind(train_data[1:6], dataset_pca) #bind with the categorical variables
dataset_pca = cbind(dataset_pca, train_data$V37) #bind with the class variable

#Save the dataset
write.csv(dataset_pca, "~/Desktop/dataset_pca")


##################
#Principal Components Analysis on the Test Set

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



#PCA on the Processed Dataset (Train and Test)

train_data<-read.table("train_data_removed",header = FALSE)
test_data<-read.table("test_data_removed",header = FALSE)

# Standardized
pca_st_train <- prcomp(train_data[,6:26], scale = TRUE)
pca_st_test <- prcomp(test_data[,6:26], scale = TRUE)

#Scree Plot - Train
fviz_eig(pca_st_train)
screeplot(pca_st_train)

#Scree Plot - Test
fviz_eig(pca_st_test)
screeplot(pca_st_test)

#7 Components
# Eigenvalues - Train
eig.val_train <- get_eigenvalue(pca_st_train)
eig.val_train

# Eigenvalues - Test
eig.val_test <- get_eigenvalue(pca_st_test)
eig.val_test

#Only at the 10th dimension we get 80% variability - Train
pca_st_train$rotation
#Lets keep 9 components

#Only at the 10th dimension we get 80% variability - Test
pca_st_test$rotation
#Lets keep 9 components

#Train - PCA
combinacoes_train = pca_st_train$rotation[,1:9]
combinacoes_train

train_data_pca = train_data[6:26]

#Get the 1st PC
final_train <- 1:12000;
for (i in 1:12000){
  final_train[i] = 0;
  for(c in 1:21){
    final_train[i] = final_train[i] + combinacoes_train[c,1] * train_data_pca[i,c];
  }
}

#Get the 2nd PC
final_train2 <- 1:12000;
for (i in 1:12000){
  final_train2[i] = 0;
  for(c in 1:21){
    final_train2[i] = final_train2[i] + combinacoes_train[c,2] * train_data_pca[i,c];
  }
}

#Get the 3rd PC
final_train3 <- 1:12000;
for (i in 1:12000){
  final_train3[i] = 0;
  for(c in 1:21){
    final_train3[i] = final_train3[i] + combinacoes_train[c,3] * train_data_pca[i,c];
  }
}

#Get the 4th PC
final_train4 <- 1:12000;
for (i in 1:12000){
  final_train4[i] = 0;
  for(c in 1:21){
    final_train4[i] = final_train4[i] + combinacoes_train[c,4] * train_data_pca[i,c];
  }
}

#Get the 5th PC
final_train5 <- 1:12000;
for (i in 1:12000){
  final_train5[i] = 0;
  for(c in 1:21){
    final_train5[i] = final_train5[i] + combinacoes_train[c,5] * train_data_pca[i,c];
  }
}

#Get the 6th PC
final_train6 <- 1:12000;
for (i in 1:12000){
  final_train6[i] = 0;
  for(c in 1:21){
    final_train6[i] = final_train6[i] + combinacoes_train[c,6] * train_data_pca[i,c];
  }
}

#Get the 7th PC
final_train7 <- 1:12000;
for (i in 1:12000){
  final_train7[i] = 0;
  for(c in 1:21){
    final_train7[i] = final_train7[i] + combinacoes_train[c,7] * train_data_pca[i,c];
  }
}

#Get the 8th PC
final_train8 <- 1:12000;
for (i in 1:12000){
  final_train8[i] = 0;
  for(c in 1:21){
    final_train8[i] = final_train8[i] + combinacoes_train[c,8] * train_data_pca[i,c];
  }
}

#Get the 9th PC
final_train9 <- 1:12000;
for (i in 1:12000){
  final_train9[i] = 0;
  for(c in 1:21){
    final_train9[i] = final_train9[i] + combinacoes_train[c,9] * train_data_pca[i,c];
  }
}

dataset_pca_train = c(final_train,final_train2, final_train3, final_train4, final_train5, final_train6,final_train7,final_train8,final_train9)

dataset_pca_train = matrix(c(final_train,final_train2,final_train3, final_train4, final_train5, final_train6,final_train7,final_train8,final_train9), nrow = 12000, ncol = 9)

dataset_pca_train = cbind(train_data[1:5], dataset_pca_train)
dataset_pca_train = cbind(dataset_pca_train, train_data$V27)

write.csv(dataset_pca_train, "/home/ishi/Documentos/University-Projects/SMDM/Projeto/Git-Rep/2020-04-22/dataset_processed_pca_train")


#Test Dataset - PCA
combinacoes_test = pca_st_train$rotation[,1:9]
combinacoes_test

test_data_pca = test_data[6:26]

#Get the 1st PC
final_test <- 1:12000;
for (j in 1:12000){
  final_test[j] = 0;
  for(k in 1:21){
    final_test[j] = final_test[j] + combinacoes_test[k,1] * test_data_pca[j,k];
  }
}

#Get the 2nd PC
final_test2 <- 1:12000;
for (j in 1:12000){
  final_test2[j] = 0;
  for(k in 1:21){
    final_test2[j] = final_test2[j] + combinacoes_test[k,2] * test_data_pca[j,k];
  }
}

#Get the 3rd PC
final_test3 <- 1:12000;
for (j in 1:12000){
  final_test3[j] = 0;
  for(k in 1:21){
    final_test3[j] = final_test3[j] + combinacoes_test[k,3] * test_data_pca[j,k];
  }
}

#Get the 4th PC
final_test4 <- 1:12000;
for (j in 1:12000){
  final_test4[j] = 0;
  for(k in 1:21){
    final_test4[j] = final_test4[j] + combinacoes_test[k,4] * test_data_pca[j,k];
  }
}

#Get the 5th PC
final_test5 <- 1:12000;
for (j in 1:12000){
  final_test5[j] = 0;
  for(k in 1:21){
    final_test5[j] = final_test5[j] + combinacoes_test[k,5] * test_data_pca[j,k];
  }
}

#Get the 6th PC
final_test6 <- 1:12000;
for (j in 1:12000){
  final_test6[j] = 0;
  for(k in 1:21){
    final_test6[j] = final_test6[j] + combinacoes_test[k,6] * test_data_pca[j,k];
  }
}

#Get the 7th PC
final_test7 <- 1:12000;
for (j in 1:12000){
  final_test7[j] = 0;
  for(k in 1:21){
    final_test7[j] = final_test7[j] + combinacoes_test[k,7] * test_data_pca[j,k];
  }
}

#Get the 8th PC
final_test8 <- 1:12000;
for (j in 1:12000){
  final_test8[j] = 0;
  for(k in 1:21){
    final_test8[j] = final_test8[j] + combinacoes_test[k,8] * test_data_pca[j,k];
  }
}

#Get the 9th PC
final_test9 <- 1:12000;
for (j in 1:12000){
  final_test9[j] = 0;
  for(k in 1:21){
    final_test9[j] = final_test9[j] + combinacoes_test[k,9] * test_data_pca[j,k];
  }
}

dataset_pca_test = c(final_test,final_test2, final_test3, final_test4, final_test5, final_test6,final_test7,final_test8,final_test9)

dataset_pca_test = matrix(c(final_test,final_test2,final_test3, final_test4, final_test5, final_test6,final_test7,final_test8,final_test9), nrow = 12000, ncol = 9)

dataset_pca_test = cbind(test_data[1:5], dataset_pca_test)
dataset_pca_test = cbind(dataset_pca_test, test_data$V27)

write.csv(dataset_pca_test, "/home/ishi/Documentos/University-Projects/SMDM/Projeto/Git-Rep/2020-04-22/dataset_processed_pca_test")



