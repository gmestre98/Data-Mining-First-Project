# Necessary Package Installation
#install.packages("DataExplorer")
#install.packages("corrplot")
#install.packages("tidyinftheo")
#install.packages("ggplot2")
#install.packages("e1071")
#devtools::install_github("pohlio/tidyinftheo")
#if(!require("devtools")) {
#  install.packages("devtools")
#  library("devtools")
#}
#devtools::install_github("mdscheuerell/muti")
# Calling the necessary Libraries
library(ggplot2)
library(e1071)
library(DataExplorer)
library(corrplot)
library(RColorBrewer)
library(tidyinftheo)
library("muti")
#install.packages("reticulate")
library(reticulate)
use_python("/usr/local/bin/python")
#py_install("scikit-learn")
sklearn1 <- import("sklearn.feature_selection")
sklearn2 <- import("sklearn.preprocessing")



# Loading data
options(warn = FALSE)
train_data <- try(read.table("Input/client_train.txt"))
if (class(train_data) == "try-error") {
  folder = "Input/"
  file = "client_train.txt"
  cat("Downloading the data instead.")
  dir.create(folder)
  train_data <- read.table("http://web.tecnico.ulisboa.pt/~ist13493/MEDM2020/Project1/Group3Data/client_train.txt")
  write.table(train_data, file=paste(folder, file, sep=''), append = FALSE, sep = " ", dec = ".",
              row.names = FALSE, col.names = FALSE)
}
test_data <- try(read.table("Input/client_test_marked.txt"))
if (class(train_data) == "try-error") {
  file = "client_test_marked.txt"
  print("Downloading the data instead.")
  train_data <- read.table("http://web.tecnico.ulisboa.pt/~ist13493/MEDM2020/Project1/Group3Data/client_test_marked.txt")
  write.table(train_data, file=paste(folder, file, sep=''), append = FALSE, sep = " ", dec = ".",
              row.names = FALSE, col.names = FALSE)
}
options(warn = TRUE)

# Checking the data
head(train_data)

# Cleaning the data
train_data[,1] <- as.factor(as.numeric(train_data[,1]) - 1)
train_data[,2] <- as.factor(as.numeric(train_data[,2]) - 1)
train_data[,3] <- as.factor(as.numeric(train_data[,3]) - 1)
train_data[,4] <- as.factor(as.numeric(train_data[,4]) - 1)
train_data[,5] <- as.factor(as.numeric(train_data[,5]) - 1)
train_data[,6] <- as.factor(as.numeric(train_data[,6]) - 1)
train_data[,37] <- as.factor(as.numeric(train_data[,37]) - 1)

# Descriptive Statistics
v <-sapply(train_data[,7:36], var)
m <-sapply(train_data[,7:36], mean)
q <-sapply(train_data[,7:36], quantile)
m0 <- sapply(train_data[,7:36], min)
m1 <- sapply(train_data[,7:36], max)
i <-sapply(train_data[,7:36], IQR)
s <-sapply(train_data[,7:36], skewness)
k <-sapply(train_data[,7:36], kurtosis)

descriptive <- data.frame("mean" = m, "variance" = v, "minimum" = m0, "Q1" = q[2,], "median" = q[2,], 
                          "Q3" = q[3,], "max" = m1, "IQR" = i, "skewness" = s, "kurtosis" = k)

show = TRUE
if(show){
  descriptive
} else {
  folder = "Output/"
  file = "descriptive.csv"
  suppressWarnings(dir.create(folder))
  write.csv(descriptive, file = paste(path, file, sep = ''))
}

# Trying some transformations on low variance columns to indetify why it has a low variance
var(log(train_data[,22]) + 1)


# Generating a data report for the data
create_report(train_data)


# Plotting histograms of each variable, and signaling the class for each data
i=8
class1 <- data.frame(V = train_data[train_data[37]=='2', i])
class0 <- data.frame(V = train_data[train_data[37]=='1', i])
class1$Class <- 'Class 1'
class0$Class <- 'Class 0'
classCounts <- rbind(class1, class0)
ggplot(classCounts, aes(V, fill = Class)) + 
  geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity', bins = 100)


# Computing the correlations and plotting the correlogram
corvar <- cor(train_data[7:36])
corrplot(corvar, method="color", col = brewer.pal(n = 8, name = "RdBu"), type="lower", tl.col = "black",
         addCoef.col = "black", tl.srt = 15, tl.cex = 0.6, number.cex = 0.7)

# Computing the mutual information using python
mtrain_data <- as.matrix(train_data, dimnames=NULL)
mutinf <- sklearn1$mutual_info_classif(mtrain_data, mtrain_data[,37])

# If we only want to source some script of python
#source_python('add.py')



# Visualizing Box plots for each variable
for (i in 7:36){
  boxplot(train_data[,12], col=rgb(0.3,0.5,0.4,0.6), las=2, cex.axis=0.70)
}
plotti <- train_data[c(7:16)]
boxplot(plotti, col=rgb(0.3,0.5,0.4,0.6),las=1, ylim = c(0, 400),cex.axis=0.70)
plotti <- train_data[c(17:26)]
boxplot(plotti, col=rgb(0.3,0.5,0.4,0.6),las=1, ylim = c(0, 400),cex.axis=0.70)
plotti <- train_data[c(27:36)]
boxplot(plotti, col=rgb(0.3,0.5,0.4,0.6),las=1, ylim = c(0, 400),cex.axis=0.70)



#########################################Doubts:######################################################
#1. Juntamos os valores nas variaveis categoricas que têm valores que correspondam a apenas uma das classes? Sim, mas explicar
#2. Ver variáveis que assinalamos
#3. Problemas das variâncias minorcas, fazer transformaçoes com o logaritmo ou assim
#4. Correlograma- threshold para tirar R:0.75
#5. Mutual Information, nao esta a funcionar, usamos outra metrica? Como corrigir? R: A prof vai mandar um pacote
#6. Deteção de outliers R: Deixar para o fim, ver com os boxplots
#7. Transformações, pedir ajuda ao dealer do Rui
#8. Pre-processar os dados




##########################################Extra code pieces if needed in future########################
# Checking some information on the data
#plot_str(train_data)
#introduce(train_data)
#plot_intro(train_data)
#plot_missing(train_data)
#plot_bar(train_data)

# Computing the Mutual Information between each variable
#mutinf <- mutual_info_matrix(train_data, 1,2,37, normalized=TRUE)
#mutual_info_heatmap(mutinf, font_sizes = c(12,12))
#mutinf$MI

#muti(train_data[,12], train_data[,14])

