# Necessary Package Installation
install.packages("DataExplorer")
install.packages("corrplot")
install.packages("tidyinftheo")
if(!require("devtools")) {
  install.packages("devtools")
  library("devtools")
}
devtools::install_github("mdscheuerell/muti")

# Calling the necessary Libraries
library(DataExplorer)
library(corrplot)
library(RColorBrewer)

# Reading data
train_data <- read.table("http://web.tecnico.ulisboa.pt/~ist13493/MEDM2020/Project1/Group3Data/client_train.txt")

# Checking some information on the data
plot_str(train_data)
introduce(train_data)
plot_intro(train_data)
plot_missing(train_data)
plot_bar(train_data)

# Generating a data report for the data
create_report(train_data)

# Computing the correlations and plotting the correlogram
corvar <- cor(train_data[7:36])
corrplot(corvar, method="color", col = brewer.pal(n = 8, name = "RdBu"), type="lower", tl.col = "black",
         addCoef.col = "black", tl.srt = 15, tl.cex = 0.6, number.cex = 0.7)

# Generating the boxplots
boxplot(train_data[,23], col=rgb(0.3,0.5,0.4,0.6), las=2, cex.axis=0.70)

##########################################Conclusions:#################################################
# No missing values
# 2 classes, balanced 6000/6000
# 6 categorical variables
# 30 numerical variables
# Histograms and QQ plots show that almost all numerical variables are constant?
# Should we remove these variables with such high correlation on this case? Given that we are searching
#for non-active clients and that the differences are what might reveal them.
# How to implement the analysis of mutual information?