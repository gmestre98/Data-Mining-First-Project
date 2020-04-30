import pandas as pd
import numpy as np
import seaborn as sns
from sklearn.feature_selection import mutual_info_classif

sns.set()

df_train = pd.read_csv("train_data_new", ' ', header=None)

label = df_train[0]

features = df_train.shape[1]
MI = np.zeros((7, 7))
L = list(range(0, 6)); L.append(features-1)
df_train = df_train[L]

for index, j in enumerate(L):
    label = df_train[j]
    MI[index, :] = mutual_info_classif(df_train, label, discrete_features=range(0,6))
    
sns.heatmap(MI)
