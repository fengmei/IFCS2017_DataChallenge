# IFCS2017_DataChallenge

This repo is for the 2017 IFCS(Conference of the International Federation of Classification Societies) data challenge.
This challenge is about data clustering on low back pain patients. 
We won the data challenge in Aug, 2017.

# Below are the libraries needed
#install.packages("missForest", dependencies=TRUE)

#install.packages("mice", dependencies=TRUE)

library(gdata)

library(mice) # for data imputation

library(randomForest) # for data imputation

library(VIM) # for data imputation

library(data.table) # for data processing

library(dplyr) # for data processing

library(missForest) # for imputation

library(Rtsne) #for visualization 

library(rgl) # for 3d visualization

library(clustMD) # for model based clustering

library(irlba) # for svd visualization

library(Matrix)

library(fpc) # for cluster.stats

library(GGally) # for one of ggplot

library(clustrd) # for MCA

library(cluster) # for daisy-gower distance

library(clustMixType) \\

Note: The clustrd package has been updated since this work submitted, so some minor different results might be seen.
