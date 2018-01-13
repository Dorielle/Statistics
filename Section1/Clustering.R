# Statistics for Computer Science
# Home Assignment 1
# Section 1 - Clustering

#### Task 3 ####
## Libraries & Clean-Up ####
rm(list=ls())

install.packages("cluster")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("clue")
install.packages("descr")
install.packages("gmodels")
install.packages("caret", dependencies = TRUE)
library(cluster)
library(ggplot2)
library(ggfortify)
library(clue)
library(descr)
library(gmodels)
library(caret)

## Data Initialisation ####
seeds <- read.csv("seed_dataset.csv")
head(seeds)
summary(seeds)

## K-means ####
# Set seed so that the clustering results do not change from run to run
# This happens since k-means is a randomised clustering algoithm
set.seed(456); 

# Remove the variety variable 
# and create a new dataframe with the features only
seeds.features = seeds
seeds.features$variety <- NULL

# Create k-means
# with k (number of clusters) set to 3
# 3 is chosen as we know from the summary that there are 3 seed varietes in the dataset
kmeansResult <- kmeans(seeds.features, 3)  
kmeansResult

## Visualisation of K-means results ####
table(seeds$variety,kmeansResult$cluster)

clusplot(seeds.features,  kmeansResult$cluster, main='2D representation of k-means cluster solution',
         color=TRUE, shade=FALSE, labels=2, lines=0)

clusplot(seeds,  seeds$variety, main='2D representation of k-means cluster solution',
         color=TRUE, shade=FALSE, labels=2, lines=0)

plot(seeds[c("kernelLength", "kernelWidth")], col=kmeansResult$cluster, main="k-means clusters")
plot(seeds[c("kernelLength", "kernelWidth")], col=seeds$variety,  main="original data cluster")

plot(seeds[c("area", "parameter")], col=kmeansResult$cluster, main="k-means clusters")
plot(seeds[c("area", "parameter")], col=seeds$variety, main="original data cluster")

plot(seeds[c("asymmetryCoefficient", "kernelGroveLength")], col=kmeansResult$cluster, main="k-means clusters")
plot(seeds[c("asymmetryCoefficient", "kernelGroveLength")], col=seeds$variety,  main="original data cluster") 

ggplot(seeds, aes(kernelLength, kernelWidth, color = variety)) + geom_point()

kmeansResult$cluster <- as.factor(kmeansResult$cluster)
ggplot(seeds, aes(kernelLength, kernelWidth, color = kmeansResult$cluster)) + geom_point()

autoplot(pam(seeds[-8], 3), frame = TRUE, frame.type = 'norm')




#### Task 4 ####
## Split data ####
# Method to scale and normalize the data
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  
  return (num/denom)
}

set.seed(455);
d.rows <- nrow(seeds)
d.sample <- sample(d.rows, d.rows*0.8)
d.train <- seeds[d.sample,]
d.test <- seeds[-d.sample,]

seed_norm <- as.data.frame(lapply(seeds[1:7], normalize))
ind <- sample(2, d.rows, replace=TRUE, prob=c(0.8,0.2))

seed.training <- seed_norm[ind==1, 1:7]
seed.test <- seed_norm[ind==2, 1:7]

seed.trainLabels <- seeds[ind==1, 8]
seed.testLabels <- seeds[ind==2, 8]

## k-means on training data ####
seeds_train_Data <- kmeans(seed.training,3, nstart = 750)
table(seeds_train_Data$cluster,seed.trainLabels)

## k-means on testing data ####
result <- cl_predict(seeds_train_Data,seed.test)
conf_tbl <- table(seed.testLabels,result)
conf_tbl

## Confusion matrix ####
confusionMatrix(seeds_train_Data$cluster, seed.trainLabels) #training data
confusionMatrix(result, seed.testLabels) # testing data
