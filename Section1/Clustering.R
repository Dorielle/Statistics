# Statistics for Computer Science
# Home Assignment 1
# Section 1 - Clustering

## Libraries & Clean-Up ####
rm(list=ls())

## Data Initialisation ####
seeds <- read.csv("seed_dataset.csv")
head(seeds)
summary(seeds)

## K-means ####
# Set seed so that the clustering results do not change from run to run
# This happens since k-means is a randomised clustering algoithm
set.seed(123); 

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
