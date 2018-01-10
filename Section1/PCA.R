# Statistics for Computer Science
# Home Assignment 1
# Section 1 - PCA

## Libraries & Clean-Up ####
install.packages("ggplot2")
install.packages("factoextra")
library(ggplot2)
library(factoextra)

rm(list=ls())

## Data Initialisation ####
seeds <- read.csv("seed_dataset.csv")
head(seeds)
summary(seeds)

## PCA Functions ####
# PCA on the first 7 attributes
pca = prcomp(seeds[,-8], scale=TRUE)
summary(pca)

# Eigen Values
eig <- (pca$sdev)^2
eig

var <- eig*100/sum(eig)
var

cumVar <- cumsum(var)
cumVar

eigVarCum <- data.frame(eigenValue=eig, variance=var, cumulativeVarience=cumVar)
eigVarCum

# Loadings
head(pca$rotation)

# Principle Components (scores)
head(pca$x)

seeds2 <- cbind(seeds, pca$x[,1:2])
head(seeds2)

write.csv(file="seeds2.csv", x=seeds2)

## PCA Visualisations ####
# DF with scores
scores = as.data.frame(pca$x)

# Obervations Plot
ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "tomato", alpha = 0.8, size = 4) +
  ggtitle("PCA plot of Seed")

# Scree plot - visualising eigenvalues
plot(pca, type="l", main="Scree Plot")
fviz_eig(pca)

biplot(pca, scale=0)