library(ggplot2)
library(gridExtra)
library(ProbBayes)
library(VGAM)
library(tidyverse)
library(coda)
library(reshape2)
library(ggridges)
library(rjags)
library(runjags)
library(bayesplot)
library("FactoMineR")
library("factoextra")
library("corrplot")

wineqt = read.csv("WineQT.csv")
head(wineqt)
wineqt_scaled = scale(wineqt)

# Analysis 1
# Dimension Reduction
wineqt_scaled = data.frame(wineqt_scaled)

summary(wineqt_scaled)

winepca = PCA(wineqt_scaled[1:1143,1:11], graph = TRUE)

eig_val <- get_eigenvalue(winepca)
eig_val

fviz_eig(winepca, addlabels = TRUE, ylim = c(0, 30))

var <- get_pca_var(winepca)
fviz_pca_var(res.pca, col.var = "blue")

corrplot(var$cos2, is.corr=FALSE)
