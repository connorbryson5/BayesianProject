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

wineqt <- read_csv("WineQT.csv")
head(wineqt)
wineqt_scaled <- scale(wineqt)

# PCA
wineqt_scaled <- data.frame(wineqt_scaled)

summary(wineqt_scaled)

winepca <- PCA(wineqt_scaled[1:1143,1:11], graph = TRUE)

eig_val <- get_eigenvalue(winepca)
eig_val

fviz_eig(winepca, addlabels = TRUE, ylim = c(0, 30))

var <- get_pca_var(winepca)
fviz_pca_var(winepca, col.var = "blue")

corrplot(var$cos2, is.corr=FALSE)

# Analysis 2

# Most important variable

# Frequentist

fit <- lm(wineqt_scaled$quality ~ wineqt_scaled$fixed.acidity+wineqt_scaled$volatile.acidity
          +wineqt_scaled$citric.acid+wineqt_scaled$residual.sugar
          +wineqt_scaled$chlorides+wineqt_scaled$free.sulfur.dioxide
          +wineqt_scaled$total.sulfur.dioxide+wineqt_scaled$density
          +wineqt_scaled$pH+wineqt_scaled$sulphates+wineqt_scaled$alcohol)

summary(fit)

# Bayesian

modelString <-"
model {
## sampling
for (i in 1:N){
y[i] ~ dnorm(beta0 + beta1 * x1[i] + beta2 * x2[i] + beta3 * x3[i] + 
beta4 * x4[i]+ beta5 * x5[i] + beta6 * x6[i] + beta7 * x7[i] + 
beta8 * x8[i]+ beta9 * x9[i] + beta10 * x10[i] + beta11 * x11[i], invsigma2)
}

beta0 ~ dnorm(mu0, g0)
beta1 ~ dnorm(mu1, g1)
beta2 ~ dnorm(mu2, g2)
beta3 ~ dnorm(mu3, g3)
beta4 ~ dnorm(mu4, g4)
beta5 ~ dnorm(mu5, g5)
beta6 ~ dnorm(mu6, g6)
beta7 ~ dnorm(mu7, g7)
beta8 ~ dnorm(mu8, g8)
beta9 ~ dnorm(mu9, g9)
beta10 ~ dnorm(mu10, g10)
beta11 ~ dnorm(mu11, g11)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}"
y <- wineqt_scaled$quality
N <- length(y)

the_data <- list("y" = wineqt_scaled$quality,
                 "x1" = wineqt_scaled$fixed.acidity,
                 "x2" = wineqt_scaled$volatile.acidity,
                 "x3" = wineqt_scaled$citric.acid,
                 "x4" = wineqt_scaled$residual.sugar,
                 "x5" = wineqt_scaled$chlorides,
                 "x6" = wineqt_scaled$free.sulfur.dioxide,
                 "x7" = wineqt_scaled$total.sulfur.dioxide,
                 "x8" = wineqt_scaled$density,
                 "x9" = wineqt_scaled$pH,
                 "x10" = wineqt_scaled$sulphates,
                 "x11" = wineqt_scaled$alcohol,
                 "N" = length(y),
                 "mu0" = 0, "g0" = 0.0025,
                 "mu1" = 0, "g1" = 0.0025,
                 "mu2" = 0, "g2" = 0.0025,
                 "mu3" = 0, "g3" = 0.0025,
                 "mu4" = 0, "g4" = 0.0025,
                 "mu5" = 0, "g5" = 0.0025,
                 "mu6" = 0, "g6" = 0.0025,
                 "mu7" = 0, "g7" = 0.0025,
                 "mu8" = 0, "g8" = 0.0025,
                 "mu9" = 0, "g9" = 0.0025,
                 "mu10" = 0, "g10" = 0.0025,
                 "mu11" = 0, "g11" = 0.0025,
                 "a" = 0.001, "b" = 0.001)


posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("beta0", "beta1",
                                  "beta2", "beta3",
                                  "beta4", "beta5",
                                  "beta6", "beta7",
                                  "beta8", "beta9",
                                  "beta10", "beta11",
                                  "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 15000)

summary(as.mcmc(posterior))

plot(posterior, vars = "beta1")

# Analysis 3 After PCA

# 7 PCs can explain 90% of variance

# Frequentist

wineqt_PCA <- read_csv("./Qianyu/PC_new_data_wine.csv")

fit <- lm(wineqt_scaled$quality ~ wineqt_PCA$PC1+wineqt_PCA$PC2+wineqt_PCA$PC3
          +wineqt_PCA$PC4+wineqt_PCA$PC5+wineqt_PCA$PC6+wineqt_PCA$PC7)

summary(fit)

# 