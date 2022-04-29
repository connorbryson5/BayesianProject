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
wineqt_scaled = scale(wineqt[,1:11])

# PCA
wineqt_scaled = data.frame(wineqt_scaled)

wineqt_scaled$quality = wineqt$quality

summary(wineqt_scaled)

winepca = PCA(wineqt_scaled[1:1143,1:11], graph = TRUE)

eig_val <- get_eigenvalue(winepca)
eig_val

fviz_eig(winepca, addlabels = TRUE, ylim = c(0, 30))

var <- get_pca_var(winepca)
fviz_pca_var(winepca, col.var = "blue")

corrplot(var$cos2, is.corr=FALSE)

##########################################################

# Analysis 2

# Train-test split

split1<- sample(c(rep(0, 0.8 * nrow(wineqt_scaled)), rep(1, 0.2 * nrow(wineqt_scaled))))

# Most important variable

# Frequentist

# Training

fit <- lm(quality ~ fixed.acidity+volatile.acidity
          +citric.acid+residual.sugar
          +chlorides+free.sulfur.dioxide
          +total.sulfur.dioxide+density
          +pH+sulphates+alcohol, data = wineqt_scaled[split1 == 0, ])

summary(fit)

# Test

y_pred = predict(fit,wineqt_scaled[split1 == 1, 1:11])
y_true = wineqt_scaled[split1 == 1, 12]
r2 = cor(y_true, y_pred)^2
r2

plot(y_true,y_pred)

# Bayesien

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
y <- wineqt_scaled[split1 == 0, ]$quality
N <- length(y)

the_data <- list("y" = wineqt_scaled[split1 == 0, ]$quality,
                 "x1" = wineqt_scaled[split1 == 0, ]$fixed.acidity,
                 "x2" = wineqt_scaled[split1 == 0, ]$volatile.acidity,
                 "x3" = wineqt_scaled[split1 == 0, ]$citric.acid,
                 "x4" = wineqt_scaled[split1 == 0, ]$residual.sugar,
                 "x5" = wineqt_scaled[split1 == 0, ]$chlorides,
                 "x6" = wineqt_scaled[split1 == 0, ]$free.sulfur.dioxide,
                 "x7" = wineqt_scaled[split1 == 0, ]$total.sulfur.dioxide,
                 "x8" = wineqt_scaled[split1 == 0, ]$density,
                 "x9" = wineqt_scaled[split1 == 0, ]$pH,
                 "x10" = wineqt_scaled[split1 == 0, ]$sulphates,
                 "x11" = wineqt_scaled[split1 == 0, ]$alcohol,
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

# test
post <- as.mcmc(posterior)
df <- as.data.frame(cbind(post[, "beta0"], post[, "beta1"],
                          post[, "beta2"], post[, "beta3"],
                          post[, "beta4"], post[, "beta5"],
                          post[, "beta6"], post[, "beta7"],
                          post[, "beta8"], post[, "beta9"],
                          post[, "beta10"], post[, "beta11"]))
names(df) <- c("beta0", "beta1","beta2", "beta3",
               "beta4", "beta5","beta6", "beta7",
               "beta8", "beta9","beta10", "beta11")

one_expected <- function(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11){
  expected_y <- post[ , "beta0"] + x1 * post[ , "beta1"] + x2 * post[ , "beta2"] 
  + x3 * post[ , "beta3"]+ x4 * post[ , "beta4"]+ x5 * post[ , "beta5"]
  + x6 * post[ , "beta6"]+ x7 * post[ , "beta7"]+ x8 * post[ , "beta8"]
  + x9 * post[ , "beta9"]+ x10 * post[ , "beta10"]+ x11 * post[ , "beta11"]
  
  data.frame(Expected_GrossSales = expected_y)
}

x1 = wineqt_scaled[split1 == 1, ]$fixed.acidity
x2 = wineqt_scaled[split1 == 1, ]$volatile.acidity
x3 = wineqt_scaled[split1 == 1, ]$citric.acid
x4 = wineqt_scaled[split1 == 1, ]$residual.sugar
x5 = wineqt_scaled[split1 == 1, ]$chlorides
x6 = wineqt_scaled[split1 == 1, ]$free.sulfur.dioxide
x7 = wineqt_scaled[split1 == 1, ]$total.sulfur.dioxide
x8 = wineqt_scaled[split1 == 1, ]$density
x9 = wineqt_scaled[split1 == 1, ]$pH
x10 = wineqt_scaled[split1 == 1, ]$sulphates
x11 = wineqt_scaled[split1 == 1, ]$alcohol

y_pred_baye = rep(1,length(x1))
for (i in 1:length(x1)){
expected_quality <- one_expected(x1[i],x2[i],x3[i],x4[i],x5[i],x6[i],x7[i],
                                 x8[i],x9[i],x10[i],x11[i])
names(expected_quality) <- c("value")
y_pred_baye[i] = quantile(expected_quality$value, 0.5)
}

r2_baye = cor(y_true, y_pred_baye)^2
r2_baye

plot(y_true,y_pred_baye)
######################################

# Analysis 3 After PCA

# 7 PCs can explain 90% of variance

# Frequentist

wineqt_PCA = read.csv("PC_new_data_wine.csv")
wineqt_PCA$quality = wineqt$quality
PCA_fit <- lm(quality ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7, 
              data = wineqt_PCA[split1 == 0, ])

summary(PCA_fit)

y_pred_PCA = predict(PCA_fit,wineqt_PCA[split1 == 1, 1:7])
r2_PCA = cor(y_true, y_pred_PCA)^2
r2_PCA
plot(y_true,y_pred_PCA)
# Bayesian

modelString <-"
model {
## sampling
for (i in 1:N){
y[i] ~ dnorm(beta0 + beta1 * x1[i] + beta2 * x2[i] + beta3 * x3[i] + 
beta4 * x4[i]+ beta5 * x5[i] + beta6 * x6[i] + beta7 * x7[i], invsigma2)
}

beta0 ~ dnorm(mu0, g0)
beta1 ~ dnorm(mu1, g1)
beta2 ~ dnorm(mu2, g2)
beta3 ~ dnorm(mu3, g3)
beta4 ~ dnorm(mu4, g4)
beta5 ~ dnorm(mu5, g5)
beta6 ~ dnorm(mu6, g6)
beta7 ~ dnorm(mu7, g7)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}"
y <- wineqt_PCA[split1 == 0, ]$quality
N <- length(y)

the_data <- list("y" = wineqt_PCA[split1 == 0, ]$quality,
                 "x1" = wineqt_PCA[split1 == 0, ]$PC1,
                 "x2" = wineqt_PCA[split1 == 0, ]$PC2,
                 "x3" = wineqt_PCA[split1 == 0, ]$PC3,
                 "x4" = wineqt_PCA[split1 == 0, ]$PC4,
                 "x5" = wineqt_PCA[split1 == 0, ]$PC5,
                 "x6" = wineqt_PCA[split1 == 0, ]$PC6,
                 "x7" = wineqt_PCA[split1 == 0, ]$PC7,
                 "N" = length(y),
                 "mu0" = 0, "g0" = 0.0025,
                 "mu1" = 0, "g1" = 0.0025,
                 "mu2" = 0, "g2" = 0.0025,
                 "mu3" = 0, "g3" = 0.0025,
                 "mu4" = 0, "g4" = 0.0025,
                 "mu5" = 0, "g5" = 0.0025,
                 "mu6" = 0, "g6" = 0.0025,
                 "mu7" = 0, "g7" = 0.0025,
                 "a" = 0.001, "b" = 0.001)


posterior_PCA <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("beta0", "beta1",
                                  "beta2", "beta3",
                                  "beta4", "beta5",
                                  "beta6", "beta7",
                                  "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 15000)

summary(as.mcmc(posterior_PCA))

plot(posterior_PCA, vars = "beta1")

# test
post_PCA <- as.mcmc(posterior_PCA)
df_PCA <- as.data.frame(cbind(post_PCA[, "beta0"], post_PCA[, "beta1"],
                          post_PCA[, "beta2"], post_PCA[, "beta3"],
                          post_PCA[, "beta4"], post_PCA[, "beta5"],
                          post_PCA[, "beta6"], post_PCA[, "beta7"]))
names(df_PCA) <- c("beta0", "beta1","beta2", "beta3",
               "beta4", "beta5","beta6", "beta7")

one_expected_PCA <- function(x1,x2,x3,x4,x5,x6,x7){
  expected_y <- post[ , "beta0"] + x1 * post[ , "beta1"] + x2 * post[ , "beta2"] 
  + x3 * post[ , "beta3"]+ x4 * post[ , "beta4"]+ x5 * post[ , "beta5"]
  + x6 * post[ , "beta6"]+ x7 * post[ , "beta7"]
  
  data.frame(Expected_GrossSales = expected_y)
}

x1_PCA = wineqt_PCA[split1 == 1, ]$PC1
x2_PCA = wineqt_PCA[split1 == 1, ]$PC2
x3_PCA = wineqt_PCA[split1 == 1, ]$PC3
x4_PCA = wineqt_PCA[split1 == 1, ]$PC4
x5_PCA = wineqt_PCA[split1 == 1, ]$PC5
x6_PCA = wineqt_PCA[split1 == 1, ]$PC6
x7_PCA = wineqt_PCA[split1 == 1, ]$PC7

y_pred_baye_PCA = rep(1,length(x1))
for (i in 1:length(x1)){
  expected_quality_PCA <- one_expected_PCA(x1_PCA[i],x2_PCA[i],x3_PCA[i],
                                   x4_PCA[i],x5_PCA[i],x6_PCA[i],x7_PCA[i])
  names(expected_quality_PCA) <- c("value")
  y_pred_baye_PCA[i] = quantile(expected_quality_PCA$value, 0.5)
}

r2_baye_PCA = cor(y_true, y_pred_baye_PCA)^2
r2_baye_PCA

plot(y_true,y_pred_baye_PCA)
