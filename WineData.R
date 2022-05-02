df <- read_csv("WineQT.csv")
view(df)
library(tidyverse)
library(ggplot2)
library(plyr)
library(ProbBayes)
library(rjags)

#bayesian and frequentist approach of t.test of citric acid effect

#df with citric acid and quality; citric_zero = 0 when zero citric acid, 1 when citric acid
df_citric <- df %>% 
  mutate("citric_zero" = ifelse(`citric acid`==0,0,1)) %>%
  select(citric_zero, quality) %>% 
  # group_by(citric_zero, quality) %>%
  # summarise(
  #   mean(quality)
  # ) %>%
  view()


#histrogram plots of quality counts with both levels of citric acid
ggplot(df_citric, aes(x=quality, color=citric_zero, fill = citric_zero)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~citric_zero) +
  geom_vline(aes(xintercept=mean(quality)),
             color="darkgreen", linetype="dashed", size=1)

# mean of both
mu0 <- mean(df_citric$quality[df_citric$citric_zero == 0])
#5.44
mu1 <- mean(df_citric$quality[df_citric$citric_zero == 1])
#5.68
mean_overall <- mean(df_citric$quality)
#5.66
sigma0 <- sd(df_citric$quality[df_citric$citric_zero == 0])/sqrt(length(citric_0))
#.703
sigma1 <- sd(df_citric$quality[df_citric$citric_zero == 1])/sqrt(length(citric_1))
#.812

citric_1 <- df_citric %>%
  filter(citric_zero == 1) %>%
  select(quality) %>%
  view()
citric_0 <- df_citric %>%
  filter(citric_zero == 0) %>%
  select(quality) %>%
  view()

cat("
model {
## sampling

for(i in 1:n0){
y_0[i] ~ dnorm(mu_0, phi0)
}
for(i in 1:n1){
y_1[i] ~ dnorm(mu_1, phi1)
}
## priors

mu_0 ~ dnorm(0, .0001)
mu_1 ~ dnorm(0, .0001)
phi0 ~ dgamma(.1,.1)
phi1 ~ dgamma(.1,.1)

##interests

diff <- mu_0 - mu_1
#ratio <- mu_0/mu_1
}
", file = "normal-normal.jags")


n0 = 99
n1 = 1044

inits <- list(mu_0 = 1, mu_1 = 1, phi0 = .1, phi1 = .1)
data <- list("y_0" = citric_0$quality, "y_1" = citric_1$quality, "n0" = n0, "n1" = n1)


jm <- jags.model("normal-normal.jags", data = data, quiet = TRUE)

samps <- coda.samples(jm, variable.names = c("diff"), n.iter = 10000)

ests <- summary(samps, quantiles = c(.025, .975))
ests
mcmc_hist(samps)
mcmc_areas(samps, "mu", prob = .95, point.est = "mean") + labs(x = expression(mu), title = "Posterior")
