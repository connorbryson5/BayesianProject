setwd("~/Downloads")
library(ggplot2)
library(tidyverse)


df <- read.csv("WineQT.csv")

view(df)

#Chlorides Density Plot
ggplot(df, aes(x = chlorides)) + geom_histogram(binwidth = .01) + 
  geom_vline(aes(xintercept=mean(chlorides)), color = "blue", linetype = "dashed", size = 1) +
  xlim(0,.2)

#Fixed Acidity Density Plot
ggplot(df, aes(x = fixed.acidity)) + geom_histogram(binwidth = .2) + 
  geom_vline(aes(xintercept=mean(fixed.acidity)), color = "blue", linetype = "dashed", size = 1) +
  xlim(0,16)

#Volatile Acidity Density Plot
ggplot(df, aes(x = volatile.acidity)) + geom_histogram(binwidth = .05) + 
  geom_vline(aes(xintercept=mean(volatile.acidity)), color = "blue", linetype = "dashed", size = 1) +
  xlim(0,1.2)

#Residual Sugar Density Plot
ggplot(df, aes(x = residual.sugar)) + geom_histogram(binwidth = .2) + 
  geom_vline(aes(xintercept=mean(residual.sugar)), color = "blue", linetype = "dashed", size = 1) +
  xlim(0,8)

#Chlorides Density Plot
ggplot(df, aes(x = chlorides)) + geom_histogram(binwidth = .005) + 
  geom_vline(aes(xintercept=mean(chlorides)), color = "blue", linetype = "dashed", size = 1) +
  xlim(0,.2)

#Free Sulfur Dioxide Density Plot
ggplot(df, aes(x = free.sulfur.dioxide)) + geom_histogram(binwidth = 2) + 
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide)), color = "blue", linetype = "dashed", size = 1) +
  xlim(0, 40)

#Total Sulfur Dioxide Density Plot
ggplot(df, aes(x = total.sulfur.dioxide)) + geom_histogram(binwidth = 4) + 
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide)), color = "blue", linetype = "dashed", size = 1) +
  xlim(0, 125)

#Density Density Plot
ggplot(df, aes(x = density)) + geom_histogram(binwidth = .0005) + 
  geom_vline(aes(xintercept=mean(density)), color = "blue", linetype = "dashed", size = 1) +
  xlim(.99, 1.005)

#pH Density Plot
ggplot(df, aes(x = pH)) + geom_histogram(binwidth = .05) + 
  geom_vline(aes(xintercept=mean(pH)), color = "blue", linetype = "dashed", size = 1) +
  xlim(3, 4)

#pH sulphate Plot
ggplot(df, aes(x = sulphates)) + geom_histogram(binwidth = .05) + 
  geom_vline(aes(xintercept=mean(sulphates)), color = "blue", linetype = "dashed", size = 1) +
  xlim(.25, 1.25)

#pH alcohol Plot
ggplot(df, aes(x = alcohol)) + geom_histogram(binwidth = .2) + 
  geom_vline(aes(xintercept=mean(alcohol)), color = "blue", linetype = "dashed", size = 1) +
  xlim(8.5, 14)

#pH quality Plot
ggplot(df, aes(x = quality)) + geom_histogram(binwidth = 1) + 
  geom_vline(aes(xintercept=mean(quality)), color = "blue", linetype = "dashed", size = 1) +
  xlim(0, 10)

quality<- df%>%
  #group_by(quality)%>%
  summarise(
    mean_fixed.acidity=mean(fixed.acidity),
    mean_chlorides= mean(chlorides),
    count= n())%>%
  view()

