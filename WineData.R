df <- read_csv("WineQT.csv")
view(df)
library(tidyverse)
library(ggplot2)
library(plyr)
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
mean(df_citric$quality[df_citric$citric_zero == 0])
#5.44
mean(df_citric$quality[df_citric$citric_zero == 1])
#5.68