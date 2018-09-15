## @knitr raw_data
rawData <- read.csv("SpeedDatingData.csv", comment.char = "#", na.strings = "..")

## @knitr data
library(dplyr)
data <- na.omit(rawData %>% 
  filter(gender == 0) %>% 
  select(attr_o, intel_o)) %>% 
  rename(attractivness = attr_o) %>% 
  rename(intelligence = intel_o)

## @knitr visualisation
library(ggplot2)
scatterplot <- ggplot(data = data, aes(attractivness, intelligence))+
  geom_jitter()+
  scale_x_discrete(limits = 0:10)+
  scale_y_discrete(limits = 0:10)+
  ggtitle("Figure 1. Relation between female attractivness and intelligence")+
  theme(plot.title = element_text(hjust = 0.5, size=11))

## @knitr correlation
correlation <- cor.test(data$attractivness, data$intelligence)
correlationCoefficient <- correlation$estimate
p.value <- correlation$p.value

## @knitr t-test
firstGroup <- data %>% 
  filter(attractivness %in% 0:5) %>% 
  mutate(group = as.factor(1))
secondGroup <- data %>% 
  filter(attractivness %in% 8:10) %>% 
  mutate(group = as.factor(2))
bothGroups <- rbind(firstGroup, secondGroup)

boxplot <- ggplot(bothGroups, aes(group, intelligence))+
  geom_boxplot()+
  ggtitle("Figure 2. Comparison of female intelligence in two groups")+
  theme(plot.title = element_text(hjust = 0.5, size=11))

statistic <- t.test(firstGroup$intelligence, secondGroup$intelligence, alternative = "less")
means <- statistic$estimate
firstGroupMeanIntel <- means[["mean of x"]]
secondGroupMeanIntel <- means[["mean of y"]]
statistic.pvalue <- statistic$p.value