#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(ggvis)
library(tidyr)
library(dplyr)
library(rmarkdown)
library(gplots) #для ROCR
library(ROCR) #для логистических регрессий
library(survival)

str(DNase)
dnase <- DNase


str(infert)
table(infert$case)
model1 <- glm(case ~age+parity+education+spontaneous+induced, data = infert, family = binomial)
summary(model1)
infert %>% 
  select(case, education) %>% 
  group_by(education, case) %>% 
  summarise(Total = n())
finding1 <- data.frame(infert %>% 
  select(case, spontaneous, induced) %>% 
  group_by(induced, spontaneous) %>% 
  summarise(TotalAbortions = sum(case),
            Total = n(),
            AbortionPercent = sum(case)/n()))
# Вот, нашли интересную гипотезу. Вероятность инфертильности тем выше, 
#чем больше индуцированных абортов было совершено до этого.
ggplot(infert, aes(y = age, x = case))+
  geom_boxplot(aes(group = case))

ggplot(finding1, aes(x = induced, y = AbortionPercent, color = factor(spontaneous)))+
  geom_point()+
  geom_line(aes(group = factor(spontaneous)))

ggplot(finding1, aes(x = spontaneous, y = AbortionPercent, color = factor(induced),
                     group = factor(induced)))+
  geom_point()+
  geom_line()

# Еще одна интересная гипотеза - для описания данных случаев достаточно хорошо подходят
#как модель с пересечениями, так и модель без пересечений

data1 <- data.frame(infert)

str(model1)
exp(model1$coefficients)
?performance
?predict
?prediction
?ggvis
?infert
?anova

#Пишем функцию для вытаскивания хороших рок-кривых из модели
get_model_perf <- function(model_name, model_number) {
  perf <- model_name %>% 
    predict(type = "response") %>% 
    prediction(infert$case) %>% 
    performance("sens", "spec")
  ans <- data.frame(perf@x.values, perf@y.values, model_number)
  colnames(ans) <- c("Specificity", "Sensitivity", "ModelNumber")
  ans
}

#Набираем статистику по моделям
model1 <- glm(case ~ spontaneous+induced, data = infert, family = binomial)
f1 <- get_model_perf(model1, 1)

model2 <- glm(case ~ age+parity+education+spontaneous+induced,
              data = infert,
              family = binomial())
f2 <- get_model_perf(model2, 2)

model3 <- glm(case ~ ., data = infert, family = binomial())
summary(model3)
f3 <- get_model_perf(model3, 3)

#Склеиваем все модели
ftot <- rbind(f1, f2, f3) %>% 
  mutate(ModelNumber = factor(ModelNumber))

#Рисуем графики
ggplot(data = ftot, aes(Specificity, Sensitivity, group = ModelNumber, color = ModelNumber))+
  geom_line()

ggvis(data = ftot, ~Specificity, ~Sensitivity, stroke = ~ModelNumber) %>% 
  group_by(ModelNumber) %>% 
  layer_lines()
