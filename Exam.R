setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(dplyr)
library(tidyr)
library(ggplot2)
library(lazyeval)

#    Задача 1 - отфильтровать данные swiss, 

df1 <- swiss
(swiss)

df1 <- swiss %>% 
  filter(Agriculture > 40, Fertility > 60) %>% 
  summarize_all(sd) %>% 
  min

#    Задача 2 - количество продаж продуктов по месяцам

purchases <- data_frame(
  item = letters[1:5], 
  May = c(10, 101, 132, 57, 66), 
  June = c(13, 98, 116, 66, 68),
  July = c(14, 105, 127, 63, 72)
) %>% 
  gather("month", "purchases", May:July) %>%
  mutate(month = factor(month, levels = c("May", "June", "July"), ordered = TRUE))

ggplot(purchases, aes(x = item, y = purchases, fill = month, group = month))+
  geom_bar(colour = 'black', stat = 'identity', position = 'dodge')

#    Задача 3 - написать функцию, принимает датафрейм с количественной и факторной переменной
#с двумя градациями в любом порядке. Функция применяет тест Шапиро-Вилка и тест Барлетта.
#Если оба теста дает положительный результат, то нужно выводить возможность
#применения Т-теста.

v1 <- c(-1.24006269, -0.16370400, 0.05740410, 0.51363377, 0.95418578, 1.24538291, -0.72661498, -2.06792327, 0.15511739, -0.69702617, -0.30606372, -0.31510914, -0.07759533, 0.47965825, 0.89031082, -0.12266611, -0.84578779, -0.11017818, 0.37173824, 0.54610256)
v2 <- c(rep('a', 10), rep('b', 10))
d <- data.frame(v1, v2)

cl <- sapply(d, class)
n <- names(cl[cl == "numeric"])
f <- names(cl[cl == "factor"])

?group_by_

shap <- d %>% 
  group_by_(f) %>% 
  summarise_all(funs(shapiro.test(.)$p))
shap <- shap[n]
bart <- bartlett.test(d[,n],d[,f])$p.value
if (any(shap < 0.05) | bart < 0.05) {
  print('We can`t use T-test :(')
} else {
  print('We can use T-test!')
}


?shapiro.test()
?bartlett.test
?summarise_all
?filter_if

t_test_use <- function(df) {
  cl <- sapply(df, class)
  n <- names(cl[cl == "numeric"])
  f <- names(cl[cl == "factor"])
  shap <- df %>% 
    group_by_(f) %>% 
    summarise_all(funs(shapiro.test(.)$p))
  shap <- shap[n]
  bart <- bartlett.test(df[,n],df[,f])$p.value
  if (any(shap < 0.05) | bart < 0.05) {
    return('We can`t use T-test :(')
  } else {
    return('We can use T-test!')
  }
}

t_test_use(d)


#    Задача 4 - функция принимает на вход таблицу сопряженности любого размера. Возвращает
#название критерия, который можно применять к данной таблице - либо
#"Chi-squared", либо "F-test"

d <- cbind(c(10, 7), c(9, 11))

test_selection <- function(x) {
  if (any(x < 5)){
    return("F-test")
  } else {
    return("Chi-squared")
  }
}
test_selection(d)


#    Задача 5 - функция принимает на вход формулу и данные (data.frame), строит 
#дисперсионный анализ по указанной формуле и возвращает FALSE, если для всех 
#уровней значимости не меньше 0.05 гипотеза об отсутствии значимых различий значений
#зависимой переменной между группами по первому фактору отвергается, и TRUE — если 
#не отвергается.

?aov

set.seed(13)
t51 <- aov(y ~ f, data.frame(y = rnorm(100), 
                      f = factor(sample(1:3, 100, replace = TRUE))))
t52 <- summary(t51)[[1]]$`Pr(>F)`
t52 <- t52[1]

is.no.differences <- function(form, df){
  res <- summary(aov(form, df))[[1]]$`Pr(>F)`
  res <- res[1]
  return(res >= 0.05)
}

is.no.differences(y ~ f, data.frame(y = rnorm(100), 
                                    f = factor(sample(1:3, 100, replace = TRUE))))
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
is.no.differences(mpg ~ cyl + vs, mtcars)
summary(aov(mpg ~ cyl + vs, mtcars))[[1]]


#    Задача 6 - применимость линейной регрессии
# Функция принимает на вход датафрейм, строит линейную регрессию первого столбца
#на остальные и возвращает именованный список
# normality - shapiro.test(residuals)$p > 0.05
# multicollinearity - any(cor.test(predictors) > 0.6)

is.lm.suitable <- function(df) {
  re <- lm(df[,1] ~ as.matrix(df[,-1]), df)
  n <- shapiro.test(re$residuals)$p > 0.05
  cormatr <- cor(df[,-1])
  diag(cormatr) <- 0
  m <- any(abs(cormatr) > 0.6)
  return(list("normality" = n, "multicollinearity" = m))
}

mt <- mtcars[c("cyl", "am", "carb")]
str(mt)
?lm

regr <- lm(mt[,1] ~ as.matrix(mt[,-1]), mt)
plot(regr$residuals)
shapiro.test(regr$residuals)$p > 0.05
cor(mt[,-1])


str(is.lm.suitable(mtcars))
is.lm.suitable(mtcars[c("cyl", "am", "carb")])


#    Задача 7 - почистить данные 

t7 <- read.csv("https://stepik.org/media/attachments/lesson/33861/nz_milk_sample.csv",
               stringsAsFactors = TRUE)

t71 <- gather(t7, year, milksolids,-region)
t71$year <- sapply(t71[,2], function(x) {str_replace(x, "X", "")})
t73 <- as.data.frame(t71 %>% 
  mutate(year = ordered(as.numeric(year)),
         milksolids = as.numeric(milksolids)) %>% 
  arrange(year) %>% 
  group_by(year) %>% 
  mutate_if(is.numeric, funs(impute(., fun = mean))))

?gather
?spread
?mutate_all
?mutate_if
?impute
??importFrom
?trimws
?arrange

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(Hmisc)

normalize_data <- function(df) {
  df <- gather(df, year, milksolids, -region)
  df$year <- sapply(df[,2], function(x) {str_replace(x, "X", "")})
  df <- as.data.frame(df %>% 
    mutate(year = ordered(as.numeric(year)),
           milksolids = as.numeric(milksolids)) %>% 
    arrange(year) %>% 
    group_by(year) %>% 
    mutate_if(is.numeric, funs(impute(., fun = mean))))
  return(df)
}



t7mixed <- t7[sample(1:ncol(t7), size = ncol(t7), replace = FALSE)]
t7n <- normalize_data(t7)
t7mixedn <- normalize_data(t7mixed)
all.equal(t7n, t7mixedn)

#    Задача 8 - применить facet_grid

universities <- read.csv('https://stepik.org/media/attachments/lesson/31919/universities.csv')

ggplot(universities[universities$year %in% c(2013:2015),], aes(x = excellent_students))+
  geom_histogram()+
  facet_grid(year ~ university)


#    Задача 9 - строим красивые графики

company <- read.csv('https://stepik.org/media/attachments/lesson/31919/company.csv')
?seq

ggplot(company, aes(y = income, x = department))+
  geom_violin()+
  geom_boxplot(width = 0.5)+
  scale_x_discrete(name = "Филиал",labels = c("Филиал №1", "Филиал №2", "Филиал №3", "Филиал №4"))+
  scale_y_continuous(name = "Прибыль", limits = c(1300,1700), breaks = seq(1300, 1700, 50))
