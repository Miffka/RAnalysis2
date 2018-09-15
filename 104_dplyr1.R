#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(tidyr)
library(dplyr)
library(lazyeval)
library(microbenchmark)

# Функция data_frame
?data_frame()
my_data <- data_frame(x = rnorm(10000), y = rnorm(10000), f = factor(rep(1:2, 5000)))
my.data <- data.frame(x = rnorm(10000), y = rnorm(10000), f = factor(rep(1:2, 5000)))
# Разница, во-первых - в выводе. dplyr выводит только head(x, 10) и выводит типы данных
my.data; my_data

(diamonds <- as_data_frame(diamonds))

# Во-вторых, разница в именах переменных
(my_data_2 <- data_frame("My var" = rnorm(10)))
my_data_2$`My var`
(my.data.2 <- data.frame("My var" = rnorm(10))) #здесь в названии появляется точка
# dplyr добавляет переменные сразу после объявления!
(my_data_2 <- data_frame(x = rnorm(10), y = abs(x)))

# Обращение к строкам и колонкам - select и slice
# select
select(diamonds, cut) #diamonds$cut - выбор колонок
select(diamonds, cut, price) # тут запись уже сложнее - diamonds[, c("cut", "price")]
select(diamonds, cut:price) #все колонки между cut и price
select(diamonds, -cut) #все, кроме колонки cut
select(diamonds, 1) # выбор колонки возможен и по номерам!
#Дополнительные аргументы - выбор строк
select(diamonds, contains("c")) #строка содержит "c"
select(diamonds, ends_with("t")) #строка оканчивается на t

# slice
slice(diamonds, 2:10) #выбор строк
slice(diamonds, c(1, 4, 5)) #diamonds[c(1, 4, 5), ]

# filter
filter(diamonds, carat > 0.3) #отбор строк по условию
#diamonds[diamonds$carat > 0.3, ] #в такой записи дублируется запись датафрейма
filter(diamonds, carat > 0.3, color == "J")
#subset(diamonds, carat > 0.3 & color == "J")
filter(diamonds, carat > 0.3 | color == "J") #возможен отбор по логической формуле

# arrange
arrange(diamonds, price) #сортирует по значению переменных
arrange(diamonds, price, depth) #можно сортировать и по нескольким переменным
#diamonds[order(diamonds$price), ]
?order
order(c(0, 2, 1, -1, 3)) #возвращает последовательность индексов по возрастанию
?sort #сортирует вектор, но нельзя использовать в качестве индекса
#diamonds[order(diamonds$price, diamonds$depth)]
arrange(diamonds, desc(price)) #если обернуть переменную в desc, получим сортировку по убыванию

# rename
rename(diamonds, new_cut = cut) #простое переименование
#names(mtcars)[1] <- "new_mpg" 
rename(diamonds, new_cut = cut, new_carat = carat) #можно и несколько именовать

# mutate
m <- mutate(diamonds, sqrt_price = sqrt(price)) #добавляет новую переменную
m <- mutate(diamonds, 
            sqrt_price = sqrt(price),
            log_carat = log(carat))
mutate(mtcars, am = factor(am), vs = factor(vs))


#    Задача 1 - сохраняем в переменную d только нечетные строчки из diamonds
?n()
?seq
d <- diamonds %>%
  slice(seq(1, 53940, by = 2))


# заметка о пайпинге
iris %>% 
  filter(Petal.Length > 1.7) %>% 
  arrange(Sepal.Length) %>% 
  select(Sepal.Length, Sepal.Width)


#    Задача 2 - отобрать из mtcars 

my_df <- mtcars %>%
  select(mpg, hp, am, vs) %>%
  filter(mpg > 14, hp > 100) %>%
  arrange(-mpg) %>% #сортировка со знаком минус - по убыванию!
  slice(1:10) %>%
  rename(`Miles per gallon` = mpg, `Gross horsepower` = hp)