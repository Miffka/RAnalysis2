#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(tidyr)
library(dplyr)
library(lazyeval)
library(microbenchmark)
library(scales)

# mutate_each
d <- as_data_frame(matrix(rnorm(30), ncol = 5))
mutate_each(d, funs(abs)) #берем модуль по каждой колонке
#abs(d)
#внутрь функции входит аргумент "."
mutate_each(d, funs(ifelse(. < 0, 0, .))) #векторизованные функции подходят!
#as.data.frame(sapply(d, function(x) ifelse(x < 0, 0, x)))
col_1 <- d$V1
ifelse(col_1 < 0, 0, col_1)
ifelse(d$V2 < 0, 0, d$V2)


#    Задача 1 - преобразуем все переменные в фактор

all_to_factor <- function(df) {
  df %>%
    mutate_each(funs(factor))
}


#    Задача 2 - преобразуем все данные в логарифм, а перед этим центрируем их
?rescale
?mutate_each_
?mutate_if
test_data <- data_frame(V1 = c(1.5, -0.1, 2.5, -0.3, -0.8), 
                        V2 = c(-0.9, -0.3, -2.4, 0.0, 0.4), 
                        V3 = c(-2.8, -3.1, -1.8, 2.1, 1.9), 
                               V4 = c("A", rep("B", 4)))


numeric_index <- colnames(test_data)[sapply(test_data, is.numeric)]
test_data %>%
  mutate_each_(funs(log(rescale(., to = c(1, 2)))), vars = numeric_index)

test_data %>%
  mutate_if(is.numeric, funs(log(rescale(., to = c(1, 2)))))

library(scales)
log_transform <- function(df) {
  df %>%
    mutate_if(is.numeric, funs(log(rescale(., to = c(1, 2)))))
}

log_transform(test_data)


# group_by
diamonds <- as_data_frame(diamonds)
(gr_diamonds <- group_by(diamonds, cut)) #создаем сгруппированные данные
sample_n(diamonds, 2)
slice(diamonds, 1)
# все функции применяются к каждой части сгруппированного датафрейма
sample_n(gr_diamonds, 2) #вытаскиваем по 2 случайных наблюдения из каждой группы!
slice(gr_diamonds, 1) #вытаскиваем по 1 первому наблюдению из каждой группы

# summarise
?summarise()
summarise(mtcars, mean(disp))
summarise(mtcars, mean_disp = mean(disp), sd_disp = sd(disp))
#можем расчитывать показатели по разным переменным
summarise(gr_diamonds, 
           mean_price = mean(price),
           mean_x = mean(x),
           median_y = median(y),
          min_y = min(y))

# группируем переменные по нескольким
gr_diamonds <- group_by(diamonds, cut, color)
s <- summarise(gr_diamonds, 
               n = n(),
              mean_price = mean(price),
              mean_x = mean(x),
              median_y = median(y),
              min_y = min(y),
              great_price = sum(price > 5000)) #можем накладывать логические условия и искать число удовлетворяющих
s
sum(filter(diamonds, cut == "Fair", color == "D")$price >5000)

# summarise - больше и больше
#позволяет получить нужные данные быстро и просто
gr_mtcars <- group_by(mtcars, am, vs)
(my_means <- summarise_all(gr_mtcars, funs(sd)))
gr_mtcars %>%
  summarise_all(funs(mean, sd))

# Пример с nse
var_to_select <- "hp"
#необходимо использовать функции с нижним подчеркиванием
select_(mtcars, var_to_select)
#Более сложный пример
mutate(mtcars, new_var = (hp - mean(hp)) / sd(hp))
mini_mtcars <- select(mtcars, hp, am, vs)
mini_mtcars <- mini_mtcars %>% 
  mutate(am = factor(am),
         vs = factor(vs))
#пишем код для динамического обращения к количественной переменной для стандартизации
#во всех следующих случаях можем помещать на место hp любое вычисленное имя переменной
#1 помещаем выражение в строку
mutate_(mini_mtcars, new_var = "(hp - mean(hp)) / sd(hp)")
#2 используем запись через формулу
mutate_(mini_mtcars, new_var = ~ (hp - mean(hp)) / sd(hp))
#3 используем функцию quote
mutate_(mini_mtcars, new_var = quote((hp - mean(hp)) / sd(hp)))

# lazyeval
num_var <- names(which(sapply(mini_mtcars, is.numeric)))
mutate_(mini_mtcars, new_var = interp(~(var - mean(var)) / sd(var), var = as.name(num_var)))
#если имя переменной - это строка, ее нужно обернуть функцией as.name()
#interp("(var - mean(var)) / sd(var)", var = as.name(num_var))

var_for_group <- c("am", "vs")
var_for_filter <- "hp"
var_for_arrange <- "mpg"
var_for_mutate <- "qsec"
var_for_summirise <- "cyl"
group_by_(mtcars, .dots = var_for_group) %>% 
  filter_(interp(~var > 100, var = as.name(var_for_filter))) %>% 
  arrange_(var_for_arrange) %>% 
  mutate_(var = interp(~ifelse(var > mean(var), 1, 0), 
                           var = as.name(var_for_mutate))) %>% 
  summarise_(max = interp(~max(var), var = as.name(var_for_summirise)))
#способ с тильдой - самый предпочтительный!

# немного побольше о mutate_?? и summarise_??
by_species <- iris %>% group_by(Species)
by_species %>% 
  summarise_if(is.numeric, mean)

#расчет среднего только для численных значений со средним больше 2
by_species %>%
  summarise_if(function(col) {
    if(!is.numeric(col)) return(FALSE) else mean(col) > 2
  }, mean)

#статистика для конкретных столбцов
by_species %>% 
  summarise_at(vars(Petal.Width), mean)
by_species %>% 
  summarise_at(vars(matches("Width")), mean)

#конкретно обращаемся к переменным по именам или номерам
by_species %>% 
  summarise_at(c("Sepal.Width", "Petal.Width"), mean)
by_species %>% 
  summarise_at(c(1, 3), mean)

#совместное использование
by_species %>% 
  summarise_at(vars(Petal.Width, Petal.Length), funs(min, max))
by_species %>% 
  summarise_at(vars(matches("Width")), funs(min, max))

#отбор колонок по условию - select_if
select_if(iris, is.numeric) %>% 
  summarise_all(funs(sd))


#    Задача 3 - группируем датафрейм и получаем описательные статистики для каждой группы

?quantile
?funs
?summarise
?mean
test_data <- read.csv("https://stepic.org/media/attachments/course/724/salary.csv")

str(test_data)
str(quantile(test_data$salary, 0.25, na.rm = T))
test_data %>% 
  group_by(gender, country) %>% 
  summarise(n = n(), 
            mean = mean(salary, na.rm = T),
            sd = sd(salary, na.rm = T), 
            median = median(salary, na.rm = T),
            first_quartile = quantile(salary, 0.25, na.rm = T),
            third_quartile = quantile(salary, 0.75, na.rm = T),
            na_values = sum(is.na(salary)))

descriptive_stats <- function(df) {
  df %>% 
    group_by(gender, country) %>% 
    summarise(n = n(), 
              mean = mean(salary, na.rm = T),
              sd = sd(salary, na.rm = T), 
              median = median(salary, na.rm = T),
              first_quartile = quantile(salary, 0.25, na.rm = T),
              third_quartile = quantile(salary, 0.75, na.rm = T),
              na_values = sum(is.na(salary)))
}

descriptive_stats(test_data)


#    Задача 4 - перевести выбранные колонки в факторы по принципу больше-меньше среднего

?mutate_at
str(mtcars)
mtcars[1:4] %>% 
  mutate_at(c(1, 3),
            funs(factor(ifelse(. > mean(.), 1, 0))))

to_factors <- function(test_data, factors) {
  test_data %>% 
    mutate_at(factors, 
              funs(factor(ifelse(. > mean(.), 1, 0))))
}

to_factors(mtcars[1:4], factors = c(1, 3))


#    Задача 5 - 10 самых дорогих бриллиантов каждого цвета

high_price <- diamonds %>% 
  group_by(color) %>% 
  arrange(-price) %>% 
  select(color, price) %>% 
  slice(1:10)
high_price
