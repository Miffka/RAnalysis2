#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(tidyr)
library(dplyr)
library(lazyeval)
library(microbenchmark)

#    Производные функции от apply
# apply в чистом виде можно применить только к датафрейму или матрице
# применяем то же самое к списку или вектору при помощи специальных функций

#lapply(list, function) 
#sapply(list, function)
#tapply(vector, index, function)

#vapply(list, function, FUN.VALUE = type, ...)
#eapply()
#mapply(function)

my_list <- list(x = rnorm(30), y = rnorm(20))
my_listNA <- list(x = c(rnorm(30), NA), y = rnorm(20))
str(my_list)

lapply(my_list, mean)
# принимает на вход список и дает тоже список такой же длины
# приняли список из двух векторов - возвратили действие над каждым из векторов
lapply(my_listNA, mean, na.rm = T)
lapply(my_list, function(x) x*2)

sapply(my_listNA, mean, na.rm = T)
# пытается представить вывод в виде вектора или матрицы, если это возможно
sapply(my_listNA, range, na.rm = T, simplify = T)


#    Задача 1 - получаем датафрейм, находим сумму положительных значений 
# в каждой переменной и сохраняем в список

test_data11 <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))
sum(test_data11$X1[test_data11$X1>0])

positive_sum <- function(df) {
  lapply(df, function(x) sum(x[x>0], na.rm = T))
}

positive_sum(test_data11)


# особенность sapply - можно применять к вектору!
cars <- c("Mazda", "Volga", "Merc")
car <- "Mazda RX4"
# ищем неточное совпадение вектора со значением
grepl("Mazda", "Mazda RX4")

#плохой код
for (i in cars) {
  if (grepl(i, car)) {
    my_car <- i
  }
}

# хороший код
sapply(cars, function(x) grepl(x, car))
cars[sapply(cars, function(x) grepl(x, car))]

# отбираем только количественные колонки в данных
iris_num <- iris[sapply(iris, is.numeric)]

# функции sapply и lapply можно применять и к датафрейму!
sapply(iris[1:4], sd)
apply(iris[1:4], 2, sd)

# однако apply приводит датафрейм к матрице с единственным типом данных
# перед применением функции
# для отбора количественных переменных и индексации подходит только sapply
sapply(iris, is.numeric)
apply(iris, 2, is.numeric)


#    Другие функции семейства apply

# tapply - получаем характеристику у переменной, сгруппированной по другой переменной
tapply(mtcars$mpg, mtcars$am, mean)
aggregate(mpg ~ am, mtcars, function(x) c(mean(x), sd(x)))

str(iris)
# функция by режет исходный датафрейм на несколько датафреймов по группирующей переменной
# необходимо прописывать функции, которые можно применить к датафреймам
by(iris[1:4], iris$Species, colMeans)
by(iris[1:4], iris$Species, function(x) sapply(x, 
                                               function(col) shapiro.test(col)$p.value))

# но мы можем сделать все то же самое в aggregate гораздо короче!
aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)


# vapply(list, function, FUN.VALUE = type, ...)
# можем задать тип данных на выводе

vapply(mtcars, mean, FUN.VALUE = numeric(1))

# mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
# пробегает по срезам поданных значений и применяет функцию к срезам

mapply(rep, c(1, 2, 3, 4), c(1, 2, 3, 4))
#rep(x, times)
x <- c(20, 25, 13)
m <- c(0, 1, 2)
s <- c(3, 5, 6)
mapply(rnorm, x, m, s)
#rnorm(n, mean = 0, sd = 1)

# применяем mapply для присвоения имен строкам и столбцам в матрице
m <- matrix(rnorm(100 * 200), nrow = 100)

m_names <- mapply(paste, list("row", "col"), list(1:100, 1:200), sep="_")
#paste(..., sep = " ", collapse = NULL) #converts arguments to character strings
str(m_names)
dimnames(m) <- m_names

# подводный камень функций семейств apply для датафреймов

get_sd <- function(x) {
  num_var <- sapply(x, is.numeric)
  sapply(x[, num_var], sd) #уязвимость вот в этой индексации
}

get_sd(iris)
# казалось бы, все работает

#проверим на датафрейме с одной количественной переменной
my_ds <- data.frame(x = 1:10, y = letters[1:10])
get_sd(my_ds)

# в случае, если у нас только одна количественная переменная, обращение x[, num_var]
#  вернет колонку в виде вектора, а sapply применит функцию sd к каждому наблюдению
# вместо того, чтобы применить ко всей переменной
#    чтобы этого избежать - применяем индексацию my_df[col_index]

get_sd <- function(x) {
  num_var <- sapply(x, is.numeric)
  sapply(x[num_var], sd)
}

get_sd(my_ds)
get_sd(iris)


#    Задача 2 - отбираем наблюдения по частичному совпадению строковой переменной с эталоном

test_data21 <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
names21 = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")

#aggregate(. ~ name, test_data21, function(row) ))

#отбираем названия при помощи регулярных выражений - удаляем начало слова включая символ @
task21 <- sub("[[:alnum:]]+@", "", test_data21[,1])

#мое решение
#находим вектор, в котором записаны имена для нужной переменной
task22 <- sapply(test_data21$short, function(x) any(grepl(x, names21)))
#выводим все по этому датафрейму
as.data.frame(apply(test_data21, 2, function(x) x[task22]))[,1:2]

#не мое решение
#выводим датафрейм по инструкции %in%
test_data21[task21 %in% names21,]

my_names <- function(df, names) {
  df$short <- sub("[[:alnum:]]+@", "", df[,1])
  index <- sapply(df$short, function(x) any(grepl(x, names)))
  df1 <- as.data.frame(apply(df, 2, function(x) x[index]))[,1:2]
  df1$expression <- as.numeric(as.character(df1$expression))
  return(df1)
}

task23 <- my_names(test_data21, names21)
str(task23)

#Совсем правильный ответ - через инструкцию %in%
my_names <- function (dataset, names){    
  gs=gsub('^.*\\@','',dataset[,1])    
  return(dataset[gs %in% names,])
}


#    Задача 3 - проверить наблюдения в группе на выбросы и создать новую переменную

?mutate
?summarise
?interp
?seq
?group_by
?mutate_all
?is.factor
?select
?cat
?filter
?Filter
?one_of
?Vectorize
?interp
vignette("nse")

#task31 <- mtcars1 %>%
  #group_by(am, cyl) %>%
  #mutate_all(funs(ifelse(. > abs(mean(.) + 2*sd(.)), 1, 0)))

task31 <- ToothGrowth %>% 
  mutate(dose = factor(dose))

task32 <- task31 %>% 
  group_by_(.dots = n32) %>%
  mutate_all(funs(ifelse(. > mean(.) + 2*sd(.) | . < mean(.) - 2*sd(.), 1, 0)))

which(task32$len == 1)
task31$is_outlier <- task32[[n33]]

n32 <- task32 %>% Filter(f = is.factor) %>% names
n33 <- task32 %>% Filter(f = is.numeric) %>% names

# вот она - функция моей мечты!!!
find_outliers <- function(df) {
  factvar <- df %>% Filter(f = is.factor) %>% names
  numvar <- df %>% Filter(f = is.numeric) %>% names
  df1 <- df %>%
    group_by_(.dots = factvar) %>%
    mutate_all(funs(ifelse(. > mean(.) + 2*sd(.) | . < mean(.) - 2*sd(.), 1, 0)))
  df$is_outlier <- df1[[numvar]]
  return(df)
}

test_data <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/hard_task_ans.csv")
all.equal(find_outliers(test_data), correct_answer)

test_data <- read.csv("https://stepic.org/media/attachments/course/724/test_data_2.csv")
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/test_data_2_ans.csv")
all.equal(find_outliers(test_data), correct_answer)

# Правильное решение в dplyr
find_outliers <- function(t){    
  numeric_index <- colnames(t)[sapply(t, is.numeric)]    
  factor_index <- colnames(t)[sapply(t, is.factor)]    
  t <- group_by_(t, .dots = factor_index) %>%    
    mutate_(is_outlier = interp(~ifelse(abs(mean(x) - x) > 2 * sd(x), 1, 0), x = as.name(numeric_index)))    
  return(t)
}


#    Задача 4 - линейная регрессия только на нормализованных переменных

?subset
?lm
str(swiss)

task4 <- sapply(swiss[,-1], function(x) shapiro.test(x)$p.value)
task4 <- names(task4[task4 > 0.05])
(task41 <- lm(swiss[[1]] ~ ., swiss[colnames(swiss) %in% task4])$coef)
swiss[colnames(swiss) %in% task4]
length(swiss)

# мое решение
smart_lm <- function(df){
  if (length(df) > 2) {
    nvec <- sapply(df[,-1], function(x) shapiro.test(x)$p.value)
  } else {
    nvec <- shapiro.test(df[, -1])$p.value
  }
  nvec <- names(nvec[nvec > 0.05])
  if (length(nvec) != 0) {
        ans <- lm(df[[1]] ~ ., df[colnames(df) %in% nvec])$coef
  } else {
    ans <- "There are no normal variables in the data"
  }
  return(ans)
}

smart_lm(swiss)
test_data <- read.csv("https://stepik.org/media/attachments/course/724/test.csv")
test_data <- data.frame(x = 1:100, y = 1:100, z = 1:100)
smart_lm(test_data)

# Правильное решение (не мое)
smart_lm <- function(x){    
  check_norm <- sapply(x[-1], function(var) shapiro.test(var)$p.value > 0.05)    
  if (any(check_norm)){    
    x = x[, c(1, (which(check_norm) + 1))]    
    coef <- lm(x[[1]] ~ ., x[-1])$coef    
    return(coef)    
  } else{    
    return('There are no normal variables in the data')}}


#    Задача 5 - сравнить все числовые переменные датасета со средним в генеральной совокупности
# вернуть список из t-значения(t), числа степеней свободы(df) и p-значения

?c
?t.test
str(iris)
numind <- colnames(iris)[sapply(iris, is.numeric)]
task51 <- t.test(iris[[1]], mu=5)
c(task51$statistic, task51$parameter, task51$p.value)
unlist(task51[c(1, 2, 3)])

lapply(iris[numind], function(col) {
  task52 <- t.test(col, mu=5)
  return(c(task52$statistic, task52$parameter, task52$p.value))
})

# мое решение - хорошее!
one_sample_t <- function(df, genmean){
  numeric_index <- colnames(df)[sapply(df, is.numeric)]
  lapply(df[numeric_index], function(col) {
    testres <- t.test(col, mu = genmean)
    return(c(testres$stat, testres$param, testres$p.value))
  })
}

one_sample_t(iris, 4)


#    Задача 6 - из списка списков вытащить список, состоящий из одного p.value

?lapply
?microbenchmark
normality_tests <- lapply(iris[, 1:4], shapiro.test)
lapply(normality_tests, function(col) col$p.value)
task6 <- apply(iris[, 1:4], 1, shapiro.test)

# Мое решение - очень даже ничего
get_p_value1 <- function(test_list) {
  lapply(test_list, function(col) return(col$p.value))
}

get_p_value(normality_tests)

# Правильное решение
get_p_value2 = function(test_data){    
  sapply(test_data, '[', 2)}

# Проверим, как эти два алгоритма работают на двух наборах данных
microbenchmark(get_p_value1(normality_tests), get_p_value2(normality_tests))
microbenchmark(get_p_value1(task6), get_p_value2(task6))
