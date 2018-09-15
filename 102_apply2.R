#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)

outliers_count <- function(x) {
  outliers <- x[abs(x - mean(x)) > 2 * sd(x)]
  if (length(outliers) > 0) {
    return(outliers)
  } else {
    return("There are no outliers")
  }
}

iris_outliers <- apply(iris[, 1:4], 2, outliers_count)


#    что такое ...

head(airquality)
?airquality

#!
apply(airquality, 2, mean) #получили не очень хороший результат

# пишем через запятую после функции дополнительные аргументы для функции
apply(airquality, 2, mean, na.rm = T)

# следующие функции оптимизированы лучше, чем apply
colMeans(airquality)
colSums(airquality)
rowMeans(airquality)
colMeans(airquality)


#    
set.seed(42) #закрепляем генератор случайных чисел в одном положении

d <- as.data.frame(matrix(rnorm(30), nrow = 5))

my_fun <- function(x) {
  y <- x * 2
}

my_fun(1:10)


#    сохраняем отрицательные переменные - создаем функцию внутри apply

#плохой код
my_list <- list()
for (i in seq_along(d)) {
  temp_col <- d[, i]
  neg_numbers <- temp_col[temp_col < 0]
  my_list[[i]] <- neg_numbers
}
names(my_list) <- colnames(d)

#код получше, но с заведением лишней функции
find_negative <- function(x) {
  x[x<0]
}
apply(d, 2, find_negative)

#совсем хороший код - функция внутри apply нигде не сохраняется
apply(d, 2, function(x) x[x>0]) #здесь x - это колонка, поскольку в MARGIN стоит 2

#проблема - пропущенные значения
d[[1]] <- NA
apply(d, 2, function(x) x[x<0 & !is.na(x)])


#    задача 1 - вернуть отрицательные значения

test_data11 <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), V2 = c(NA, -10.2, -10.1, -9.3, -12.2), V3 = c(NA, NA, -9.3, -10.9, -9.8)))
test_data12 <- as.data.frame(list(V1 = c(NA, 0.5, 0.7, 8), V2 = c(-0.3, NA, 2, 1.2), V3 = c(2, -1, -5, -1.2)))
test_data13 <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), V2 = c(-0.3, NA, -2, -1.2), V3 = c(1, 2, 3, NA)))
test_data11[, ]

get_negative_values <- function(df) {
  ans <- apply(df, 2, function(col) col[col < 0 & !is.na(col)])
  sapply(ans[sapply(ans, function(x) length(x) > 0)], function(x) x)
}

task11 <- get_negative_values(test_data11)
task12 <- get_negative_values(test_data12)
task13 <- get_negative_values(test_data13)
?sapply

#    

head(iris)
str(iris)

#ищем АНОВА - делаем статистический тест для многих кол. переменных по факторной
aov_iris <- apply(iris[, 1:4], 2, function(x) aov(x ~ iris$Species))
norm_test <- apply(iris[, 1:4], 2, function(x) shapiro.test(x)$p.value)


#    задача 2 - заменить все пропущенные значения на средние по столбцу в группе

test_data21 <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))

na_rm <- function(df) {
  as.data.frame(apply(df, 2, function(x) {
    x[which(is.na(x))] <- mean(x, na.rm = T)
    return(c(x))
    }))
}

task21 <- na_rm(test_data21)
str(task21)
