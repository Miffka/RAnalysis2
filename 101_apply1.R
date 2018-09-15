#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
data(diamonds)
str(diamonds)


#     ищем минимум для x, y и z

# совсем неэффективный код
min_size <- c() #ошибка номер 1 - 
for (i in 1:nrow(diamonds)) {
  min_size <- c(min_size, min(diamonds[i, 8:10])) #все очень плохо здесь 
}

# более эффективный код
min_size <- numeric(nrow(diamonds))
for (i in 1:nrow(diamonds)) {
  min_size[i] <- min(diamonds[i, 8:10])
}

# apply(array, margin, ...)
min_size <- apply(diamonds[, 8:10], 1, min)
#   если Вы используете в R цикл - скорее всего, Вы что-то делаете не так


#    все о функции apply
?apply

d <- matrix(rnorm(30), nrow = 5)
apply(d, 1, sd) #применяем функцию sd к строкам
apply(d, 2, sd) #применяем функцию sd к столбцам
apply(mtcars, 2, sd) #находим sd каждой переменной
apply(mtcars, 1, sd) #бессмысленное выражение - стандартное отклонение строки из всех переменных


# задача 1 - найти максимальное значение в каждой строке


row_max <- apply(my_df, 1, max)
row_max <- apply(d, 1, max)


# задача 2 - расчитать медиану для всех столбцов

col_median <- apply(my_df, 2, median)

apply(d, 2, median)


#    мы подаем функции данные, что функция возвращает?
# матрица - если функция возвращает несколько значений, количество значений одно и то же

my_range <- apply(d, 2, range)

# список - если функция возвращает разные значения в зависимости от данных

outliers_count <- function(x) {
  outliers <- x[abs(x - mean(x)) > 2 * sd(x)]
  if (length(outliers) > 0) {
    return(outliers)
  } else {
    return("There are no outliers")
  }
}

iris_num <- iris[,1:4]
out_iris <- apply(iris_num, 2, outliers_count)
str(out_iris)