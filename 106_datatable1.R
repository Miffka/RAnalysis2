#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(tidyr)
library(dplyr)
library(lazyeval)
library(microbenchmark)
library(scales)
library(data.table)

# Общие сведения

# Объект data.table
# как data.frame - у каждого столбца свой тип, все ячейки в столбце этого типа
# все столбцы имеют одинаковое количество ячеек, все строки имеют одинаковое кол-во ячеек

# Создание data.table
as.data.table(iris)
data.table(col1 = c(1:3), col2 = letters[1:3])
# функция fread("path.csv") - загружает csv-файл очень быстро!
products <- fread("./stepik/products.csv")
?microbenchmark
microbenchmark(fread("./stepik/products.csv"), read.csv("./stepik/products.csv"), times = 10)
#разница в 10 раз!!!! читаем файлы только им!!!

# Делаем выборки из этих таблиц
# выборки как в датафрейме - строки
products[1:10,]
products[products$price > 10000, ] #как в датафрейме
# with
with(iris, iris[Species == "virginica", ])
# обойдемся вообще без повтора имени дататэйбла
products[price > 10000]
# множественные условия
products[(price > 10000) & (brand %in% c("Epson", "Apple"))]

#  Особенности фильтрации
# если есть колонка с логическим типом
products[available, ] #дает ошибку
products[available == T, ] #работает так
# Запятая
products[3, ] #эта запись универсальна и для дататэйбл, и для датафрейм
iris[3, ]
products[3] #без запятой выдаст третью строку
iris[3] #датафрейм без запятой дает третий столбец
# Инвертируем условие фильтрации!!!!
products[!(brand %in% c("Apple", "Epson"))] #просто добавляем в условие восклицательный знак
products[1:10]
products[!(1:10)] #выдает дататэйбл без первых 10 строк

#выборки по столбцам
#x[i, j, by, with = TRUE, ...]
#j = list(name1 = {expr1}, name2 = {expr2}, ...) #где имена - это названия переменных
products[, list(name, price.1k = price/1000)] #expr можно опускать

#совмещаем выборки по столбцам с выборками по строкам
order(products$price, decreasing = T)
products[order(price, decreasing = T)]
products[order(price, decreasing = T), list(name, price.1k = price / 1000)]
products[order(price, decreasing = T),
         list(name, price.1k = paste0(price / 1000, " тыс. руб"))]
head(products[order(price, decreasing = T),
              list(name, price.1k = paste0(price / 1000, " тыс. руб"))], 5)

# Особенности фильтрации - часть 2
#взять отдельную переменную можно при помощи знака $
products[order(price, decreasing = T),
         list(name, price.1k = paste0(price / 1000, " тыс. руб"))]$price.1k
#функцию list можно заменить на .
products[order(price, decreasing = T),
         .(name, price.1k = paste0(price / 1000, " тыс. руб"))]
#можно фильтровать дататэйбл как датафрейм, указав with = F
#отличие от функции transform ->
products[order(-price), .(name = head(name), price = head(price))]
#-> количество элементов в векторе после трансформации не обязательно должно совпадать с
#количеством значений в столбце

#можно возвращать и вовсе один элемент!
products[, .(price = sum(price))]
#просто нихрена себе!!!!
#применяем квадратные скобочки два раза!
products[, list(name.with.brand = paste0(brand, " - ", name))][order(name.with.brand)]
#пишем сложное выражение при трансформации, заключая его в {}
products[, .(price = {
  a <- mean(price)
  b <- median(price)
  c(min(price), max(price), a/b)
})]
#фактически, это мы вызвали анонимную функцию!

# аггрегация

#x[i, j, by, with = TRUE, ...]
#SELECT j FROM ...
#WHERE i
#GROUP BY by
# передаем в by столбец, по которому будем аггрегировать
# by принимает и list(column1, column2, ...)
products[, .(mean.price = mean(price)), by = brand]
#при взятии аггрегата порядок строк сохраняется
#выбираем из каждого бренда три самых дорогих товара
products[order(-price), .(name = head(name, 3),
                          price = head(price, 3)), by = brand]
#когда берем тройку по head, не возникает вставки NA, если у бренда нет трех товаров
products[order(-price), .(name = name[1:3],
                          price = price[1:3]), by = brand]


#    Задача 1 - вернуть строки с ценой больше 500000, доступные на складе и принадлежат brands

max(products$price)
products[price > 50000][available == T][brand %in% sample(products$brand, 5)]

filter.expensive.available <- function(dt, brands) {
  dt[price >= 500000][available == T][brand %in% brands]
}

sample.products <- data.table(price = c(10000, 600000, 700000, 1000000),
                              brand = c("a", "b", "c", "d"),
                              available = c(T, T, F, T))
filter.expensive.available(sample.products, c("a", "c", "d"))


#    Задача 2 - вернуть столбцы только с номером заказа и ID продукта
# + упорядочить по убыванию стоимости
# + удалить возвраты - записи с отрицательным количеством предметов в позиции

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

ordered.short.purchase.data <- function(dt) {
  dt[order(-price)][quantity >= 0][, .(ordernumber, product_id)]
}

ordered.short.purchase.data(sample.purchases)
?order


#    Задача 3 - вернуть медианную стоимость заказа (число).
# записи с неположительным количеством купленных товаров игнорировать
# один заказ может содержать несколько позиций с одинаковым ordernumber, их нужно учитывать

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
#а вот так получится неверно! потому что будет применяться группировка
sample.purchases[quantity > 0][, .(median = {
  cumpr <- sum(price*quantity)
  median(cumpr)
  }), by = ordernumber]

purchases.median.order.price <- function(dt) {
  dt[quantity > 0][, .(cumprice = sum(price*quantity)), by = ordernumber][, median(cumprice)]
}

purchases.median.order.price(sample.purchases)
