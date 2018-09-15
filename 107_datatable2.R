#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(tidyr)
library(dplyr)
library(lazyeval)
library(microbenchmark)
library(scales)
library(data.table)

# Вычисление выражений в квадратных скобочках - специальные переменные
# .SD - содержит дататэйбл, сгруппированный по переменным
products <- fread("./stepik/products.csv")
products[order(-price), 
         .(
           name = head(name, 3),
           price = head(price, 3)
         ), by = brand]
products[order(-price), head(.SD, 3), by = brand]

# .N - количество элементов в группе
products[price > 1000, .(expensive.items = .N), by = brand]

# Все операции создают копию дататэйбла - это загрязняет память
# оператор := создает/изменяет столбец без создания копии объекта, но нельзя изменить тип столбца
x[, new.column := expr]
x[, c("col1", "col2") := list(expr1, expr2)]
#x[, `:=` list(col1 = expr1, col2 = expr2)]
# можно применять совместно с фильтрацией и агрегацией
x[i, new.column := expr, by]
# то же самое, что оператор set
products[price < 1000, 
         name.with.price := paste0(name, " ( ", price, " руб.")]
products[order(-price)]
# строки, не подошедшие по условию, будут заменены на NA
products[, .(price = price / max(price)), by = brand]

# Можно создавать ключи 
#создаем ключи
purchases <- fread("./stepik/purchases.csv")
setkey(purchases, product_id, externalsessionid)
setkey(products, product_id, brand)
#проверяем, какие ключи создались
key(purchases)
key(products)
#соединяем таблицы по ключам
merge(purchases, products, by = "product_id")
# merge(purchases, products, by = c("col1", "col2"))
#если аргумент, по которому нужно объединять таблицы, разный, его указывают как 
# by.x и by.y для первой и второй таблицы, соответственно
merge(purchases, products, by.x = "product_id", by.y = "product_id")
# Левое и правое соединения - в результирующей записи гарантируется наличие переменных
#из x и y при помощи флажков all.x и all.y
#Если для каких-то записей не находится пары, колонки добиваются NA
merge(purchases, products, all.x = T, all.y = F)
merge(purchases, products)
#Можно запускать merge и при помощи квадратных скобочек!
purchases[products, on = "product_id"]
#Попытка смёрджить таблицы по всем ключам - если не указываем конкретный ключ
purchases[products]
#дает ошибку
setkey(products, product_id, price)
setkey(purchases, product_id, ordernumber)
purchases[products]
#ошибки нет, поскольку проведен merge по переменным с одинаковым типом
#столбцы, тем не менее, имели разные значения
#merge всегда выполняет innerjoin

# Вспомогательные функции для поиска - J, SJ, CJ
products[J(c(158, 208, 10001, 826355, 958238))]
#возвращает таблицу, которая соединяется с первым ключом для products
products[data.table(c(158, 208, 10001, 826355, 958238))]
products[list(c(158, 208, 10001, 826355, 958238))]
products[.(c(158, 208, 10001, 826355, 958238))]
#функции выше дают то же самое
# SJ приводит к созданию ключей - штаааааа?
print(SJ(c(158, 208, 10001, 826355, 958238)))
key(SJ(c(158, 208, 10001, 826355, 958238)))
# CJ выполняет декартово произведение аргументов, создает таблицу и ключи
print(CJ(c(158, 826355, 958238), c("Supra", "Func")))
key(CJ(c(158, 826355, 958238), c("Supra", "Func")))

# Разбираем пример - найдем брэнды с высокой лояльностью
#если пользователь лоялен брэнду, он всегда покупает этот брэнд
purchases <- fread("./stepik/purchases.csv")
products <- fread("./stepik/products.csv")
#Получаем таблицу, в которой есть и данные о покупке, и брэнд
purchases.with.brands <- merge(
  purchases,
  products[, list(product_id, brand)],
  by = "product_id"
)

pop.20.brands <- head(
  purchases.with.brands[, 
                        .(
                          total.brand.users = length(unique(externalsessionid))
                        ),
                        by = brand][order(-total.brand.users)], 20)

users <- purchases.with.brands[, list(unique.brands = length(unique(brand)),
                                      items = .N,
                                      brand = brand[1]),
                               by = externalsessionid]

brand.loyal.users <- users[items > 1][unique.brands == 1][, list(total.loyal.orders = .N), by = brand]

brand.stats <- merge(
  pop.20.brands,
  brand.loyal.users,
  by = "brand"
)

brand.stats[, loyal := total.loyal.orders/ total.brand.users]
brand.stats[order(-loyal)]

# Порог вхождения по обороту - не самая лучшая идея, Apple-то в списке лояльностей нет!
#Можно разбить отчет на категории - бытовая техника и гаджеты? Или на уровень ниже?
#Apple, например, не выпускает холодильники)
#Постановка задачи крайне важна
#Самые главные вопросы - "зачем это нужно? как это будет использоваться?"
#Варианты уточнений
# Таблица нужна для почтовой рассылки - можно много брэндов
# Лояльные бренды нужны для проверки бизнес-гипотез - делаем срез по категориям
#и брендам с наибольшим оборотом
# Нужна инфографика - по 3-4 бренда с разных категорий
#Обращать внимание
# Легкость и простота интерпретации результата
# Четкость постановки задачи; решает ли результат задачу полностью
# Вводимые дополнительные предположения и определения
# ... в том числе возникающие при решении метрик!
lm(y ~ ., data)
#у - линейная комбинация независимых переменных
#измерения у содержат погрешность
#ошибка у распределена нормально
#эта ошибка аддитивна, а не мультипликативна
#дисперсия погрешности постоянно во времени и не зависит от значений независимых переменных


#    Задача 1 - возвращать суммарный оборот с учетом скидок каждой категории и количество
#купленных предметов по таблице покупок и таблице принадлежности товара к категории.
#Если товар принадлежит нескольким категориям, учитывать его во всех.

product.category <- data.table(product_id = c(1,1,2,2,3),
                               category_id = c(1,2,1,3,3))
purchases <- data.table(product_id = c(1, 2, 3),
                        totalcents = c(100, 200, 300),
                        quantity = c(1, 1, 3))
setkey(product.category, product_id)
setkey(purchases, product_id)
task1 <- merge(product.category, purchases)
task1[, `:=`(
  totalcents = sum(totalcents),
  quantity = sum(quantity)),
  by = "category_id"]
task11 <- task1[, .(totalcents = sum(totalcents), quantity = sum(quantity)), by = "category_id"]


get.category.ratings <- function(purchases, product.category) {
  setkey(product.category, product_id)
  setkey(purchases, product_id)
  task1 <- merge(product.category, purchases)
  ans <- task1[, .(
    totalcents = sum(totalcents), 
    quantity = sum(quantity)),
    by = "category_id"]
  return(ans)
}

get.category.ratings(purchases = purchases, product.category = product.category)


#    Задача 2 - функция добавляет с помощью := столбец price.portion
# этот столбец содержит процент стоимости товара в заказе с двумя знаками после запятой
# Тип нового столбца - character. Записи с неположительным количеством товаров убрать
# перед расчетом.


sample.purchases <- data.table(price = c(100, 300, 50, 700, 30),
                               ordernumber = c(1,1,1,2,3),
                               quantity = c(1,1,2,1,-1),
                               product_id = 1:5)
?":="
?as.double
?sprintf
task2 <- sample.purchases[quantity > 0][, 
    price.portion := as.character(sprintf("%.2f", price*quantity/sum(price*quantity)*100)), 
    by = "ordernumber"]
print(task2)

mark.position.portion <- function(purchases) {
  purchases <- purchases[quantity > 0][, 
                          price.portion := as.character(sprintf("%.2f", price*quantity/sum(price*quantity)*100)), 
                          by = "ordernumber"]
  return(purchases)
}

mark.position.portion(sample.purchases)
