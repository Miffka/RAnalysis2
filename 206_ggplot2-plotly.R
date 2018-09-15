#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(dplyr)
library(ggthemes)
library(grid)
library(plotly)
library(data.table)

#    Пакет plotly

products <- fread("./stepik/products.csv")
purchases <- fread("./stepik/purchases.csv")

#Функция ggplotly - создание интерактивных графиков
price.hist <- ggplot(purchases) +
  geom_histogram(aes(totalcents), fill="white", color = "black")+
  scale_x_log10("Item price, RUB", labels = function(x) {
    format(x/100, scientific = F, big.mark = " ")
  }) +
  ylab("Times purchases")

price.hist

interactive.price.hist <- ggplotly(price.hist)
interactive.price.hist

#Функция plot_ly - создание интерактивных графиков "с нуля"
?plot_ly
#plot_ly(data = data.frame(), ..., type = "scatter", group, color, colors,
#        symbol, symbols, size, width = NULL, height = NULL, inherit = T,
#        evaluate = F)
#data - dataframe, источник данных
#group - вектор/столбец для группировки данных
#color - вектор, столбец с цветом
#symbol - вектор/столбец, отвечающий за форму точки
#size - вектор/столбец, отвечающий за размер точек
#type - тип графика
#... - параметры, релевантные для данного type(x, y, z, ...)

mt <- mtcars
help('plotly_data')
plot_ly(mt, x = mpg, y = ~disp, text = rownames(mtcars), group = ~as.factor(cyl),
        mode = "markers", type = "scatter")
#параметр text - текст, появляющийся при наведении на точку
#параметр mode - задает то, что отображается на графике
# добавление add_trace и layout
smoothed.vector <- fitted(loess(uempmed ~ as.numeric(date), economics, span = 0.2))

#нихуя не работающая функция добавления сглаженной кривой
plot_ly(mt, x = mpg, y = ~disp, text = rownames(mtcars), group = ~as.factor(cyl),
        mode = "markers", type = "scatter") %>% add_trace(x = date, y = smoothed.vector)
?add_trace
str(economics)

my.plot <- ggplot(economics)+
  geom_line(aes(x = date, y = uempmed, color = "red"))+
  geom_line(aes(x = date, y = smoothed.vector), color = "blue")
ggplotly(my.plot)

#график нормализованного распределения какой-то херни
purchases[, log.tc := log(purchases$totalcents)]
purchases[, normal.approx.prob := dnorm(log.tc, mean(log.tc), sd(log.tc))]
ggplotly(ggplot(purchases, aes(log.tc))+
           geom_histogram(aes(y = ..count.. / max(..count..)), fill = "white", 
                          color = "blue", binwidth = 0.2)+
           geom_line(aes(log.tc, normal.approx.prob / max(normal.approx.prob)), 
                     color = "red"))

#тип surface
plot_ly(z = volcano, type = "surface")

#тип mesh3d - cоздает полигоны по алгоритмам триангуляции
#alphahull задает алгоритмы: -1 - Делоне, 0 - выпуклая, 1 - альфа-норма
mesh <- data.table(x = rnorm(40), y = rnorm(40), z = rnorm(40))
plot_ly(mesh, type = "mesh3d", x = ~x, y = ~y, z = ~z, alphahull = 0)
#вместо алгоритма можно задать индекты точек в исходном массиве, которые станут вершинами
po <- data.table(x = c(0.2, 0.8, 0, 1), y = c(0, 0, 1, 1), z = c(0, 0, 0, 0))
i.s <- c(0, 2)
j.s <- c(1, 1)
k.s <- c(2, 3)
plot_ly(po, x = ~x, y = ~y, z = ~z, i = i.s, j = j.s, k = k.s, type = "mesh3d")


# Работа с сайтом plot.ly
#signup("Glycosylase", "miffka1992@inbox.ru") #автоматическая генерация
Sys.setenv("plotly_username" = "Glycosylase")
Sys.setenv("plotly_api_key" = "czV74EhlAZq1vfIEXRil")
plotly_POST(interactive.price.hist, "price_distribution")


#    Задача 2 - рисуем трехмерный чайник
task2 <- fread("https://stepic.org/media/attachments/course/724/teapot.csv")
i.s <- seq(0, nrow(task2) - 1, 3)
j.s <- seq(1, nrow(task2), 3)
k.s <- seq(2, nrow(task2), 3)
plot_ly(task2, type = "mesh3d", x = ~x, y = ~y, z = ~z, i = i.s, j = j.s, k = k.s)

make.fancy.teapot <- function(teapot.coords) {
  iind <- seq(0, nrow(teapot.coords) - 1, 3)
  jind <- seq(1, nrow(teapot.coords), 3)
  kind <- seq(2, nrow(teapot.coords), 3)
  plot_ly(teapot.coords, type = "mesh3d", x = ~x, y = ~y, z = ~z, i = iind, j = jind, k = kind)
}
make.fancy.teapot(task2)
