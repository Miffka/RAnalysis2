#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(dplyr)

#    Принципы:
# Aesthetic attributes - определяют, какие данные будут на графике и где.
# Geometric objects - определяют, как именно будут отображены данные (линии, точки, столбики)
# Statistical transformations - определяют, какие трансформации с данными будут отображены на графике
#регрессионная прямая или сглаживание
# Scales - какие именно значения в данных будут отображены на графике
# Coordinates - система координат графика
# Faceting - группировка данных

my_mtcars <- mtcars %>% 
  mutate(am = factor(am))

ggplot(data = my_mtcars, aes(x = hp, y = mpg, col = am))+
  geom_point()

# Функция qplot - простой подход к графикам
# Основные параметры - x, y, data, geom, color, size, fill, alpha, shape. Функция I()

?qplot
?data
data("diamonds")
# 1 - самый простой график
qplot(x = price, data = diamonds)
#по умолчанию строит гистограмму
# 2 - еще один простой график, строит дотплот
qplot(x = price, y = carat, data = diamonds)
# 3 - даем фактор, строит все, как он хочет
qplot(x = cut, y = carat, data = diamonds)

# 4 - принимает и вектор на вход!
v = diamonds$carat
qplot(v)
#не обязательно прописывать названия аргументов, очень похожа на функцию hist
#qplot пытается угадать, что рисовать.
#для вектора double - гистограмму, для двух double векторов - точки, для фактора и double
#точки для разных значений фактора


#    Задача 1 - строим гистограмму
depth_hist <- qplot(diamonds$depth)


#идея вставки векторов вместо переменных работает и для двумерного случая
qplot(diamonds$carat, diamonds$price)
#можно сохранять графики в переменные!
str(depth_hist)
#mapping - как расположены переменные

qplot(x = price,
      y = carat,
      color = color,
      shape = cut,
      geom = "point",
      data = diamonds)
#мы можем изменять цвет, форму и прочая и прочая
#основные параметры, которые можем изменять - shape и size

qplot(mpg,
      hp,
      color = factor(am),
      shape = factor(cyl),
      size = I(6),
      data = mtcars)
#поскольку shape бывает ограниченное количество, ggplot ожидает, что за нее будет отвечать фактор
#появилась надпись с точкой - куда ее девать-то? оборачиваем в I()
qplot(mpg,
      hp,
      color = I("blue"),
      shape = factor(cyl),
      size = I(6),
      data = mtcars)


#    Задача 2 - строим scatter plot
price_carat_clarity_points <- qplot(data = diamonds,
                                    x = carat,
                                    y = price,
                                    color = clarity)


# Аргумент прозрачности - alpha
qplot(mpg,
      hp,
      color = factor(am),
      shape = factor(cyl),
      size = I(6),
      alpha = I(0.3),
      data = mtcars)
?I()

# Свойства объекта histogram
qplot(x = price, 
      geom = "histogram",
      fill = I("white"),
      col = I("black"),
      data = diamonds)
# fill - заливка, col - цвет контура
qplot(x = price, 
      geom = "histogram",
      fill = color,
      col = I("black"),
      data = diamonds)
# можно использовать другой способ отображения
qplot(x = price, 
      geom = "density",
      fill = color,
      col = I("black"),
      alpha = I(0.3),
      data = diamonds)


#    Задача 3 - строим график плотности переменной x из diamonds.
x_density <- qplot(x = x,
                   geom = "density",
                   data = diamonds)


#    Задача 4 - строим плотность х для сгруппированных по cut данным
x_cut_density <- qplot(x = x,
                       color = cut,
                       geom = "density",
                       data = diamonds)


#    Задача 5 - строим violin plot для price для групп по color
price_violin <- qplot(x = color,
                      y = price,
                      geom = "violin",
                      data = diamonds)
