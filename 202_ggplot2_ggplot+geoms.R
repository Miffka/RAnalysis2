#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(dplyr)

# Функция ggplot + geoms + stas_summary
#geom_point - точки
#geom_smooth - линия сглаживания, берет х и у из aes
ggplot(diamonds)
#функция без аргументов создает "холст" для графика
ggplot(diamonds, aes(x = price))
#первый этап - возвращает заготовку со значениями price на оси x
ggplot(diamonds, aes(x = price))+
  geom_histogram()
#добавляем тип графика - и получаем гистограмму!

# график с несколькими переменными
ggplot(diamonds, aes(price, carat))+
  geom_point()
#самая полезная особенность - можем не ограничивать себя в количестве geom'ов
ggplot(diamonds, aes(price, carat))+
  geom_point()+
  geom_smooth()
#указываем aes отдельно)
ggplot(diamonds)+
  geom_point(aes(price, carat))+
  geom_smooth(aes(price, carat))

# добавляем на отдельный график цвет точек
ggplot(diamonds, aes(x = price,
                     y = carat, 
                     color = cut))+
  geom_point()+
  geom_smooth()
#внутри каждого geom aes берется из головной функции
#но если пропишем aes для отдельного geom, он будет применяться только к нему
ggplot(diamonds, aes(x = price,
                     y = carat))+
  geom_point(aes(color = cut))+
  geom_smooth()

#внутри ggplot всегда даем два основных момента - data и aes(x, y)
#настройки из одного geom не переходят в другой geom
ggplot(diamonds, aes(x = price,
                     y = carat))+
  geom_point(size = 0.5, alpha = 0.2)+
  geom_smooth(size = 2, color = "red")

# Закрепляем материал
str(airquality)
#взаимосвязь месяца и температуры
glimpse(airquality) #то же самое, что str
gr_airquality <- group_by(airquality, Month)
t <- summarise(gr_airquality, 
               mean_temp = mean(Temp),
               mean_wind = mean(Wind))
#не забываем о то, чтобы применять аргументы для тех geom, для которых хотим)
ggplot(t, aes(Month, mean_temp, size = mean_wind))+
  geom_point()+
  geom_line()
#каждый новый слой отображается поверх предыдущего
ggplot(t, aes(Month, mean_temp))+
  geom_line()+
  geom_point(aes(size = mean_wind), color = "red")
#можно добавлять geom'ы, не связанные с данными
ggplot(t, aes(Month, mean_temp))+
  geom_line()+
  geom_point(aes(size = mean_wind), color = "red", shape = 15)+
  geom_hline(yintercept = 75, linetype = "dotted", size = 2, color = "blue")


#    EROORBAR
#errorbar - geom для доверительного интервала, среднего +- sd и прочее
#функции требуют предобработки данных - создаем датафрейм, делаем новый вектор и пр.
?geom_errorbar()
?geom_pointrange() #добавляет среднее значение точкой

gr_mtcars <- group_by(mtcars, am)
se_data <- summarise(gr_mtcars,
                     mean_mpg = mean(mpg),
                     y_max = mean(mpg) + 1.96*sd(mpg)/sqrt(length(mpg)),
                     y_min = mean(mpg) - 1.96*sd(mpg)/sqrt(length(mpg)))
#при помощи функции aes в geom_errorbar отсылаем его в заданные ранее данные
ggplot(se_data, aes(x = factor(am),
                    y = mean_mpg))+
  geom_errorbar(aes(ymin = y_min, ymax = y_max, width = 0.2))+
  geom_point(size = 10, shape = 21, fill = "white")
#делаем все то же самое при помощи geom_pointrange
ggplot(se_data, aes(x = factor(am),
                    y = mean_mpg))+
  geom_pointrange(aes(ymin = y_min, ymax = y_max), size = 1)

# делим начальный набор данных на 6 групп
gr_mtcars <- group_by(mtcars, am, cyl)
se_data <- summarise(gr_mtcars,
                     mean_mpg = mean(mpg),
                     y_max = mean(mpg) + 1.96*sd(mpg)/sqrt(length(mpg)),
                     y_min = mean(mpg) - 1.96*sd(mpg)/sqrt(length(mpg)))
#для соединения линий не забываем добавить в нужный geom или в головную функцию
#параметр group и соответствующую переменную
ggplot(se_data, aes(x = factor(am),
                    y = mean_mpg,
                    col = factor(cyl)))+
  geom_errorbar(aes(ymin = y_min, ymax = y_max, width = 0.2))+
  geom_line(aes(group = factor(cyl)))+
  geom_point(size = 5, shape = 21, fill = "white")
#то же самое в pointrange
ggplot(se_data, aes(x = factor(am),
                    y = mean_mpg,
                    col = factor(cyl)))+
  geom_pointrange(aes(ymin = y_min, ymax = y_max), size = 0.5)+
  geom_line(aes(group = factor(cyl)))


#    Предобработка данных внутри ggplot
?stat_summary
?mean_cl_boot
mean_cl_boot(mtcars$mpg)
mean_cl_normal(mtcars$mpg)
#в функцию stat_summary можно писать функцию, которая дает все нужные данные
ggplot(mtcars, aes(factor(am), mpg, col = factor(cyl), group = factor(cyl)))+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", 
               width = 0.1)+ #по умолчанию работает с y
  stat_summary(fun.data = mean_cl_boot, geom = "point")+ #добавляем точку в середине
  stat_summary(fun.y = mean, geom = "line") #соединяем первые наблюдения
#из результатов функции и соединяем их линией

#пишем функцию для получения среднего и плюс-минус интервалов
sd_error <- function(x) {
  c(y = mean(x), ymin = mean(x) - sd(x), ymax = mean(x) + sd(x))
}

#подаем полученную функцию в stat_summary
#нужный аргумент - position = position_dodge(значение сдвига)
ggplot(mtcars, aes(factor(am), mpg, col = factor(cyl), group = factor(cyl)))+
  stat_summary(fun.data = sd_error, geom = "errorbar", 
               width = 0.1,
               position = position_dodge(0.2))+ #по умолчанию работает с y
  stat_summary(fun.data = sd_error, geom = "point",
               position = position_dodge(0.2))+ #добавляем точку в середине
  stat_summary(fun.y = mean, geom = "line",
               position = position_dodge(0.2)) #соединяем первые наблюдения


#    Задача 1 - скомбинировать violin и boxplot
my_plot <- ggplot(mtcars, aes(x = factor(am), y = mpg))+
  geom_violin()+
  geom_boxplot(width = 0.2)


#    Скачиваем набор данных для следующих задач
sales <-  read.csv("https://stepic.org/media/attachments/course/724/sales.csv")
glimpse(sales)


#    Задача 2 - взаимосвязь между income и sale, цвет - номер магазина
my_plot <- ggplot(sales, aes(x = income, y = sale))+
  geom_point(aes(color = shop))+
  geom_smooth()


#    Задача 3 - различия в доходах двух магазинов с учетом времени года
my_plot <- ggplot(sales, aes(x = shop, y = income, color = season))+
  stat_summary(fun.data = mean_cl_boot, position = position_dodge(0.2))


#    Задача 4 - отображаем различия в продажах в зависимости от года и номера магазина
ggplot(sales, aes(x = date, y = sale, color = shop))+
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", 
               width = 0.1,
               position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, 
               geom = "point",
               position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, 
               geom = "line",
               position = position_dodge(0.2))
