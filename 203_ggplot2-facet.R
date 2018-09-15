#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(dplyr)

ggplot(mtcars, aes(hp, mpg, col = factor(am), size = disp, shape = factor(cyl)))+
  geom_point()
#добавление новых переменных в качестве параметров перегружает график и затрудняет восприятие
#поэтому бывает разумно разбить один график на несколько

# facet_grid
#основная идея функции - разбить график по одной или нескольким переменным
#разбиение можно делать на строки и на столбцы
ggplot(diamonds, aes(carat, fill = clarity))+
  geom_density()+
  facet_grid(color ~ cut) #разбивает на подгруппы по двум переменным - color и cut
#первая переменная - в строчках, вторая - в столбцах
#часто достаточно не разбивать на подгруппы по пересечению факторов, а просто
#разбить по одному фактору
ggplot(diamonds, aes(carat, fill = clarity))+
  geom_density(alpha = 0.2)+
  facet_grid(color ~ .) #так разбиваем по строкам
  #facet_grid(. ~ color) #а так - по столбцам
glimpse(diamonds)
?facet_grid

# еще одна особенность facet_grid
mtcars <- mutate(mtcars,
                 am = factor(am, labels = c("A", "M")),
                 vs = factor(vs, labels = c("V", "S")))
ggplot(mtcars, aes(hp))+
  geom_dotplot()+ #гистограмма из точек
  facet_grid(am ~ vs, margins = T)
#аргумент margins делает то же самое, что и в функции для построения таблиц сопряженности
#он добавляет распределения по строкам и столбцам, а также общую
?table
chisq.test(table(mtcars$am, mtcars$vs))

#связь hp и mpg в группах по am ~ vs
#добавляем разбиение точек по цвету в зависимости от количества цилиндров
ggplot(mtcars, aes(hp, mpg))+
  geom_point(aes(col = factor(cyl)))+
  facet_grid(am ~ vs)
#можем сразу сказать, что в определенных группах только определенных типы других факторов
?mtcars
#можем разбить и только по одной переменной, а затем добавить линейную регрессию
ggplot(mtcars, aes(hp, mpg))+
  geom_point(aes(col = factor(cyl)))+
  facet_grid(. ~ am)+
  geom_smooth(method = "lm")

# facet_wrap
#строит не таблицу сопряженности с графиками, а набор совершенно новых графиков
#у каждого графика свое название
ggplot(diamonds, aes(carat, fill = color))+
  geom_density()+
  facet_wrap(~ cut + color, ncol = length(levels(diamonds$color))) #несколько переменных записываем через плюс
#можно указать количество строк или столбцов
#разбиение по одному фактору
ggplot(diamonds, aes(carat, fill = color))+
  geom_density(alpha = .2)+
  facet_wrap(~ cut, ncol = 1)
#можем регулировать график туда-сюда
#отличие от facet_grid
ggplot(diamonds, aes(carat, fill = color))+
  geom_density(alpha = .2)+
  facet_grid(cut ~ .)

#основная прикладная функция фасетов - иллюстрация взаимодействия факторов,
#разнесение информации в разные графики для улучшения читаемости
ggplot(diamonds, aes(carat, price, col = color))+
  geom_smooth()+
  facet_wrap( ~ color)

#основная идея графика - чтобы читатель смог сразу понять все основные идеи этого графика


#    Задача 1 - разбиваем на подгруппы
mpg_facet <- ggplot(mtcars, aes(mpg))+
  geom_dotplot()+
  facet_grid(am ~ vs)


#    Задача 2 - строим график плотности переменной Sepal.Length
#разбиваем по Species при помощи facet_wrap
sl_wrap <- ggplot(iris, aes(Sepal.Length))+
  geom_density()+
  facet_wrap(~ Species)


#    Задача 3 - строим график взаимосвязи Sepal.Length и Sepal.Width внутри каждого вида
my_plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ Species)


#    Задача 4 - анализируем фильмы!\
#строим график, выясняем различия в бюджетах фильмов разного жанра из года в год
myMovieData <- read.csv("https://stepik.org/media/attachments/course/724/myMovieData.csv")
my_plot <- ggplot(myMovieData, aes(Type, Budget))+
  geom_boxplot()+
  facet_grid(. ~ Year)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
