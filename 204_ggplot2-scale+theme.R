#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(dplyr)
library(ggthemes)

# Scale
#получаем контроль над осями или легендой
ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am)))+
  geom_point()+
  scale_x_continuous(name = "Miles/(US) gallon",
                     breaks = seq_x) #обязательно выбираем тип переменной 
  #xlab("Miles/(US) gallon") #функция выполняет переименование
?scale_x_continuous
#аргумент breaks ожидает вектор из чисел, отображает деления на оси
seq_x <- round(seq(min(mtcars$mpg), max(mtcars$mpg), length.out = 3))

#
ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am)))+
  geom_point()+
  scale_x_continuous(name = "Miles/(US) gallon",
                     breaks = c(1, seq(10, 35, 5)),
                     limits = c(1, 35),
                     expand = c(1, 1))+
  #scale_x_continuous(expand = c(1, 1))+
  scale_y_continuous(limits = c(50, 400))+
#аргумент limits определяет границы оси
#xlim(c(1, 35)) #делает то же самое, что аргумент limits
#аргумент expand расширяет ось за пределы limits

#scale_color
# можно менять название цветовой легенты, название уровней градации
  #scale_color_discrete(name = "Legend name", labels = c("Auto", "Manual"))+
# в следующей функции можно вручную задать цвета точек, но нужно в ней же задавать
  #легенду и название уровней градации
  scale_color_manual(values = c("Red", "Blue"),
                     name = "Legend name",
                     labels = c("Auto", "Manual"))

ggplot(mtcars, aes(hp, fill = factor(am)))+
  geom_density(alpha = 0.2)+
  #для обращения к настройкам отображения переменной задаем аргументы фvункции
  #scale_<аргумент для переменной из aes>_<тип переменной>
  
  #scale_fill_discrete(name = "Тип коробки передач",labels = c("Авто", "Ручная"))
  #для ручного отображения цветов задаем аргументы функции
  #scale_<аргумент для переменной из aes>_manual
  scale_fill_manual(values = c("Red", "Green"),
                    name = "Тип коробки передач",
                    labels = c("Авто", "Ручная"))

ggplot(mtcars, aes(hp, mpg, size = disp, shape = factor(vs)))+
  geom_point()+
  #задаем градацию размеров через breaks
  scale_size_continuous(name = "Имя легенды",
                        breaks = seq(100, 400, 50))+
  scale_shape_discrete(name = "Имя легенды для формы")

ggplot(mtcars, aes(factor(cyl), hp))+
  geom_boxplot()+
  scale_x_discrete(name = "Цилиндры",
                   labels = c("4 цилиндра", "6 цилиндров", "8 цилиндров"))

ggplot(mtcars, aes(factor(am), hp))+
  geom_boxplot()+
  scale_x_discrete(name = "Тип коробки передач",
                   labels = c("Авто", "Ручной"))


#    Задача 1 - привести визуализацию iris в порядок
iris_plot <- ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous(name = "Длина чашелистика",
                     limits = c(4, 8))+
  scale_y_continuous(name = "Длина лепестка",
                     limits = c(1, 7),
                     breaks = seq(1, 7, 1))+
  scale_color_discrete(name = "Вид цветка",
                       labels = c("Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский"))


# Внешний вид графиков
#theme
#scale_fill_brewer
ggplot(mtcars, aes(factor(am), hp, fill = factor(cyl)))+
  geom_boxplot()+
  scale_fill_brewer(type = "qual", palette = 3)+
  theme_bw()
?scale_fill_brewer
#theme_bw дает черно-белую тему фона и линий

ggplot(mtcars, aes(hp, mpg, color = factor(cyl)))+
  geom_point(size = 5)+
  scale_color_brewer(type = "seq", palette = 15)+
  theme_bw()

?theme
ggplot(mtcars, aes(hp, mpg, color = factor(cyl)))+
  geom_point(size = 5)+
  scale_color_brewer(type = "seq", palette = 15)+
  #theme(text = element_text(size = 14),
   #     axis.line.x = element_line(size = 2),
    #    axis.line.y = element_line(size = 2))
#готовые темы
  #theme_classic()
  #theme_bw()
  theme_dark()

#пакет ggthemes - красивенькие темки
ggplot(mtcars, aes(hp, mpg, col = factor(cyl)))+
  geom_point(size = 2)+
  theme_pander()
