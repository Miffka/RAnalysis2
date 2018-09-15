#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(dplyr)
library(rmarkdown)
library(data.table)

#    R Markdown
# средство для создания динамических документов, отчетов и презентаций
# инструмент для встраивания результатов исполнения кода на R в текст документов

# Зачем это нужно
# для полной воспроизводимости результатов
# для конвертирования исходного .rmd файла в разные форматы
#html, pdf, ms word, markdown, презентации, шаблоны для веб-сайтов
# для удобства написания, разметки и форматирования документа
# для распространения открытого кода, идей, алгоритмов и результатов исследований

# О reproducible research
# более 70% исследователей не могут воспроизвести результаты, полученные коллегами
# в последнее время ситуация ухудшается
#растут объемы данных
#используются сложные схемы обработки данных
#применяемые модели и алгоритмы требуют тонкой настройки
#формат традиционной журнальной статьи не отвечает современным требованиям
# ropenci reproducible guide

# Кусочки кода - это чанки, их можно использовать отдельно

# Inline code
# Простейший случай - вставка результата в виде текста прямо в строку
# Синтаксис: "`" - "r" - пробел - код на R - "`"
# Пример "Теорема доказана в предположении, что число пи равно `r ceiling(pi)`
# Результат: "Теорема доказана в предположении, что число пи равно 4"

#Code chunks
# Основной способ вставки кода
# Каждый кусок настраивается с помощью опций
# Чанк может печатать, рисовать или вообще не иметь вывода

#Синтаксис чанков 1
#```{r}
#head(iris)
#```
# выводится и код, и результат исполнения кода

#Опция echo=FALSE
# В фигурных скобках можно указывать через запятую опции чанка
# По умолчанию печатается и код, и его результат
# Опция echo=FALSE подавляет вывод кода


#    Анализ ледников в канаде
glacier <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20markdown/demos/glacier.csv",
                    na.strings = "..", comment.char = "#")
?read.csv
??fread


#    Задача 2 - подставить нужные значения в текст
ans21 <- glacier %>% 
  group_by(GEO) %>% 
  summarise(max(Ref_Date) - min(Ref_Date))
ans21[which.min(ans21$`max(Ref_Date) - min(Ref_Date)`), ][[1]]

ans22 <- glacier %>% 
  filter(MEASURE == "Annual mass balance") %>% 
  select(GEO, Value) %>% 
  group_by(GEO) %>% 
  summarise_all(median, na.rm = TRUE)
?median
?summarise
ans22[which.min(abs(ans22$Value)), ][[1]]

