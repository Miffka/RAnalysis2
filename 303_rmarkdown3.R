#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(tidyr)
library(dplyr)
library(rmarkdown)

#Как устроен R Markdown
# Исходный файл: .Rmd
# knitr: .Rmd -> .md
# pandoc: .md -> .html, .docx, .tex
# latex: .tex ->.pdf

#  YAML header
#В заголовке .Rmd-файла можно указывать различные опции
#Эти опции имеют отношение к генерации конечных файлов
#Опции задаются в особо формате в виде списка ключей и значений

#  Конфигурации форматов
#Секция output отвечает за формат конечного документа
#Каждый формат имеет собственные настройки
#output:
#  html-document:
#  toc_float: TRUE #отдельная секция в HTML-файле, где указано оглавление

#  Параметры документа
#Содержимое документа может зависеть от внешних параметров
#Это могут быть различные сценарии, настройки алгоритмов, имена файлов и т.д.
#params:
#  n: 100
#  d: !r Sys.Date()
#Использовать можно через список params:
#`r params$n`
#к этим параметрам можно обращаться через пункт "Knit with parameters" 
#если поменялось что-то, а в каких-то пунктах возникли проблемы в кешем, можно просто удалить кеш

# Дополнительные возможности html
# HTML tags
#При сборке .rmd разметка маркдауна превращается в html-теги
#но теги можно использовать и напрямую
#<u>Подчеркнутый текст</u>

#CSS
#Способ изменять стили отображения элементов, не меняя текст маркдаун-файла
#разметка- отдельно, стили - отдельно
#output:
# html_document:
#   css: styles.css

#  HTML-презентации
#ioslides, slidy, revealjs (+beamer)
#Слайды отделяются друг от друга заголовками первого и второго уровней
#Или принудительно при помощи строки из дефисов
#----
#У каждого формата свои удобства для показа, визуального оформления, конфигурации и т.п.

# ggvis
#HTML может быть интерактивным за счет javascript
#ggvis - "web-friendly" наследник ggplot2
#Синтаксис очень похож:
#   library(ggplot2)
# ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl)))+
# geom_point(size = 5)
#   library(ggvis)
# ggvis(mtcars, x = ~wt, y = ~mpg, fill = ~factor(cyl)) %>% 
# layer_points(size := 100)

# 