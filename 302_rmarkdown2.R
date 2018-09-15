#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(tidyr)
library(dplyr)
library(rmarkdown)
library(data.table)

#Полный список функций в RMarkdown
#http://www.rstudio.com/wp-content/uploads/2016/03/rmarkdown-cheatsheet-2.0.pdf

#Все возможные опции чанков в knitr
#http://yihui.name/knitr/options

# Зачем нужен Markdown
# html - набор инструкций для браузера
# написание в ручном режиме очень неудобно
# markdown - текстовый формат, удобный для написания и чтения
# markdown - инструмент для конвертирования такого текста в html

#  Основные элементы маркдаун

#Символы
#**Жирный** __шрифт__ -> жирный шрифт
#*Курсивный* _шрифт_ -> курсивный шрифт
#Моноширинный шрифт `sum(x)` -> sum(x)
#Верхние и нижние индексы A^2^~i~ - ^ - верхний 
#~~зачеркнутый текст~~
#Экранирование символов (escape): \*\_\\ \$ -> *_\$
#Автозамена тире и длинного тире: -- --- -> тире длинное_тире

#Заголовки: # Header 1 - ###### Header 6
#Гиперссылки <>
#Гиперссылки в тексте: [Мой гитхаб](адрес_ссылки)
#http://www.rstudio.com/wp-content/uploads/2016/03/rmarkdown-cheatsheet-2.0.pdf

#Поддержка latex
#Для сборки pdf нужна установка TeX (TeX Live)
# $\hat{\beta} = (X^T X)^{-1} X^T y$
#Для оформления в выносном блоке используется обрамление $$
# $$i \hbar \frac{\partial}{\partial t} ... $$
#Полноценные возможности TeX (настройки, пакеты, макросы и тд)
#Формулы корректно отображаются в html благодаря MathJax

#    Задача 1 - сопоставляем rich text с тем, что он отображает

# Опции чанков
# knitr
#обрабатывает код на R
#изначальный фокус knitr - компиляция pdf на основе кода и latex

#существуют глобальные настройки knitr
#каждый чанк может иметь локальные настройки, переопределяющие глобальные
#локальные настройки указываются через запятую
#все возможные опции приведены на http://yihui.name/knitr/options

# Основные опции
# echo (default: TRUE) - отображать ли исходный код чанка?
# eval (default: TRUE) - исполнять ли код чанка?
# include (default: TRUE) - отобрать ли результат исполнения чанка?
# error (default: FALSE) - добавлять ли в документ текст ошибок?
# message (default: TRUE) - добавлять ли в документ текст сообщений?
# warning (default: TRUE) - добавлять ли в документ текст предупреждений?
# comment (default: "##") - какой префикс добавлять перед каждой строкой вывода результатов?
# results (default: 'markup') - каким образом выводить результаты исполнения? (см справку)
# highlight (default: TRUE) - подсвечивать ли синтаксис кода?
# tidy (default: FALSE) - отформатировать ли код (отступы, пробелы)? 

# Графики
# fig.heigth, fig.width - размеры изображения (в дюймах)
# fig.align (default: 'default') - расположение графика на странице
# fig.cap (default: NULL) - строка с подписью к графику

# Кеширование чанков
#Некоторые чанки могут содержать длинные вычисления
#Для таких чанков нужно выставить cache=TRUE
#Кеш срабатывает в том случае, если содержимое кода внутри чанка не изменилось
#Важно: если чанки взаимозависимы, нужно помнить о кеше! Например, предобработка и статистика.
#В этих случаях поможет опция dependson
#Важно: некоторые алгоритмы используют генераторы случайных чисел! Если кеш включен
#код запустится один раз.

# Метки чанков
#В фигурных скобках между "r" и опциями чанка может находиться метка чанка
#Чанки могут обращаться друг к другу по меткам

## Example of chunk labeling
```{r chunk1, echo=TRUE, results='hide'}
cos(2*pi)
```

```{r chunk2, echo=FALSE, ref.label='chunk1'}
```
# Глобальные опции
#Задаются, как правило, в начале документа
#knitr::opts_chunk$set(echo=FALSE, fig.width=5, fig.height=5)
#Этот вызов удобно поместить в отдельный чанк с опцией include=FALSE

#    Решаем задачу с ледниками дальше
glacier1 <- glacier %>% 
  select(Ref_Date, GEO, MEASURE, Value) %>% 
  filter(MEASURE == "Annual mass balance") %>% 
  separate(GEO, c("Name", "Location"), sep = " - ")

# descriptive analysis
g1 <- glacier1 %>% 
  group_by(Name) %>% 
  summarise(YearsObserved = n(),
            MeanChange = mean(Value, na.rm = T),
            WorstChange = min(Value, na.rm = T),
            WorstYear = Ref_Date[which.min(Value)])

# t-test
g2 <- glacier1 %>% 
  group_by(Name) %>% 
  do({
    tt <- t.test(.$Value, alternative = "less", mu = 0, conf.level = 0.99)
    data.frame(PValue = tt$p.value,
               ConfidenceLimit = tt$conf.int[2])
  })

# Сливаем две таблицы в одну
left_join(g1, g2, by = "Name") %>% 
  knitr::kable(caption = "Descriptive statistics and confidence intervals",
               digits = c(0, 0, 2, 0, 0, 10, 2))

# ggplot
ggplot(glacier1, aes(Ref_Date, Value))+
  geom_line()+
  geom_hline(data = g1, aes(yintercept = MeanChange),
             color = "red", linetype = "dashed", alpha = 0.8)+
  facet_wrap(~Name, nrow = 2)
