setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

options(scipen = 2, digits = 5)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(lazyeval)
library(vcd)
library(ggmosaic)

#   ЭТА ЧАСТЬ РАБОТЫ ПОЙДЕТ В ЗАДАНИЕ ДЛЯ КУРСА ПО R
# Сперва загружаем датасет в виде датасета
allinfo <- read.delim("https://raw.githubusercontent.com/Miffka/RAnalysis2FinalTask/master/IMG_all-more%2Bpathog.xls", stringsAsFactors = F)

nrow(allinfo)

# Не будем отбирать только записи, содержащие Bioproject.Accession, однако обязательно
#отберем только те записи, в которых есть информация о фенотипе

length(table(allinfo$Phenotype))

# Попробуем отфильтровать по значению фенотипа
#  Сначала отбрасываем записи без указания фенотипа
allinfo01 <- allinfo %>% 
  filter(nchar(Phenotype) > 0)
unique(allinfo01$Phylum)
unique(allinfo01$Class)
unique(allinfo01$Family)
unique(allinfo01$Species)

#  Затем пробуем при помощи регулярных выражений найти СЛОВО "Pathogen"
?mutate_if
?contains
?select
?which
?grep
?
grep("^Pathogen", unique(allinfo01$Phenotype), value = TRUE)
grep("[[:space:]]Pathogen", unique(allinfo01$Phenotype), value = TRUE)
# Вот в этой строке получаем вектор, содержащий названия фенотипов патогенов
#этот вектор и пойдет затем в функцию mutate_
allpathogens <- c(grep("^Pathogen", unique(allinfo01$Phenotype), value = TRUE), 
                  grep("[[:space:]]Pathogen", unique(allinfo01$Phenotype), value = TRUE))
# идея с sapply работает не так, как хочется
#sapply(c("^Pathogen", "[[:space:]]Pathogen"), function(x){
#  grep(x, unique(allinfo01$Phenotype), value = TRUE)
#})


# Теперь заведем новую переменную Phenotype.New, которая будет содержать всего два уровня
# "Патоген" и "Не-патоген"

allinfo02 <- allinfo01 %>% 
  mutate_(Phenotype.New = interp(~ifelse(var %in% allpathogens, "Pathogen", "Non-Pathogen"), 
                                 var = as.name("Phenotype")))
table(allinfo02$Phenotype.New)

# Построим таблицу, чтобы посмотреть, сколько каких наблюдений у нас в ячейках.
(table01 <- table(allinfo02$Phylum, allinfo02$Phenotype.New))

# Сформулируем первое правило отбора: если для отдельного филума общее число наблюдений менее 8,
#исключаем его из всей дальнейшей обработки.
rownames(table01[apply(table01, 1, sum)<8,])
(table02 <- table01[apply(table01, 1, sum) > 8,])

# Сформулируем второе правило отбора - берем только филумы с числом наблюдений более 43
allinfo03 <- allinfo02 %>% 
  filter(Phylum %in% rownames(table01[apply(table01, 1, sum) > 43,]))
rownames(table01[apply(table01, 1, sum)<=43,])
#(table03 <- table01[apply(table01, 1, sum) > 43,])
sum(table03)
?chisq.test
# Добавим сюда немножко визуализации - два pie chart по патогенам и не-патогенам
df05 <- as.data.frame(table05)
# Пробуем одним графиком 
# Распределяем по фенотипу
ggplot(allinfo03, aes(x = "", fill = Phylum))+
  geom_histogram(width = 1, position = "fill", stat = "count")+
  facet_wrap(~ Phenotype.New)+
  coord_polar(theta = "y")+
  scale_x_discrete(name = "", labels = c("", "", "", ""), breaks = NULL)+
  scale_y_continuous(name = "Phylum distribution", breaks = NULL)+
  scale_fill_brewer(type = "qual", palette = 3)+
  theme(strip.background = element_rect(fill = "blue"), 
        panel.background = element_rect(fill = "white"), 
        panel.spacing = unit(x = 0.1, units = "cm"),
        axis.title = element_text(size = 18, vjust = 1),
        legend.title = element_text(size = 16, vjust = 0),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 14, colour = "green", face = "bold"))

# Распределяем по филумам
ggplot(allinfo03, aes(x = "", fill = Phenotype.New))+
  geom_histogram(width = 1, position = "fill", stat = "count")+
  facet_wrap(~ Phylum, nrow = 1)+
  coord_polar(theta = "y")+
  scale_x_discrete(name = "", labels = c("", "", "", ""), breaks = NULL)+
  scale_y_continuous(name = "", breaks = NULL)+
  scale_fill_brewer(type = "qual", palette = 3, name = "Phenotype: ")+
  theme(strip.background = element_rect(fill = "blue"), 
        panel.background = element_rect(fill = "white"), 
        panel.spacing = unit(x = 0.1, units = "cm"),
        axis.title = element_text(size = 18, vjust = 1),
        legend.position = "bottom",
        legend.title = element_text(size = 16, vjust = 0),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 14, colour = "white", face = "bold"))


# Одним графиком не получается - сделаем два!
ggplot(df05[df05$Var2 == "Pathogen", ], aes(x = "", y = Freq, fill = Var1))+
  geom_col(width = 1, position = "stack")+
  coord_polar(theta = "y")+
  scale_x_discrete(name = "", labels = c("", "", "", ""), breaks = NULL)+
  scale_y_continuous(name = "Pathogens", breaks = NULL)+
  scale_fill_brewer(type = "qual", palette = 1, name = "Phylum")+
  theme(strip.background = element_rect(fill = "purple"), 
        panel.background = element_rect(fill = "white"), 
        panel.spacing = unit(x = 0.1, units = "cm"),
        axis.title = element_text(size = 18, vjust = 1),
        legend.title = element_text(size = 16, vjust = 0),
        strip.text.x = element_text(size = 14, colour = "green", face = "bold"))

ggplot(df05[df05$Var2 == "Non-Pathogen", ], aes(x = "", y = Freq, fill = Var1))+
  geom_col(width = 1, position = "stack")+
  coord_polar(theta = "y")+
  scale_x_discrete(name = "", labels = c("", "", "", ""), breaks = NULL)+
  scale_y_continuous(name = "Non-Pathogens", breaks = NULL)+
  scale_fill_brewer(type = "qual", palette = 1, name = "Phylum")+
  theme(strip.background = element_rect(fill = "purple"), 
        panel.background = element_rect(fill = "white"), 
        panel.spacing = unit(x = 0.1, units = "cm"),
        axis.title = element_text(size = 18, vjust = 1),
        legend.title = element_text(size = 16, vjust = 0),
        strip.text.x = element_text(size = 14, colour = "green", face = "bold"))

# Альтернативное второе правило отбора - берем только филумы, в которых число наблюдений
#по каждому из параметров не равно 0

(table04 <- table02[apply(table02, 1, all),])
rownames(table02[!apply(table02, 1, all),])

# Альтернативное третье правило отбора - берем из table04 только те филумы, в которых число
#наблюдений по каждому из параметра Phenotype.New больше 5 (то есть снова приходим к table02)
(table05 <- table04[apply(table04, 1, function(x){all(x > 5)}),])
df05 <- as.data.frame(table05)
rownames(table04)[!apply(table04, 1, function(x){all(x > 5)})]




# проводим тест хи-квадрат - с предупреждением! откуда предупреждение? - избавились от него)
?fisher.test
#chisqres_t02 <- chisq.test(table02)
#chires_dt02 <- as.data.frame(chisqres_t02$residuals)
#chisqres_t03 <- chisq.test(table03)
#chisqres_t03$residuals
chisqres_t05 <- chisq.test(table05)
fisherres_t04 <- fisher.test(table04, workspace = 2e+06)
chisqres_dt05 <- as.data.frame(chisqres_t05$residuals)

chisqres_t05$observed
chisqres_t05$expected
chisqres_t05$residuals
chisqres_t05$p.value
fisherres_t04$p.value

# Графики и визуализация
# cпособ через vcd пакет
require(vcd)
mosaic(table02, shade=T, legend=T)
assoc(table02, shade=T, legend=T)

# способ из курса по статистике 2 - пока видится как наиболее предпочтительный
?mosaicplot
mosaicplot(table05, 
           color = T, 
           shade = T, 
           main = "", 
           cex.axis = 0.8, 
           #off = 1, 
           type = "pearson")

# через ggplot2
?facet_grid
?scale_fill_brewer
??"else if"

chisqres_dt05 <- chisqres_dt05 %>% 
  mutate(Freq2 = factor(sapply(Freq, function(x){
    if (x < -4){-4
    } else if (x < -2) {-2
    } else if (x < 0) {-1
    } else if (x > 4) {4
    } else if (x > 2) {2
    } else if (x > 0) {1
    }
  })))
as.data.frame(chisqres_t05$observed)
chisqres_dt05$Count <- as.data.frame(chisqres_t05$observed)$Freq

ggplot(chisqres_dt05, aes(x = Var2, y = Count, fill = Freq2))+
  geom_col(position = "fill", width = 1)+
  facet_grid(Var1 ~ Var2, switch = "y", scales = "free_x", space = "free")+
  scale_x_discrete(name = "Phenotype", labels = c("", "", "", ""), breaks = NULL, position = "top")+
  scale_y_continuous(name = "Phylum", breaks = NULL)+
  scale_fill_brewer(type = "div", 
                    palette = 7, 
                    name = "Pearson's\nresiduals",
                    labels = c("<-4", "-4:-2", "-2:0", "0:2", "2:4", ">4"))+
  theme(strip.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"), 
        panel.spacing = unit(x = 0.05, units = "cm"),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14, face = "italic"),
        strip.text.y = element_text(angle = 180, hjust = 1, vjust = 0.5))
?element_text
?element_rect
