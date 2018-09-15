#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RAnalysis2")

library(ggplot2)
library(dplyr)
library(ggthemes)
library(grid)

d <- read.csv("https://stepic.org/media/attachments/course/724/example_data.csv")
glimpse(d)

ggplot(d, aes(date, percent, col = system, group = system))+
  geom_line(size = 1.3)+
  geom_point(shape = 21, size = 4, fill = "black")+
  geom_point(shape = 21, size = 3.5)+
  geom_point(shape = 21, size = 3)+
  geom_vline(xintercept = 7.5, color = "white",
             linetype = "dotted")+
  scale_y_continuous(breaks = c(0, .04, .08, .11, .15),
                     limits = c(0, .15),
                     labels = scales::percent)+
  scale_color_manual(values = c("orangered1", "red", "cyan", "yellow1", "springgreen2"))+
  xlab("")+
  ylab("")+
  theme_classic()+
  ggtitle("Top 5 Linux distributions (% of total per year)")+
  theme(legend.title = element_blank(),
             legend.position = "top",
             plot.background = element_rect(color = "black", fill = "black"),
             text = element_text(color = "white"),
             panel.background = element_rect(color = "black", fill = "black"),
             legend.background = element_rect(fill = "black"),
        panel.grid.major.y = element_line(color = "gray50",
                                          linetype = "longdash"),
        axis.text.x = element_text(face = "bold", size = 16, color = "white"),
        axis.text.y = element_text(face = "bold", size = 14, color = "white"),
        legend.text = element_text(size = 14),
        title = element_text(face = "bold", size = 16, color = "white"))

#сохраняем тему в отдельную переменную
my_theme <-   theme(legend.title = element_blank(),
                    legend.position = "top",
                    plot.background = element_rect(color = "black", fill = "black"),
                    text = element_text(color = "white"),
                    panel.background = element_rect(color = "black", fill = "black"),
                    legend.background = element_rect(fill = "black"),
                    panel.grid.major.y = element_line(color = "gray50",
                                                      linetype = "longdash"),
                    axis.text.x = element_text(face = "bold", size = 16, color = "white"),
                    axis.text.y = element_text(face = "bold", size = 14, color = "white"),
                    legend.text = element_text(size = 14),
                    title = element_text(face = "bold", size = 16, color = "white"))
plot + my_theme
# функция grid добавляет текст послойно на уже нарисованный график
grid.text("Data sourse: The DistroWatch's Page Hit Ranking (Nov. 23, 2011)",
          x = 0.02, y = 0.005, just = c("left", "bottom"),
          gp = gpar(fontface = "bold", fontsize = 9, col = "white"))
grid.text("www.pingdom. com",
          x = 0.98, y = 0.005, just = c("right", "bottom"),
          gp = gpar(fontface = "bold", fontsize = 9, col = "white"))
