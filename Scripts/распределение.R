#библиотеки
library(dplyr)
library(ggridges) #библиотека с готовыми темами
library(ggplot2)
library(anytime)


#импорт базы данных
east <- read.csv("https://raw.githubusercontent.com/or-sirina/data_for_lessons/main/east.csv")

#преобразуем фактор и даты
east <- east %>%
  filter(fatalities > 4)
east$country <- as.factor(east$country)
east$event_date <- anydate(east$event_date)


#простой пример
ggplot(east, aes(x = fatalities, y = country, fill = country)) +
  geom_density_ridges() +
  geom_text(aes(x = fatalities, y = 0.5, label = event_date), size = 2, angle=45) +
  theme_ridges() + 
  theme(legend.position = "none")

#расширяем границы (пока графика)
ggplot(east, aes(x = fatalities, y = country, fill = country)) +
  geom_density_ridges() +
  geom_text(aes(x = fatalities, y = -0.1, label = event_date), 
            size = 3, angle = 45, check_overlap = TRUE,
            vjust = "inward") +
  theme_ridges() + 
  theme(legend.position = "none") +
  scale_y_discrete(expand = expansion(mult = 0.3))

#расширяем границы (пока графика)
ggplot(east, aes(x = fatalities, y = country, fill = country)) +
  geom_density_ridges() +
  geom_text(aes(x = fatalities, y = -0.1, label = event_date), 
            size = 3, angle = 45, check_overlap = TRUE,
            vjust = "inward") +
  theme_ridges() + 
  theme(legend.position = "none") +
  scale_y_discrete(expand = expansion(mult = 0.3)) + scale_x_log10()




#зачем нужна логарифмическая шкала???
#сейчас покажу
#импорт базы данных
east <- read.csv("https://raw.githubusercontent.com/or-sirina/data_for_lessons/main/east.csv")
#преобразуем фактор и даты
east$country <- as.factor(east$country)
east$event_date <- anydate(east$event_date)
east$fatalities <- east$fatalities + 1


#сравним график с log10 и без
withlog10 <- ggplot(east, aes(x = fatalities, y = country, fill = country)) +
  geom_density_ridges() +
  geom_text(aes(x = fatalities, y = -0.1, label = event_date), 
            size = 3, angle = 45, check_overlap = TRUE,
            vjust = "inward") +
  theme_ridges() + 
  theme(legend.position = "none") +
  scale_y_discrete(expand = expansion(mult = 0.3)) + 
  scale_x_log10()

withoutlog10 <- ggplot(east, aes(x = fatalities, y = country, fill = country)) +
  geom_density_ridges() +
  geom_text(aes(x = fatalities, y = -0.1, label = event_date), 
            size = 3, angle = 45, check_overlap = TRUE,
            vjust = "inward") +
  theme_ridges() + 
  theme(legend.position = "none") +
  scale_y_discrete(expand = expansion(mult = 0.3))

#для комбинирования графиков
library(patchwork)
withlog10 + withoutlog10
