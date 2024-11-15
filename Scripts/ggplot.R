#вот тут мы грузим библиотеки
#предварительно их надо установить
library(ggplot2) #для визуализации
library(dplyr) #для удобной работы с таблицами
library(anytime) #для простой работы с датами


#импорт базы данных
east <- read.csv("https://raw.githubusercontent.com/or-sirina/data_for_lessons/main/east.csv")
  
#фильтр таблицы с помощью библиотеки dplyr
east <- east %>% filter(fatalities > 10)
east$country <- as.factor(east$country)
syria <- east %>% filter(country == "Syria")
iraq <- east %>% filter(country == "Iraq")

#создаем таблицу из таблиц, сделанных из таблицы
#больше таблиц
#аве
data <- data.frame(
  var1 = syria$fatalities,
  var2 = iraq$fatalities)



##############генерим чарт распределения###################

########      #          #       #      ########
#      #    #   #       # #     # #     #      #
#      #  #########    #   #   #   #    #      #
#      #  #       #   #     # #     #   #      #
#      #  #       #  #       #       #  #      #

#график можно сохранять в переменную
p <- ggplot(data) +
  #верхний график
  geom_density(aes(x = var1, y = ..density..), #комментарии можно писать в любом
               fill="#69b3a2" ) + #месте
  geom_label(aes(x = 25, y = 0.15, label = "Сирия"), 
              color="#69b3a2") +
  #нижний график
  geom_density(aes(x = var2, y = -..density..), 
                fill= "#404080") +
  geom_label(aes(x = 25, y = -0.1, label = "Ирак"), 
              color="#404080") +
  labs(x = "Жертвы конфликтов",
                       y = "Плотность распределения") +
  theme_void()

p

#теперь то же самое, но диаграмму

p <- ggplot(data) +
  #верхний график
  geom_histogram( aes(x = var1, y = ..density..), 
                  fill = "#69b3a2" ) +
  geom_label(aes(x = 25, y = 0.15, label = "Сирия"), 
              color="#69b3a2") +
  #нижний график
  geom_histogram(aes(x = var2, y = -..density..), 
                  fill = "#404080") +
  geom_label(aes(x = 25, y = - 0.1, label = "Ирак"), 
              color = "#404080") +
  theme_void() +
  labs(x = "Жертвы конфликтов",
       y = "Плотность распределения")
p



#работа с датами
#   %d = Day number of month (5, 17, 28, etc.)
#   %m = Month number (e.g. 01, 02, 03, 04)
#   %b = Abbreviated month (Jan, Feb, etc.)
#   %B = Full month (January, February, etc.)
#   %y = 2-digit year (e.g. 89)
#   %Y = 4-digit year (e.g. 1989)

#но мы это забываем, так как есть библиотека anydate()
east$event_date <- anydate(east$event_date)

#смотрим, какая красота получилась
east$event_date

neweast <- east %>%
  mutate(country = ifelse(country == "Syria", "Сирия", country)) %>%
  mutate(country = ifelse(country == "Yemen", "Йемен", country))

#изобразим обычную временную прямую
#но так, чтоб было много прямых в зависимости от страны

p <- ggplot(neweast %>%
         filter(country == "Syria" | country == "Yemen")) + 
  geom_line(aes(x = event_date,
                y = fatalities,
                color = country)) +
  facet_grid(country ~.)


#заменим на geom_smooth()

p <- ggplot(neweast %>%
         filter(country == "Сирия" | country == "Йемен")) + 
  geom_smooth(aes(x = event_date, 
                  y = fatalities,
                  color = country)) +
  facet_grid(country ~.)

#сохранение графика в переменную позволяет добавить новые графические элементы
#прямо к переменной

p <- p + labs(x = "Дата", y = "Жертвы")



p <- p + scale_color_discrete(name = "Страны")


#добавим кастомизации


stilniy_graphic_crasota_1998_bez_sms_i_registracii <- p +
  theme_bw() + 
  theme(
    #шрифт и размер букав
    plot.title = element_text(face = "bold", size = 12),
    #фон легенды
    legend.background = element_rect(
      fill = "white", 
      linewidth = 4, 
      colour = "white"),
    #чёрточки на оси x
    axis.ticks = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.major = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )

stilniy_graphic_crasota_1998_bez_sms_i_registracii