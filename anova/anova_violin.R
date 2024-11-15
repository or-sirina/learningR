##########анова тест######

#выполнить если нет библиотек
#remotes::install_github("IndrajeetPatil/statsExpressions")
#remotes::install_github("IndrajeetPatil/ggstatsplot")

#устанавливаем библиотеки
library(ggplot2)
library(haven)
library(ggplot2)
library(multcompView)
library(dplyr)
library(ggstatsplot)

options(skipen = 999)

GSS2021 <- read_dta("GSS2021.dta")

#перобразуем в факторы
GSS2021$sex <- as.factor(GSS2021$sex)
GSS2021$partyid <- as.factor(GSS2021$partyid)
GSS2021$realinc <- as.numeric(GSS2021$realinc)


#преобразуем partyid
partyid_3 <- GSS2021$partyid #вычленим вектор отдельно, чтобы упростить манипуляции

#меняем уровни фактора
levels(partyid_3)[levels(partyid_3) == "0"] <- "Democrat"
levels(partyid_3)[levels(partyid_3) == "1"] <- "Democrat"
levels(partyid_3)[levels(partyid_3) == "5"] <- "Republican"
levels(partyid_3)[levels(partyid_3) == "6"] <- "Republican"
levels(partyid_3)[levels(partyid_3) == "2"] <- "Independent"
levels(partyid_3)[levels(partyid_3) == "3"] <- "Independent"
levels(partyid_3)[levels(partyid_3) == "4"] <- "Independent"
levels(partyid_3)[levels(partyid_3) == "7"] <- "Other party"

GSS2021 <- cbind(GSS2021, partyid_3) #присоединяем измененный фактор

#преобразуем пол
levels(GSS2021$sex) <- c("male", "female")

#сохраняем переменные в отдельную таблицу
dat <- GSS2021 %>%
  select(sex, partyid_3, realinc)

#удаляем пустые ячейки
dat <- na.omit(dat)

#смотрим саммери таблицы
summary(dat)

#простая визуализация распределения
ggplot(dat) +
  geom_jitter(aes(x = partyid_3, y = realinc, color = sex)) +
  theme(legend.position = "none") + facet_grid(.~sex)

#альтернативная визуализация распределения
qplot(x = partyid_3, y = realinc, geom = "point", data = dat) +
  facet_grid(.~sex, labeller = label_both)

#анализ распределения
anova <- aov(realinc ~ partyid_3*sex, data = dat)
summary(anova)

#факторная таблица с средним и ст.дев.
data_summary <- group_by(dat, sex, partyid_3) %>%
  summarise(mean=mean(realinc), sd=sd(realinc)) %>%
  arrange(desc(mean))
print(data_summary)

#Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

colnames(dat) <- c("Sex", "Party", "Income")
levels(dat$Sex) <- c("Мужчины", "Женщины")
#боксплот
grouped_ggwithinstats(data = dat, 
               x = Party, 
               y = Income, 
               grouping.var = Sex,
               ylab = "Партия",
               xlab = "Доход домохозяйства",
               pairwise.comparisons = FALSE)