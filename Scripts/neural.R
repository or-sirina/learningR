#установка пакетов
install.packages("tensorflow")
install.packages("reticulate")
library(reticulate)
library(tensorflow)
#install_miniconda()
install_tensorflow(
  method = "auto",
  conda = "auto",
  version = "default",
  envname = "r-tensorflow")

install.packages("keras")
install.packages("mlbench")
install.packages("dplyr")
install.packages("magrittr")
install.packages("neuralnet")
install.packages("fastDummies")


#загрузка пакетов
library(ggplot2)
library(fastDummies)
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

#загрузка данных
data("BostonHousing")
data <- BostonHousing
str(data)

#хитрая манипуляция данными
#новый пайп от магриттр - %<>%
#новая функция отбора данных - mutate_if
data %<>% mutate_if(is.factor, as.numeric)

#аналог с базовым мутейт
data %<>% mutate(across(where(is.factor), ~ as.numeric(.x)))

#строим нейросеть
#это только визуализация, инициализация будет позднее
n <- neuralnet(medv ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat, #формула нейросети
               data = data, #источник данных
               hidden = 5, #кол-во скрытых нейронов
               linear.output = FALSE,
               lifesign = 'full', #печатать ли все результаты
               rep = 1) #сколько раз прогонять обучение

#попробуем визуализировать
plot(n,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = TRUE,
     information = TRUE,
     fill = 'lightblue')

#переводим в матрицу
data <- as.matrix(data)
dimnames(data) <- NULL

#разбиваем сет на тренировочную и тестовую выборки
#создание индексов разделения датасета
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(.7, .3))
sum(ind == 1)
sum(ind == 2)

#создаем выборки
training <- data[ind == 1, 1:13]
test <- data[ind == 2, 1:13]
trainingtarget <- data[ind == 1, 14]
testtarget <- data[ind == 2, 14]

#нормализация данных
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

#попробуем провести нормализацию в ручном режиме
#нормализация min-max (значения от 0 до 1)
minmax <- c()
for (i in 1:length(testtarget)) {
  norm <- (testtarget[i] - min(testtarget)) / (max(testtarget) - min(testtarget))
  minmax <- c(minmax, norm)
}

#z-score (среднее после нормализации - 0, а отклонение - 1)
zscore <- c()
for (i in 1:length(testtarget)) {
  norm <- (testtarget[i] - mean(testtarget)) / sd(testtarget)
  zscore <- c(zscore, norm)
}


#запускаем модель
model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(13)) %>%
  layer_dense(units = 1)


model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop', 
                  metrics = 'mae') 


mymodel <- model %>%          
  fit(training,trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

#проверка модели
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget - pred)^2)

plot(testtarget, pred)
lines(testtarget, testtarget, col = "red", 
      lwd = 2, lty = 1)

#продвинутая модель
#model %>%
#  layer_dense(units = 100, activation = 'relu', input_shape = c(13)) %>%
#  layer_dropout(rate=0.4)  %>%
#  layer_dense(units = 50, activation = 'relu')  %>%
#  layer_dropout(rate=0.2)  %>%
#  layer_dense(units = 1)


#классификатор
data("BostonHousing")
data <- BostonHousing
data$medv <- cut(data$medv, 2, c = c("1", "2")) %>%
  as.numeric()
data$medv <- data$medv - 1

data %<>% mutate_if(is.factor, as.numeric)

data <- as.matrix(data)
dimnames(data) <- NULL

ind <- sample(2, nrow(data), replace = TRUE, prob = c(.7, .3))
sum(ind == 1)
sum(ind == 2)

#создаем выборки
training <- data[ind == 1, 1:13]
test <- data[ind == 2, 1:13]
trainingtarget <- to_categorical(data[ind == 1, 14])
testtarget <- to_categorical(data[ind == 2, 14])

#нормализация
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

#созадем модель
model <- keras_model_sequential() 

model %>% 
  layer_dense(units = 60, activation = 'relu', input_shape = ncol(training)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 30, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy'))

#запускаем
model %>% fit(
  training, trainingtarget, 
  epochs = 500, 
  batch_size = 5,
  validation_split = 0.3)

