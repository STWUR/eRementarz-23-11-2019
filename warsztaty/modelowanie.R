# wczytanie bibliotek
library(ggplot2)
library(kernlab)
library(mlr)

#wczytanie danych
dataset <- read.csv("dane/heart.csv") #należy zastąpić podaną tu ścieżkę odpowiednią ścieżką do danych
# można też readr::read_csv("dane/heart.csv")

#prosta charakterystyka danych
summary(dataset)

ggplot(data = dataset, aes(x = thalach, y = chol, color = as.factor(target))) +
  geom_point() +
  theme_bw()

#tworzenie zadania klasyfikacji
set.seed(123)
task <- makeClassifTask("prediction task", data = dataset, target = "target")

#przygotowanie modeli
learner_ksvm <- makeLearner("classif.ksvm")
learner_logreg <- makeLearner("classif.logreg")

#trenowanie modeli
model_ksvm <- train(learner_ksvm, task)
model_logreg <- train(learner_logreg, task)

#predykcja dla nowych danych
predict(model_logreg, newdata = data.frame(age = 32, 
                                           sex = 1, 
                                           cp = 2, 
                                           trestbps = 120, 
                                           chol = 203, 
                                           fbs = 1, 
                                           restecg = 0, 
                                           thalach = 180, 
                                           exang = 1, 
                                           oldpeak = 0.9, 
                                           slope = 1, 
                                           ca = 1, 
                                           thal = 1))

#predykcja dla oryginalnego zbioru
predict(model_logreg, newdata = dataset[, -14]) -> pred

#wektor predykcji
pred$data$response

#porównnanie oryginalnego zbioru i oryginalnej wartości docelowej z predykcją
ggplot(data = rbind(cbind(dataset, group = "target"),
                    cbind(dataset[, -14], target = pred$data$response, group = "response")),
       aes(x = thalach, y = chol, color = as.factor(target))) +
  geom_point() +
  theme_bw() +
  facet_wrap(~group)

#skuteczność modelu
sum(dataset[ ,14] == pred$data$response) / nrow(dataset)

#walidacja krzyżowa modeli
crossval(learner_ksvm, task, measures = list(acc))
crossval(learner_logreg, task, measures = list(acc))