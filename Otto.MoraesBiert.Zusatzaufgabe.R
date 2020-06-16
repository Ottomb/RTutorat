#Zusatzaufgabe Soziologie

library(tidyverse)
library(dplyr)
library(ggthemes)

#1
#a
setwd("C:/Users/ottom/Desktop/Tutorat R")
StudPerf <- read.csv("StudentsPerformance.csv", header = TRUE, sep = ";", fill = TRUE, quote = "")
summarise(StudPerf)

#b
ggplot(data = StudPerf, aes(x = "", y = math_score)) +
  geom_boxplot() +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  labs(x = "Schüler*innen", y = "Mathe Punktzahlen") +
  theme_economist() + scale_colour_economist()

#c
AlleScores <- data.frame(Werte = c(StudPerf[,"math_score"], StudPerf[,"writing_score"], StudPerf[,"reading_score"]))
Mittelwert <- mean(AlleScores$Werte)
Median <- median(AlleScores$Werte)

#2
#a
lineareRegression <- lm(formula = writing_score ~ reading_score, data = StudPerf)
summary(lineareRegression)

#b
ggplot(data = StudPerf, aes(x = reading_score, y = writing_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Lese score", y = "Schreib score", title= "Lineare Regression", subtitle = "Zusammenhang der Lese- und Schreibfähigkeiten")+
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme_economist()+
  scale_colour_economist()

#c
lineareRegression[["coefficients"]][["writing_score"]] * 75 + lineareRegression[["coefficients"]][["(Intercept)"]]

#d
lineareRegressionGender <- lm(formula = writing_score ~ reading_score + gender, data = StudPerf)
summary(lineareRegressionGender)

#3
#a
Prep <- StudPerf %>% 
  filter(test_preparation_course == "completed") %>% 
  sample_n(300) %>% 
  select(math_score)

NoPrep <- StudPerf %>% 
  filter(test_preparation_course == "none") %>% 
  sample_n(300) %>% 
  select(math_score)

t.test(Prep, NoPrep, alternative = "two.sided", mu=0, conf.level = 0.95)

#b
PrepFull <- StudPerf %>% 
  filter(test_preparation_course == "completed") %>% 
  select(math_score)

NoPrepFull <- StudPerf %>% 
  filter(test_preparation_course == "none") %>% 
  select(math_score)

t.test(PrepFull, NoPrepFull, alternative = "two.sided", mu=0, conf.level = 0.95)
