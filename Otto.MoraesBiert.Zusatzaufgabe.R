#Zusatzaufgabe Soziologie

library(tidyverse)
library(dplyr)
library(ggthemes)


#1
setwd("C:/Users/ottom/Desktop/Tutorat R")
StudPerf <- read.csv("StudentsPerformance.csv", header = TRUE, sep = ";", fill = TRUE, quote = "")
summarise(StudPerf)


#b
ggplot(data = StudPerf, aes(x = "", y = math_score)) +
  geom_boxplot() +
  labs(x= "Schüler*innen", y= "Mathe Punktzahlen") +
  theme_economist() + 
  scale_color_economist()

#c
Mittelwert <- (mean(StudPerf$math_score) + mean(StudPerf$reading_score) + mean(StudPerf$writing_score)) / 3
Median <- StudPerf %>% 
  select(math_score, writing_score, reading_score) %>% 
  median()
Median <- data.frame(Werte = c(StudPerf[,"math_score"], StudPerf[,"writing_score"], StudPerf[,"reading_score"]))
median(Median$Werte)

#2
ggplot(data = StudPerf, aes(x = reading_score, y = writing_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#c
lineareRegression <- lm(formula = reading_score ~ writing_score, data = StudPerf)
lineareRegression[["coefficients"]][["writing_score"]] * 75 + lineareRegression[["coefficients"]][["(Intercept)"]]

#3
#a
Prep <- StudPerf %>% 
  filter(test_preparation_course == "completed") %>% 
  sample_n(300)

NoPrep <- StudPerf %>% 
  filter(test_preparation_course == "none") %>% 
  sample_n(300)


