library(dplyr)
races <- c("Asian", "Hispanic", "Caucasian", "Black")

school <- c("One", "Two")

students <- 1:100

gender <- c("M", "F")

df <- data.frame(students = students, race = sample(races, 100, TRUE), 
                 school = sample(school, 100, TRUE), gender = sample(gender, 100, TRUE),
                 gpa = rnorm(100, 3, 0.1), prof_race = sample(races, 100, TRUE), 
                 prof_gender = sample(gender, 100, TRUE)) %>% 
  mutate(same_race = ifelse(race == prof_race, 1, 0),
         same_gender = ifelse(gender == prof_gender, 1,0)) %>% 
  mutate(gpa = ifelse(same_race | same_gender, gpa + .2, gpa))

library(lme4)

fit <- lmer(gpa ~ same_race + same_gender + same_race * same_gender + (1|school) + 
              (1|prof_race) + (1|race), data = df)
summary(fit)
arm::display(fit)
library(sjPlot)
plot_model(fit)
summary(fit)
sjt.lmer(fit)
