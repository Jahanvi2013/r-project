rm(list = ls())

summary(US_Shooting_Dataset)
unique(US_Shooting_Dataset$gender)

ggplot(data=US_Shooting_Dataset, aes(x=place_of_shooting)) + geom_bar()

library(dplyr)
average_victims_by_year <- US_Shooting_Dataset %>%
  group_by(year) %>%
  summarize(average_total_victims = mean(total_victims))

average_victims_by_year

print(average_victims_by_year, n = 500)

library(ggplot2)
ggplot(average_victims_by_year, aes(x = year, y = average_total_victims)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Average Total Victims",
    title = "Average Total Victims in Mass Shootings Over the Years"
  )

age_by_place <- US_Shooting_Dataset %>%
  group_by(place_of_shooting) %>%
  summarize(mean_age = mean(age_of_shooter), median_age = median(age_of_shooter))

ggplot(US_Shooting_Dataset, aes(x = place_of_shooting, y = age_of_shooter)) +
  geom_boxplot() +
  labs(
    x = "Place of Shooting",
    y = "Age of Shooter",
    title = "Association Between Place of Shooting and Shooter's Age"
  )

anova(age_by_place)

mental_health_summary <- US_Shooting_Dataset %>%
  group_by(prior_signs_mental_health_issues) %>%
  summarize(mean_fatalities = mean(fatalities), mean_injuries = mean(injured))

ggplot(mental_health_summary, aes(x = prior_signs_mental_health_issues)) +
  geom_bar(aes(y = mean_fatalities), stat = "identity", position = "dodge") +
  geom_bar(aes(y = mean_injuries), stat = "identity", position = "dodge") +
  labs(
    x = "Prior Signs of Mental Health Issues",
    y = "Mean Count",
    title = "Association Between Prior Mental Health Issues and Fatalities/Injuries"
  )

anova(mental_health_summary)

model <- glm(prior_signs_bi ~ place_of_shooting, data = US_Shooting_Dataset, family = "binomial")
anova_results <- summary(model)
anova_results

model1 <- glm(prior_signs_bi ~ race + gender, data = US_Shooting_Dataset, family = "binomial")
summary(model1)

autoplot(model1)

model2 <- glm(weapons_bi ~ total_victims, data = US_Shooting_Dataset, family = "binomial")
summary(model2)

autoplot(model2)

correlation_fatalities <- cor(US_Shooting_Dataset$number_of_weapons, US_Shooting_Dataset$fatalities)
correlation_injuries <- cor(US_Shooting_Dataset$number_of_weapons, US_Shooting_Dataset$injured)


fatalities_model <- lm(fatalities ~ number_of_weapons, data = US_Shooting_Dataset)

injuries_model <- lm(injured ~ number_of_weapons, data = US_Shooting_Dataset)


model3 <- glm(prior_signs_bi ~ age_of_shooter, data = US_Shooting_Dataset, family = "binomial")
summary(model3)


