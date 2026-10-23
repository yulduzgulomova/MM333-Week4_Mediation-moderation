rm(list = ls())

options (scipen = 999)

setwd("C:/Users/HP Envy/OneDrive - University of Reading/Year 3/Advanced data analytics/Week 4")

library(car)
library(lmtest)
library(mediation)
library(interactions)
library(tidyverse)

staff <- read_csv("MM333_week4_mediation.csv")
regions <- read_csv("MM333_week4_moderation.csv", show_col_types = FALSE)

# Structure and descriptive statistics
glimpse(staff)
summary(staff)
glimpse(regions)
summary(regions)

# Number of observations
nrow(staff)
nrow(regions)

# Missing values and duplicate IDs
colSums(is.na(staff))
colSums(is.na(regions))
sum(duplicated(staff$staff_id))
sum(duplicated(regions$region_id))

# Correlations between the three main variables
staff |>
  dplyr::select(training_hours, confidence, performance) |>
  cor() |>
    round(2)

sd(regions$ad_spend_k)
sd(regions$loyalty)


# Visualizing relationships

# Total effect: training and performance
ggplot(staff, aes(x = training_hours, y = performance)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Training hours", y = "Performance") +
  theme_minimal()

# Path a: training and confidence
ggplot(staff, aes(x = training_hours, y = confidence)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Training hours", y = "Confidence") +
  theme_minimal()

# Confidence and performance
ggplot(staff, aes(x = confidence, y = performance)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Confidence", y = "Performance") +
  theme_minimal()


# Estimating the path

# Total effect c: training -> performance
model_total <- lm(performance ~ training_hours, data = staff)

# Path a: training -> confidence
path_a <- lm(confidence ~ training_hours, data = staff)

# Paths b and c’: training and confidence -> performance
path_b <- lm(performance ~ training_hours + confidence, data = staff)

summary(model_total)
summary(path_a)
summary(path_b)
confint(model_total)
confint(path_a)
confint(path_b)


# Calculating indirect effect by hand

a <- coef(path_a)["training_hours"]
b <- coef(path_b)["confidence"]
c_prime <- coef(path_b)["training_hours"]
c_total <- coef(model_total)["training_hours"]

# indirect effect
a * b

# direct plus indirect
c_prime + a * b

# proportion mediated
a * b / c_total


# Bootstrapped mediation

set.seed(123)
result <- mediate(path_a, path_b,
                  treat = "training_hours", mediator = "confidence",
                  boot = TRUE, sims = 5000)
summary(result)

plot(result)

#Checking the statistical assumptions
# Diagnostic plots

par(mfrow = c(2, 2))
plot(path_b)
par(mfrow = c(1, 1))

# Statistical tests for path_b
durbinWatsonTest(path_b)
shapiro.test(residuals(path_b))
bptest(path_b)
vif(path_b)

# Influential observations
cooks <- cooks.distance(path_b)
print(cooks)
sum(cooks > 4 / nrow(staff))
print(4/nrow(staff))

(max(cooks))

# Moderation Analysis

# Visualising the interaction
regions <- regions %>%
  mutate(loyalty_group = cut(loyalty,
                             breaks = quantile(loyalty, c(0, 1/3, 2/3, 1)),
                             include.lowest = TRUE,
                             labels = c("Low", "Medium", "High")))

ggplot(regions, aes(x = ad_spend_k, y = sales_k, colour = loyalty_group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Advertising spend (£1,000s)", y = "Sales (£1,000s)",
       colour = "Loyalty") +
  theme_minimal()



# Fitting the interaction model

model_raw <- lm(sales_k ~ ad_spend_k * loyalty, data = regions)
summary(model_raw)
confint(model_raw)

# Mean centering

regions <- regions %>%
  mutate(ad_c  = ad_spend_k - mean(ad_spend_k),
         loy_c = loyalty - mean(loyalty))

model_c <- lm(sales_k ~ ad_c * loy_c, data = regions)
summary(model_c)


#Checking the assumptions for Moderation

par(mfrow = c(2, 2))
plot(model_c)
par(mfrow = c(1, 1))

durbinWatsonTest(model_c)
shapiro.test(residuals(model_c))
bptest(model_c)

cooks_c <- cooks.distance(model_c)
sum(cooks_c > 4 / nrow(regions))

# Coverage of X and W: are there regions at every combination?
ggplot(regions, aes(x = ad_spend_k, y = loyalty)) +
  geom_point() +
  theme_minimal()


# Simple slopes and probing the interaction

# Simple slopes at loyalty = 2, 5 and 8
sim_slopes(model_raw, pred = ad_spend_k, modx = loyalty,
           modx.values = c(2, 5, 8))



# Quick plot with the interactions package
interact_plot(model_raw, pred = ad_spend_k, modx = loyalty,
              modx.values = c(2, 5, 8), interval = TRUE, plot.points = TRUE,
              x.label = "Advertising spend (£1,000s)",
              y.label = "Predicted sales (£1,000s)",
              legend.main = "Loyalty")


