
library(ggplot2)
library(MASS)  
library(leaps)  
library(car)   
library(Metrics)
library(glmnet)
library(dplyr)

#Data collection
bil_data <- read.csv("C:/Users/makis/OneDrive/Skrivbord/R_programmering_k_2/R_kunskapskontroll_2_regression_analysis/skrapning_v3_cleaned.csv")

summary(bil_data)
names(bil_data)

data_price_filter <- subset(bil_data, Price > 30000)
bil_data_clean <- na.omit(data_price_filter)
summary(bil_data_clean)

hist(bil_data_clean$Price)
hist(bil_data_clean$Horsepower)
hist(bil_data_clean$Year)
hist(bil_data_clean$Miles)
hist(log(bil_data_clean$Miles))

frequency_table <- table(bil_data_clean$Location)
print(frequency_table)

model_l <- lm(Price ~ . -Brand -Model -Engine.Volume -Name -Company, data = bil_data_clean) 
summary(model_l)
par(mfrow=c(2,2))
plot(model_l)
vif(model_l)

cor(bil_data_clean$Miles, bil_data_clean$Year)

#Creating additional features
bil_data_clean$Age <- 2024 - bil_data_clean$Year

plot(Price ~ Age, data=bil_data_clean)
plot(Price ~ I(Age * Miles), data=bil_data_clean)
plot(Price ~ log(Age), data=bil_data_clean)
plot(Price ~ I(Miles / Age), data=bil_data_clean)

#creating model
model_l_int <- lm(Price ~ . -Brand -Model -Engine.Volume -Name -Company -Year + Horsepower:Age + Miles:Age, data = bil_data_clean)
summary(model_l_int)
par(mfrow=c(2,2))
plot(model_l_int)
vif(model_l_int)
anova(model_l_int)

model_l_log <- lm(I(log(Price)) ~ . -X -Brand -Model -Engine.Volume -Name -Company -Year + Miles:Age, data = bil_data_clean)
summary(model_l_log)

plot(model_l_log)
vif(model_l_log)

bil_data[10736,]
par(mfrow=c(1,2))
hist(bil_data_clean$Price)
hist(log(bil_data_clean$Price))

# Adding data
external_data <- read.csv("C:/Users/makis/OneDrive/Skrivbord/R_programmering_k_2/R_kunskapskontroll_2_regression_analysis/sverigesmedelinkomst.csv")
medel_inkomst_lan <- external_data[grepl("län", external_data$region), ]

medel_inkomst <- list(
  Stockholm = 415.4,
  Dalarna = 313.3,
  Västmanland = 325.1,
  Skåne = 333.2,
  Örebro = 312.1,
  Kronoberg = 325.9,
  Västernorrland = 314.0,
  Göteborg = 337.6,
  Kalmar = 309.8,
  Södermanland = 318.5,
  Halland = 363.8,
  Blekinge = 313.1,
  Älvsborg = 337.6,
  Östergötland = 320.0,
  Uppsala = 326.1,
  Västerbotten = 317.4,
  Jämtland = 325.9,
  Skaraborg = 337.6,
  Jönköping = 325.0,
  Värmland = 316.6,
  Norrbotten = 328.2,
  Gävleborg = 312.2
)

library(dplyr)
medel_inkomst_vector <- unlist(medel_inkomst)
bil_data_clean_1 <- bil_data_clean %>%
  mutate(Mean_income = medel_inkomst_vector[Location])

print(bil_data_clean_1)
summary(bil_data_clean_1)

model_l_log_ext <- lm(I(log(Price)) ~ . -X -Brand -Model -Engine.Volume -Name -Company -Location -Year + Miles:Age, data = bil_data_clean_1)
summary(model_l_log_ext)
par(mfrow=c(2,2))
plot(model_l_log_ext)
vif(model_l_log_ext)

# Plot the data to check different assumptions
predicted_values <- predict(model_l_log_ext, bil_data_clean_1)

plot_data <- data.frame(True_Pris = log(bil_data_clean_1$Price), Predicted_Price = predicted_values, Location = bil_data_clean_1$Location)

ggplot(plot_data, aes(x = True_Pris, y = Predicted_Price, color = Location)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Log True Prices", y = "Log Predicted Prices", title = "True vs. Predicted Prices by Location") +
  theme_minimal()

# Ridge Regression
library(glmnet)

data_modified <- bil_data_clean[, !(names(bil_data_clean) %in% c("Brand", "Model", "Engine.Volume", "Name", "Company"))]
data_modified$Horsepower_Year <- bil_data_clean$Horsepower * bil_data_clean$Year
data_modified$Miles_Year <- bil_data_clean$Miles * bil_data_clean$Year
X <- model.matrix(~ . - Price, data = data_modified)
Y <- bil_data_clean$Price

set.seed(123)
cv_model <- cv.glmnet(X, Y, alpha = 0, type.measure = "mse")

best_lambda <- cv_model$lambda.min
print(best_lambda)

predictions <- predict(cv_model, s = best_lambda, newx = X)

# Comparing the two models
set.seed(123)
sample_size <- floor(0.8 * nrow(bil_data_clean))
train_indices <- sample(seq_len(nrow(bil_data_clean)), size = sample_size)

train_data <- bil_data_clean[train_indices, ]
test_data <- bil_data_clean[-train_indices, ]

unnecessary_columns <- c("Brand", "Model", "Engine.Volume", "Name", "Company")
train_data <- train_data[, !(names(train_data) %in% unnecessary_columns)]
test_data <- test_data[, !(names(test_data) %in% unnecessary_columns)]

train_data$Horsepower_Year <- train_data$Horsepower * train_data$Year
train_data$Miles_Year <- train_data$Miles * train_data$Year
test_data$Horsepower_Year <- test_data$Horsepower * test_data$Year
test_data$Miles_Year <- test_data$Miles * test_data$Year

model_l <- lm(Price ~ . + Horsepower:Year + Miles:Year, data = train_data)

X_train <- model.matrix(~ . - Price, data = train_data)
Y_train <- train_data$Price

cv_model <- cv.glmnet(X_train, Y_train, alpha = 0, type.measure = "mse")

predictions_l <- predict(model_l, newdata = test_data)

X_test <- model.matrix(~ . - Price, data = test_data)

predictions_ridge <- predict(cv_model, s = cv_model$lambda.min, newx = X_test)

rmse_l <- sqrt(mean((test_data$Price - predictions_l)^2))
rmse_ridge <- sqrt(mean((test_data$Price - predictions_ridge)^2))

cat("RMSE for Linear Model:", rmse_l, "\n")
cat("RMSE for Ridge Model:", rmse_ridge, "\n")
