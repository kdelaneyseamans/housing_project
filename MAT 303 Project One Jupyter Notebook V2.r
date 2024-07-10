
housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
   view <- factor(view)
   backyard <- factor(backyard)
})

# number of columns
ncol(housing)

# number of rows
nrow(housing)

# Loading housing_v2 data set from a housing_v2.csv file
housing_v2 <- read.csv(file='housing_v2.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
housing_v2 <- within(housing_v2, {
   backyard <- factor(backyard)
   view <- factor(view)
})

# Variables and their type
print("Variables")
sapply(housing_v2, class)


# Print the data set
print("dataset")
housing_v2


# Print the first 10 rows
print("head")
head(housing_v2, 10)

plot(housing_v2$price, housing_v2$sqft_living, 
     main = "Scatterplot of Living Area against Price",
     xlab = "Price", ylab = "Living Area",
     xlim=c(0, 1600000),
     ylim=c(300, 5000),
     col="blue", 
     pch = 19, frame = FALSE)

plot(housing_v2$price, housing_v2$age, 
     main = "Scatterplot of Age against Price",
     xlab = "Price", ylab = "Age",
     xlim=c(0, 1700000),
     ylim=c(0, 120),
     col="green", 
     pch = 19, frame = FALSE)

# Selecting price, sqft_living to subset the data
myvars <- c("price","sqft_living")
housing_v2_subset <- housing_v2[myvars]

# Print the first 10 rows
print("head")
head(housing_v2_subset, 10)

# Print the correlation matrix
print("cor")
corr_matrix <- cor(housing_v2_subset, method = "pearson")
round(corr_matrix, 4)

# Selecting price, age to subset the data
myvars <- c("price","age")
housing_v2_subset <- housing_v2[myvars]

# Print the first 10 rows
print("head")
head(housing_v2_subset, 10)

# Print the correlation matrix
print("cor")
corr_matrix <- cor(housing_v2_subset, method = "pearson")
round(corr_matrix, 4)

# Create the multiple regression model and print the statistics
model1 <- lm(price ~ sqft_living + sqft_above + age + bathrooms + view, data=housing_v2)
summary(model1)

fitted_values <- fitted.values(model1) 
fitted_values

residuals <- residuals(model1)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="skyblue", 
     pch = 19)

qqnorm(residuals, pch = 19, col="magenta", frame = FALSE)
qqline(residuals, col = "orange", lwd = 2)

# confidence intervals for model parameters
print("confint")
conf_95_int <- confint(model1, level=0.95) 
round(conf_95_int, 4)

newdata <- data.frame(sqft_living=2150, sqft_above=1050, age=15, bathrooms=3, view='0')

print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

newdata <- data.frame(sqft_living=4250, sqft_above=2100, age=5, bathrooms=5, view='2')

print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

plot(housing_v2$price, housing_v2$school_rating, 
     main = "Scatterplot of Age against School Rating",
     xlab = "Price", ylab = "School Rating",
     xlim=c(0, 1700000),
     ylim=c(0, 10),
     col="maroon3", 
     pch = 19, frame = FALSE)

plot(housing_v2$price, housing_v2$crime, 
     main = "Scatterplot of Age against Crime",
     xlab = "Price", ylab = "Crime",
     xlim=c(0, 1700000),
     ylim=c(0, 425),
     col="royalblue1", 
     pch = 19, frame = FALSE)

# Create the second order model
model2 <- lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2) + I(crime^2), data=housing_v2)
summary(model2)

fitted_values <- fitted.values(model2) 
fitted_values

residuals <- residuals(model2)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="yellow1", 
     pch = 19)

qqnorm(residuals, pch = 19, col="deeppink", frame = FALSE)
qqline(residuals, col = "darkgreen", lwd = 2)

newdata <- data.frame(school_rating=9.8, crime=81.02)

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

newdata <- data.frame(school_rating=4.28, crime=215.50)

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

# Create the first order regression model
model3 <- lm(price ~ school_rating + crime + school_rating:crime, data=housing_v2)
summary(model3)

# Create the complete model
fit_complete <- lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2) + I(crime^2), data=housing_v2)

# Create the reduced model
fit_reduced <- lm(price ~ school_rating + crime + school_rating:crime, data=housing_v2)

# Perform the F-test
anova(fit_complete, fit_reduced)
