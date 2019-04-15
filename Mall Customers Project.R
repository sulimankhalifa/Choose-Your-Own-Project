## Load the required libraries
library(tidyverse)
library(ggplot2)
library(caret)

## Read dataset from my local directory
my_dataset <- read.csv("Mall_Customers.csv", header = TRUE)

## Change columns names for simplicity
colnames(my_dataset) <- c("customerID", "gender", "age", "annual_income", "spending_score")

## Split dataset into training and testing sets
set.seed(1)
test_index <- createDataPartition(my_dataset$gender, p=0.80, list = FALSE)
test_set <- my_dataset[-test_index, ]
train_set <- my_dataset[test_index, ]

## Look at dimensions of training set
dim(train_set)

## Look at classes of each column in training set
sapply(train_set, class)

## Take a peek at the training set
head(train_set)

## Calculate percentage of gender in the training set
percentage <- prop.table(table(train_set$gender)) * 100
cbind(freq = table(train_set$gender), percentage = percentage)

## Look at the summary of the training set
summary(train_set)

## Distribution of gender
plot(train_set$gender)

## Distribution of age
histogram(train_set$age)

## Distribution of annual income
qplot(train_set$annual_income, bins = 30, col = "grey", 
      main = "Annual Income Distribution", xlab = "Income")

## Boxplot for spending scores
boxplot(train_set$spending_score, main = "Spending Score", col = "light blue")

## Top spending customers
train_set %>% group_by(customerID) %>%
  summarize(spending_score) %>%
  arrange(desc(spending_score))

## 10-fold cross validation
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

## Linear Disciminant Analysis model
set.seed(7)
fit.lda <- train(gender~., data = train_set, method = "lda", metric = metric, trControl = control)

## k-Nearest Neighbors model
set.seed(7)
fit.knn <- train(gender~., data = train_set, method = "knn", metric = metric, trControl = control)

## Random Forest model
set.seed(7)
fit.rf <- train(gender~., data = train_set, method = "rf", metric = metric, trControl = control)

## Classifaction and Regression Trees model
set.seed(7)
fit.cart <- train(gender~., data = train_set, method = "rpart", metric = metric, trControl = control)

## Summary of models results
results <- resamples(list(lda = fit.lda, knn = fit.knn, rf = fit.rf, cart = fit.cart))
summary(results)

## Plot of models results
dotplot(results)

## Testing the best model
predictions <- predict(fit.lda , test_set)
confusionMatrix(predictions, test_set$gender)
