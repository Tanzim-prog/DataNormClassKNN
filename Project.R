library(caTools)
library(class)
library(caret)

getwd()

wheat <- read.csv("wheat.csv", header = TRUE, sep = ",")
wheat
View(wheat)
summary (wheat)

min_max_normalization <- function(parameter)
  {
  result <- (parameter - min(parameter)) / (max(parameter) - min(parameter))
  return(result)
  }
normalized_wheat <- as.data.frame((sapply(wheat[,1:7], min_max_normalization)))
normalized_wheat
View(normalized_wheat)

summary (normalized_wheat)

categorical_normalized_wheat <- cbind(wheat[8], normalized_wheat)
categorical_normalized_wheat
View(categorical_normalized_wheat)

split = sample.split(categorical_normalized_wheat$category, SplitRatio = 0.8)
training_set = subset(categorical_normalized_wheat, split == TRUE)
test_set = subset(categorical_normalized_wheat, split == FALSE)

training_set
View(training_set)
test_set
View(test_set)

training_set_class <- training_set[1:168, 1]
test_set_class <- test_set[1:42, 1]
training_set_class
test_set_class

NROW(training_set)
NROW(test_set)
sqrt(168)

knn_12 <- knn(train = training_set, test = test_set, cl = training_set_class, k = 12)
knn_12
knn_13 <- knn(train = training_set, test = test_set, cl = training_set_class, k = 13)
knn_13

accuracy_12 <- 100 * sum(test_set_class == knn_12) / NROW(test_set_class)
accuracy_12
table(knn_12, test_set_class)
accuracy_13 <- 100 * sum(test_set_class == knn_13) / NROW(test_set_class)
accuracy_13
table(knn_12, test_set_class)

confusionMatrix(table(knn_12, test_set_class))
confusionMatrix(table(knn_13, test_set_class))