# Import our required libraries
library(rpart)
library(rpart.plot)

install.packages("rattle")
library(rattle)
library(randomForest)
library(readr)
library(tidyverse)
library(inspectdf)
library(glue)
library(caret)
library(funModeling)
library(caretEnsemble)
library(DMwR)
library(pROC)

#Import dataset
mushroom <- "/Users/HP/Desktop/EightSem/Decision Analysis Lab/Mushroom Classification/mushrooms.csv"
data <- read.csv(mushroom)
head(data)

#Columns of Dataset:
colnames(data) <- c("Class","cap.shape","cap.surface","cap.color","bruises","odor","gill.attachment","gill.spacing",
                         "gill.size","gill.color","stalk.shape","stalk.root","stalk.surface.above.ring",
                         "stalk.surface.below.ring","stalk.color.above.ring","stalk.color.below.ring","veil.type","veil.color",
                         "ring.number","ring.type","print","population","habitat")
head(data)

#Summary of the Data:
summary(data)

#Coersion & Clean - Conert all variables to numeric except target(class)
data_x <- data.frame(sapply(data[1:22], function (x) as.numeric(as.factor(x))))
data <- data.frame(data_x, class = data$Class)


cross_plot(data = data, input = c("population", "habitat","spore.print.color","cap.color", "odor"), target = "Class")
################################################################################################

#Decision Tree:

# Define the factor names for "Class"
levels(data$Class) <- c("Edible","Poisonous")

# Define the factor names for "odor"
levels(data$odor) <- c("Almonds","Anise","Creosote","Fishy","Foul","Musty","None","Pungent","Spicy")

# Define the factor names for "print"
levels(data$print) <- c("Black","Brown","Buff","Chocolate","Green","Orange","Purple","White","Yellow")
head(data)


# Create a classification decision tree using "Class" as the variable we want to predict and everything else as its predictors.
myDecisionTree <- rpart(Class ~ ., data = data, method = "class")

fancyRpartPlot(myDecisionTree, sub = NULL)

# Print out a summary of our created model.
print(myDecisionTree)

rpart.plot(myDecisionTree, type = 3, extra = 2, under = TRUE, faclen=5, cex = .75)

newCase  <- data[10,-1]
newCase

predict(myDecisionTree, newCase, type = "class")

train_ind <- sample(c(1:nrow(data)), size = 10)

## 75% of the sample size
n <- nrow(data)
smp_size <- floor(0.75 * n)

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(c(1:n), size = smp_size)

mushrooms_train <- data[train_ind, ]
mushrooms_test <- data[-train_ind, ]

newDT <- rpart(Class ~ ., data = mushrooms_train, method = "class")
result <- predict(newDT, mushrooms_test[,-1], type = "class")
head(result)               

head(mushrooms_test$Class)
#Confusion Matrix
table(mushrooms_test$Class, result)

##################################################################################################

#Random Forest

#Test and Train the Dataset:
set.seed(123)
#Create data for training
sample.ind = sample(2,  
                    nrow(data),
                    replace = T,
                    prob = c(0.05,0.95))
data.dev = data[sample.ind==1,]  
data.val = data[sample.ind==2,]  

# Original Data
table(data$class)/nrow(data)  

# Training Data
table(data.dev$class)/nrow(data.dev)  


# Testing Data
table(data.val$class)/nrow(data.val)  

#Fit Random Forest Model
rf = randomForest(Class~ .,  
                  ntree = 100,
                  data = data.dev)
plot(rf)  

train_index <- sample(nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

#Building Model:
rf_model <- randomForest(as.factor(class)~ ., data = train_data, importance = TRUE)

#Confusion Matrix:
predicted_class <- predict(rf_model, test_data)
confusion_matrix <- table(test_data$class, predicted_class)
confusion_matrix

