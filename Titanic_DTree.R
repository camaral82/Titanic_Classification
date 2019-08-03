' 03/08/2019 - Saturday 

Self-Study of Classification Methods

Comparison of Accuracy in Titanic dataset using KNN and Decision Tree methods
'

setwd("~/3. DATACAMP/Machine Learning")

library(rpart)
library(class)
library(ROCR)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

'1st step: Spliting the data set in Train and Test'

titanic <- read.csv("~/3. DATACAMP/Machine Learning/titanic.csv.txt")
str(titanic)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)

str(titanic)

# Set random seed
set.seed(1)

# Shuffle (embaralhe) the dataset
n <- nrow(titanic)
shuffled <- titanic[sample(n),]

# Spliting the data in train (70%) and test (30%)
train_indices <- 1:round(0.7 * n)
train <- shuffled[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
test <- shuffled[test_indices, ]

str(train)
str(test)

'2nd step: building a Decision Tree Model - maximun tree'
set.seed(1)
tree <- rpart(Survived ~ . , 
              data = train, 
              method = "class",
              control = rpart.control(cp = 0))

# Draw the decision tree
fancyRpartPlot(tree)


'3rd step: doing prediction'
# Predictoing the values of the test set: pred
pred <- predict(tree, test, type="class")

# Construct the confusion matrix: conf
conf <- table(test$Survived, pred)

# Print out the accuracy
sum(diag(conf))/sum(conf)
#Accuracy = 76.63%

# Another way to compute the accuracy on the test dataset
mean(pred == test$Survived)

'4th step: prunning the tree'
# Examine the complexity plot 
plotcp(tree)

#test the accuracy with cp = 0.1 and 0.013
# Pruning the tree
tree_pruned1 <- prune(tree, cp = 0.013)
pred_pruned1 <- predict(tree_pruned1, test, type = "class")
mean(pred_pruned1 == test$Survived)
#accuracy = 73.83

tree_pruned2 <- prune(tree, cp = 0.1)
pred_pruned2 <- predict(tree_pruned2, test, type = "class")
mean(pred_pruned2 == test$Survived)
#accuracy = 73.36


'5th step: Creating a ROC Curve'
# Predict probability values using the model: all_probs
all_probs <- predict(tree, test, type="prob")

# Print out all_probs
all_probs

# Select second column of all_probs: probs
probs <- all_probs[ , 2]

# Make a prediction object: pred
'Use prediction() with probs and the true labels of 
the test set (in the income column of test) to get a prediction object. '
pred2 <- prediction(probs, test$Survived)

# Make a performance object: perf
#TPR - True Positive Rate and False Positive Rate
perf <- performance(pred2, "tpr", "fpr")

# Plot this curve
plot(perf)

'The area under the curve'
# Make a performance object: perf
perf <- performance(pred2, "auc")

# Print out the AUC - result = 81.55%
perf@y.values[[1]]


'Randon Forest'
# Load the randomForest package
library(randomForest)

# Build a random forest model
rf_model <- randomForest(Survived~., data = train)

# Compute the accuracy of the random forest
rf_pred <- predict(rf_model, test)
mean(rf_pred == test$Survived)

#Accuracy: 76.16%