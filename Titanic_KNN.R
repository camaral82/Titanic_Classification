' 02/08/2019 - Friday 

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

'1st step: split the data set in Train and Test'

titanic <- read.csv("~/3. DATACAMP/Machine Learning/titanic2.csv.txt")
#the dataset titanic2 contains the value of Sex as a integer
#0 Female, 1 Male

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

#Defining the 'Survived' as a factor
train$Survived <- as.factor(train$Survived)
test$Survived <- as.factor(test$Survived)

# Printing the structure of train and test
str(train)
str(test)

'2nd step: KNN Model - normalizing data'

#function to normalize the data
nor <- function(x) {
  (x -min(x))/(max(x)-min(x))   }

# Storing the Survived column of train and test in train_labels and test_labels
train_labels <- train$Survived
test_labels <- test$Survived

# Copying train and test to knn_train and knn_test
knn_train <- train
knn_test <- test

# Droping Survived column for knn_train and knn_test
knn_train$Survived <- NULL
knn_test$Survived <- NULL

# Normalizing Pclass
knn_train$Pclass <- nor(knn_train$Pclass)
knn_test$Pclass <- nor(knn_test$Pclass)

# Normalizing Age
knn_train$Age <- nor(knn_train$Age)
knn_test$Age <- nor(knn_test$Age)


######


'3rd step: The knn() function'
set.seed(1)
# Making predictions using knn: pred
pred <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 10)

# Construct the confusion matrix: conf
conf <- table(test_labels, pred)

# Print out the confusion matrix
conf
sum(diag(conf))/sum(conf) * 100
#with k = 10, accuracy = 76.16%

'Ks choice
Analysing the best number for the parameter K'
range <- 1:round(0.2 * nrow(knn_train))
accs <- rep(0, length(range))

for (k in range) {
  pred <- knn(knn_train, knn_test, train_labels, k = k)
  conf <- table(test_labels, pred)
  accs[k] <- sum(diag(conf))/sum(conf)
}

# Plot the accuracies. Title of x-axis is "k".
plot(range, accs, xlab = "k")

# Calculate the best k
which.max(accs) #using this formula, the best k is 8

sqrt(nrow(titanic)) #using the squared root of the dataset's nrows, k is 27


'testing the K'
# K = 14, accuracy = 78.03
set.seed(1)
pred_14 <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 14)
conf <- table(test_labels, pred_14)
sum(diag(conf))/sum(conf) * 100


# K = 27, accuracy = 74.29
set.seed(1)
pred_27 <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 27)
conf <- table(test_labels, pred_27)
sum(diag(conf))/sum(conf) * 100

