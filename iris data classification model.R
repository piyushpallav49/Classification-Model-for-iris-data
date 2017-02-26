#Loading Dependencies
library(caTools)
library(ggplot2)
library(GGally)
library(e1071)

# Loading the dataset
dataset = iris

# Spliting the Dataset into test set and training set
split = sample.split(dataset$Species, SplitRatio = .8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Exploratory Visualization
ggpairs(training_set, ggplot2::aes(colour = Species, alpha = 0.4))

# Feature Scaling
training_set[,1:4] = scale(training_set[,1:4])
test_set[,1:4] = scale(test_set[,1:4])

# Fitting Logistic Regression on the training set
classifier = svm(formula = Species~., data = training_set, type = 'C-classification', kernel = 'radial')

#Predicting the Test set Data
prob_pred = predict(classifier, type = 'response', newdata = test_set[-5])
test_pred = prob_pred

# Making Confusion Matrix
cm = table(test_set[,5], test_pred)
cm