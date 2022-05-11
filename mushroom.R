
#install.packages("e1071")
require(e1071) #For the Naive Bayes Classifier

#install.packages("caret")
library(caret) # For confusion Matrix function

#install.packages("caTools")
library(caTools) #For Random Split using function

#install.packages("FSelectorRcpp")
library(FSelectorRcpp) #For Information Gain function

#install.packages("Hmisc")
library(Hmisc) #For discribe method

#Dataset file path
path <- "\\Users\\arsxl\\OneDrive\\Desktop\\Mushroom Data\\mushrooms.csv"

# reading dataset of csv file
dataset <- read.csv(path)

# dataset of the csv file
print(dataset)

#finding the number of features 
names(dataset)



#summary of the datset
describe(dataset)

#finding the missing values
print(sum(is.na(dataset)))

#converting the dataset to table for ploting
class <- table(dataset$class)
barplot(class,
        width = 0.3,
        main = "Mushrooms Edibility",
        ylab = "Count",
        names.arg = c("Edible", "Poisonus"),
        col = "darkred")



par(mfrow = c(2, 3))  # Set up a 2 x 3 plotting space

# Create the loop.vector for columns
loop.vector <- 2:7

for (i in loop.vector) { # Loop over loop.vector
  
  # conveting class and others columns in table one by one
  data <- table(dataset$class, dataset[,i])
  #ploting the variable w.r.t class
  barplot(data,
          xlab=names(dataset)[i], col=c("darkblue","red"),
          legend = rownames(data))
}
  
# Create the loop.vector
loop.vector <- 8:13

for (i in loop.vector) { # Loop over loop.vector
  
  # conveting class and others columns in table one by one
  data <- table(dataset$class, dataset[,i])
  #ploting the variable w.r.t class
  barplot(data,
          xlab=names(dataset)[i], col=c("darkblue","red"),
          legend = rownames(data))
}

# Create the loop.vector
loop.vector <- 14:19

for (i in loop.vector) { # Loop over loop.vector
  
  # conveting class and others columns in table one by one
  data <- table(dataset$class, dataset[,i])
  #ploting the variable w.r.t class
  barplot(data,
          xlab=names(dataset)[i], col=c("darkblue","red"),
          legend = rownames(data))
}

# Create the loop.vector
loop.vector <- 20:23

for (i in loop.vector) { # Loop over loop.vector
  
  # conveting class and others columns in table one by one
  data <- table(dataset$class, dataset[,i])
  #ploting the variable w.r.t class
  barplot(data,
          xlab=names(dataset)[i], col=c("darkblue","red"),
          legend = rownames(data))
}

#finding the types of features
str(dataset)

#dividing the dataset
x <- dataset[2:23]

# y is a target variable
y <- dataset$class

# Information Gain of the each feature w.r.t class
igtable <-information_gain(x=x, y=y)

# Sortinng it in desecending order
igtable <-igtable[order(igtable$importance, decreasing = TRUE),]
igtable


#converting the ploting space back to 1 x 1
par(mfrow = c(1, 1))

#barplot of the Information Gain table
barplot(igtable$importance,
        cex.names = 0.6,
        names.arg = igtable$attributes)

# Getting the names of the features with good information gain
features <- igtable$attributes[1:14]
features

features <- append(features, "class")


#Selecting those features
dataset = dataset[features]


#dividing the dataset
x <- dataset[1:14]

# y is a target variable
y <- dataset$class

# Information Gain of the each feature w.r.t class
igtable <-information_gain(x=x, y=y)

# Sortinng it in desecending order
igtable <-igtable[order(igtable$importance, decreasing = TRUE),]
igtable


#converting the ploting space back to 1 x 1
par(mfrow = c(1, 1))

#barplot of the Information Gain table
barplot(igtable$importance,
        cex.names = 0.6,
        names.arg = igtable$attributes)










#random split of 80/20 for train & test set respectively
sample_data = sample.split(dataset, SplitRatio = 0.8)
train_data <- subset(dataset, sample_data == TRUE)
test_data <- subset(dataset, sample_data == FALSE)


#training the model on the training set
model <- naiveBayes(class~., data = train_data)

#showing the class of model
class(model) 

#spliting the target variable and others features for testing purposes
x_test = test_data[1:14]
y_test = test_data["class"]


#predicting using the trained model
pred <- predict(model,x_test)

#checking the class of model which is factor
class(pred)
#checking the class of target variable which is dataframe
class(y_test)

#conveting target variable to factor for confustion matrix
y_test <- as.factor(y_test$class)

#creating the confusion matrix of the data
confusionMatrix(pred, as.factor(y_test))





































