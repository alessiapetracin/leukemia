# Data preparation

dataf <-read.table(file = './gene_expr.tsv', sep = '\t', header = TRUE) # read table
head(dataf) # visualize first rows
head(dplyr::select(dataf, 'sampleID', 'y')) # visualize prediction subgroups
anyNA(dataf) # check for missing values

hist(dataf$y, main = 'Target variable') # visualizee target variable

# Support vector machines
library(e1071) # load the library
x <- dplyr::select(dataf, -'sampleID', -'y') # select the predictors

y <- dataf[,'y'] # select the prediction variable
y <- as.factor(y) # encode it as a factor

#Create the dataframe
dat <- data.frame(x, y)


# Polynomial kernel

set.seed(1200) # set seed for reproducibility
# fit polynomial kernel
tune.out.pol <- tune(svm, y ~ ., data = dat, kernel = 'polynomial', 
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100), 
                  degree = c(1, 2, 3, 4, 5), 
                  scale = c(TRUE, FALSE))) 
bestmod.pol <- tune.out.pol$best.model # select model with lowest error
summary(bestmod.pol) # visualize insights on the best model


# Radial kernel

tune.out.rad <- tune(svm, y ~ ., data = dat, kernel = 'radial',  # fit radial kernel
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100), 
                gamma = c(0.5, 1, 10, 100, 1000), scale = c(TRUE, FALSE)))

bestmod.rad <- tune.out.rad$best.model # save best model
summary(bestmod.rad) # examine best model


# Select best model through ROC curves

# Divide data into training and test set
library(caret)
train_size <- 0.5
train <- createDataPartition(y, p = train_size, list = FALSE)
train_data <- dat[train, ]
test_data <- dat[-train, ]

#Confusion matrix for linear kernel
linear <- svm(y ~., data = train_data, kernel = 'polynomial', 
              cost = bestmod.pol$cost, degree = bestmod.pol$degree, 
              gamma = bestmod.pol$gamma, scale = bestmod.pol$scaled)
ypred <- predict(linear, test_data)
table(predict=ypred, truth=test_data$y)

#Confusion matrix for radial kernel
rad <- svm(y ~., data = train_data, kernel = 'radial', cost = bestmod.rad$cost, 
           gamma = bestmod.rad$gamma, scale = bestmod.rad$scaled)
ypred.rad <- predict(rad, test_data)
table(predict=ypred.rad, truth=test_data$y)

# Plot ROC curves
library(ROCR)

# define function
rocplot <- function(pred, truth, ...){
    predob <- prediction(pred, truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf, ...)
}

# ROC curve for radial fit
svmfit.rad <- svm(y ~ ., data=train_data, kernel="radial", 
    gamma=bestmod.rad$gamma, cost=bestmod.rad$cost, decision.values=TRUE, 
                  scale = bestmod.rad$scaled)
fitted.rad <- attributes(predict(svmfit.rad, test_data, 
                                  decision.values=TRUE))$decision.values


# ROC curve for the polynomial fit
svmfit.pol <- svm(y ~ ., data=train_data, kernel="polynomial", 
        degree=bestmod.pol$degree, gamma=bestmod.pol$gamma, 
        cost=bestmod.pol$cost, scale = bestmod.pol$scaled, 
        decision.values=TRUE)
fitted.pol <- attributes(predict(svmfit.pol, test_data, 
                                  decision.values=TRUE))$decision.values

# Plot curves
rocplot(fitted.rad, test_data$y, main="ROC curves on test data", col="red")
rocplot(fitted.pol, test_data$y, add=T, col="green")
legend("bottomright", legend=c("Radial", 'Polynomial '), 
       col=c("red", 'green'), lty=1)
