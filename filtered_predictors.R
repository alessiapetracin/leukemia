# Filter the dataset

dataf <-read.table(file = './gene_expr.tsv', sep = '\t', header = TRUE)

#Select top 5 % standard deviation
sd_values <- apply(x, 2, sd)
sorted_indices <- order(sd_values, decreasing = TRUE)
top_5_percent_index <- ceiling(0.05 * length(sd_values))
selected_variables <- names(x)[sorted_indices[1:top_5_percent_index]]

new_x <- x[,colnames(x) %in% selected_variables] 

#Create the new dataframe for the analysis
new_dat <- data.frame(new_x, y)


# Polynomial kernel

set.seed(1200)
tune.out.pol.sub <- tune(svm, y ~ ., data = new_dat, kernel = 'polynomial', 
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100), 
                  degree = c(1, 2, 3, 4, 5), gamma = c(0.5, 1, 2, 3, 4), 
                                scale = c(TRUE, FALSE)))

bestmod.pol.sub <- tune.out.pol.sub$best.model 
summary(bestmod.pol.sub) # insights on best model

# Radial kernel

tune.out.sub.rad <- tune(svm, y ~ ., data = new_dat, kernel = 'radial', 
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100), 
                                gamma = c(0.1, 1, 10, 100, 1000), 
                              scale = c(TRUE, FALSE)))

bestmod.rad.sub <- tune.out.sub.rad$best.model
summary(bestmod.rad.sub) # insights on best model

#Confusion matrix for polynomial kernel with subset of features

svmfit.pol.sub <- svm(y ~ ., data=train_data, kernel="polynomial", 
              gamma = bestmod.pol.sub$gamma, degree = bestmod.pol.sub$degree,
                   cost=bestmod.pol.sub$cost, scale = bestmod.pol.sub$scaled, 
                   decision.values=TRUE)
ypred.pol.sub <- predict(svmfit.pol.sub, test_data)
table(predict=ypred.pol.sub, truth=test_data$y)

#Confusion matrix for radial kernel with subset of features

svmfit.rad.sub <- svm(y ~ ., data=train_data, kernel="radial", 
                      gamma=bestmod.rad.sub$gamma, 
                   cost=bestmod.rad.sub$cost, decision.values=TRUE, 
                  scale = bestmod.rad.sub$scaled)
ypred.rad.sub <- predict(svmfit.rad.sub, test_data)
table(predict=ypred.rad.sub, truth=test_data$y)

#ROC curve for radial fit with a subset of features

fitted.rad.sub <- attributes(predict(svmfit.rad.sub, test_data, 
                                  decision.values=TRUE))$decision.values

#ROC curve for the polynomial fit with a subset of features

fitted.pol.sub <- attributes(predict(svmfit.pol.sub, test_data, 
                                  decision.values=TRUE))$decision.values

#plot curves
rocplot(fitted.rad.sub, test_data$y, main="ROC curves on test data", col="red")
rocplot(fitted.pol.sub, test_data$y, add=T, col="green")
legend("bottomright", legend=c("Radial subset", 'Polynomial subset'), 
       col=c("red", 'green'), lty=1)
