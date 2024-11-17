library(caret)

# TEST RANDOMNESS OF THE PARTITION
sum <- 0
for (i in 1:100){
    partition <- createDataPartition(y, p = 0.8, list = FALSE)
    if (partition[1] == 1){
        sum <- sum + 1
    }
}
print(sum)  # should be around p

# TEST THE DISTRIBUTION OF ACCURACY FOR LASSO AND RIDGE ON MIN AND 1SE
accuracy_lasso <- function(X, y, p, lambda){
    partition <- createDataPartition(y, p = p, list = FALSE)
    X_train <- X[partition, ] 
    y_train <- y[partition]
    X_test <- X[-partition, ]
    y_test <- y[-partition]
    cvfit_lasso_train <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1, type.measure = "deviance", nfolds = 10, trace.it = TRUE)
    y_pred_lasso <- predict(cvfit_lasso_train, newx = X_test, s = lambda, type = "class")
    return(mean(y_pred_lasso == y_test))
}

accuracy_ridge <- function(X, y, p, lambda){
    partition <- createDataPartition(y, p = p, list = FALSE)
    X_train <- X[partition, ] 
    y_train <- y[partition]
    X_test <- X[-partition, ]
    y_test <- y[-partition]
    cvfit_ridge_train <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0, type.measure = "deviance", nfolds = 10, trace.it = TRUE)
    y_pred_ridge <- predict(cvfit_ridge_train, newx = X_test, s = lambda, type = "class")
    return(mean(y_pred_ridge == y_test))
}

# CHOOSE IMPUTATION METHOD
X_i <- X_3

p <- 0.7
lambda <- "lambda.1se"
acc_dist_lasso <- c()
acc_dist_ridge <- c()
for (i in 1:50){
    acc_dist_lasso <- c(acc_dist_lasso, accuracy_lasso(X_i, y, p, lambda))
    acc_dist_ridge <- c(acc_dist_ridge, accuracy_ridge(X_i, y, p, lambda))
    # acc_dist_ridge <- c(acc_dist_ridge, accuracy_ridge(X, y, p, lambda))
}

# PLOT DENSTIY OF ACCURACY FOR LASSO AND RIDGE
density_lasso <- density(acc_dist_lasso)
density_ridge <- density(acc_dist_ridge)
max_density <- max(max(density_lasso$y), max(density_ridge$y))
plot(density_lasso, col = "red", main = "Distribution of Accuracy for Lasso and Ridge", xlab = "Accuracy", ylab = "Density", ylim = c(0, max_density)) 
lines(density_ridge, col = "blue")

max_acc_lasso <- density_lasso$x[which.max(density_lasso$y)]
max_acc_ridge <- density_ridge$x[which.max(density_ridge$y)]
abline(v = max_acc_lasso, col = "red", lty = 2)
abline(v = max_acc_ridge, col = "blue", lty = 2)

 # ADD A LABEL TO SHOW THE MAXIMUM OF DENSITY
text(x = max_acc_lasso, y = max_density, labels = paste("Max Density: ", round(max_density, 2)), pos = 4, col = "red")
text(x = max_acc_ridge, y = max_density, labels = paste("Max Density: ", round(max_density, 2)), pos = 2, col = "blue")


p <- 0.7
acc_dist_1se <- c()
acc_dist_min <- c()
for (i in 1:50){
    acc_dist_1se <- c(acc_dist_1se, accuracy_lasso(X, y, p, "lambda.1se"))
    acc_dist_min <- c(acc_dist_min, accuracy_lasso(X, y, p, "lambda.min"))
    # acc_dist_ridge <- c(acc_dist_ridge, accuracy_ridge(X, y, p, lambda))
}

# PLOT DENSTIY OF ACCURACY FOR LASSO AND RIDGE
density_1se <- density(acc_dist_1se)
density_min <- density(acc_dist_min)
max_density <- max(max(density_1se$y), max(density_min$y))
plot(density_1se, col = "red", main = "Distribution of Accuracy for 1se and min", xlab = "Accuracy", ylab = "Density", ylim = c(0, max_density)) 
lines(density_min, col = "blue")

max_acc_1se <- density_1se$x[which.max(density_1se$y)]
max_acc_min <- density_min$x[which.max(density_min$y)]
abline(v = max_acc_1se, col = "red", lty = 2)
abline(v = max_acc_min, col = "blue", lty = 2)

legend("topright", legend = c("1se", "min"), fill = c("red", "blue"))

 # ADD A LABEL TO SHOW THE MAXIMUM OF DENSITY
text(x = max_acc_1se, y = max_density, labels = paste("Max Density: ", round(max_density, 2)), pos = 4, col = "red")
text(x = max_acc_min, y = max_density, labels = paste("Max Density: ", round(max_density, 2)), pos = 2, col = "blue")