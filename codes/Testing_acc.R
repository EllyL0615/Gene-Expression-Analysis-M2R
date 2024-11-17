library(caret)

# CREATE A FUNCTION FOR TESTING:
testing <- function(X_func, y_func, repetitions=20) {
    acc_list <- c()
    for (i in 1:repetitions) {
        partition <- createDataPartition(y_func, p = 0.7, list = FALSE)  # creates a partition for training and testing
        X_train <- X_func[partition, ] 
        y_train <- y_func[partition]
        X_test <- X_func[-partition, ]
        y_test <- y_func[-partition]

        cvfit_lasso_train <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1, type.measure = "deviance", nfolds = 10, trace.it = TRUE)
        y_pred_lasso <- predict(cvfit_lasso_train, newx = X_test, s = "lambda.1se", type = "class")

        acc_list <- c(acc_list, mean(y_pred_lasso == y_test))
    }
    return(acc_list)
}
