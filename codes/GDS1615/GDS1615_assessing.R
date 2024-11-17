library(glmnet)

# FIT THE MODEL
cvfit_lasso_train <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1, type.measure = "deviance", nfolds = 5, trace.it = TRUE)  # fits the model with lasso penalty
print(cvfit_lasso_train)
plot(cvfit_lasso_train)

cvfit_ridge_train <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0, type.measure = "deviance", nfolds = 5, trace.it = TRUE)  # fits the model with ridge penalty
print(cvfit_ridge_train)
plot(cvfit_ridge_train)


# PREDICT THE MODEL
y_pred_lasso <- predict(cvfit_lasso_train, newx = X_test, s = "lambda.min", type = "class")  # predicts the model with lasso penalty
y_pred_ridge <- predict(cvfit_ridge_train, newx = X_test, s = "lambda.min", type = "class")  # predicts the model with ridge penalty

# EVALUATE THE MODEL: ASSESSES THE MODEL
assess.glmnet(cvfit_lasso_train, newx = X_test, newy = y_test, s="lambda.min")
assess.glmnet(cvfit_ridge_train, newx = X_test, newy = y_test, s="lambda.min")

# EVALUATE THE MODEL: ROC CURVE
cvfit_lasso_train <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1, type.measure = "auc", nfolds = 10, keep=TRUE, trace.it = TRUE)
cvfit_ridge_train <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0, type.measure = "auc", nfolds = 10, keep=TRUE, trace.it = TRUE)
rocs <- roc.glmnet(cvfit_lasso_train$fit.preval, newy = y_train)

best <- cvfit_lasso_train$index["min",]
plot(rocs[[best]], type = "l")
invisible(sapply(rocs, lines, col="grey"))
lines(rocs[[best]], lwd = 2,col = "red")

best <- cvfit_ridge_train$index["min",]
plot(rocs[[best]], type = "l")
invisible(sapply(rocs, lines, col="grey"))
lines(rocs[[best]], lwd = 2,col = "red")


# EVALUATE THE MODEL: CONFUSION MATRIX
cf_matrix_lasso <- confusion.glmnet(cvfit_lasso_train, newx = X_test, newy = y_test)  # evaluates the model
cf_matrix_ridge <- confusion.glmnet(cvfit_ridge_train, newx = X_test, newy = y_test)  # evaluates the model
print(cf_matrix_lasso)
print(cf_matrix_ridge)

# TESTING BETWEEN LAMBDA.MIN AND LAMBDA.1SE
print(cvfit_lasso_train$lambda.min)
assess.glmnet(cvfit_lasso_train, newx = X_test, newy = y_test, s="lambda.min")
print(cvfit_lasso_train$lambda.1se)
assess.glmnet(cvfit_lasso_train, newx = X_test, newy = y_test, s="lambda.1se")
