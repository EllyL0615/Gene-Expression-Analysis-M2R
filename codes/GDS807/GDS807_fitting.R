library(glmnet)
library(caret)

X_i <- X_3  # imputed dataset


# FIT THE MODEL - WHOLE DATASET
cvfit_lasso <- cv.glmnet(X_i, y, family = "binomial", alpha = 1, type.measure = "deviance", nfolds = 10, trace.it = TRUE)  # fits the model with lasso penalty
print(cvfit_lasso)
plot(cvfit_lasso)

cvfit_ridge <- cv.glmnet(X_i, y, family = "binomial", alpha = 0, type.measure = "deviance", nfolds = 10, trace.it = TRUE)  # fits the model with ridge penalty
print(cvfit_ridge)
plot(cvfit_ridge)
