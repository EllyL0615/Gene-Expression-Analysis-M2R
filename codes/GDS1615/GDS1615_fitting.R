library(glmnet)
library(caret)

# FIT THE MODEL - WHOLE DATASET
cvfit_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1, type.measure = "deviance", nfolds = 10, trace.it = TRUE)  # lasso penalty
print(cvfit_lasso)
plot(cvfit_lasso)

cvfit_ridge <- cv.glmnet(X, y, family = "binomial", alpha = 0, type.measure = "deviance", nfolds = 10, trace.it = TRUE)  # ridge penalty
print(cvfit_ridge)
plot(cvfit_ridge)
