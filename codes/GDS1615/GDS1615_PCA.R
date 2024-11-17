library(ggbiplot)
library(pls)
library(factoextra)
# ggbiplot()
# fviz_contrib()




# PCR FOR LASSO
# extract matrix of significant genes(X) according to LASSO
lasso_coef_idx <- vector()
for (i in 1:ncol(X)) {
  if (coef_lasso[-1][i] != 0) {
    lasso_coef_idx[length(lasso_coef_idx) + 1] <- i
  }
}
X_lasso <- X[, lasso_coef_idx]

# PCA & visualisation
pca_lasso <- prcomp(X_lasso, scale. = TRUE)  # PCA
ggscreeplot(pca_lasso)  # scree plot
ggbiplot(pca_lasso,  # 2D plot
         obs.scale = 1,
         groups = y,
         ellipse = T,
         var.axes = F)
fviz_contrib(pca_lasso, lasso_coef_idx)


# PCR & prediction
X_lasso_train <- X_lasso[partition, ]  # split to test and training sets
X_lasso_test <- X_lasso[-partition, ]
pcr_lasso <- pcr(y_train ~ ., data = as.data.frame(X_lasso_train), validation = "CV")  # PCR
validationplot(pcr_lasso, val.type = "RMSEP", main="")
pcr_lasso_pred <- predict(pcr_lasso, as.data.frame(X_lasso_test), ncomp = 1)  # PCR prediction
plot(pcr_lasso_pred, y_test,  # PCR prediction plot
     xlab="Predicted (LASSO)", ylab="Observed")



# PCR FOR UNIVARIATE
# extract matrix of significant genes(X) according to Univariate
uni_coef_idx <- vector()
for (i in 1:ncol(X)) {
  uni_model <- glm(y ~ X[, i], family = "binomial")
  if (summary(uni_model)$coefficients[2, 4] < bonf_correction) {
    uni_coef_idx[length(uni_coef_idx) + 1] <- i
  }
}
X_uni <- X[, uni_coef_idx]

# PCA & visualisation
pca_uni <- prcomp(X_uni, scale. = TRUE)  # PCA
ggscreeplot(pca_uni)  # scree plot
ggbiplot(pca_uni,  # 2D plot
         obs.scale = 1,
         groups = y,
         ellipse = TRUE,
         var.axes = F)
fviz_contrib(pca_uni, uni_coef_idx)

# PCR & prediction
X_uni_train <- X_uni[partition, ]  # split to test and training sets
X_uni_test <- X_uni[-partition, ]
pcr_uni <- pcr(y_train ~ ., data = as.data.frame(X_uni_train), validation = "CV")  # PCR
validationplot(pcr_uni, val.type = "RMSEP", main="")
pcr_uni_pred <- predict(pcr_uni, as.data.frame(X_uni_test), ncomp = 1)  # PCR prediction
plot(pcr_uni_pred, y_test,  # PCR prediction plot
     xlab="Predicted (Univariate)", ylab="Observed")



# PCR FOR INTERSECTION OF LASSO AND UNIVARIATE
# extract matrix of significant genes(X) according to both LASSO and Univariate
int_coef_idx <- intersect(lasso_coef_idx, uni_coef_idx)
X_int <- X[, int_coef_idx]

# PCA & visualisation
pca_int <- prcomp(X_int, scale. = TRUE)  # PCA
ggscreeplot(pca_int)  # scree plot
ggbiplot(pca_int,  # 2D plot
         obs.scale = 1,
         groups = y,
         ellipse = T,
         var.axes = F)
fviz_contrib(pca_int, int_coef_idx)

# PCR & prediction
X_int_train <- X_int[partition, ]  # split to test and training sets
X_int_test <- X_int[-partition, ]
pcr_int <- pcr(y_train ~ ., data = as.data.frame(X_int_train), validation = "CV")  # PCR
pcr_int_pred <- predict(pcr_int, as.data.frame(X_int_test), ncomp = 1)  # PCR prediction
plot(pcr_int_pred, y_test,  # PCR prediction plot
     xlab="Predicted (LASSO and Univariate)", ylab="Observed")



