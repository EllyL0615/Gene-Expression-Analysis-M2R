library(ggbiplot)
library(factoextra)
library(pls)

# PCR FOR LASSO
# extract matrix of significant genes(X) according to LASSO
lasso_coef_idx <- vector()
for (i in 1:ncol(X_i)) {
  if (coef_lasso[-1][i] != 0) {
    lasso_coef_idx[length(lasso_coef_idx) + 1] <- i
  }
}
X_lasso <- X_i[, lasso_coef_idx]

# PCA & visualisation
pca_lasso <- prcomp(X_lasso, scale. = TRUE)  # PCA
ggscreeplot(pca_lasso)  # scree plot
ggbiplot(pca_lasso,  # 2D plot
         obs.scale = 1,
         groups = y,
         ellipse = TRUE,
         var.axes = F)
fviz_contrib(pca_lasso, choice="var", axes=1)

# PCR & prediction
X_lasso_train <- X_lasso[partition, ]  # split to test and training sets
X_lasso_test <- X_lasso[-partition, ]
pcr_lasso <- pcr(y_train ~ ., data = as.data.frame(X_lasso_train), validation = "CV")  # PCR
validationplot(pcr_lasso, val.type ="RMSEP", main="")
pcr_lasso_pred <- predict(pcr_lasso, as.data.frame(X_lasso_test), ncomp = 1)  # PCR prediction
plot(pcr_lasso_pred, y_test,  # PCR prediction plot
     xlab="Predicted", ylab="Observed")


