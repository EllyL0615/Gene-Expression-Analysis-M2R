X_0_1 <- X[, colMeans(is.na(X)) < 0.1]
dim(X_0_1) # (60, 7474
X_0_2 <- X[, colMeans(is.na(X)) < 0.2]
dim(X_0_2) # (60, 7474
X_0_3 <- X[, colMeans(is.na(X)) < 0.3]
dim(X_0_3) # (60, 7474
X_0_4 <- X[, colMeans(is.na(X)) < 0.4]
dim(X_0_4) # (60, 7474
X_0_5 <- X[, colMeans(is.na(X)) < 0.5]
dim(X_0_5) # (60, 7474
X_0_6 <- X[, colMeans(is.na(X)) < 0.6]
dim(X_0_6) # (60, 7474
X_0_7 <- X[, colMeans(is.na(X)) < 0.7]
dim(X_0_7) # (60, 7474

X_5_1 <- imputation_5(X_0_1)
X_5_2 <- imputation_5(X_0_2)
X_5_3 <- imputation_5(X_0_3)
X_5_4 <- imputation_5(X_0_4)
X_5_5 <- imputation_5(X_0_5)
X_5_6 <- imputation_5(X_0_6)
X_5_7 <- imputation_5(X_0_7)

list_5_1 <- testing(X_5_1, y)
list_5_2 <- testing(X_5_2, y)
list_5_3 <- testing(X_5_3, y)
list_5_4 <- testing(X_5_4, y)
list_5_5 <- testing(X_5_5, y)
list_5_6 <- testing(X_5_6, y)
list_5_7 <- testing(X_5_7, y)

density_5_1 <- density(list_5_1)
density_5_2 <- density(list_5_2)
density_5_3 <- density(list_5_3)
density_5_4 <- density(list_5_4)
density_5_5 <- density(list_5_5)
density_5_6 <- density(list_5_6)
density_5_7 <- density(list_5_7)

plot(density_5_1, col = "red", main = "Distribution of Accuracy for Five Imputations", xlab = "Accuracy", ylab = "Density", xlim = c(0, 1.2), ylim = c(0, max_density ))
lines(density_5_2, col = "blue")
lines(density_5_3, col = "darkgreen")
lines(density_5_4, col = "purple")
lines(density_5_5, col = "orange")
lines(density_5_6, col = "black")
lines(density_5_7, col = "brown")

legend("topright", legend = c("0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7"), fill = c("red", "blue", "darkgreen", "purple", "orange", "black", "brown"))

