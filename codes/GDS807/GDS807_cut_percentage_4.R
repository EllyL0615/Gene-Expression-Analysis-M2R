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

X_4_1 <- imputation_4(X_0_1)
X_4_2 <- imputation_4(X_0_2)
X_4_3 <- imputation_4(X_0_3)
X_4_4 <- imputation_4(X_0_4)
X_4_5 <- imputation_4(X_0_5)


list_4_1 <- testing(X_4_1, y)
list_4_2 <- testing(X_4_2, y)
list_4_3 <- testing(X_4_3, y)
list_4_4 <- testing(X_4_4, y)
list_4_5 <- testing(X_4_5, y)


density_4_1 <- density(list_4_1)
density_4_2 <- density(list_4_2)
density_4_3 <- density(list_4_3)
density_4_4 <- density(list_4_4)
density_4_5 <- density(list_4_5)

plot(density_4_1, col = "red", main = "Distribution of Accuracy for Five Imputations", xlab = "Accuracy", ylab = "Density", xlim = c(0, 1.2), ylim = c(0, max_density ))
line(density_4_2, col = "blue")
line(density_4_3, col = "darkgreen")
line(density_4_4, col = "purple")
line(density_4_5, col = "orange")

legend("topright", legend = c("0.1", "0.2", "0.3", "0.4", "0.5"), fill = c("red", "blue", "darkgreen", "purple", "orange"))



