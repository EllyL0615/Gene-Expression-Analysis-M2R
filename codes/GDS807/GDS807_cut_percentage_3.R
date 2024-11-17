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


X_3_1 <- imputation_3(X_0_1)
X_3_2 <- imputation_3(X_0_2)
X_3_3 <- imputation_3(X_0_3)
X_3_4 <- imputation_3(X_0_4)
X_3_5 <- imputation_3(X_0_5)


list_3_1 <- testing(X_3_1, y)
list_3_2 <- testing(X_3_2, y)
list_3_3 <- testing(X_3_3, y)
list_3_4 <- testing(X_3_4, y)
list_3_5 <- testing(X_3_5, y)

density_3_1 <- density(list_3_1)
density_3_2 <- density(list_3_2)
density_3_3 <- density(list_3_3)
density_3_4 <- density(list_3_4)
density_3_5 <- density(list_3_5)

boxplot(list_3_1, list_3_2, list_3_3, list_3_4, list_3_5,names = c("0.1", "0.2", "0.3", "0.4", "0.5"), main = "Accuracy of Different Cut-off Percentages", ylab = "Accuracy", col = "lightblue", border = "brown")


max_density <- max(c(density_3_1$y, density_3_2$y, density_3_3$y, density_3_4$y, density_3_5$y))

plot(density_3_1, col = "red", main = "Distribution of Accuracy for Five Imputations", xlab = "Accuracy", ylab = "Density", xlim = c(0, 1.2), ylim = c(0, max_density ))
lines(density_3_2, col = "blue")
lines(density_3_3, col = "darkgreen")
lines(density_3_4, col = "purple")
lines(density_3_5, col = "orange")

legend("topright", legend = c("0.1", "0.2", "0.3", "0.4", "0.5"), fill = c("red", "blue", "darkgreen", "purple", "orange"))
