# TEST THE IMPUTATION METHODS

# NA imputation 1: replace NA with 0
print("NA imputation 1: replace NA with 0")
X_1 <- imputation_1(X_0)
list_1 <- testing(X_1, y)

# NA imputation 2: replace NA with the mean of the column
print("NA imputation 2: replace NA with the mean of the column")
X_2 <- imputation_2(X_0)
list_2 <- testing(X_2, y)

# NA imputation 3: replace NA by mean from the column with same y value
print("NA imputation 3: replace NA by mean from the column with same y value")
X_3 <- imputation_3(X_0)
list_3 <- testing(X_3, y)

# NA imputation 4: replace NA by sampling from the column with same y value
print("NA imputation 4: replace NA by sampling from the column with same y value")
X_4 <- imputation_4(X_0)
list_4 <- testing(X_4, y)

# NA imputation 5: replace NA by using normal distribution
print("NA imputation 5: replace NA by using normal distribution")
X_5 <- imputation_5(X_0)
list_5 <- testing(X_5, y)


# BOXPLOT
boxplot(list_1, list_2, list_3, list_4, list_5, names = c("Imputation 1", "Imputation 2", "Imputation 3", "Imputation 4", "Imputation 5"), main = "Accuracy of Different Imputation Methods", ylab = "Accuracy", col = "lightblue", border = "brown")

# CALCULATE FIVE DENSITY 
density_1 <- density(list_1)
density_2 <- density(list_2)
density_3 <- density(list_3)
density_4 <- density(list_4)
density_5 <- density(list_5)

# FIND MAX DENSITY
max_density <- max(c(density_1$y, density_2$y, density_3$y, density_4$y, density_5$y))

# PLOT 1
plot(density_1, col = "red", main = "Distribution of Accuracy for Five Imputations", xlab = "Accuracy", ylab = "Density", xlim = c(0, 1.2), ylim = c(0, max_density ))

# PLOT OTHERS
lines(density_2, col = "blue")
lines(density_3, col = "darkgreen")
lines(density_4, col = "purple")
lines(density_5, col = "orange")

abline(v = density_1$x[which.max(density_1$y)], col = "red", lty = 2)
abline(v = density_2$x[which.max(density_2$y)], col = "blue", lty = 2)
abline(v = density_3$x[which.max(density_3$y)], col = "darkgreen", lty = 2)
abline(v = density_4$x[which.max(density_4$y)], col = "purple", lty = 2)
abline(v = density_5$x[which.max(density_5$y)], col = "orange", lty = 2)

# CALCULATE MAXIMUM AND LABEL IT
max_density_1 <- max(density_1$y)
max_density_2 <- max(density_2$y)
max_density_3 <- max(density_3$y)
max_density_4 <- max(density_4$y)
max_density_5 <- max(density_5$y)
text(x = density_1$x[which.max(density_1$y)], y = max_density_1, labels = paste("Max Density 1: ", round(density_1$x[which.max(density_1$y)], 2)), pos = 4, col = "red")
text(x = density_2$x[which.max(density_2$y)], y = max_density_2, labels = paste("Max Density 2: ", round(density_2$x[which.max(density_2$y)], 2)), pos = 4, col = "blue")
text(x = density_3$x[which.max(density_3$y)], y = max_density_3, labels = paste("Max Density 3: ", round(density_3$x[which.max(density_3$y)], 2)), pos = 4, col = "darkgreen")
text(x = density_4$x[which.max(density_4$y)], y = max_density_4, labels = paste("Max Density 4: ", round(density_4$x[which.max(density_4$y)], 2)), pos = 4, col = "purple")
text(x = density_5$x[which.max(density_5$y)], y = max_density_5, labels = paste("Max Density 5: ", round(density_5$x[which.max(density_5$y)], 2)), pos = 4, col = "orange")

# ADD LEGEND
legend("topright", legend = c("Imputation 1", "Imputation 2", "Imputation 3", "Imputation 4", "Imputation 5"), fill = c("red", "blue", "green", "purple", "orange"))

