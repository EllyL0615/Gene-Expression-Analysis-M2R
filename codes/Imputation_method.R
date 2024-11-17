library(glmnet)

# NA imputation 1: replace NA with 0
imputation_1 <- function(X_func) {
    X_func[is.na(X_func)] <- 0
    return(X_func)
}

# NA imputation 2: replace NA with the mean of the column
imputation_2 <- function(X_func) {
    for (i in 1:ncol(X_func)) {
        col_mean <- mean(X_func[, i], na.rm = TRUE)
        X_func[is.na(X_func[, i]), i] <- col_mean
    }
    return(X_func)
}
 # note: stack overflow when using makeX_func() function


# NA imputation 3: replace NA by mean from the column with same y value
imputation_3 <- function(X_func) {
    one_y_indeX_func <- y == 1 # get the indeX_func of y = 1
    zero_y_indeX_func <- y == 0 # get the indeX_func of y = 0
    for (i in 1:ncol(X_func)) {
        col_mean_1 <- mean(X_func[one_y_indeX_func, i], na.rm = TRUE)
        col_mean_2 <- mean(X_func[zero_y_indeX_func, i], na.rm = TRUE)
        X_func[is.na(X_func[, i]) & y == 1, i] <- col_mean_1
        X_func[is.na(X_func[, i]) & y == 0, i] <- col_mean_2
    }
    return (X_func)
}

# NA imputation 4: replace NA by sampling from the column with same y value
imputation_4 <- function(X_func) {
    for (i in 1:ncol(X_func)) {
        X_func[is.na(X_func[, i]) & y == 1, i] <- sample(X_func[y == 1 & !is.na(X_func[, i]), i], sum(is.na(X_func[, i]) & y == 1), replace = TRUE)
        X_func[is.na(X_func[, i]) & y == 0, i] <- sample(X_func[y == 0 & !is.na(X_func[, i]), i], sum(is.na(X_func[, i]) & y == 0), replace = TRUE)
    }
    return (X_func)
}

# NA imputation 5: replace NA by using normal distribution
imputation_5 <- function(X_func) {
    for (i in 1:ncol(X_func)) {
        X_func[is.na(X_func[, i]) & y == 1, i] <- rnorm(sum(is.na(X_func[, i]) & y == 1), mean = mean(X_func[y == 1 & !is.na(X_func[, i]), i], na.rm = TRUE), sd = sd(X_func[y == 1 & !is.na(X_func[, i]), i], na.rm = TRUE))
        X_func[is.na(X_func[, i]) & y == 0, i] <- rnorm(sum(is.na(X_func[, i]) & y == 0), mean = mean(X_func[y == 0 & !is.na(X_func[, i]), i], na.rm = TRUE), sd = sd(X_func[y == 0 & !is.na(X_func[, i]), i], na.rm = TRUE))
    }
    return (X_func)
}


