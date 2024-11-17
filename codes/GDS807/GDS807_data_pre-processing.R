library(caret)

# DATA PREPROCESSING
num_zero <- sum(X == 0, na.rm = TRUE)  # 2
num_na <- sum(is.na(X))  # 641660

# REMOVE THE COLUMNS WITH more than 20% NA
X_0 <- X[, colMeans(is.na(X)) < 0.5]
dim(X_0) # (60, 7474)

# 计算每行的缺失值数量
missing_values_per_row <- colSums(is.na(X))

# 创建直方图
hist(missing_values_per_row, main = "Distribution of Missing Values", xlab = "Number of Missing Values", ylab = "Frequency")


# TRY DIFFERENT IMPUTATION METHODS
X_1 <- imputation_1(X_0)
X_2 <- imputation_2(X_0)
X_3 <- imputation_3(X_0)
X_4 <- imputation_4(X_0)
X_5 <- imputation_5(X_0)

# CHOOSING THE IMPUTATION METHOD
X_i <- X_3

# SPLIT THE DATA INTO TRAINING AND TESTING DATASET
partition <- createDataPartition(y, p = 0.7, list = FALSE)  # creates a partition for training and testing
X_train <- X[partition, ] 
dim(X_train)  # (102, 22283)
y_train <- y[partition]
length(y_train)  # (102)
X_test <- X[-partition, ]
dim(X_test)  # (25, 22283)
y_test <- y[-partition]
length(y_test)  # (25)
