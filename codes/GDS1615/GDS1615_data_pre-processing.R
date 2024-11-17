library(caret)

# DATA PREPROCESSING
num_zero <- sum(X == 0, na.rm = TRUE)
num_na <- sum(is.na(X))
print(num_zero)  # 1
print(num_na)  # 0

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

