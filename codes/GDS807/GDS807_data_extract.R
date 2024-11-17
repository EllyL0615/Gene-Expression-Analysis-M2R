library(GEOquery)

# EXTRACTS THE DATASET
gds807 <- getGEO('GDS807')  # extracts the dataset
eset <- GDS2eSet(gds807, do.log2 = TRUE)  # converts the dataset to an ExpressionSet
data <- pData(eset)  # converts ExpressionSet to a data frame
colnames(data)  #  {"sample"  "disease.state"  "description"}  
dim(data)  # (60, 3)
levels(data$disease.state)  # {"cancer recurred" "disease free""}

# APPLY THE DATASET ON REGRESSION MODEL
y <- (as.numeric(data$disease.state) == 2)  # set the level "normal" to be TRUE, otherwise will be FALSE
length(y) # (60)
X <- t(exprs(eset))  # constructs the design matrix and response
X[1:10, 1:10]  # small part of matrix for visualise
dim(X)  # (60, 22575)

