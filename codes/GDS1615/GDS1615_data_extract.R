library(GEOquery)

# EXTRACTS THE DATASET
gds1615 <- getGEO('GDS1615')  # extracts the dataset
eset <- GDS2eSet(gds1615, do.log2 = TRUE)  # converts the dataset to an ExpressionSet
data <- pData(eset)  # converts ExpressionSet to a data frame
colnames(data)  #  {"sample"  "disease.state"  "description"}
dim(data)  # (127, 3)
levels(data$disease.state)  # { "Crohn's disease"  "normal"  "ulcerative colitis"}

# APPLY THE DATASET ON REGRESSION MODEL
y <- (as.numeric(data$disease.state) == 2)  # set the level "normal" to be TRUE, otherwise will be FALSE
length(y) # (127)
X <- t(exprs(eset))  # constructs the design matrix and response
X[1:10, 1:10]  # small part of matrix for visualise
dim(X)  # (127, 22283)

