# Load necessary packages
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("GEOquery")
install.packages("glmnet")
install.packages("multtest")  # For Bonferroni correction
install.packages("dplyr")      # For data manipulation

library(GEOquery)
library(glmnet)
library(multtest)
library(dplyr)

# Load the GEO dataset
gds1615 <- getGEO("GDS1615")

# Name the genes
Gene.Identifiers <- Table(gds1615)[,2]

# Convert the GDS object to an ExpressionSet without log2 transformation
eset <- GDS2eSet(gds1615, do.log2 = TRUE)

# Extract expression data
expr_data <- exprs(eset)

# Create the response vector
y <- (as.numeric(pData(eset)$disease.state) == 2)

# Initialize vector to store p-values
p_values <- numeric(nrow(expr_data))

# Loop through each gene
for (i in 1:nrow(expr_data)) {
  expr <- expr_data[i,]
  
  # Fit logistic regression model
  model <- glm(y ~ expr, family = binomial)
  
  # Perform Wald test for coefficient significance
  wald_test <- summary(model)$coefficients["expr", "Pr(>|z|)"]
  
  # Store p-value
  p_values[i] <- wald_test
}

# Apply Bonferroni correction to contral FWER(type I error), could do it with
# method = "fdr" which controls the false discovery rate
adjusted_p_values <- p.adjust(p_values, method = "bonferroni")

# Identify significantly associated genes
significant_genes <- which(adjusted_p_values < 0.05)

# Output significant genes
significant_genes_names <- Gene.Identifiers[significant_genes]
print(significant_genes_names)

# Bonferroni gives the fewest number of significant genes: 554! which is still 
# a lot more than what multivariate logistic regression gives: 40, fdr even gives 6429


## t test
# Create the response vector
disease_state <- as.numeric(pData(eset)$disease.state)

# Separate the data into disease and non-disease groups
disease_group <- expr_data[, disease_state == 2]
non_disease_group <- expr_data[, disease_state != 2]

# Initialize vector to store p-values
p_values <- numeric(nrow(expr_data))

# Loop through each gene
for (i in 1:nrow(expr_data)) {
  # Perform t-test
  t_test <- t.test(disease_group[i, ], non_disease_group[i, ])
  
  # Store p-value
  p_values[i] <- t_test$p.value
}

# Apply FDR correction
adjusted_p_values <- p.adjust(p_values, method = "fdr")

# Identify significantly associated genes
significant_genes_t <- which(adjusted_p_values < 0.05)

# Output significant genes
significant_genes_names_t <- Gene.Identifiers[significant_genes_t]
print(significant_genes_names_t)

# gives 7123 significant genes
# only 7 common genes for Bonferroni and 22 for t test (uni vs multi)


## correlation heatmap
# Extract the data of the significant genes
Heatmap_data <- t(expr_data[c(significant_genes),])
colnames(Heatmap_data) <- significant_genes_names

# Create the correlation matrix
cormat <- round(cor(Heatmap_data),2)
head(cormat)

# Reduce the redundant part of the matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

lower_tri <- get_lower_tri(cormat)
lower_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(lower_tri, na.rm = TRUE)

# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_raster(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Useful function to reorder the matrix according to the pattern
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
lower_tri <- get_lower_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(lower_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
 geom_raster(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
    name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
 coord_fixed()
# Print the heatmap
print(ggheatmap)
