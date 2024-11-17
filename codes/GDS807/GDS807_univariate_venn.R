library(VennDiagram)
library(ggvenn)
library(RColorBrewer)

# LOADING GENE EXPRESSION DATA
Gene.Identifiers <- Table(gds807)[,2]  # Get the gene identifiers
length(Gene.Identifiers)  # (22575)

new_Gene.Identifiers <- Gene.Identifiers[colMeans(is.na(X)) < 0.5]
length(new_Gene.Identifiers)  # (11855）
# CHOOSING THE IMPUTATION METHOD
X_i <- X_3
dim(X_i)  # (60, 11855

# MULTI-VARIATE TESTING WITH LASSO
cvfit_lasso <- cv.glmnet(X_i, y, family = "binomial", alpha = 1, nfolds = 10)
coef_lasso <- coef(cvfit_lasso, s = "lambda.1se")
dim(coef_lasso)  # (11856, 1)
lasso_coef_set <- new_Gene.Identifiers[coef_lasso[-1] != 0]
print(paste("The number of nonzero coefficients is", length(lasso_coef_set)))

# UNIVARIATE TESTING
alpha <- 0.05
bonf_correction <- alpha / ncol(X_i)

uni_coef_set <- list()
for (i in 1:ncol(X_i)) {
    uni_model <- glm(y ~ X_i[, i], family = "binomial")
    #CHECK IF THE COEFFICIENT IS SIGNIFICANT
    if (summary(uni_model)$coefficients[2, 4] < bonf_correction) {
        uni_coef_set[[length(uni_coef_set) + 1]] <- Gene.Identifiers[i]
    }
}
print(paste("The number of significant coefficients is", length(uni_coef_set)))

# CREATE A VENN DIAGRAM
x <- list(
  "Lasso" = lasso_coef_set,
  "Univariate" = uni_coef_set
)
ggvenn(x, show_elements = F, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3))


# 从数据框 X 中选择 lasso_coef_set 对应的列
selected_X <- X_i[, coef_lasso[-1] != 0]
# 计算这些列的相关性
cor_matrix <- cor(selected_X)
# 创建相关性热图
heatmap(cor_matrix)
title("Correlation Heatmap for Lasso Coefficients")


library(pheatmap)
pheatmap(cor_matrix, show_rownames = TRUE , show_colnames = TRUE, fontsize_row=10, fontsize_col = 10 )

