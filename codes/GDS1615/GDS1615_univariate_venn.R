library(VennDiagram)
library(ggvenn)
library(RColorBrewer)
library(glmnet)

# LOADING GENE EXPRESSION DATA
Gene.Identifiers <- Table(gds1615)[,2]  # Get the gene identifiers
length(Gene.Identifiers)  # (22283)

# MULTI-VARIATE TESTING WITH LASSO
cvfit_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1, nfolds = 10)
coef_lasso <- coef(cvfit_lasso, s = "lambda.1se")
dim(coef_lasso)  # (22284, 1)
lasso_coef_set <- Gene.Identifiers[coef_lasso[-1] != 0]
print(paste("The number of nonzero coefficients is", length(lasso_coef_set)))

# UNIVARIATE TESTING
alpha <- 0.05
bonf_correction <- alpha / ncol(X)


uni_coef_set <- list()
uni_index <- c()
for (i in 1:ncol(X)) {
    uni_model <- glm(y ~ X[, i], family = "binomial")
    #CHECK IF THE COEFFICIENT IS SIGNIFICANT
    if (summary(uni_model)$coefficients[2, 4] < bonf_correction) {
        uni_coef_set[[length(uni_coef_set) + 1]] <- Gene.Identifiers[i]
        uni_index <- c(uni_index, i)
    }
}
print(paste("The number of significant coefficients is", length(uni_coef_set)))

# CREATE A VENN DIAGRAM
x <- list(
  "Lasso_1615" = lasso_coef_set,
  "Univariate_1615" = uni_coef_set
)
ggvenn(x, show_elements = F, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 5)


# 从数据框 X 中选择 lasso_coef_set 对应的列
selected_X <- X[, coef_lasso[-1] != 0]
# 计算这些列的相关性
cor_matrix <- cor(selected_X)
# 创建相关性热图
heatmap(cor_matrix)
title("Correlation Heatmap for Lasso Coefficients")

selected2_X <- X[, uni_index]
cor_matrix2 <- cor(selected2_X)
heatmap(cor_matrix2)
title("Correlation Heatmap for Univariate Regression Coefficients")


library(pheatmap)
pheatmap(cor_matrix2, show_rownames = FALSE , show_colnames = FALSE, fontsize_row=10, fontsize_col = 10 )
