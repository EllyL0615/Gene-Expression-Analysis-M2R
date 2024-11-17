fviz_contrib <- function(pca, idx)

{
  dd <- facto_summarize(pca, element = "var", result = "contrib", axes = 1)
  
  
  # expected Average contribution
  theo_contrib <- 100/length(dd$contrib)
  
  Groups <- idx %in% int_coef_idx
  
  df <- data.frame(name = factor(rownames(dd),levels = rownames(dd)),
                   contrib = dd$contrib,
                   Groups = Groups,
                   stringsAsFactors = TRUE)
  
  
  
  c <- ggpubr::ggbarplot(df, x = "name", y = "contrib", fill = "Groups", color = "Groups", palette = c("royalblue1", "orange"), 
                         sort.val = "desc",
                         main = "Contribution of Variables to PC1", xlab = FALSE, ylab ="Contributions (%)",
                         xtickslab.rt = 45, 
                         ggtheme = theme_minimal(),
                         sort.by.groups = FALSE
  )+
    geom_hline(yintercept=theo_contrib, linetype=2, color="red")
  
  return(c)
}