# 主成分分析
cor.matrix <- Harman23.cor$cov
eigen(cor.matrix)
# 计算特征值和特征向量
eigen(cor.matrix)$values
round(eigen(cor.matrix)$vectors, 3)

# 上述计算过程也可以用
PCA <- princomp(covmat = cor.matrix)
summary(PCA, loadings = T)

screeplot(PCA, type = "lines", main = "Screeplot of PCA")
abline(h = 1)

load <- loadings(PCA)
plot(load[, 1:2], xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5), 
     xlab = "PC1", ylab = "PC2")
text(load[, 1], load[, 2], adj = c(-0.3, 0))
abline(h = 0, v = 0, lty = 2)

# 因子分析
library(psych)
fa.parallel(Harman23.cor$cov, n.iter = 100, fa = "both", 
            n.obs = 305, main = "Parallel Analysis Scree Plots")

fa1 <- fa(Harman23.cor$cov, nfactors = 3, rotate = "varimax",
          fm = "minres")          
fa1
fa1$loadings

fa2 <- fa(Harman23.cor$cov, nfactors = 3, rotate = "oblimin",
          fm = "minres")
fa2
fa2$loadings
fa.diagram(fa2, simple = TRUE, cut = 0.3)

fa3 <- fa(Harman23.cor$cov, nfactors = 3, rotate = "promax",
          fm = "minres")
fa3
fa3$loadings
fa.diagram(fa3, simple = TRUE, cut = 0.3)

fa4 <- fa(Harman23.cor$cov, nfactors = 3, rotate = "varimax",
          fm = "ml")
fa4
fa4$loadings
fa.diagram(fa4, simple = TRUE, cut = 0.3)

fa5 <- fa(Harman23.cor$cov, nfactors = 3, rotate = "oblimin",
          fm = "ml")
fa5
fa5$loadings
fa.diagram(fa5, simple = TRUE, cut = 0.3)

