# Q型聚类
library(flexclust)
data("nutrient")
head(nutrient)

# 把大写行名转变为小写
row.names(nutrient) <- tolower(row.names(nutrient))
row.names(nutrient)

str(nutrient)

summary(nutrient)

nutrient.scaled <- scale(nutrient)
d.eu <- dist(nutrient.scaled, method = "euclidean") # 计算欧氏距离

hc1 <- hclust(d.eu, method = "average")

plot(hc1, hang = -1, labels = row.names(nutrient), 
     main = "Hierarchical clustering with average linkage", 
     xlab = "", ylab = "")

rect.hclust(hc1, k = 5) # 画出4个聚类的矩形框

library(NbClust)
# 选择最优的聚类数
NbClust(nutrient.scaled, 
        distance = "euclidean", 
        method = "average") 

cutree(hc1, k = 5) # 切割树状图，得到5个聚类

data.frame(group = sort(cutree(hc1, k = 5))) # 查看每个聚类的成员

# K型聚类
library(cluster)
library(fpc)
data("iris")
head(iris)
# 只选择数值型变量
iris.num <- iris[, -5]
# 计算距离矩阵
d.eu <- dist(iris.num, method = "euclidean")
# 计算K均值聚类
k.means <- kmeans(iris.num, centers = 3, nstart = 20)
# 计算K均值聚类的轮廓系数
silhouette.kmeans <- silhouette(k.means$cluster, d.eu)
plot(silhouette.kmeans, 
     main = "Silhouette plot for K-means clustering", 
     col = 1:3, 
     border = NA)

# R型聚类
library(cluster)
library(fpc)
data("iris")
head(iris)
# 只选择数值型变量
iris.num <- iris[, -5]
# 计算距离矩阵
d.eu <- dist(iris.num, method = "euclidean")
# 计算R均值聚类
pam.kmeans <- pam(iris.num, k = 3)
# 计算R均值聚类的轮廓系数
silhouette.pam <- silhouette(pam.kmeans$clustering, d.eu)
plot(silhouette.pam, 
     main = "Silhouette plot for PAM clustering", 
     col = 1:3, 
     border = NA)

