# discriminant analysis
# 距离判别
data(iris)
cor(iris[, 1:4])

m.setosa <- colMeans(iris[1:50, 1:4])
m.setosa

m.versicolor <- colMeans(iris[51:100, 1:4])
m.versicolor

m.virginica <- colMeans(iris[101:150, 1:4])
m.virginica

library(biotools)
boxM(iris[, -5], iris[, 5])

# 计算协方差矩阵
v.setosa <- cov(iris[1:50, 1:4])
v.setosa

v.versicolor <- cov(iris[51:100, 1:4])
v.versicolor

v.virginica <- cov(iris[101:150, 1:4])
v.virginica

mahalanobis(iris[1, 1:4], m.setosa, v.setosa)
mahalanobis(iris[1, 1:4], m.versicolor, v.versicolor)
mahalanobis(iris[1, 1:4], m.virginica, v.virginica)

d.setosa <- mahalanobis(iris[, 1:4], m.setosa, v.setosa)
d.versicolor <- mahalanobis(iris[, 1:4], m.versicolor, v.versicolor)
d.virginica <- mahalanobis(iris[, 1:4], m.virginica, v.virginica)

d <- data.frame(d.setosa, d.versicolor, d.virginica)
head(d)

index <- apply(d, MARGIN = 1, FUN = which.min)
index

type <- factor(index, labels = c("setosa", "versicolor", "virginica"))
type

table(type, iris$Species)

which(type == "virginica" & iris$Species == "versicolor")

# K最邻近判别
set.seed(1234)
nrow(iris)
s <- sample(1:150, 100)
train <- iris[s, ]
test <- iris[-s, ]
cl <- train[, 5]

library(class)
iris.knn <- knn(train[, -5], test[, -5], cl)
iris.knn

confusion.matrix <- table(iris.knn, test[, 5])
confusion.matrix

accuracy <- sum(diag(confusion.matrix)) / nrow(test)
accuracy

i <- NULL # 事先定义一个i，就不会报错找不到对象i

accuracy <- vector(length = 20)
for (i in 1:20) {
  iris.knn <- knn(train[, -5], test[, -5], cl, k = i)
  confusion.matrix <- table(iris.knn, test[, 5])
  accuracy[i] <- sum(diag(confusion.matrix)) / nrow(test)
}
accuracy

plot(accuracy, type = "b", xlab = "k", ylab = "accuracy")

# Fisher判别
library(MASS)
iris.ld <- lda(Species ~ ., data = iris)
iris.ld

iris.pred <- predict(iris.ld)
iris.pred$class

table(iris.pred$class, iris$Species)

which(iris.pred$class == "versicolor" & iris$Species == "virginica")
which(iris.pred$class == "virginica" & iris$Species == "versicolor")
iris.pred$x

LD1 <- iris.pred$x[, 1]
LD2 <- iris.pred$x[, 2]
col <- as.numeric(iris$Species)
pch <- as.numeric(iris$Species)
plot(LD1, LD2, col = col, pch = pch, xlab = "LD1", ylab = "LD2")
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 1:3)
points(LD1[c(71, 84)], LD2[c(71, 84)], cex = 2)
points(LD1[134], LD2[134], cex = 2)

# Bayes判别
library(klaR)
iris.bayes <- NaiveBayes(Species ~ ., data = iris)
names(iris.bayes)
plot(iris.bayes)

iris.bayes.pred <- predict(iris.bayes, iris[, -5])
table(iris.bayes.pred$class, iris$Species)

