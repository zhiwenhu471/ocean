rm(list = ls())

# 缺失值的处理
height <- c(12, 54, 56, NA, 79)
height
is.na(height) # 识别是否有缺失值
table(is.na(height)) # 识别缺失值并输出数量

mean(height, na.rm = T) # 移除NA才能进行计算
# 否则输出为NA
mean(height)

summary(height) # summary()能忽略对象中的缺失值

# missForest包中的prodNA()可以随机生成缺失值
library(missForest)
data(iris)
set.seed(1234) # 设置生成随机数的种子
iris.miss <- prodNA(iris) # 默认生成数据10%的缺失值
summary(iris.miss)

library(VIM) # vim包可视化缺失值
aggr(iris.miss, prop = F, numbers = T, cex.axis = 0.7)

# NA处理方法
# 删除
iris.sub <- na.omit(iris.miss)
nrow(iris.sub)
nrow(iris.miss)

# 使用特定数值替换
Sepal.Length.Mean <- mean(iris.miss$Sepal.Length, na.rm = T) # 先计算平均值
Sepal.Length.Mean

iris.miss1 <- iris.miss # 将数据另存，便于日后使用
iris.miss1$Sepal.Length[is.na(iris.miss1$Sepal.Length)] <- Sepal.Length.Mean
# 为计算差异，可以计算偏差
summary((iris$Sepal.Length - iris.miss1$Sepal.Length)/iris$Sepal.Length)

# 多重插补
library(mice)
imputed.data <- mice(iris.miss, seed = 1234)
View(imputed.data)
summary(imputed.data)

# 查看目标变量的查补值
imputed.data$imp$Sepal.Length

complete.data <- complete(imputed.data, 3)

rm(complte.data) # 删除某个变量

complete.data

summary((iris$Sepal.Length - complete.data$Sepal.Length)/iris$Sepal.Length)

table(iris$Species, complete.data$Species)

rm(list = ls(all = T))
ls()

# 模拟一个大型数据集
bigdata <- as.data.frame(matrix(rnorm(50000 * 200), ncol = 200))
varnames <- NULL # 准备命名
for (i in letters[1:20]) {
  for (j in 1:10) {
    varnames <- c(varnames, paste(i, j, sep = "_"))
  }
}
names(bigdata) <- varnames
names(bigdata)

library(dplyr)
library(tidyselect)

# 使用select()系列函数选择或剔除变量
subdata1 <- select(bigdata, starts_with("a")) # “a”开头
names(subdata1)

subdata2 <- select(bigdata, ends_with("2")) # “2”结尾
names(subdata2)

subdata3 <- select_at(bigdata, vars(starts_with("a"), # “a”和“b”开头
                                    starts_with("b")))
names(subdata3)

subdata4 <- select_at(bigdata, vars(contains("1"))) # 含有“1”
names(subdata4)

subdata5 <- select_at(subdata3, vars(-starts_with("a"))) # 删除“a”开头的
names(subdata5)

