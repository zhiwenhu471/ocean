getwd()

# 生成100个随机数，设置特定平均值，标准差
x <- rnorm(100, mean = 5, sd = 0.1)
summary(x)

# 在发行出版物里引用所使用的包
citation(package = "ggplot2")

# 搜索
help.search('ggplot2')
help()
??tidyverse

library(ggplot2)
.packages(all.available = T) # 查看所有已下载的包

a <- data.frame(
  name = c("Andy", "John", "Mary"), 
  age = c(12, 34, 45), 
  grade = c("A", "B", "C")
)
print(a)

# 查看数据类型
class(a)
class(a$name)
class(a$age)
class(a$grade)

b <- c(0:100)
sample(b, 3, replace = T) # 有放回地从b中抽取3项

# 查看当前日期
Sys.Date()

# 查看当前时间
Sys.time()

search() # 查看哪些包被加载到内存中

example("mean") # 运行示例代码

x <- c(0:10, 50) # 生成序列 0:10，并且添加 50后组合
print(x)

log10(10)

pi # 输出 pi
exp(1) # 输出 e
sqrt(25) # 求平方根
c <- 8 ^ (1/3) # 开立方根
c

round(pi, digits = 2) # 指定小数位数
round(pi, 2)

ls() # 查看当前工作空间的所有对象

d <- seq(from = 2, to = 10, by = 2)
d

f <- rep("a", times = 4)
f

# 数据因子转换
sex <- c(1, 1, 2, 1, 2, 2)
sex_f <- factor(sex, levels = c(1, 2), labels = c("male", "female"))
sex_f
levels(sex_f) # 查看 levels

# 有序因子
status <- c(1, 2, 3, 3, 1, 2, 2)
status_f <- factor(
  status,
  levels = c(1, 2, 3),
  labels = c("poor", "improved", "excellent"),
  ordered = T
)
status_f

# 创建矩阵
M <- matrix(1:6, nrow = 2) # 默认按列排列
M
M <- matrix(1:6, nrow = 2, byrow = T)
M

# 矩阵乘法要求第一个矩阵的列数等于第二个矩阵的行数
mat1 <- matrix(1:6, nrow = 3)
mat1

mat2 <- matrix(5:10,nrow = 2)
mat2

dim(mat1) # 得到矩阵的维数，即行列
dim(mat2)

mat1 %*% mat2 # 矩阵乘法

# 矩阵的转置运算，行列互换
t(mat1)
t(mat2)

# 求方阵的行列式
mat3 <- matrix(1:4, nrow = 2)
det(mat3)
mat3

# 求逆矩阵
solve(mat3)

mat1
rowSums(mat1) # 按行求和
colSums(mat1) # 按列求和
rowMeans(mat1) # 按行求平均值
colMeans(mat1) # 按列求平均值

# 数组
A <- 1:24
dim(A) <- c(3, 4, 2) # 3行，4列，2层
A

# 通过array()创建数组
dim1 <- c("A1", "A2", "A3")
dim2 <- c("B1", "B2", "B3", "B4")
dim3 <- c("C1", "C2")
array(1:24, dim = c(3, 4, 2), dimnames = list(dim1, dim2, dim3))

# 列表
list1 <- list(a = 1, b = 1:5, c = c("red", "green", "blue"))
list1
list1$c

# 数据框
ID <- 1:5
sex <- c("male", "female", "male", "male", "female")
age <- c(12, 45, 34, 54, 78)
pain <- c(1, 2, 3, 2, 3)
pain_f <- factor(pain,
                 levels = c(1, 2, 3),
                 labels = c("mild", "medium", "severe"))
pain_f
patients <- data.frame(ID, sex, age, pain_f)
patients
patients$age
mean(patients$age)

data(iris)
View(iris)

library(MASS)
View(bacteria)

# 用 rio 包导入导出数据

library(epiDisplay)
data("Familydata")

ls()
Familydata # 直接查看
head(Familydata) # 显示前6行
tail(Familydata) # 显示后6行

names(Familydata) # 列出所有变量的名字
str(Familydata) # 查看数据框结构
attributes(Familydata) # 显示数据框属性的全部信息，输出是一个列表

des(Familydata) # epiDiaplay包的des()函数，比str()的输出简洁

Familydata$wt # 选取数据框的子集
View(Familydata)
Familydata[1:3, c(3, 4, 6)] # 显示前3行，第3,4,6个变量（也就是列）

Familydata[Familydata$sex == "F", ] # 显示性别为女性的数据
# 条件表达式后必须跟一个逗号，表示选择所有的列，用==

sample_rows <- sample(1:nrow(Familydata), size = 3, replace = F)
# 不放回抽样
sample_rows
Familydata[sample_rows, ]

Familydata[order(Familydata$age), ] # 按变量age的值排序，默认升序
Familydata[order(Familydata$age, decreasing = T), ] # 按变量age的值排序，降序

Familydata[order(-Familydata$age), ] # 将age的值取相反数，也是降序

duplicated(Familydata$code) # 检查变量code有无重复值,FALSE即没有重复值
# 若行数较多，查看很麻烦，所以
any(duplicated(Familydata$code))
# 或者用table()
table(duplicated(Familydata$code)) # 还能输出数量

# 删除重复的行
# 首先新建一个数据框
Familydata1 <- Familydata
Familydata1[12, ] <- Familydata[2, ] # 把第2行插入到第12行，以得到一个重复行
Familydata1

table(duplicated(Familydata1$code)) # 显示有1个重复行
which(duplicated(Familydata1$code)) # 找出重复的是哪一行
unique.code.data <- Familydata1[!duplicated(Familydata1$code), ] # 删除重复行
identical(unique.code.data, Familydata) # 与原来的进行判断，表明重复行已被删除

# 添加变量 log10money
Familydata$log10money <- log10(Familydata$money)
Familydata
# 让某一个变量只保留2位小数，用round()四舍五入，同时保留原数据类型
Familydata$log10money <- round(Familydata$log10money, 2) 
Familydata

# 删除一个变量
Familydata[, -7]
Familydata # 但对原来的数据框无影响
# 若要永久删除
Familydata$log10money <- NULL
Familydata

attach(Familydata) # 把数据框添加到搜索路径,就可以直接引用其中变量
search()
summary(age) # 等效于
summary(Familydata$age) # 但如果环境中存在同名变量，还是这样更好
detach(Familydata)

ls()
search()

rm(list = ls()) # 清除所有对象

library(dplyr)
data("birthwt", package = "MASS")
?birthwt
View(birthwt)

filter(birthwt, age > 35) # 筛选
filter(birthwt, bwt < 2500 | bwt > 4000)

slice(birthwt, 2:5) # 选择指定的行

arrange(birthwt, bwt) # 按某个变量排列，默认升序
arrange(birthwt, bwt, age) # 第1个变量值相等，按第2个排列
arrange(birthwt, desc(bwt)) # 按某个变量降序排列

mutate(birthwt, lwt.kg = lwt * 0.4536) # 用mutate()添加新变量
mutate(birthwt, lwt = lwt * 0.4536) # 若要替换某个变量

# 计算某个变量的指定统计量
summarise(birthwt, Mean.bwt = mean(bwt), Sd.bwt = sd(bwt))

a <- group_by(birthwt, race)
View(a)

as_tibble(birthwt) # 将数据框转换为tibble格式
# tibble是tidyverse系列包提供的类似数据框的格式
as.data.frame(birthwt) # 转换为数据框

# %>% 开始登场，把前一个值传递给后一个，免去设置中间变量的过程，还可节约内存
c(2, 3, 4, 5) %>% matrix(nrow = 2)

birthwt %>% 
  mutate(race = factor(race, labels = c("White", "Black", "Other"))) %>% 
  group_by(race) %>% 
  summarise(mean(bwt))

# 数据框的合并
data1 <- data.frame(id = 1:5,
                    sex = c("female", "male", "male", "female", "female"), 
                    age = c(12, 55, 34, 45, 56))
data1

data2 <- data.frame(id = 6:10, 
                    sex = c("male", "female", "female", "male", "female"), 
                    age = c(57, 68, 38, 48, 39))  
data2  
rbind(data1, data2) # 按行合并，列变量要相同

data3 <- data.frame(days = c(72, 28, 38, 29, 40), 
                    outcome = c("dead", "discharge", "discharge", "dead", "transfer"))
data3
cbind(data1, data3) # 按列合并，行数要相同

data4 <- data.frame(id = c(1, 3, 2, 4, 5), 
                    outcome = c("dead", "discharge", "discharge", "dead", "transfer"))
data4
mydata <- merge(data1, data4, by = "id") # 按某个共有变量排列
mydata

# 数据框的长宽格式的转换
# 长格式 long form
# 宽格式 wide form
?Indometh
data("Indometh")
View(Indometh)

# 转换为宽格式
wide <- reshape(Indometh, v.names = "conc", idvar = "Subject", 
                timevar = "time", direction = "wide")
View(wide)

# 重新转化为长格式
long <- reshape(wide, idvar = "Subject", varying = list(2:12), 
                v.names = "conc", direction = "long")
View(long)

# reshape包功能强大，但是参数很多，不好记
# tidyr包提供了更为简洁的操作方式
library(tidyr)
wide1 <- pivot_wider(as.data.frame(Indometh), 
                     names_from = time, 
                     values_from = conc)
View(wide1)

long1 <- pivot_longer(wide1, -Subject, 
                      names_to = "time", values_to = "conc")
View(long1)

# 一个整洁的数据集（tidy data）应该满足：每一行代表一个观测（observation）,
# 每一列代表一个变量（variable）。
# 在对医学数据分析之前，通常情况下，应先把数据集转化为长格式，因为R中的大多数
# 函数都支持这种格式的数据。

