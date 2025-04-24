getwd()
setwd("D:/R/draft")
getwd()

rm(list = ls())

ls()

# 加载数据集
data(birthwt, package = "MASS")
str(birthwt)

# 因子化
library(dplyr)
birthwt <- birthwt %>% 
  mutate(low = factor(low, labels = c("no", "yes")), 
         race = factor(race, labels = c("white", "black", "other")), 
         smoke = factor(smoke, labels = c("no", "yes")), 
         ht = factor(ht, labels = c("no", "yes")), 
         ui = factor(ui, labels = c("no", "yes")))

str(birthwt)

summary(birthwt)

# 得到另一种格式的汇总输出
library(epiDisplay)
summ(birthwt)

# 数值型变量的描述性统计分析
cont.vars <- dplyr::select(birthwt, age, lwt, bwt)
length(cont.vars$age) # 总数
mean(cont.vars$age) # 均值
sd(cont.vars$age) # 标准差

# 计算多个变量的指定统计量
sapply(cont.vars, sd)

library(psych)
describe(cont.vars) # 除基本统计外，还有偏度和峰度，均值的标准误

# 计算某个分类变量各个类别下的统计量
aggregate(cont.vars, by = list(smoke = birthwt$smoke), mean)
aggregate(cont.vars, by = list(smoke = birthwt$smoke), sd)
aggregate(cont.vars, by = list(smoke = birthwt$smoke), length)

# 两个分类变量
aggregate(cont.vars, by = list(smoke = birthwt$smoke, 
                               race = birthwt$race), mean)
# tapply 函数可以实现类似的功能，不同的是他的第一个参数必须是一个变量，
# 第二个参数名是 INDEX，而不是 by

# 就计算bwt在不同吸烟情况下的均值
tapply(birthwt$bwt, INDEX = birthwt$smoke, mean)

# epiDisplay包里的函数 summ()
summ(birthwt$bwt, by = birthwt$smoke)
# 这个不仅有统计数据，还输出有序点图，更加强大
# 可用于探索数值型变量的分布，数据的密集趋势，异常值

# psych包里的describeBy()
describeBy(cont.vars, birthwt$smoke)

# dplyr包，推荐这种分析方法
library(dplyr)
birthwt %>% 
  group_by(smoke) %>% 
  summarise(Mean.bwt = mean(bwt), sd.bwt = sd(bwt))

# 分类变量的列联表和独立性检验

# 一维频数表
mytable <- table(birthwt$low) # 绝对数
mytable

prop.table(mytable) # 相对数

round(prop.table(mytable) * 100, 1) # 百分比，并且指定保留一位小数

library(epiDisplay)
tab1(birthwt$low) # 绝对数，相对数，百分比，还有图！！

# 若对于数值型变量，也同样可以
tab1(birthwt$age)

# 二维列联表
mytable <- table(birthwt$smoke, birthwt$low)
mytable

addmargins(mytable) # 生成边际频数

prop.table(mytable, margin = 1) # 行相加比例为1
prop.table(mytable, margin = 2) # 列相加比例为1

# epiDisplay包
tabpct(birthwt$smoke, birthwt$low) # 行，列，加图，无敌

# 多维列联表
mytable <- table(birthwt$smoke, birthwt$low, birthwt$race)
margin.table(mytable, 3)
margin.table(mytable, c(1, 3))
addmargins(mytable)

ftable(mytable) # 让三维列联表的显示更紧凑一点

# 独立性检验

# 卡方独立性检验
mytable <- table(birthwt$smoke, birthwt$low)
mytable
chisq.test(mytable)

# 查看期望频数表
chisq.test(mytable)$expected

# 期望频数都比较大，不进行连续性校正
chisq.test(mytable, correct = F) # p < 0.05

# Fisher精确概率检验
fisher.test(mytable)

mytable

# RR与OR
library(epiDisplay) # 这个包好强
# cc()和cs()第一个参数需要设置为结果变量
cc(birthwt$low, birthwt$smoke, decimal = 3)
cs(birthwt$low, birthwt$smoke, decimal = 3) # 不行，为什么？

# Cochran-Mentel-Haenzel 卡方检验
# 检验两个分类变量在调整第三个变量的情况下是否独立
mytable <- table(birthwt$low, birthwt$smoke, birthwt$race)
mantelhaen.test(mytable)

# epiDisplay包
mhor(mhtable = mytable)

# 配对列联表的卡方检验（Mcnemar检验）

# 先创建一个2*2列联表
my.matrix <- matrix(c(11, 2, 12, 33), nrow = 2)
my.matrix

mcnemar.test(my.matrix)

# 连续性变量组间差异的比较
# 独立样本的t检验
var.test(bwt ~ smoke, data = birthwt) #检查是否具有方差齐性
# 没有统计学意义，说明方差齐性

t.test(bwt ~ smoke, var.equal = T, data = birthwt) # var,equal=T 意思是方差齐

# 设置99%置信区间
t.test(bwt ~ smoke, var.equal = T, data = birthwt, conf.level = 0.99) 


# 单侧检验
t.test(bwt ~ smoke, var.equal = T, alt = "greater", data = birthwt)

# 非独立样本的t检验
x <- c(0.23, 0.34, 0.45, 0.56, 0.67)
y <- c(0.78, 0.89, 0.90, 0.12, 0.23)
t.test(x, y, paired = T) # 配对设计

# 单因素方差分析（one-way ANOVA）
# 先分别进行 Shapiro-Wilk 正态性检验
tapply(birthwt$bwt, birthwt$race, shapiro.test)
# 满足正态性,则进一步检查方差齐性
bartlett.test(bwt ~ race, data = birthwt)

# 另外一种非参数方差齐性检验方法，
library(car)
leveneTest(bwt ~ race, data = birthwt)

race.aov <- aov(bwt ~ race, data = birthwt)
summary(race.aov)

# 进行组间两两比较
TukeyHSD(race.aov)
plot(TukeyHSD(race.aov), las = 0)

# 组间差异的非参数检验
# Wilcoxon 秩和检验
wilcox.test(bwt ~ smoke, data = birthwt)

str(birthwt)

# 给每个变量加上标签
attr(birthwt, "var.labels") <- c("Low birth weight", 
                                 "Mother's age(yr)",
                                 "Mother's weight(lbs)",
                                 "Mother's race",
                                 "Smoking status",
                                 "Number of premature births",
                                 "History of hypertension",
                                 "Uterine irritability",
                                 "Number of physician visits",
                                 "Birth weight(g)")

des(birthwt)

# 以low为结果变量
tableStack(vars = age:smoke, by = low, dataFrame = birthwt)

tableStack(vars = 2:5, by = low, iqr = lwt, dataFrame = birthwt)

# 不显示统计检验结果及方法名称
tableStack(vars = age:smoke, by = low, test = F, dataFrame = birthwt)

tableStack(vars = age:smoke, by = low, name.test = F, dataFrame = birthwt)

# 参数by可以是一个包含多于2个水平的因子型变量
tableStack(vars = c(low, age, lwt, smoke), by = race, dataFrame = birthwt)

tableStack(vars = 2:5, by = "none", dataFrame = birthwt) # 只显示 total 列

table1 <- tableStack(vars = age:smoke, by = low, dataFrame = birthwt)
write.csv(table1, file = "table1.csv")

# 变量间的相关性

# 连续型变量
# Pearson, Spearman, Kendall's Tau
# Pearson 一般要求2个变量服从正态分布
cov(cont.vars) # 协方差
cor(cont.vars, method = "pearson")
cor(cont.vars, method = "kendall")
cor(cont.vars, method = "spearman")

# 相关系数的假设检验
cor.test(birthwt$lwt, birthwt$bwt) #  每次只能检验一个相关系数

library(psych)
corr.test(cont.vars) # 计算相关系数矩阵，并进行显著性检验

print(corr.test(cont.vars), short = F) # 得到相关系数的置信区间

# 偏相关系数
library(ggm)
names(cont.vars)
pcor(c(2, 3, 1), cov(cont.vars))
# 函数pcor()里第一个参数中的前2个数字代表要计算相关系数的变量的下标，
# 其余数字代表条件变量（控制变量）的下标

# 进一步进行偏相关系数的显著性检验
r <- pcor(c(2, 3, 1), cov(cont.vars))
nrow(cont.vars)
pcor.test(r, q = 1, n = 189)
# r是前面定义的变量，q是条件变量的个数，n是样本量

# 分类变量间的相关性
library(vcd)
mytable <- table(Arthritis$Treatment, Arthritis$Improved)
assocstats(mytable)

# 对于配对列联表，可以计算一致性指标Kappa统计量的值
my.matrix <- matrix(c(11, 2, 12, 31), nrow = 2)
kappa(my.matrix)
library(epiDisplay)
kap(my.matrix)

chisq.test(my.matrix)$expected

# 相关性的可视化
pairs(cont.vars)

library(car)
scatterplotMatrix(cont.vars)

# 相关系数矩阵的可视化
library(corrplot)
corrplot(cor(cont.vars))

library(corrgram)
corrgram(cont.vars, upper.panel = panel.pie)

# 对于分类变量
library(vcd)
mytable
chisq.test(mytable)
assocplot(mytable)

mosaic(~ Sex + Treatment + Improved, 
       data = Arthritis) # 马赛克图，多维列联表数据

# 在一幅图中同时展示数值型变量和分类变量的关联
library(GGally)
dat <- dplyr::select(birthwt, age, lwt, bwt, race, smoke)
ggpairs(dat)
