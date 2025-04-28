rm(list = ls())

setwd("D:/R/draft")
getwd()

# 结果变量（因变量），解释变量（自变量）
# 解释变量只有一个---简单线性回归
# 解释变量多于一个---多重线性回归

# 加载之前已经保存好的UCR数据
load("UCR.rdata")

library(epiDisplay)
des(UCR)
summary(UCR)

plot(ucr ~ age, data = UCR, 
     xlab = "Age in years", ylab = "Urine creatinine (mmol)")

mod <- lm(ucr ~ age, data = UCR) # 建立线性回归模型
mod
attributes(mod) # 查看属性

summary(mod)

mod$fitted.values

summary(aov(mod)) # 方差分析表

# UCR变异的总的平方和为
SST <- sum((UCR$ucr - mean(UCR$ucr))^2)
SST

# 残差平方和为
SSR <- sum(residuals(mod)^2)
SSR

# 回归平方和为
SSW <- sum((fitted(mod) - mean(UCR$ucr))^2)
SSW

#SST = SSR + SSW

# 决定系数 = SSW/SST
SSW/SST

# 决定系数也可以认为是自变量解释了因变量总变异的百分比
summary(mod)

# 添加回归直线
abline(mod)

points(UCR$age, fitted(mod), pch = 18, col = "blue") # 画出残差值

segments(UCR$age, UCR$ucr, UCR$age, fitted(mod), col = "green")

# 提取出每个样本的残差值
res <- residuals(mod)
res

# 检查残差的和及其平方和
sum(res)
sum(res^2) # 与之前算出的SSR一样

# 检验残差正态性
hist(res)
# 正态QQ图
qqnorm(res)
qqline(res)

# 定量地
shapiro.test(res)

# 作残差与拟合值之间的散点图来看残差的分布模式
plot(fitted(mod), res, xlab = "Fitted values", 
     type = "n") # type=n, 表示不显示散点
text(fitted(mod), res, 
     labels = rownames(UCR)) # 在散点位置标注了样本点的编号
abline(h = 0, col = "green") # 添加绿色的水平线作为参考线

# 关于模型的残差诊断图（散点图，QQ图，位置-尺度图，残差-杠杆图）
# 位置-尺度图用于检验残差的方差齐性
# 残差-杠杆图用于鉴别离群点、高杠杆值点、强影响点
par(mfrow = c(2, 2)) # 把画布分隔成2*2
plot(mod)
par(mfrow = c(1, 1)) # 恢复画布为1*1

# 分层线性回归
mod1 <- lm(ucr ~ age + group, data = UCR)
summary(mod1)

col <- ifelse(UCR$group == 0, "blue", "red")
pch <- ifelse(UCR$group == 0, 1, 19)
plot(ucr ~ age, data = UCR, 
     xlab = "Age in years", ylab = "Urine creatinine (mmol)", 
     col = col, pch = pch)
# 添加图例
legend("topleft", 
       legend = c("Normal children", "Diseased children"), 
       col = c("blue", "red"), pch = c(1, 19))

coef(mod1) # 提取模型包含的系数

b <- coef(mod1)[2]
b
# 对于未患病组，group为0时，其截距是系数的第一项，计为a0
a0 <- coef(mod1)[1]
a0
# 对于患病组，group为1时，其截距是系数的第一项与第三项之和，计为a1
a1 <- coef(mod1)[1] + coef(mod1)[3]
a1

# 添加两条回归直线
abline(a = a0, b = b, col = "blue")
abline(a = a1, b = b, col = "red")

# 添加一个age和group的交互项（age:group）
mod2 <- lm(ucr ~ age + group + age:group, data = UCR)
coef(mod2)

# mod2可简化成，ucr=1.662 + 0.139*age -0.566*group + 0.033*(age:group)
# 未患病组，group=0
# 患病组，group=1

# 未患病组的截距
a0 <- coef(mod2)[1]
# 患病组的截距
a1 <- coef(mod2)[1] + coef(mod2)[3]
# 未患病组的斜率
b0 <- coef(mod2)[2]
# 患病组的斜率
b1 <- coef(mod2)[2] + coef(mod2)[4]

col <- ifelse(UCR$group == 0, "blue", "red")
pch <- ifelse(UCR$group == 0, 1, 19)
plot(ucr ~ age, data = UCR, 
     xlab = "Age in years", ylab = "Urine creatinine (mmol)", 
     col = col, pch = pch)
# 添加图例
legend("topleft", 
       legend = c("Normal children", "Diseased children"), 
       col = c("blue", "red"), pch = c(1, 19))

# 添加两条回归直线
abline(a = a0, b = b, col = "blue")
abline(a = a1, b = b, col = "red")

summary(mod2)
# 表明交互项 age:group 并没有统计学意义（p = 0.405）
# 所以模型中没必要包含，mod1即为最终的模型

# 多重线性回归

# 拟合多重线性回归模型
library(ISwR)
data("cystfibr")
?cystfibr # 查看数据集的介绍
str(cystfibr) # 查看数据结构

# 将sex转化为因子，0代表男性，1代表女性
cystfibr$sex <- factor(cystfibr$sex, labels = c("male", "female"))
str(cystfibr)

# 数据集最后5个变量分别为
# fev1 --- 第一秒用力呼气量
# rv --- 残气量
# frc --- 功能残气量
# tlc --- 肺总量
# pemax --- 最大呼气压力

# 查看这5个变量的相关性
cor(cystfibr[, 6:10])

# 下面选择fev1作为结果变量建立多重线性回归模型
fit1 <- lm(fev1 ~ age + sex + height + weight + bmp, data = cystfibr)
summary(fit1)

# 多重共线性，自变量之间存在相关性
cor(cystfibr[, 3:5])

# 方差膨胀因子 VIF
lm.age <- summary(lm(age ~ sex + height + weight + bmp, data = cystfibr))
lm.age
1 / (1 - lm.age$r.squared) # VIF > 5 可认为存在多重共线性

library(car)
vif(fit1) # 计算模型中所有自变量VIF的值

# 逐步回归
drop1(fit1)

drop1(fit1, test = "F")

fit2 <- step(fit1) # 默认"both"向前向后法，"backward"向后法，"forward"向前法
fit_both <- step(fit1)
fit_backward <- step(fit1, direction = "backward")
fit_forward <- step(fit1, direction = "forward")

summary(fit2)

# 得到回归系数的置信区间，默认2.5%-97.5%
confint(fit2)

anova(fit2)

# 回归诊断，判断模型是否满足最小二乘法的统计假设
library(gvlma)
gvlma(fit2)

library(epiDisplay)
regress.display(fit2)

write.csv(regress.display(fit2)$table, file = "mytable.csv")
