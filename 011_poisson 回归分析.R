rm(list = ls())

# Poisson回归处理结果变量为计数的资料
library(robust)
data("breslow.dat")
str(breslow.dat)

summary(breslow.dat)

breslow <- breslow.dat[, 6:9]
summary(breslow)

# 直方图查看分布
hist(breslow$Ysum, breaks = 20,
     main = "Distribution of Seizure Count",
     xlab = "Seizure Count")

fit <- glm(Ysum ~ Base + Age + Trt, data = breslow.dat,
           family = poisson)
summary(fit)

coef(fit)
exp(coef(fit))

exp(confint(fit))

library(epiDisplay)
idr.display(fit)

# 过度离散的判定及处理
library(qcc)
qcc.overdispersion.test(breslow$Ysum, type = "poisson") 
# 若p值很小，表明存在过度离散

# 对渺小进行拟合优度检验
library(epiDisplay)
poisgof(fit)
# 若p值很小，表明拟合较差

# 过度离散的处理

# 1. 使用负二项分布
library(MASS)
fit2 <- glm.nb(Ysum ~ Base + Age + Trt, data = breslow)
summary(fit2)
coef(fit2)
exp(coef(fit2))
exp(confint(fit2))

# 2. 使用拟Poissom回归
fit.od <- glm(Ysum ~ Base + Age + Trt,
                 data = breslow,
                 family = quasipoisson)
summary(fit.od)

poisgof(fit2)

# 对数线性模型，用于列联表资料的分析
data <- array(c(100, 63, 4, 2, 36, 84, 10, 25), 
              dim = c(2, 2, 2), 
              dimnames = list(pill = c("no", "yes"), 
                           gene = c("V-", "V+"), 
                           group = c("control", "case")))
data

data <- as.table(data)
mydata <- as.data.frame(data)
mydata

mod1 <- glm(Freq ~ pill*gene*group, 
            data = mydata, 
            family = poisson)
summary(mod1)
# pill*gene*group等价于 pill + gene + group + pill:gene + 
# pill:group + gene:group + pill:gene:group

mod2 <- step(mod1)
summary(mod2)

exp(coef(mod2))

library(epiDisplay)
poisgof(mod2) # 进行拟合优度检验

