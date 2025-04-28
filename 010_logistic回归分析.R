getwd()
rm(list = ls())

# 二分类logistic回归分析模型
p <- seq(from = 0, to = 1, by = .01)
odds <- p / (1 - p)
plot(log(odds), p, type = "l", col = "blue", ylab = "Probability", las = 1)
abline(h = 0.5, lty = "dashed")
abline(v = 0, lty = "dashed")

# 数据准备
library(MASS)
data("birthwt")
str(birthwt)

library(epiDisplay)
tab1(birthwt$ptl)
tab1(birthwt$ftv)

library(dplyr)
birthweight <- birthwt %>% 
  mutate(race = factor(race, labels = c("white", "black", "other")), 
         smoke = factor(smoke, labels = c("no", "yes")), 
         ptl = ifelse(ptl > 0, "1+", ptl), 
         ptl = factor(ptl), 
         ht = factor(ht, labels = c("no", "yes")), 
         ui = factor(ui, labels = c("no", "yes")), 
         ftv = ifelse(ftv > 0, "2+", ftv), 
         ftv = factor(ftv)
         )
str(birthweight)

# 模型的建立
glm1 <- glm(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv, 
            data = birthweight)

summary(glm1)

# Null deviance 空离差平方和,只包含常数项的模型
# residual deviance 当前模型的离差平方和

pchisq(40.582 - 32.948, df = 188 - 179, lower.tail = F)

# 自变量的筛选
drop1(glm1)

glm2 <- step(glm1, trace = F)
summary(glm2)

# 模型的比较

# 似然比检验
lnL1 <- as.numeric(logLik(glm1))
lnL2 <- as.numeric(logLik(glm2))

pchisq(2 * (lnL1 - lnL2), df = 3, lower.tail = F)

anova(glm1, glm2, test = "Chisq")

AIC(glm1, glm2) # AIC越小，拟合越好

coef(glm2)
exp(coef(glm2))

confint(glm2)
exp(confint(glm2))

# 预测
newdata1 <- data.frame(lwt = 20, race = "black", smoke = "yes", ptl = "0", 
                      ht = "yes", ui = "no")
logit <- predict(glm2, newdata = newdata1)
logit

exp(logit) / (1 + exp(logit))
predict(glm2, newdata = newdata1, type = "response")

# 模型的检查
library(ResourceSelection)
hoslem.test(birthweight$low, fitted(glm2))

# 模型结果的汇总输出
library(epiDisplay)
logistic.display(glm2)

# 表格数据的logistic回归
dat.array <- array(c(136, 57, 107, 151, 63, 44, 63, 265), 
                   dim = c(2, 2, 2), 
                   dimnames = list(smoke = c("no", "yes"), 
                                   drink = c("no", "yes"), 
                                   outcome = c("control", "case")))
dat.table <- as.table(dat.array)
dat.table
View(dat.table)
dat <- as.data.frame(dat.table)
dat

mod1 <- glm(outcome ~ smoke + drink, family = binomial, 
            weights = Freq, data = dat)
summary(mod1)

# 条件logistic回归模型，1:1配对，1:m配对
library(epiDisplay)
data("VC1to1") # 来自1:1配对的病例对照研究
str(VC1to1)
head(VC1to1)

clogit1 <- clogit(case ~ smoking + alcohol + rubber + strata(matset), 
                  data = VC1to1)
clogit1

drop1(clogit1, test = "Chisq")

# 去掉smoking
clogit2 <- clogit(case ~ alcohol + rubber + strata(matset), data = VC1to1)

drop1(clogit2, test = "Chisq")

# 去掉rubber
clogit3 <- clogit(case ~ alcohol + strata(matset), data = VC1to1)

drop1(clogit3, test = "Chisq")

AIC(clogit1, clogit2, clogit3)

summary(clogit3)

clogistic.display(clogit3)

# 无序多分类logistic回归
library(epiDisplay)
data("Ectopic")
str(Ectopic)

library(nnet)
multil <- multinom(outc ~ hia, data = Ectopic)
summary(multil)

st <- summary(multil)$standard.errors
z <- coef(multil) / st
z

p.values <- pnorm(abs(z), lower.tail = F) * 2
p.values

confint(multil)

exp(confint(multil))

library(epiDisplay)
mlogit.display(multil)

# 把另一个变量gravi也放入模型中
multil2 <- multinom(outc ~ hia + gravi, data = Ectopic)
summary(multil2)
mlogit.display(multil2)

AIC(multil, multil2)

# 有序多分类logistic回归

# 累计优势logistic回归

dat <- array(c(10, 7, 19, 6, 0, 2, 7, 5, 1, 5, 6, 16), 
                dim = c(2, 2, 3), 
                dimnames = list(method = c("old", "new"), 
                                sex = c("male", "female"), 
                                outcome = c("effectiveless", "effective", 
                                            "recover")))
dat <- as.table(dat)
data1 <- as.data.frame(dat)
data1
head(data1)
str(data1)
View(data1)

data1$outcome <- ordered(data1$outcome) # 默认按字母顺序由低到高排列因子的各个水平
data1$outcome

library(MASS)
polr1 <- polr(outcome ~ sex + method, weights = Freq, data = data1)
summary(polr1)

data2 <- data1[rep(1:nrow(data1), data1$Freq), -4]

head(data2)

str(data2)

polr2 <- polr(outcome ~ sex, data = data2)

summary(polr2)

exp(coef(polr2))
exp(confint(polr2))

# 对模型的平行性假设做检验
library(VGAM)
polr.p <- vglm(outcome ~ sex + method, 
               family = cumulative(parallel = T), 
               data = data2)
polr.np <- vglm(outcome ~ sex + method, 
                 family = cumulative(parallel = F), 
                 data = data2)
VGAM::lrtest(polr.p, polr.np)               

ordinal.or.display(polr1)
