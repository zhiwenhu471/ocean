getwd()
setwd("D:/R/draft")
getwd()

library(survival)
data(ovarian)
View(ovarian)

str(ovarian)

# 把后3个变量转换成因子
ovarian$resid.ds <- factor(ovarian$resid.ds, 
                           levels = c(1, 2), 
                           labels = c("no", "yes"))
ovarian$rx <- factor(ovarian$rx, 
                     levels = c(1, 2), 
                     labels = c("A", "B"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, 
                          levels = c(1, 2), 
                          labels = c("good", "bad"))

hist(ovarian$age)

ovarian$agegr <- cut(ovarian$age, 
                     breaks = c(0, 50, 75), 
                     labels = c("<=50", ">50"))
table(ovarian$agegr)

# 创建生存对象
surv.obj <- Surv(time = ovarian$futime, 
              event = ovarian$fustat)
surv.obj

# + 表示删失数据

# 生存率的估计与生存曲线
survfit.obj <- survfit(surv.obj ~ 1)
summary(survfit.obj) # 没有删失数据
summary(survfit.obj, censored = T) # 包括删失数据

plot(survfit.obj, mark.time = T, # 添加了 + 标记
     xlab = "Time in days", 
     ylab = "Survival probability", 
     main = "Kaplan-Meier survival curve for ovarian cancer patients")

# 虚线表示置信区间，如果不想显示置信区间，可以设置conf.int = F
plot(survfit.obj, mark.time = T, conf.int = F, 
     xlab = "Time in days", 
     ylab = "Survival probability", 
     main = "Kaplan-Meier survival curve for ovarian cancer patients")

# 生存率的比较
surv.treat <- survfit(surv.obj ~ rx, data = ovarian)
summary(surv.treat)
plot(surv.treat, mark.time = T, conf.int = T,
     xlab = "Time in days", 
     ylab = "Survival probability", 
     main = "Kaplan-Meier survival curve for ovarian cancer patients by treatment",
     col = c("red", "blue"), 
     lty = 1:2)
# 添加图例
legend("topright", 
       legend = c("A", "B"), 
       col = c("red", "blue"), 
       lty = 1:2, 
       bty = "n")

library(survminer)
ggsurvplot(surv.treat, 
         data = ovarian, 
         risk.table = F, # 添加风险表
         pval = T, # 添加p值
         conf.int = T, # 添加置信区间
         xlab = "Time in days", 
         ylab = "Survival probability", 
         title = "Kaplan-Meier survival curve for ovarian cancer patients by treatment",
         legend.title = "Treatment",
         legend.labs = c("A", "B"),
         palette = c("red", "blue"))

# 时序检验
survdiff(surv.obj ~ rx, data = ovarian)

# Cox比例风险模型
# Cox比例风险模型是生存分析中最常用的模型之一
# 它假设不同组之间的生存函数比值是恒定的
# Cox比例风险模型的基本形式为：
# h(t) = h0(t) * exp(β1X1 + β2X2 + ... + βpXp)
# 其中h(t)是生存函数，h0(t)是基准生存函数，β1, β2, ..., βp是回归系数，X1, X2, ..., Xp是协变量
# Cox比例风险模型的假设是：不同组之间的生存函数比值是恒定的

cox1 <- coxph(surv.obj ~ rx + resid.ds + agegr + ecog.ps, data = ovarian)
summary(cox1)

# 通过p值来判断变量是否显著
# 通过回归系数来判断变量的影响方向
# 通过exp(coef)来判断变量的影响程度
# exp(coef) > 1 表示变量对生存时间有负面影响
# exp(coef) < 1 表示变量对生存时间有正面影响
# exp(coef) = 1 表示变量对生存时间没有影响

drop1(cox1, test = "Chisq") # 检验变量的显著性

# 通过AIC来判断变量的显著性
# AIC越小，模型越好
# AIC = -2 * log-likelihood + 2 * k
# k = 变量的个数

# ecog.ps的p值最大，将其从模型中去掉
cox2 <- coxph(surv.obj ~ rx + resid.ds + agegr, data = ovarian)
summary(cox2)

step.cox <- step(cox1) # 逐步回归
summary(step.cox)
# 逐步回归的结果与手动去掉ecog.ps的结果一致

# Cox比例风险模型的假设检验
# Cox比例风险模型的假设检验主要是检验比例风险假设是否成立
# 比例风险假设是指不同组之间的生存函数比值是恒定的
cox.zph(cox2) # 检验比例风险假设

# 生存的预测
newdata <- data.frame(rx = c("A", "B"), 
                      resid.ds = c("no", "no"), 
                      agegr = c(">50", ">50"))
newdata

hr <- predict(cox2, newdata = newdata, type = "risk")
hr

hr[1] / hr[2] # 计算风险比

cox.fit <- survfit(cox2, newdata = newdata, type = "Kaplan-Meier")
summary(cox.fit)

plot(cox.fit, mark.time = T, conf.int = T,
     xlab = "Time in days", 
     ylab = "Survival probability", 
     main = "Kaplan-Meier survival curve for ovarian cancer patients by treatment",
     col = c("red", "blue"), 
     lty = 1:2)
# 添加图例
legend("topright", 
       legend = c("A", "B"), 
       col = c("red", "blue"), 
       lty = 1:2, 
       bty = "n")

