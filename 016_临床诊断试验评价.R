# 二分类结果的评价指标

# 假设在一项研究中，有100名患者和100名非患者，某一检测试验的灵敏度是80%，特异度
# 是90%，数据可以描述如下：
table1 <- as.table(cbind(c(80, 20), c(10, 90)))
dimnames(table1) <- list("test" = c("positive", "negative"), 
                         "disease" = c("present", "absent"))
table1

mosaicplot(table1, 
           main = "Mosaic plot of test results",
           xlab = "Test result", 
           ylab = "Disease status", 
           color = TRUE)

# 计算灵敏度、特异度、阳性预测值和阴性预测值
sensitivity <- table1[1, 1] / sum(table1[1, ])
sensitivity

specificity <- table1[2, 2] / sum(table1[2, ])
specificity

ppv <- table1[1, 1] / sum(table1[, 1])
ppv

npv <- table1[2, 2] / sum(table1[, 2])
npv

# 计算阳性似然比和阴性似然比
positive_likelihood_ratio <- sensitivity / (1 - specificity)
positive_likelihood_ratio

negative_likelihood_ratio <- (1 - sensitivity) / specificity
negative_likelihood_ratio

# 计算准确率
accuracy <- sum(diag(table1)) / sum(table1)
accuracy

# 计算F1分数
f1_score <- 2 * (ppv * sensitivity) / (ppv + sensitivity)
f1_score

# 计算ROC曲线下面积（AUC）
library(pROC)
roc_obj <- roc(c(rep(1, 80), rep(0, 20)), 
                c(rep(1, 10), rep(0, 90)))
roc_obj
plot(roc_obj, 
     main = "ROC curve", 
     col = "blue", 
     lwd = 2)
auc(roc_obj)
# 计算Youden指数
youden_index <- sensitivity + specificity - 1
youden_index

# 计算Kappa系数
library(irr)
kappa_value <- kappa2(table1)
kappa_value
# 计算Bland-Altman图
library(ggplot2)
library(ggpubr)
library(dplyr)
# 生成数据
set.seed(123)
n <- 100
x <- rnorm(n, mean = 50, sd = 10)
y <- x + rnorm(n, mean = 0, sd = 5)
data <- data.frame(x, y)
# 计算均值和差值
data <- data %>%
  mutate(mean = (x + y) / 2,
         diff = x - y)
# 绘制Bland-Altman图
ggplot(data, aes(x = mean, y = diff)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(data$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(data$diff) + 1.96 * sd(data$diff), linetype = "dashed", color = "green") +
  geom_hline(yintercept = mean(data$diff) - 1.96 * sd(data$diff), linetype = "dashed", color = "green") +
  labs(title = "Bland-Altman plot",
       x = "Mean of two measurements",
       y = "Difference between two measurements")
# 计算一致性相关系数（ICC）
library(psych)
icc_value <- ICC(data[, c("x", "y")])
icc_value

# 单个ROC分析
library(pROC)
data("aSAH")
str(aSAH)

roc1 <- roc(outcome ~ s100b, data = aSAH)
attributes(roc1)

roc1$auc

roc.result <- data.frame(threshold = roc1$thresholds,
                          sensitivity = roc1$sensitivities,
                          specificity = roc1$specificities)
roc.result$youden <- roc.result$sensitivity + roc.result$specificity - 1
head(roc.result)

which.max(roc.result$youden)
roc.result[18, ]

# 实际上，也可以
coords(roc1, "best", transpose = F)

plot(1 - roc1$specificities, 
     roc1$sensitivities, 
     type = "l", 
     lwd = 2, 
     xlab = "1 - Specificity", 
     ylab = "Sensitivity", 
     main = "ROC Curve")
abline(0, 1, lty = 2)
# 计算AUC
roc1$auc

# pROC包里的函数，绘制ROC曲线，图形更丰富
plot.roc(roc1, 
     print.auc = TRUE, 
     auc.polygon = T, 
     grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     auc.polygon.col = "lightblue",
     print.thres = TRUE,
     main = "ROC Curve for s100b")
ci.auc(roc1, conf.level = 0.95) # 得到AUC的95%置信区间

# 2个ROC曲线的比较
roc1 <- roc(aSAH$outcome, aSAH$s100b)
roc2 <- roc(aSAH$outcome, aSAH$ndka)

roc.test(roc1, roc2, method = "delong")

plot(roc1, col = "blue", lwd = 2)
plot(roc2, col = "red", lwd = 2, add = TRUE)
legend("bottomright", 
       legend = c("s100b", "ndka"), 
       col = c("blue", "red"), 
       lwd = 2)

# logistic回归的ROC曲线
fit <- glm(case ~ induced + spontaneous, family = binomial, data = infert)
library(epiDisplay)
logistic.display(fit)

lroc(fit, line.col = "red", lwd = 3)

# 联合试验
cut.point1 <- coords(roc1, "best", transpose = F)$threshold
cut.point1
cut.point2 <- coords(roc2, "best", transpose = F)$threshold
cut.point2

parallel <- ifelse(aSAH$s100b > cut.point1 | aSAH$ndka > cut.point2, 
                   "positive", "negative")
table(parallel, aSAH$outcome)

serial <- ifelse(aSAH$s100b > cut.point1 & aSAH$ndka > cut.point2, 
                   "positive", "negative")
table(serial, aSAH$outcome)

# 计算联合试验的灵敏度、特异度、阳性预测值和阴性预测值
sensitivity_parallel <- sum(parallel == "positive" & aSAH$outcome == 1) / sum(aSAH$outcome == 1)
sensitivity_parallel

specificity_parallel <- sum(parallel == "negative" & aSAH$outcome == 0) / sum(aSAH$outcome == 0)
specificity_parallel

ppv_parallel <- sum(parallel == "positive" & aSAH$outcome == 1) / sum(parallel == "positive")
ppv_parallel

npv_parallel <- sum(parallel == "negative" & aSAH$outcome == 0) / sum(parallel == "negative")
sensitivity_serial <- sum(serial == "positive" & aSAH$outcome == 1) / sum(aSAH$outcome == 1)
sensitivity_serial

specificity_serial <- sum(serial == "negative" & aSAH$outcome == 0) / sum(aSAH$outcome == 0)
specificity_serial

