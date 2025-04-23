rm(list = ls(all = T))

# 基础绘图系统：graphics 包和 grDevices 包
dose <- c(12, 23, 34, 45, 65)
drugA <- c(16, 20, 27, 37, 58)
drugB <- c(27, 37, 28, 19, 49)

plot(dose, drugA)
plot(dose, drugA, type = "b")

### 这是一幅图
plot(dose, drugA, xlab = "dose", ylab = "response", lty = 1, pch = 15)
lines(dose, drugB, type = "b", lty = 2, pch = 17)
legend("topleft", title = "Drug type", 
       legend = c("A", "B"), 
       lty = c(1, 2),
       pch = c(15, 17))
###这是一幅图

# 导入MASS包中的anorexia数据集
data(anorexia, package = "MASS")
str(anorexia)
attach(anorexia)
hist(Prewt)

plot(density(Prewt)) # 密度曲线更平滑

# 设置字体显示
windowsFonts(Song = windowsFont("Microsoft YaHei UI")) # 到“C:\Windows\Fonts”找
loadfonts(device = "win")
hist(Prewt, freq = F, col = "pink", 
     xlab = "体重（lbs）", family = "Song",
     main = "治疗前体重分布直方图",
     las = 1) # 将纵轴的刻度标签横向显示
lines(density(Prewt), col = "blue", lwd = 2)
rug(Prewt) # 在横轴上添加轴须图，展示数据分布的密集趋势
detach(anorexia)

# 条形图（bar chart）
library(vcd)
data("Arthritis")
attach(Arthritis)
counts <- table(Improved)
counts

barplot(counts, xlab = "Improvement", ylab = "Frequency", las = 1)
counts <- table(Improved, Treatment)
counts

barplot(counts, 
        col = c("red", "green", "blue"), 
        xlab = "Improvement", ylab = "Frequency",
        beside = T, las = 1)
legend("top", legend = rownames(counts), 
       fill = c("red", "yellow", "green"))

# 让条形图显示更多的统计数据
library(epiDisplay)
aggregate.plot(anorexia$Postwt, by = list(anorexia$Treat), 
               error = "sd", legend = F, 
               bar.col = c("red", "green", "blue"), 
               ylim = c(0, 100), las = 1, 
               main = "")

# 饼图（pie chart）
percent <- c(5.8, 27.0, 0.5, 20.8, 12.8, 33.1)
diseases <- c("上感", "中风", "外伤", "晕厥", "食物中毒", "其他")
lbs <- paste0(diseases, percent, "%")
pie(percent, labels = lbs, col = rainbow(6),
    cex = 1, radius = 1.05, family = "Song") # radius调整半径，cex调整字体大小

class(length(percent)) # 查看数据类型
percent[1]

sum(percent) # 直接求总和

# 箱线图（boxplot）
library(epiDisplay)
View(anorexia)
anorexia$wt.change <- anorexia$Postwt - anorexia$Prewt
boxplot(anorexia$wt.change, ylab = "weight change(lbs)", las = 1)

boxplot(wt.change ~ Treat, data = anorexia, 
        ylab = "weight change (lbs)", las = 1)

# 小提琴图可以看成是箱线图和密度图的结合
library(vioplot)
vioplot(wt.change ~ Treat, data = anorexia, 
        ylab = "weight change (lbs)", 
        col = "gold", las = 1)

# Cleveland dot plot，本质也是散点图，通过点的位置展示数据大小
pdf("vioplot.pdf") # 导出为 pdf
vioplot(wt.change ~ Treat, data = anorexia, 
        ylab = "weight change (lbs)", 
        col = "gold", las = 1)
dev.off()

# 导出为 tiff 格式
tiff(filename = "dotcart.tiff", 
     width = 15, height = 15, units = "cm", res = 300)
dotchart(VADeaths)
dev.off()

# 下面用 ggplot 包绘图
library(ggplot2)
p <- ggplot(data = mtcars, mapping = aes(x = wt, y = mpg))
p # 这时候的 p 只是一个空白的画布， 需要进一步定义图形
p + geom_point() # 使用点来展示数据

mtcars$am <- factor(mtcars$am) # 因为 am 是分类变量，先转化为因子
# 将变量映射到颜色上
ggplot(data = mtcars, aes(x = wt, y = mpg, color = am)) + geom_point()
# 将变量映射到形状上
ggplot(data = mtcars, aes(x = wt, y = mpg, shape = am)) + geom_point()
# 将变量映射到大小上
ggplot(data = mtcars, aes(x = wt, y = mpg, size = am)) + geom_point()

# 进行散点拟合
ggplot(data = mtcars, aes(x = wt, y = mpg, color = am)) + geom_smooth()
# 默认为 loess 局部加权回归

ggplot(data = mtcars, aes(x = wt, y = mpg, color = am)) + 
  geom_smooth(method = "lm") # 切换为线性拟合

ggplot(data = mtcars, aes(x = wt, y = mpg)) + 
  geom_smooth() + geom_point(aes(color = am)) # 只显示一条拟合线

ggplot(data = mtcars, aes(x = wt, y = mpg)) + 
  geom_smooth() + geom_point(aes(color = am)) +
  scale_color_manual(values = c("blue", "red")) + # 手动设定颜色
  stat_smooth()

ggplot(data = mtcars, aes(x = wt, y = mpg)) + 
  stat_smooth() +
  geom_point() +
  facet_grid(~ am) # 对 am 的 0 和 1 分别作图

# 分布的特征
data(anorexia, package = "MASS")
anorexia$wt.change <- anorexia$Postwt - anorexia$Prewt

# 绘制 wt.change 的直方图
ggplot(anorexia, aes(x = wt.change)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "white") +
  labs(x = "weight change (lbs)") +
  theme_bw()
# binwidth 用于设置组距， 默认值是全距除以 30， 作图时可以自行尝试
# fill 设置填充色，color 设置矩形边框的颜色

# 添加密度曲线
# y = ..density.. 设置y轴为频率（密度）
ggplot(anorexia, aes(x = wt.change, y = ..density..)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "white") +
  stat_density(geom = "line", linetype = "dashed", size = 1) +
  labs(x = "weight change (lbs)") +
  theme_bw()

# 密度曲线还能用于对不同数据的分布进行比较
ggplot(anorexia, aes(x = wt.change, color = Treat, linetype = Treat)) +
  stat_density(geom = "line", size = 1) +
  labs(x = "weight change (lbs)") +
  theme_bw()

# 箱线图也经常用于展示数值型变量的分布，尤其是各组分布间的比较
ggplot(anorexia, aes(x = Treat, y = wt.change, fill = Treat)) +
  geom_boxplot() +
  theme_bw()

# ggpubr包可在平行箱线图上添加组间比较的统计学差异
library(ggpubr)
my_comparisons <- list(c("CBT", "Cont"), c("CBT", "FT"), c("Cont", "FT"))
p <- ggplot(anorexia, aes(x = Treat, y = wt.change)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     method = "t.test", 
                     color = "blue") +
  theme_bw()

# 比例的构成
# 叠加条形图，纵坐标是绝对技术
data(Arthritis, package = "vcd")
ggplot(Arthritis, aes(x = Treatment, fill = Improved)) +
  geom_bar(color = "black") +
  scale_fill_brewer() +
  theme_bw()

# 要想看到相对比例
ggplot(Arthritis, aes(x = Treatment, fill = Improved)) +
  geom_bar(color = "black", position = "fill") +
  scale_fill_brewer() +
  theme_bw()

# 将条形图并排放置
ggplot(Arthritis, aes(x = Treatment, fill = Improved)) +
  geom_bar(color = "black", position = "dodge") +
  scale_fill_brewer() +
  theme_bw()

# ggsave()专门用于保存ggplot2绘制的图形
ggsave("statistic differences.pdf", p, width = 15, height = 15, units = 'cm')

# 人口金字塔图
library(epiDisplay)
data("Oswego")
pyramid(Oswego$age, Oswego$sex, col.gender = c(2, 4), bar.label = T)

# 横向堆栈条形图，以问卷的形式
library(sjPlot)
data(efc)
names(efc)
view_df(efc) # 在 viewer 中查看数据集信息
View(efc)
qdata <- dplyr::select(efc, c82cop1:c90cop9)

pdf("横向堆栈条形图.pdf", width = 20, height = 12)
plot_stackfrq(qdata)
dev.off()

# 热图，生信聚类分析
data(mtcars)
dat <- scale(mtcars)
class(dat)
heatmap(dat)

# 三维散点图
library(scatterplot3d)
data("trees")
scatterplot3d(trees, type = "h", highlight.3d = T, angle = 55, pch = 16)
# h 表示垂线段，默认 p 表示点，angle 表示x轴和y轴的角度

# 想进行三维交互（外观真不如python的好看^-^）
library(rgl)
plot3d(trees)

# 词云图
library(wordcloud2)
head(demoFreqC)
pdf("示例词云图.pdf", width = 15, height = 12) # 效果有点糊哈哈
wordcloud2(demoFreqC)
dev.off()

# 动态图形
library(gganimate)
library(ggplot2)
airquality$date <- 
  as.Date(paste(1973, airquality$Month, airquality$Day, sep = "-"))
g <- ggplot(airquality, aes(date, Temp)) +
  geom_line() +
  transition_time(Month) +
  ease_aes("sine-in-out") 
anim_save(g, filename = "animation.gif")

