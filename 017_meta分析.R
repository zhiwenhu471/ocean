# meta分析是针对具有相同或相似研究问题的多项研究结果进行定量分析的方法。
# 它可以提高研究结果的统计效能，提供更精确的估计，并帮助识别潜在的异质性和偏倚。

# meta分析的基本步骤

# 1. 提出问题，指定研究计划
# 2. 检索相关文献
# 3. 选择符合要求的纳入文献
# 4. 文献的信息提取
# 5. 纳入研究的质量评价
# 6. 资料的统计学处理
# 7. 结果的分析和讨论

# 二分类变量资料的meta分析
# 选择OR值作为效应量，RR和RD作为效应量反映临床疗效最为直观

library(meta)
data("Fleiss93")
# Fleiss93数据集是一个包含多个研究的二分类变量数据集
# 其中每个研究都有一个治疗组和一个对照组的事件数和总人数
# 该数据集用于进行meta分析
# 该数据集包含了93个研究的结果
Fleiss93

# 合并OR值
metabin(event.e, n.e, event.c, n.c,
        data = Fleiss93,
        sm = "OR")
# sm = "OR"表示使用OR值作为效应量

# 看异质性检验，Q检验，I^2统计量

# I^2 = (Q-(k-1))/Q * 100%
# k是纳入研究的项目

m <- metabin(event.e, n.e, event.c, n.c,
        data = Fleiss93,
        sm = "OR",
        studlab = paste(study, year)) # 显示研究编号和研究年份

names(Fleiss93)

forest(m, comb.random = F)
# 该函数用于绘制森林图，comb.random = F表示不显示随机效应模型的结果

forest(m, comb.common = F)
# 该函数用于绘制森林图，comb.common = F表示不显示固定效应模型的结果

forest(m)

# 合并RR值
# 对于前瞻性研究，RR值是最常用的效应量
# 对于回顾性研究，OR值是最常用的效应量

metabin(event.e, n.e, event.c, n.c,
        data = Fleiss93,
        sm = "RR",
        studlab = paste(study, year)) # 显示研究编号和研究年份

# 合并RD值
metabin(event.e, n.e, event.c, n.c,
        data = Fleiss93,
        sm = "RD",
        studlab = paste(study, year)) # 显示研究编号和研究年份

# 发表偏倚的识别
# 最常用的方法是作漏斗图（funnel plot）
# 漏斗图是一个散点图，横坐标是效应量，纵坐标是标准误
# 如果漏斗图呈现对称的漏斗形状，说明没有发表偏倚
# 如果漏斗图呈现不对称的漏斗形状，说明有发表偏倚
funnel(m)

# 发表偏倚的检验
metabias(m)

metabias(m, k.min = 7)
# k.min = 7表示至少有7个研究才能进行发表偏倚的检验，默认是k=10

# 用剪补法评估(trim and fill method)发表偏倚，方法同基于漏斗图的对称原理
# 首先剪切掉漏斗图中不对称的小样本研究，用剪切后的对称部分估计漏斗图的中心值，
# 再在漏斗图的中心值两侧添补被剪切部分以及相应的缺失部分，最后用添补后的漏斗图
# 重新估计校正后的效应量
tf1 <- trimfill(m)
summary(tf1)

funnel(tf1)

# 敏感性分析
# 敏感性分析是指通过改变某些参数或假设来评估结果的稳健性和可靠性
metainf(m, pooled = "common")
# pooled = "common"表示使用固定效应模型

# 连续性变量资料的meta分析
data("Fleiss93cont")
Fleiss93cont

metacont(n.e, mean.e, sd.e,
         n.c, mean.c, sd.c,
         data = Fleiss93cont,
         sm = "SMD",
         studlab = paste(study, year)) # 显示研究编号和研究年份
# sm = "SMD"表示使用标准化均数差作为效应量

