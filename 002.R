install.packages("shiny")
setwd("D:/R")
install.packages("remotes")
remotes::install_github("luisdva/annotater")
1
library(ggplot2)
install.packages("Hmisc")
my_data <- read.csv("example.csv")
my_data

# 使用readr包读取txt/csv文件
install.packages("readr")
library("readr")

# 交互式选择my_data1
my_data1 <- read.delim(file.choose())

# 使用readr包导出数据
library("readr")
data("mtcars")
write_tsv(mtcars, path = "mtcars.txt")

install.packages("ggmap")
getwd()

remotes::install_github("daattali/addinslist")
??ggmap

install.packages("colourpicker")
library(colourpicker)
CPCOLS <- c("#1F2EB5", "#33a02c", "#e31a1c")

library(ggplot2)

ggplot(iris, aes(Sepal.Length, Petal.Length)) +
      geom_point(aes(col = Species)) +
      scale_colour_manual(values = CPCOLS)

c("#76EEC6", "#104E8B", "#528B8B")

remotes::install_github("RhoInc/CRANsearcher")

# 数据集预览增强r包：viewenhance包
install.packages("viewenhace")
remotes::install_github("kieranjmartin/viewenhance")

unlink("C:/Program Files/R/R-4.2.3/library/00LOCK", recursive = TRUE)

data(mtcars)
viewenhanceAddin(mtcars)

#管理R包的包
install.packages("pacman")
library(pacman) # 加载包
p_cite("ggplot2") # 输出ggplot2的引用文献
p_data("ggplot2")

# R实现文献计量
install.packages("bibliometrix", dependencies = TRUE)
library(bibliometrix)
install.packages("vctrs")
biblioshiny()

# 处理日期时间数据
install.packages("lubridate")
library(lubridate)
Sys.Date()
Sys.time() # CST means Central Standard Time, 即北京时间
date() # 返回当前的日期和时间 注意：这种方法返回的是字符串类型
Sys.timezone() # 输出当前时区
OlsonNames()

# 文本转日期
a <- as.Date("2023 09 20", "%Y %m %d")
a
a + 10
as.Date(x, origin) # 返回参数origin(日期)起第x天
as.Date(10, "2023-09-20")

# 文本转时间

a <- as.Date("2023-09-20")
b <- as.Date("2003-08-04")
birth_days <- a - b
birth_days

library(survival)
data(cancer, package = "survival")
View(colon)
# 计算男性和女性年龄的均值
tapply(colon$age, colon$sex, mean) # 1 for male
tapply(colon$age, colon$sex, function(x) max(x) - min(x)) # 分别计算男女性年龄的极差
# # 我多行注释
# 更好
stat_01 <- tapply(colon$age, colon[, c(4,6)], mean)
library(sjPlot)
tab_df(stat_01)
stat_01

lung
View(lung)
sapply(lung[, c(2,4,7,8,9)], FUN = mean, na.rm = T)
lapply(lung[, c(2,4,7,8,9)], FUN = mean, na.rm = T)

library(tidyverse)
# 列举tidyverse的依赖包
tidyverse_deps(recursive = FALSE, repos = getOption("repos"))
tidyverse_logo()

library(ggplot2)
plot(iris$Sepal.Length, iris$Sepal.Width) # plot 绘图
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(color = "darkred")

ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, 
                 colour = Species, shape = Species))
ggsave(filename = "test.pdf",
       width = 8,
       height = 6)

view(iris)
set.seed(1234)
rm(my_iris)
p <- ggplot(iris) +
  geom_bar(aes(x = Species, fill = Species))
p           

library(ggplot2)
library(tidyr)
library(dplyr)
my_iris1 <- iris %>% gather(feature_name,
                            feature_value,
                            one_of(c("Sepal.Length",
                                     "Sepal.Width",
                                     "Petal.Length",
                                     "Petal.Width"))) # 数据变换
ggplot(my_iris1) +
  geom_violin(aes(x = Species, y = feature_value)) +
  facet_grid(feature_name ~ Species, 
             scales = "free") # 分面

ggplot(my_iris1) +
  geom_violin(aes(x = Species, y = feature_value)) +
  facet_wrap(~feature_name + Species, 
             scales = "free")

setwd("D:/R")
chic <- readr::read_csv("chicago-nmmaps.csv")
library(tidyverse)
g <- ggplot(data = chic, aes(x = date, y = temp))
g
g + geom_point()
g + geom_line()
g + geom_point() + geom_line()
theme_set(theme_bw())
g + geom_point(color = "pink",
               size = 2)

# 输入上下标
ggplot(chic, aes(x = date, y = temp)) +
  geom_point(col = "firebrick") +
  labs(x = "Year", 
       y = expression(paste("Temperature (", degree ~ F, ")"^"(Hello!)")))
ggsave(filename = "数学上下标.tiff",
       width = 8,
       height = 6)

a <- ggplot(economics, aes(date, unemploy))
b <- ggplot(seals, aes(x = long, y = lat))
a + geom_blank()
a + expand_limits()
b + geom_curve(aes(yend = lat + 1, xend = long + 1))
a + geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
  theme(axis.title.x = element_text(size = 15, color = "orangered", face = "italic"), 
        axis.title.y = element_text(size = 15, color = "cyan", face = "italic"))

data = head(mtcars, 30)
q1 <- ggplot(data, aes(wt, mpg)) +
  geom_point() +
  geom_text(
    label = rownames(data),
    nudge_x = 0.25, nudge_y = 0.25,
    check_overlap = T) + 
  labs(title = "q1") +
  theme(plot.title = element_text(hjust = 0.5))
q2 <- ggplot(data, aes(wt, mpg)) +
  geom_point() +
  geom_text_repel(
    label = rownames(data)) +
  labs(title = "q2") +
  theme(plot.title = element_text(hjust = 0.5))
gridExtra::grid.arrange(q1, q2, ncol = 2)
    
library(ggrepel)
set.seed(42)
dat <- subset(mtcars, wt > 2.75 & wt < 3.45)
dat$car <- rownames(dat)
p <- ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = "red")
p1 <- p + geom_text() + labs(title = "geom_text()")
p2 <- p + geom_text_repel() + labs(title = "geom_text_repel()")
gridExtra::grid.arrange(p1, p2, ncol = 2)

ggplot(data, aes(wt, mpg)) +
  geom_point() +
  geom_label(label = rownames(data),
             nudge_x = 0.25, nudge_y = 0.25,
             check_overlap = T) # 带有边框的标签

ggplot(data, aes(wt, mpg)) +
  geom_point() + 
  geom_label(
    label = "I am hzwyyds!",
    x = 4.1, y = 20,
    label.padding = unit(0.55, "lines"), # rectangle size around label
    label.size = 0.35,
    color = "black",
    fill = "#69b3a2"
  )

library(dplyr)
library(tibble)
data = head(mtcars, 30)
data <- data %>%
  rownames_to_column(var = "carName")
# plot
ggplot(data, aes(wt, mpg)) +
  geom_point() + 
  geom_label(
    data = data %>% filter(mpg > 20 & wt > 3), # filter data
    aes(label = carName)
    )

search() # 查看已经加载的包
sessionInfo()

install.packages("mapdata")
library(maps)
library(mapdata)
world <- map_data("world")
ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), 
               fill = "lightblue",color = "black") + 
  theme_void()

library(remotes)
remotes::install_github("r-lib/conflicted")

install.packages("sf")
library(sf)

library(tidyverse)
library(conflicted)
download.file("https://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip",
              destfile = "D:/R/world_shape_file.zip")
install.packages("rgdal")
library(rgdal)
my_spdf <- readOGR(
  dsn = "D:/R/world_shape_file",
  layer = "TM_WORLD_BORDERS_SIMPL-0.3",
  verbose = F
)
install.packages("broom")
library(broom)
library(ggplot2)
ggplot() +
  geom_polygon(data = my_spdf,aes(x = long, y = lat,group = group),
               fill = "#69b3a2", color = "white") +
  theme_void()

library(maps)
maps::map("world", fill = T, col = rainbow(200),
    ylim = c(-60, 90), mar = c(0, 0, 0, 0))

