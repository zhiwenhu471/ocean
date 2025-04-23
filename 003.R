setwd("D:/R")
# create a heat map
data <- as.matrix(mtcars)

# default heatmap
heatmap(data)

# use 'scale' to normalize
heatmap(data, scale = "column") # based on column
heatmap(data, scale = "row") # based on row

# no dendrogram nor recording for neither column or row
heatmap(data, Colv = NA, Rowv = NA, scale = "column")
View(data)

library(RColorBrewer)
par(mar = c(3,4,2,2))
display.brewer.all()

# cm.colors() the color is very beautiful!
heatmap(data, Colv = NA, Rowv = NA,scale="column", col = cm.colors(256))

library(ggplot2)

# Create a temperature data frame
temperature <- data.frame(x = 1:100, y = 1:100, temperature = runif(10000, 10, 30))

# Create a heatmap of the temperature data
ggplot(temperature, aes(x = x, y = y, fill = temperature)) +
  geom_raster() +
  scale_fill_gradient(low = cm.colors(100), high = cm.colors(100, alpha = 0.75))

# Rcolorbrewer palette
library(RColorBrewer)
cou1 <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(data, scale = "column", col = cou1)
heatmap(data, Colv = NA, Rowv = NA, scale = "column", col = cou1)

# add classic arguments like main title and axis title
heatmap(
  data, Colv = NA, Rowv = NA, scale = "column", col = cou1,
        xlab = "varible", ylab = "car", main = "heatmap"
  )

# you can change labels with labrow / colrow and their size with cexrow / cexcol
heatmap(data, scale = "column", cexRow = 1.5,
        labRow = paste("new_", rownames(data), sep = ""),
        col = colorRampPalette(brewer.pal(8, "Blues"))(25))
heatmap(data, scale="column", cexRow=1.5, 
        labRow=paste("new_", rownames(data),sep=""), 
        col= colorRampPalette(brewer.pal(8, "Blues"))(25))

# add color beside heatmap
# 您可以在热图旁边添加颜色矢量，以使用该 RowSideColors 参数表示预期的结构
# example: grouping from the first letter
my_group <- as.numeric(as.factor(substr(rownames(data), 1 , 1)))
colSide <- brewer.pal(9, "Set1")[my_group]
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(data, Colv = NA, Rowv = NA, scale="column" , RowSideColors=colSide, col=colMain)

q()
y
