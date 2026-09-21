library(tidyverse)
library(geomtextpath)
library(MetBrewer)
library(viridis) 
library(RColorBrewer)
# 读取Excel文件
library(openxlsx)
library(showtext)
showtext_auto()

mat1 <- read.xlsx(".xlsx")

df <- mat1 %>%
  pivot_longer(-c(id, label))

df$id <- factor(df$id,levels = mat1$id)
df$name <- factor(df$name, levels = c("Accuracy", "Precision", "Recall", "F1-score"))

# 定义四个区间对应的鲜明渐变色

# 1.提取YlGnBu中的四个颜色
custom_colors <- brewer.pal(4, "YlGnBu")
# 2. 转换为 RGB 并添加透明度
custom_colors  <- sapply(custom_colors, function(color) {
  rgb_vals <- col2rgb(color)
  rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha = 180, maxColorValue = 255) # alpha = 180 (大约 70% 不透明)
})

# 3. 调整最深颜色 (这里增加亮度)
darkest_color <- col2rgb(custom_colors[4])
darkest_color <- pmin(darkest_color + 40, 255)  # 增加亮度，但不要超过 255
custom_colors [4] <- rgb(darkest_color[1], darkest_color[2], darkest_color[3], alpha = 180, maxColorValue = 255)

df$label<-as.factor(df$label)
p<-ggplot(df, aes(id, y = name, fill = value)) +
  geom_tile(color = "white", linewidth = 1) +
  # 使用自定义的颜色方案，根据分区设置breaks
  scale_fill_gradientn(
    colors = custom_colors,
    values = scales::rescale(c(0, 70, 80, 90, 100)),
    limits = c(0, 100),
    breaks = c(0, 70, 80, 90,100),  # 在各区间中间设置标签
    guide = "none"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  geom_textpath(aes(label = id, y = 5), size = 3, parse = TRUE) +
  coord_radial(start = 0.125, end = pi * 2-0.125 , inner.radius = 0.6) +
  geom_rect(xmin = 0.5,  ymin = 6, xmax = 2.48, ymax = 6, linewidth = 7, color =  "#D9D0D3") +  
  geom_rect(xmin = 2.5,  ymin = 6, xmax = 13.48, ymax = 6, linewidth = 7, color = "#7294D4") +
  geom_rect(xmin = 13.5, ymin = 6, xmax = 16.48, ymax = 6, linewidth = 7, color ="#C6CDF7", fill = NA) +
  geom_rect(xmin = 16.5, ymin = 6, xmax = 23.48, ymax = 6, linewidth = 7, color = "#78B7C5", fill = NA) +
  geom_rect(xmin = 23.5, ymin = 6, xmax = 26.48, ymax = 6, linewidth = 7, color = "#85D4E3", fill = NA) +
  geom_rect(xmin = 26.5, ymin = 6, xmax = 31.48, ymax = 6, linewidth = 7, color = "#B9E3CB", fill = NA) +
  geom_rect(xmin = 31.5, ymin = 6, xmax = 35.48, ymax = 6, linewidth = 7, color = "#CDE0EC" , fill = NA) +
  geom_rect(xmin = 35.5, ymin = 6, xmax = 38.48, ymax = 6, linewidth = 7, color = "#D9D0D3" , fill = NA) +
  geom_rect(xmin = 38.5, ymin = 6, xmax = 40.48, ymax = 6, linewidth = 7, color = "#7294D4" , fill = NA) +
  geom_rect(xmin = 40.5, ymin = 6, xmax = 46.48, ymax = 6, linewidth = 7, color = "#C6CDF7" , fill = NA) +
  geom_rect(xmin = 46.5, ymin = 6, xmax = 47.48, ymax = 6, linewidth = 7, color = "#78B7C5" , fill = NA) +
  geom_rect(xmin = 47.5, ymin = 6, xmax = 52.48, ymax = 6, linewidth = 7, color = "#85D4E3" , fill = NA) +
  geom_rect(xmin = 52.5, ymin = 6, xmax = 54.48, ymax = 6, linewidth = 7, color = "#B9E3CB" , fill = NA) +
  geom_textpath(size = 4, hjust = 0.5, label = "A", x = 1.5, y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "B", x = 8, y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "C", x = 15, y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "D", x = 20, y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "E", x = 25, y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "F", x =29,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "G", x =33.5,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "H", x =37,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "I", x =39.5,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "J", x =43.5,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "K", x =47,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "L", x =50,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "M", x =53.5,y = 6) +
  geom_textpath(data = df %>% filter(name == "F1-score"), aes(label = round(value, digits = 1), y = 4), size = 3, hjust = 0.5, vjust = 0.5) +
  geom_textpath(data = df %>% filter(name == "Recall"), aes(label = round(value, digits = 1), y = 3), size = 3, hjust = 0.5, vjust = 0.5) +
  geom_textpath(data = df %>% filter(name == "Precision"), aes(label = round(value, digits = 1), y = 2), size = 3, hjust = 0.5, vjust = 0.5) +
  geom_textpath(data = df %>% filter(name == "Accuracy"), aes(label = round(value, digits = 1), y = 1), size = 3, hjust = 0.5, vjust = 0.5) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.r = element_text(colour = "black", size = 10, hjust = 0.5, vjust = 0.5,
                               margin = margin(r = 0, unit = "cm")),
    legend.title = element_blank(),
    legend.direction = "horizontal"
  )+
  geom_textpath(aes(label = label, y = 0), size = 3, parse = F,angle=90,hjust=1,vjust=0.5) 
p
library(showtext)
library(sysfonts)
font_add("YaHei", regular = "C:/Windows/Fonts/msyh.ttc")  # Windows 系统上的宋体
Font <- c('STKaiti.TTF','simhei.TTF') ##华文楷体;黑体;
for (i in Font) {
  font_path = i
  font_name = tools::file_path_sans_ext(basename(font_path))
  font_add(font_name, font_path)
}
font_families() ### 查看当前字体
showtext_auto(enable=TRUE)
print(font_families())

p<-ggplot(df, aes(id, y = name, fill = value)) +
  geom_tile(color = "white", linewidth = 1) +
  # 使用自定义的颜色方案，根据分区设置breaks
  scale_fill_gradientn(
    colors = custom_colors,
    values = scales::rescale(c(0, 70, 80, 90, 100)),
    limits = c(0, 100),
    breaks = c(0, 70, 80, 90,100),  # 在各区间中间设置标签
    guide = "none"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  geom_textpath(aes(label = id, y = 5), size = 3, parse = TRUE) +
  coord_radial(start = 0.125, end = pi * 2-0.125 , inner.radius = 0.6) +
  geom_rect(xmin = 0.5,  ymin = 6, xmax = 2.48, ymax = 6, linewidth = 7, color =  "#D9D0D3") +  
  geom_rect(xmin = 2.5,  ymin = 6, xmax = 13.48, ymax = 6, linewidth = 7, color = "#7294D4") +
  geom_rect(xmin = 13.5, ymin = 6, xmax = 16.48, ymax = 6, linewidth = 7, color ="#C6CDF7", fill = NA) +
  geom_rect(xmin = 16.5, ymin = 6, xmax = 23.48, ymax = 6, linewidth = 7, color = "#78B7C5", fill = NA) +
  geom_rect(xmin = 23.5, ymin = 6, xmax = 26.48, ymax = 6, linewidth = 7, color = "#85D4E3", fill = NA) +
  geom_rect(xmin = 26.5, ymin = 6, xmax = 31.48, ymax = 6, linewidth = 7, color = "#B9E3CB", fill = NA) +
  geom_rect(xmin = 31.5, ymin = 6, xmax = 35.48, ymax = 6, linewidth = 7, color = "#CDE0EC" , fill = NA) +
  geom_rect(xmin = 35.5, ymin = 6, xmax = 38.48, ymax = 6, linewidth = 7, color = "#D9D0D3" , fill = NA) +
  geom_rect(xmin = 38.5, ymin = 6, xmax = 40.48, ymax = 6, linewidth = 7, color = "#7294D4" , fill = NA) +
  geom_rect(xmin = 40.5, ymin = 6, xmax = 46.48, ymax = 6, linewidth = 7, color = "#C6CDF7" , fill = NA) +
  geom_rect(xmin = 46.5, ymin = 6, xmax = 47.48, ymax = 6, linewidth = 7, color = "#78B7C5" , fill = NA) +
  geom_rect(xmin = 47.5, ymin = 6, xmax = 52.48, ymax = 6, linewidth = 7, color = "#85D4E3" , fill = NA) +
  geom_rect(xmin = 52.5, ymin = 6, xmax = 54.48, ymax = 6, linewidth = 7, color = "#B9E3CB" , fill = NA) +
  geom_textpath(size = 4, hjust = 0.5, label = "A", x = 1.5, y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "B", x = 8, y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "C", x = 15, y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "D", x = 20, y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "E", x = 25, y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "F", x =29,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "G", x =33.5,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "H", x =37,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "I", x =39.5,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "J", x =43.5,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "K", x =47,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "L", x =50,y = 6) +
  geom_textpath(size = 4, hjust = 0.5, label = "M", x =53.5,y = 6) +
  geom_textpath(data = df %>% filter(name == "F1-score"), aes(label = round(value, digits = 1), y = 4), size = 3, hjust = 0.5, vjust = 0.5) +
  geom_textpath(data = df %>% filter(name == "Recall"), aes(label = round(value, digits = 1), y = 3), size = 3, hjust = 0.5, vjust = 0.5) +
  geom_textpath(data = df %>% filter(name == "Precision"), aes(label = round(value, digits = 1), y = 2), size = 3, hjust = 0.5, vjust = 0.5) +
  geom_textpath(data = df %>% filter(name == "Accuracy"), aes(label = round(value, digits = 1), y = 1), size = 3, hjust = 0.5, vjust = 0.5) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.r = element_text(colour = "black", size = 10, hjust = 0.5, vjust = 0.5,
                               margin = margin(r = 0, unit = "cm")),
    legend.title = element_blank(),
    legend.direction = "horizontal"
  )+geom_textpath(aes(label = label, y = 0), size = 3, parse = F,angle=90,hjust=1,vjust=0.5,family="simhei") 
p
# 创建一个新的数据框，用于自定义图例
# 这里我们使用均匀分布的x值
legend_data <- data.frame(
  x = seq(0, 4, length.out = 100),  # 使用0-4均匀分布的x值
  y = rep(1, 100)
)
library(cowplot)
# 创建自定义图例
legend_plot <- ggplot(legend_data, aes(x = x, y = y, fill = x)) +
  geom_tile() +
  # 使用自定义颜色映射
  scale_fill_gradientn(
    colors = custom_colors,
    breaks = c(0, 1, 2, 3, 4),
    labels = c("0", "70", "80", "90", "100"),
    limits = c(0, 4),
    guide = guide_colorbar(
      title = "Performance",         # 图例标题内容
      direction = "horizontal",
      barwidth = unit(4, "cm"),
      barheight = unit(0.5, "cm"),
      ticks = TRUE,
      title.position = "top",       # 标题在颜色条上方
      title.hjust = 0            # 标题居中
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(
      size = 8,       # 设置标题字体大小 (可选)
      face = "bold"    # <<< 这里设置标题为粗体 >>>
    ),
    legend.text = element_text(size = 7), # 刻度标签字体大小
    legend.margin = margin(t = 10)       # 图例与其他元素的边距
  )

print(legend_plot)
# 提取图例
# 创建一个函数来提取图例
library(gridExtra)
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
} 
# 提取图例
legend_only <- g_legend(legend_plot)


library(grid)
final_plot <- function() {
  # 绘制主图
  print(p, newpage = FALSE)
   # 在中心位置绘制图例
  pushViewport(viewport(x = 0.5, y = 0.5, width = 0.4, height = 0.1))
  grid.draw(legend_only)
  popViewport()
}

final_plot()

# --- 2. Create the Legend Data ---
legend_dataa <- data.frame(
  letter = LETTERS[1:14],
  name = c(
    "Previous Medications and Diagnoses",
    "Dietary habits",
    "Sleep problems",
    "Physiological functions",
    "Environmental factors",
    "Birth records",
    "Allergy history",
    "Genetic factors",
    "Growth indicators",
    "Sexual characteristics",
    "Clinical diagnoses",
    "Medication orders",
    "Follow-up orders",
    "Item description" # Added placeholder for N
  )
)
legend_dataa <- data.frame(
  letter = LETTERS[1:14],
  name = c(
    "既往诊断和用药史",
    "饮食习惯",
    "睡眠问题",
    "日常功能",
    "环境因素",
    "围生期",
    "过敏史",
    "遗传因素",
    "体格测量",
    "第一/二性征",
    "临床诊断",
    "药物处方",
    "其他医嘱",
    "小类名称" # Added placeholder for N
  )
)
legend_text <- paste(legend_dataa$letter[1:13], legend_dataa$name[1:13], sep = ": ") # 使用制表符对齐
legend_text <- paste(legend_text, collapse = "\n") # 合并所有行为一个字符串，用换行符分隔
# 创建一个只包含文本的 ggplot 对象
p_legend <- ggplot() +
  # --- 文本注释部分保持不变，但建议使用数值坐标 ---
  annotate(geom = "text",
           x = 0,             # 使用数值 0 作为 x 坐标
           y = 0.5,           # y 坐标 (在空图的中间)
           label = legend_text, # 要显示的文本
           hjust = 0,         # 左对齐
           vjust = 0.5,       # 垂直居中对齐
           size = 3,          # 字体大小
           lineheight = 1.6) + # 行高
  
  annotate(geom = "text",
           x = 0,             # 使用数值 0 作为 x 坐标 (与列表对齐)
           y = 0.68,           # 标题的 y 坐标 (比列表高)
           label = "小类名称", # 标题文本
           hjust = 0,         # 左对齐
           vjust = 0.5,         # 顶部对齐 (文本的顶部在 y=0.7)
           size = 3,          # 字体大小
           fontface = "bold") + # 粗体
  
  # --- 关键修改部分 ---
  
  # 1. 设置坐标系，限制范围并移除坐标轴扩展
  #    ylim 定义了文本的垂直范围。
  #    xlim 定义了水平范围，因为 hjust=0，文本从 x=0 开始向右延伸。
  #    需要设置一个足够宽的 xlim 来容纳文本，例如 c(0, 1)，具体值可能需要根据文本长度调整。
  #    expand = FALSE 是移除坐标轴两端自动添加的额外空间的关键。
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "on") +
  
  # 2. 使用 theme_void() 移除所有主题元素（坐标轴、网格线、背景等）
  theme_void() +
  
  # 3. 再次确认并移除任何可能残留的边距和背景
  theme(
    plot.margin = margin(0, 0, 0, 0),  # 移除图形外部的边距
    panel.border = element_blank(),    # 确保面板边框被移除
    plot.background = element_blank(), # 确保整个绘图区域背景是透明/空的
    panel.background = element_blank() # 确保绘图面板背景是透明/空的
  )

# 显示图形
print(p_legend)
p_legend
library(patchwork)
p+ inset_element(legend_only, left = 0.5, bottom = 0.5, right = 0.5, top = 0.5)+p_legend####18*18
# --- 4. Combine the Plots ---
# Adjust widths as needed: e.g., c(3, 1) means main plot is 3x wider than legend
final_plot_gridextra <- grid.arrange(final_plot(), p_legend, ncol = 2, widths = c(3, 1))
final_plot_gridextra


print(p, newpage = FALSE)
# 在中心位置绘制图例
pushViewport(viewport(x = 1, y = 0.5, width = 0.4, height = 0.1))
grid.draw(p_legend)
popViewport()


#配色
custom_colors <- c(
  "#F7FCB9",  # 小于70 - 浅黄色
  "#ADDD8E",  # 70~80 - 黄绿色
  "#31A354",  # 80~90 - 中等绿色
  "#006837"   # 大于90 - 深绿色
)
