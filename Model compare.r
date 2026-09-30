# 加载所需库
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(fmsb)      # 用于雷达图 (For radar chart)
library(forcats)   # 用于因子重排序 (For factor reordering - used for features now)
library(viridis)   # 用于色盲友好的颜色方案 (For colorblind-friendly color palettes)
library(patchwork) # 可选，用于组合ggplot图形 (Optional, for combining ggplot plots)
# library(gridExtra) # 备选，用于组合图形 (Alternative, for combining plots)
library(showtext)
showtext_auto()
# --- 数据准备 (Data Preparation) ---

# 定义模型名称，确保一致性 (Define model names for consistency)
# **** 使用用户指定的顺序 (Using user-specified order) ****
model_names <- c("GLM4:9B", "Qwen2:7B", "Yi:6B", "Llama3.1:8B", "Gemma2:9B")

# 1. 创建模型总体性能数据框
# 1. Create DataFrame for Overall Model Performance
overall_df <- data.frame(
  # **** 强制使用指定的因子水平顺序 (Force the specified factor level order) ****
  model_name = factor(model_names, levels = model_names),
  accuracy = c(97.15, 84.46, 91.78, 82.89, 82.17),
  precision = c(98.31, 98.63, 94.47, 94.07, 96.36),
  recall = c(98.16, 81.86, 95.39, 84.09, 80.95),
  f1 = c(98.23, 89.47, 94.93, 88.80, 87.99)
)

# 转换为长格式，便于ggplot2使用
# Pivot to long format for ggplot2
overall_long_df <- overall_df %>%
  pivot_longer(cols = -model_name,
               names_to = "metric",
               values_to = "value") %>%
  # 定义评估指标的因子水平和标签 (Define factor levels and labels for metrics)
  mutate(metric = factor(metric, levels = c("accuracy", "precision", "recall", "f1"),
                         labels = c("准确率", "精确率", "召回率", "F1分数")))
# 注意：model_name 的因子顺序已在 overall_df 中设定好，这里会继承
# Note: The factor order for model_name is already set in overall_df and inherited here

# 2. 创建各特征F1值比较数据框
# 2. Create DataFrame for Feature-wise F1 Score Comparison
feature_f1_data_long <- data.frame(
  feature = rep(c('零食情况', '饮料情况', '豆浆情况', '蜂蜜情况', '保健品服用情况',
                  '补品服用情况', '睡眠质量', '是否常开灯睡觉', '运动情况', '母亲初潮年龄'), 5),
  model = rep(c('GLM4:9B', 'Qwen2:7B', 'Yi:6B', 'Llama3.1:8B', 'Gemma2:9B'), each = 10),
  f1_score = c(97.64, 95.03, 99.71, 99.90, 97.54, 96.19, 99.22, 99.66, 97.10, 99.54,
               97.37, 85.27, 84.01, 82.08, 82.80, 80.84, 98.51, 88.47, 95.80, 97.38,
               95.50, 94.75, 94.14, 96.67, 94.36, 91.08, 98.51, 95.01, 88.20, 96.85,
               94.28, 89.18, 97.13, 95.50, 75.28, 74.42, 99.14, 80.22, 77.71, 95.33,
               97.62, 94.47, 97.01, 97.18, 61.92, 60.24, 99.15, 72.86, 96.84, 97.29
               )
)
feature_f1_data_long$feature <-factor(feature_f1_data_long$feature,levels = c('零食情况', '饮料情况', '豆浆情况', '蜂蜜情况', '保健品服用情况',
                                                                              '补品服用情况', '睡眠质量', '是否常开灯睡觉', '运动情况', '母亲初潮年龄'))
# 3. 创建雷达图数据
# 3. Create data for Radar Chart
# 直接使用 overall_df 的数据 (Use data directly from overall_df)
# **** 由于 overall_df 的 model_name 已经是按指定顺序的因子，这里的行顺序是正确的 ****
# **** Since model_name in overall_df is already a factor with the specified order, the row order here is correct ****
radar_metrics <- c('准确率 (Accuracy)', '精确率 (Precision)', '召回率 (Recall)', 'F1值 (F1-Score)')
radar_df_prep <- overall_df %>%
  select(model_name, accuracy, precision, recall, f1) %>%
  # 重命名以匹配标签 (Rename to match labels)
  rename(
    `准确率 (Accuracy)` = accuracy,
    `精确率 (Precision)` = precision,
    `召回率 (Recall)` = recall,
    `F1值 (F1-Score)` = f1
  )

# 转置数据并添加最大/最小值行以供fmsb使用
# Transpose data and add Max/Min rows for fmsb
# **** 转置后，列的顺序将对应原始行的顺序，即指定的模型顺序 ****
# **** After transposing, the column order will correspond to the original row order, i.e., the specified model order ****
radar_data_fmsb <- as.data.frame(t(radar_df_prep[, -1]))
colnames(radar_data_fmsb) <- radar_df_prep$model_name # 列名已经是正确的顺序 (Column names are already in the correct order)

# 确定合适的范围 (Determine appropriate range)
min_val <- floor(min(radar_data_fmsb) / 5) * 5
max_val <- ceiling(max(radar_data_fmsb) / 5) * 5
# min_val <- 70; max_val <- 100 # Or use fixed range

radar_data_fmsb <- rbind(rep(max_val, ncol(radar_data_fmsb)),
                         rep(min_val, ncol(radar_data_fmsb)),
                         radar_data_fmsb)


# --- 图表绘制 (Plotting) ---

# 定义统一的颜色方案 (Define a consistent color scheme)
num_models <- length(model_names)
model_colors <- RColorBrewer::brewer.pal(n = num_models, name = "Dark2")
# **** 确保颜色名称与模型名称精确对应 (Ensure color names exactly match model names) ****

names(model_colors) <- model_names

num_metrics <- length(levels(overall_long_df$metric)) # Should be 4
metric_labels <- levels(overall_long_df$metric)      # Get the actual labels used

# Use the "Dark2" qualitative palette for the 4 metrics
metric_colors_dark2 <- RColorBrewer::brewer.pal(n = num_metrics, name = "Dark2")
names(metric_colors_dark2) <- metric_labels

# 设置统一的主题风格 (Set a consistent theme style)
academic_theme <- theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = rel(1.2)),
    axis.title = element_text(face = "bold", size = rel(1.1)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.0),colour = "black"),
    axis.text.y = element_text(size = rel(1.0),colour = "black"),
    legend.title = element_text(face = "bold", size = rel(1.0)),
    legend.text = element_text(size = rel(0.8)),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, face = "italic"),
    strip.text = element_text(face = "bold", size=rel(1.0)),
    panel.grid.minor = element_blank()
  )

# 1. 模型总体性能柱状图 (Bar Chart for Overall Model Performance)
#    **** 移除按性能重排序模型的代码，使用数据框中 model_name 的因子顺序 ****
#    **** Remove code that reorders models by performance; use the factor order of model_name from the DataFrame ****
plot1_overall <- ggplot(overall_long_df,
                        # X axis uses ordered model_name factor
                        # Fill color is determined by the metric factor
                        aes(x = model_name, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  # **** Use scale_fill_manual with the "Dark2" colors ****
  scale_fill_manual(values = metric_colors_dark2, # Apply the Dark2 colors
                    name = "评价指标",     # Legend title
                    breaks = metric_labels         # Ensure legend order matches factor levels
  ) +
  # **** Use only ONE coord_cartesian call for y-axis limits ****
  # Set Y-axis limits (e.g., 70 to 100, or dynamically based on min/max)
  coord_cartesian(ylim = c(70, 100.5)) + # Added slight buffer at top for labels
  # Add labels and title
  labs(
    title = "a.各大语言模型总体性能指标比较",
    x = "大语言模型",
    y = "评价指标(%)"
  ) +
  # Add value labels on top of bars
  geom_text(aes(label = sprintf("%.1f", value)),
            position = position_dodge(width = 0.8), # Match bar dodge width
            vjust = -0.4,                           # Adjust vertical position slightly above bar
            size = 3) +                             # Adjust text size
  # Apply the theme
  academic_theme
# Note: The theme already sets axis.text.x angle to 30 degrees

# --- Display the plot ---
print(plot1_overall)

# 2. 各特征 F1 值点图 (Dot Plot for Feature-wise F1 Scores)
#    **** 模型颜色和分组将遵循 model_name 的因子顺序 ****
#    **** Model colors and grouping will follow the factor order of model_name ****
#    按特征字母顺序排序 (Order features alphabetically)
plot2_feature_f1 <- ggplot(feature_f1_data_long,
                           # x uses default feature order (alphabetical)
                           # color/grouping uses the ORDERED factor levels of 'model'
                           aes(x = feature, y = f1_score, color = model, group = model)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3, alpha = 1) + # Points overlay lines
  # Apply the manually defined colors
  # Use 'breaks' to explicitly enforce legend order
  scale_color_manual(values = model_colors,
                     name = "大语言模型",   # Legend title
                     breaks = model_names     # ENSURE legend order matches model_names
  ) +
  coord_cartesian(ylim = c(55, 100)) +
  labs(
    title = "b.各大语言模型在性早熟危险因素上的F1分数",
    x = "危险因素",
    y = "F1分数"
  ) +
  academic_theme

# --- Display the plot ---
print(plot2_feature_f1)

plot1_overall/plot2_feature_f1

# 3. 模型综合表现雷达图 (Radar Chart for Overall Model Profile)
#    **** fmsb 将按 radar_data_fmsb 的列顺序绘制，该顺序已设为 model_names 顺序 ****
#    **** fmsb will plot according to the column order of radar_data_fmsb, which is already set to the model_names order ****
# 定义雷达图颜色 (Define radar chart colors - use the same as ggplot)
radar_colors_rgb <- col2rgb(model_colors[colnames(radar_data_fmsb)]) / 255
radar_fill_colors <- apply(radar_colors_rgb, 2, function(col) rgb(col[1], col[2], col[3], alpha=0.2))
radar_line_colors <- model_colors[colnames(radar_data_fmsb)]

# 创建雷达图
par(mar=c(1, 1, 2, 1), mfrow=c(1,1))
radarchart(
  radar_data_fmsb,
  axistype = 1,
  pcol = radar_line_colors,
  pfcol = radar_fill_colors,
  plwd = 2,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey40",
  caxislabels = pretty(c(min_val, max_val)),
  cglwd = 0.8,
  vlcex = 0.9,
  title = "模型综合性能雷达图 (Radar Chart of Overall Model Performance)"
)

# 添加图例
# **** 图例标签将按 colnames(radar_data_fmsb) 的顺序，即 model_names 顺序 ****
# **** Legend labels will follow the order of colnames(radar_data_fmsb), i.e., model_names order ****
legend(
  x = "bottom",
  legend = colnames(radar_data_fmsb), # 使用列名作为图例标签 Use column names as legend labels
  bty = "n",
  fill = radar_fill_colors,
  col = radar_line_colors,
  seg.len = 1.5,
  cex = 0.8,
  horiz = TRUE
)


# 4. 各模型在不同特征上的F1值热图 (Heatmap of Model F1-Scores Across Features)
#    **** 移除按模型平均 F1 值排序的代码，使用 model_name 的因子顺序 ****
#    **** Remove code ordering by model mean F1; use the factor order of model_name ****

heatmap_data <- feature_f1_long_df %>%
  # 计算每个特征的平均 F1 (Calculate mean F1 for each feature - for feature ordering)
  group_by(feature) %>%
  mutate(feature_mean_f1 = mean(f1_score)) %>%
  ungroup() %>%
  # 重排序因子 (Reorder factors)
  mutate(
    # **** model_name 不再重排序，将使用来自 feature_f1_long_df 的原始因子顺序 ****
    # **** model_name is no longer reordered, will use the original factor order from feature_f1_long_df ****
    # model_name = fct_reorder(model_name, model_mean_f1, .desc = TRUE), # <- 移除此行 Removed this line
    feature = fct_reorder(feature, feature_mean_f1, .desc = TRUE)     # 特征排序保留 (Feature ordering kept)
    # 如果特征也不想排序，可以改成 feature = factor(feature) 或 fct_reorder(feature, feature) 按字母排序
    # If feature ordering is not desired either, change to feature = factor(feature) or fct_reorder(feature, feature) for alphabetical
  )

plot4_heatmap <- ggplot(heatmap_data, aes(x = feature, y = model_name, fill = f1_score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(option = "plasma", name = "F1值 (%) (F1-Score (%))", limits = c(min(feature_f1_long_df$f1_score, na.rm = TRUE), 100)) + # Handle potential NAs
  geom_text(aes(label = sprintf("%.1f", f1_score)), color = "white", size = 3) +
  # **** 使用 scale_y_discrete 指定 breaks 来强制 Y 轴顺序 ****
  # **** Use scale_y_discrete specifying breaks to force Y-axis order ****
  scale_y_discrete(limits = rev(model_names)) + # 热图 Y 轴通常从上到下，所以用 rev() 反转顺序
  # Heatmap Y-axis usually goes top-down, so use rev() to reverse the order
  labs(
    title = "各模型在不同特征上的 F1 值热图 (Heatmap of Model F1-Scores Across Features)",
    x = "特征 (Feature) (按平均F1值降序排列)",
    y = "模型 (Model)", # Y 轴现在按指定顺序排列 (Y-axis now in specified order)
    caption = "颜色越深表示 F1 值越高 (Darker color indicates higher F1-Score)"
  ) +
  academic_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1), # 确保 Y 轴标签水平 Ensure Y-axis labels are horizontal
    legend.position = "right"
  )


# --- 显示或保存图形 (Display or Save Plots) ---

# 显示所有图形 (Display all plots)
print(plot1_overall)
# Sys.sleep(1) # 如果需要，在绘图间添加暂停 Add pause between plots if needed
print(plot2_feature_f1)
# Sys.sleep(1)
# 雷达图已在上面绘制，如果需要重新绘制：
# Radar chart plotted above, if replotting is needed:
# par(mar=c(1, 1, 2, 1), mfrow=c(1,1))
# radarchart(...)
# legend(...)
# Sys.sleep(1)
print(plot4_heatmap)

# 保存代码不变 (Saving code remains the same)
# ggsave("plot1_overall_performance_fixed_order.png", plot = plot1_overall, width = 8, height = 6, dpi = 300)
# ggsave("plot2_feature_f1_fixed_order.png", plot = plot2_feature_f1, width = 10, height = 6, dpi = 300)
# # 保存雷达图
# png("plot3_radar_chart_fixed_order.png", width = 7, height = 7, units = "in", res = 300)
# par(mar=c(1, 1, 2, 1), mfrow=c(1,1))
# radarchart(...) # 粘贴上面的 radarchart 代码 Paste radarchart code from above
# legend(...)   # 粘贴上面的 legend 代码 Paste legend code from above
# dev.off()
# ggsave("plot4_heatmap_fixed_order.png", plot = plot4_heatmap, width = 10, height = 7, dpi = 300)


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(fmsb)      # For radar chart
library(forcats)   # For factor reordering
library(viridis)   # For colorblind-friendly color palettes
library(patchwork) # Optional, for combining ggplot plots
# library(gridExtra) # Alternative, for combining plots
library(showtext)
showtext_auto()

# --- Data Preparation ---

# Define model names, ensuring consistency
# Using user-specified order
model_names <- c("GLM4:9B", "Qwen2:7B", "Yi:6B", "Llama3.1:8B", "Gemma2:9B")

# 1. Create DataFrame for Overall Model Performance
overall_df <- data.frame(
  # Force the specified factor level order
  model_name = factor(model_names, levels = model_names),
  accuracy   = c(97.15, 84.46, 91.78, 82.89, 82.17),
  precision  = c(98.31, 98.63, 94.47, 94.07, 96.36),
  recall     = c(98.16, 81.86, 95.39, 84.09, 80.95),
  f1         = c(98.23, 89.47, 94.93, 88.80, 87.99)
)

# Pivot to long format for ggplot2
overall_long_df <- overall_df %>%
  pivot_longer(cols = -model_name,
               names_to  = "metric",
               values_to = "value") %>%
  # Define factor levels and labels for evaluation metrics
  mutate(metric = factor(metric,
                         levels = c("accuracy", "precision", "recall", "f1"),
                         labels = c("Accuracy", "Precision", "Recall", "F1-Score")))
# Note: The factor order for model_name is already set in overall_df and inherited here

# 2. Create DataFrame for Feature-wise F1 Score Comparison
feature_f1_data_long <- data.frame(
  feature = rep(c("Snack", "Beverage", "SoyMilk",
                  "Honey", "Health supplements", "Tonic food",
                  "Sleep quality", "Sleeping with light", "Exercise",
                  "Maternal age of menarche"), 5),
  model = rep(c("GLM4:9B", "Qwen2:7B", "Yi:6B", "Llama3.1:8B", "Gemma2:9B"), each = 10),
  f1_score = c(
    97.64, 95.03, 99.71, 99.90, 97.54, 96.19, 99.22, 99.66, 97.10, 99.54,
    97.37, 85.27, 84.01, 82.08, 82.80, 80.84, 98.51, 88.47, 95.80, 97.38,
    95.50, 94.75, 94.14, 96.67, 94.36, 91.08, 98.51, 95.01, 88.20, 96.85,
    94.28, 89.18, 97.13, 95.50, 75.28, 74.42, 99.14, 80.22, 77.71, 95.33,
    97.62, 94.47, 97.01, 97.18, 61.92, 60.24, 99.15, 72.86, 96.84, 97.29
  )
)

feature_f1_data_long$feature <- factor(
  feature_f1_data_long$feature,
  levels = c("Snack", "Beverage", "SoyMilk",
             "Honey", "Health supplements", "Tonic food",
             "Sleep quality", "Sleeping with light", "Exercise",
             "Maternal age of menarche")
)

# 3. Create data for Radar Chart
# Use data directly from overall_df
# Since model_name in overall_df is already a factor with the specified order,
# the row order here is correct
radar_metrics <- c("Accuracy", "Precision", "Recall", "F1-Score")

radar_df_prep <- overall_df %>%
  select(model_name, accuracy, precision, recall, f1) %>%
  # Rename columns to match labels
  rename(
    "Accuracy"  = accuracy,
    "Precision" = precision,
    "Recall"    = recall,
    "F1-Score"  = f1
  )

# Transpose data and add Max/Min rows for fmsb
# After transposing, the column order corresponds to the original row order (specified model order)
radar_data_fmsb <- as.data.frame(t(radar_df_prep[, -1]))
colnames(radar_data_fmsb) <- radar_df_prep$model_name  # Column names are already in the correct order

# Determine appropriate range
min_val <- floor(min(radar_data_fmsb) / 5) * 5
max_val <- ceiling(max(radar_data_fmsb) / 5) * 5
# min_val <- 70; max_val <- 100  # Or use a fixed range

radar_data_fmsb <- rbind(
  rep(max_val, ncol(radar_data_fmsb)),
  rep(min_val, ncol(radar_data_fmsb)),
  radar_data_fmsb
)


# --- Plotting ---

# Define a consistent color scheme
num_models  <- length(model_names)
model_colors <- RColorBrewer::brewer.pal(n = num_models, name = "Dark2")
# Ensure color names exactly match model names
names(model_colors) <- model_names

num_metrics   <- length(levels(overall_long_df$metric))  # Should be 4
metric_labels <- levels(overall_long_df$metric)          # Get the actual labels used

# Use the "Dark2" qualitative palette for the 4 metrics
metric_colors_dark2 <- RColorBrewer::brewer.pal(n = num_metrics, name = "Dark2")
names(metric_colors_dark2) <- metric_labels

# Set a consistent academic theme style
academic_theme <- theme_bw(base_size = 10) +
  theme(
    plot.title      = element_text(hjust = 0, face = "bold", size = rel(1.2)),
    axis.title      = element_text(face = "bold", size = rel(1.1)),
    axis.text.x     = element_text(angle = 45, hjust = 1, size = rel(1.0), colour = "black"),
    axis.text.y     = element_text(size = rel(1.0), colour = "black"),
    legend.title    = element_text(face = "bold", size = rel(1.0)),
    legend.text     = element_text(size = rel(0.8)),
    legend.position = "bottom",
    plot.caption    = element_text(hjust = 0, face = "italic"),
    strip.text      = element_text(face = "bold", size = rel(1.0)),
    panel.grid.minor = element_blank()
  )

# 1. Bar Chart for Overall Model Performance
# The factor order of model_name in the DataFrame is used directly (no reordering by performance)
plot1_overall <- ggplot(overall_long_df,
                        aes(x = model_name, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  # Apply the Dark2 colors
  scale_fill_manual(values = metric_colors_dark2,
                    name   = "Evaluation Metric",
                    breaks = metric_labels          # Ensure legend order matches factor levels
  ) +
  # Set Y-axis limits with slight buffer at top for labels
  coord_cartesian(ylim = c(70, 100.5)) +
  labs(
    title = "a. Overall Performance Metrics Comparison Across Large Language Models",
    x     = "Large Language Model",
    y     = "Metric Value (%)"
  ) +
  # Add value labels on top of bars
  geom_text(aes(label = sprintf("%.1f", value)),
            position = position_dodge(width = 0.8),  # Match bar dodge width
            vjust    = -0.4,                          # Slightly above bar
            size     = 3) +
  academic_theme

print(plot1_overall)

# 2. Line-Dot Plot for Feature-wise F1 Scores
# Model colors and grouping follow the factor order of model_name
plot2_feature_f1 <- ggplot(feature_f1_data_long,
                           aes(x = feature, y = f1_score, color = model, group = model)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3, alpha = 1) +
  # Apply manually defined colors; use 'breaks' to enforce legend order
  scale_color_manual(values = model_colors,
                     name   = "Large Language Model",
                     breaks = model_names
  ) +
  coord_cartesian(ylim = c(55, 100)) +
  labs(
    title = "b. F1-Scores of Large Language Models on Precocious Puberty Risk Factors",
    x     = "Risk Factor",
    y     = "F1-Score (%)"
  ) +
  academic_theme

print(plot2_feature_f1)

# Combined plot (plot1 over plot2)
plot1_overall / plot2_feature_f1

# 3. Radar Chart for Overall Model Performance Profile
# fmsb plots according to the column order of radar_data_fmsb,
# which is already set to the model_names order

# Define radar chart colors (same palette as ggplot)
radar_colors_rgb  <- col2rgb(model_colors[colnames(radar_data_fmsb)]) / 255
radar_fill_colors <- apply(radar_colors_rgb, 2, function(col) rgb(col[1], col[2], col[3], alpha = 0.2))
radar_line_colors <- model_colors[colnames(radar_data_fmsb)]

# Draw radar chart
par(mar = c(1, 1, 2, 1), mfrow = c(1, 1))
radarchart(
  radar_data_fmsb,
  axistype    = 1,
  pcol        = radar_line_colors,
  pfcol       = radar_fill_colors,
  plwd        = 2,
  plty        = 1,
  cglcol      = "grey",
  cglty       = 1,
  axislabcol  = "grey40",
  caxislabels = pretty(c(min_val, max_val)),
  cglwd       = 0.8,
  vlcex       = 0.9,
  title       = "Radar Chart of Overall Model Performance"
)

# Add legend
# Legend labels follow the order of colnames(radar_data_fmsb), i.e., model_names order
legend(
  x      = "bottom",
  legend = colnames(radar_data_fmsb),  # Use column names as legend labels
  bty    = "n",
  fill   = radar_fill_colors,
  col    = radar_line_colors,
  seg.len = 1.5,
  cex    = 0.8,
  horiz  = TRUE
)

# 4. Heatmap of Model F1-Scores Across Features
# The factor order of model_name is used directly (no reordering by mean F1)

heatmap_data <- feature_f1_data_long %>%
  rename(model_name = model) %>%
  # Apply specified factor order to model_name
  mutate(model_name = factor(model_name, levels = model_names)) %>%
  # Calculate mean F1 per feature for feature ordering
  group_by(feature) %>%
  mutate(feature_mean_f1 = mean(f1_score)) %>%
  ungroup() %>%
  mutate(
    # model_name retains the specified factor order (not reordered by performance)
    feature = fct_reorder(feature, feature_mean_f1, .desc = TRUE)  # Features ordered by mean F1
    # To use alphabetical feature order instead:
    # feature = factor(feature)
  )

plot4_heatmap <- ggplot(heatmap_data, aes(x = feature, y = model_name, fill = f1_score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(
    option = "plasma",
    name   = "F1-Score (%)",
    limits = c(min(feature_f1_data_long$f1_score, na.rm = TRUE), 100)
  ) +
  geom_text(aes(label = sprintf("%.1f", f1_score)), color = "white", size = 3) +
  # Use scale_y_discrete with rev() so Y-axis reads top-to-bottom in specified order
  scale_y_discrete(limits = rev(model_names)) +
  labs(
    title   = "Heatmap of Model F1-Scores Across Risk Factor Features",
    x       = "Feature (sorted by descending mean F1-Score)",
    y       = "Model",
    caption = "Darker color indicates a higher F1-Score"
  ) +
  academic_theme +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    axis.text.y     = element_text(angle = 0,  hjust = 1),  # Horizontal Y-axis labels
    legend.position = "right"
  )

# --- Display all plots ---
print(plot1_overall)
print(plot2_feature_f1)
# Radar chart is already rendered above; re-run the radarchart() and legend() blocks if needed.
print(plot4_heatmap)

# --- Save plots (uncomment as needed) ---
# ggsave("plot1_overall_performance.png", plot = plot1_overall,   width = 8,  height = 6,  dpi = 300)
# ggsave("plot2_feature_f1.png",          plot = plot2_feature_f1, width = 10, height = 6,  dpi = 300)
# # Save radar chart:
# png("plot3_radar_chart.png", width = 7, height = 7, units = "in", res = 300)
# par(mar = c(1, 1, 2, 1), mfrow = c(1, 1))
# radarchart(...)  # Paste radarchart code from above
# legend(...)      # Paste legend code from above
# dev.off()
# ggsave("plot4_heatmap.png", plot = plot4_heatmap, width = 10, height = 7, dpi = 300)