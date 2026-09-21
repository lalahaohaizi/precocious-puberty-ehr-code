# --- 1. 加载所需的 R 包 ---
library(readxl)       # 用于读取 Excel 文件 (.xlsx)
library(dplyr)        # 用于数据处理和转换 (如 mutate, %>%, filter 等)
library(ggplot2)      # 核心绘图库
library(RColorBrewer) # 提供颜色方案 (brewer.pal)
library(patchwork)    # 用于轻松组合多个 ggplot 图形
library(psych)
library(corrr)
library(linkET)
library(ggtext)
library(showtext)

# --- 2. 加载数据 ---
tryCatch({
  df0 <- read_xlsx(".xlsx")
  df <- read_xlsx(".xlsx")
  df2 <- read_xlsx(".xlsx")
  df3 <- read_xlsx(".xlsx")
  
}, error = function(e) {
  stop("加载 Excel 文件时出错。请检查文件路径是否正确，并确保已安装 'readxl' 包。\n原始错误信息: ", e$message)
})

# --- 3. 定义变量分组并提取数据 ---
chem_vars <- c("Cosine", "Dice", "Levenshtein", "Jaccard")
spec_vars <- c("input length", "output length","time")

if (!all(chem_vars %in% names(df)) || !all(spec_vars %in% names(df))) {
  stop("数据框 df (体格营养D) 中缺少必要的列。请检查: ",
       paste(c(chem_vars, spec_vars)[!c(chem_vars, spec_vars) %in% names(df)], collapse=", "))
}
if (!all(chem_vars %in% names(df2)) || !all(spec_vars %in% names(df2))) {
  stop("数据框 df2 (诊断名称G) 中缺少必要的列。请检查: ",
       paste(c(chem_vars, spec_vars)[!c(chem_vars, spec_vars) %in% names(df2)], collapse=", "))
}
if (!all(chem_vars %in% names(df3)) || !all(spec_vars %in% names(df3))) {
  stop("数据框 df3 (诊治建议h) 中缺少必要的列。请检查: ",
       paste(c(chem_vars, spec_vars)[!c(chem_vars, spec_vars) %in% names(df2)], collapse=", "))
}
varechem_df0 <- data.frame(lapply(df0[, chem_vars], function(x) as.numeric(as.character(x))))
varespec_df0 <- data.frame(lapply(df0[, spec_vars], function(x) as.numeric(as.character(x))))
varechem_df <- data.frame(lapply(df[, chem_vars], function(x) as.numeric(as.character(x))))
varespec_df <- data.frame(lapply(df[, spec_vars], function(x) as.numeric(as.character(x))))
varechem_df2 <- data.frame(lapply(df2[, chem_vars], function(x) as.numeric(as.character(x))))
varespec_df2 <- data.frame(lapply(df2[, spec_vars], function(x) as.numeric(as.character(x))))
varechem_df3 <- data.frame(lapply(df3[, chem_vars], function(x) as.numeric(as.character(x))))
varespec_df3 <- data.frame(lapply(df3[, spec_vars], function(x) as.numeric(as.character(x))))
if(any(is.na(varechem_df0)) || any(is.na(varespec_df0))) warning("df 数据在转换为数值时引入了 NA。请检查原始数据。")
if(any(is.na(varechem_df)) || any(is.na(varespec_df))) warning("df 数据在转换为数值时引入了 NA。请检查原始数据。")
if(any(is.na(varechem_df2)) || any(is.na(varespec_df2))) warning("df2 数据在转换为数值时引入了 NA。请检查原始数据。")
if(any(is.na(varechem_df3)) || any(is.na(varespec_df3))) warning("df3 数据在转换为数值时引入了 NA。请检查原始数据。")


# --- 4. 计算相关性 ---
cor_chem_df0 <- correlate(varechem_df0)
cor_spec_chem_df0_res <- corr.test(varespec_df0, varechem_df0, method = "pearson")
cor_chem_df <- correlate(varechem_df)
cor_spec_chem_df_res <- corr.test(varespec_df, varechem_df, method = "pearson")
cor_chem_df2 <- correlate(varechem_df2)
cor_spec_chem_df2_res <- corr.test(varespec_df2, varechem_df2, method = "pearson")
cor_chem_df3 <- correlate(varechem_df3)
cor_spec_chem_df3_res <- corr.test(varespec_df3, varechem_df3, method = "pearson")
# --- 5. 准备用于 geom_couple 的汇总数据 ---
create_cor_summary <- function(corr_test_result) {
  if (!is.list(corr_test_result) || !all(c("r", "p") %in% names(corr_test_result))) {
    stop("输入必须是包含 'r' 和 'p' 相关性矩阵的列表 (例如 ggcor::corr.test 的输出)")
  }
  if (!is.matrix(corr_test_result$r) || !is.matrix(corr_test_result$p)) {
    stop("'r' 和 'p' 组件必须是矩阵。")
  }
  if (inherits(corr_test_result, "cor_list")) {
    cor_summary <- fortify(corr_test_result)
  } else {
    r_long <- as.data.frame.table(corr_test_result[["r"]], responseName = "r", stringsAsFactors = FALSE)
    p_long <- as.data.frame.table(corr_test_result[["p"]], responseName = "p", stringsAsFactors = FALSE)
    if (nrow(r_long) != nrow(p_long)) stop("r 和 p 表格的行数不匹配。")
    cor_summary <- cbind(r_long, p = p_long$p)
    names(cor_summary)[names(cor_summary) == "Var1"] <- ".rownames"
    names(cor_summary)[names(cor_summary) == "Var2"] <- ".colnames"
  }
  
  # 定义全局一致的因子水平 (确保顺序和内容在两个数据集中一致)
  all_rd_levels <- c("< 0.3", "0.3 - 0.7", "≥ 0.7")
  all_pd_levels <- c("p < 1e-5", "p < 0.001")
  
  cor_summary <- cor_summary %>%
    filter(.rownames %in% names(varespec_df)) %>%
    mutate(
      rd = cut(abs(r), breaks = c(-Inf, 0.3, 0.7, Inf), labels = all_rd_levels, right = FALSE),
      pd = cut(p, breaks = c(-Inf, 0.000001, 0.001), labels = all_pd_levels, right = FALSE)
    ) %>%
    # 确保因子具有所有预期的水平
    mutate(rd = factor(rd, levels = all_rd_levels),
           pd = factor(pd, levels = all_pd_levels))
  
  if(any(is.na(cor_summary$rd)) || any(is.na(cor_summary$pd))) {
    warning("在 rd 或 pd 因子中生成了 NA。请检查 r/p 值和 cut() 函数的 breaks 设置。")
  }
  return(cor_summary)
}
cor_summary_df0 <- create_cor_summary(cor_spec_chem_df0_res)
cor_summary_df <- create_cor_summary(cor_spec_chem_df_res)
cor_summary_df2 <- create_cor_summary(cor_spec_chem_df2_res)
cor_summary_df3 <- create_cor_summary(cor_spec_chem_df3_res)

cor_summary_df
cor_summary_df2
cor_summary_df3 
# --- 6. 创建单个图形 (使用标准化的标度) ---

# 定义全局一致的因子水平 (重复定义以确保在函数作用域内可见)
ALL_RD_LEVELS <- c("< 0.3", "0.3 - 0.7", "≥ 0.7")
ALL_PD_LEVELS <- c("p < 1e-5", "p < 0.001")

# 定义颜色和大小映射 (全局一致)
link_color_pal <- function(n_levels) {
  brewer.pal(n = max(3, n_levels), name = "Blues")
}

# 创建图形的函数 (修改了标度部分)
# 添加 Arial 和宋体
font_add("arial", "arial.ttf") 
font_add("SimSun", "simsun.ttc")  # Windows 系统上的宋体
showtext_auto()
font_families()
# 如果是 Mac 系统，路径可能不同
# font_add("Arial", "/Library/Fonts/Arial.ttf")
# font_add("SimSun", "/Library/Fonts/SimSun.ttf")

# 修改 create_complex_corr_plot 函数，应用新的字体设置
create_complex_corr_plot <- function(chem_cor_data, spec_chem_summary, title) {
  
  # --- 绘图基础部分 (大部分保持不变) ---
  p <- qcorrplot(chem_cor_data, type = "lower", diag = FALSE) +
    geom_square() +
    geom_couple(aes(colour = pd, size = rd),
                data = spec_chem_summary,
                curvature = nice_curvature(0.15),
                drop = TRUE,
                node.colour = c("#984EA3", "#5785C1"),
                node.fill = c("#984EA3","#5785C1"),
                node.size = c(4,4),
                node.shape=c(20,20),
                nudge_x = 0.15)  +
    geom_mark(size=2.5,mark = c(""))+
    scale_fill_gradientn(colours = custom_colors ,
                         limits = c(0.28, 1), name = "Pearson's r\n(within Chem)") +
    scale_size_manual(
      name = "Strength |r|\n(Spec vs Chem)",
      values = c(0.25, 0.5, 1),
      limits = ALL_RD_LEVELS,
      breaks = ALL_RD_LEVELS,
      labels = ALL_RD_LEVELS,
      drop = FALSE) +
    scale_colour_manual(
      name = "p-value",
      values = color_pal(4),
      limits = ALL_PD_LEVELS,
      breaks = ALL_PD_LEVELS,
      labels = ALL_PD_LEVELS,
      drop = FALSE) +
    ggtitle(title) +
    guides(size = guide_legend(title = "Pearson's r", override.aes = list(colour = "grey35"), order = 2),
           colour = guide_legend(title = "p-value", override.aes = list(size = 3), order = 1),
           fill = guide_colorbar(title = "Pearson's r", order = 3))  +
    theme(plot.margin = unit(c(0, 0, 0, 1), "cm"),  # Increased right margin to 2 cm
          axis.text=element_markdown(color="black",size=10, family="arial"),  # 特别为此元素设置字体
          plot.title = element_text(size = 10,face = "bold"))    # 特别为标题设置宋体

  
  return(p)
}
custom_colors <- c(
  "#ABD09D",  # 小于70 - 柔和粉红色 (表示较差性能)
  "#E1EA95",  # 70~80 - 柔和橙色 (表示一般性能)
  "#FCDC89",  # 80~90 - 柔和黄绿色 (表示良好性能)
  "#E16843"   # 大于90 - 柔和绿色 (表示优秀性能)
)
# --- 重新生成图形 ---
plot_df0 <- create_complex_corr_plot(cor_chem_df0, cor_summary_df0, "A. Medical history")
plot_df0
plot_df <- create_complex_corr_plot(cor_chem_df, cor_summary_df, "B. Physical examination")
plot_df
plot_df2 <- create_complex_corr_plot(cor_chem_df2, cor_summary_df2, "C. Clinical diagnosis")
plot_df2
plot_df3 <- create_complex_corr_plot(cor_chem_df3, cor_summary_df3, "D. Medical order")
plot_df3

# 1. 首先提取第一个图的图例
legend_plot <- cowplot::get_legend(
  plot_df + 
    theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.key.size = unit(0.8, "cm"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9)
    )
)
legend_plot
# 2. 移除所有图的图例并调整边距
plot_df0_no_legend <- plot_df0 + 
  guides(size = "none", colour = "none", fill = "none") 
plot_df0_no_legend
plot_df_no_legend <- plot_df + 
  guides(size = "none", colour = "none", fill = "none")

plot_df2_no_legend <- plot_df2 + 
  guides(size = "none", colour = "none", fill = "none") 

plot_df3_no_legend <- plot_df3 + 
  guides(size = "none", colour = "none", fill = "none") 
library(gridExtra)
P<-plot_df0_no_legend+plot_df_no_legend+plot_df2_no_legend+plot_df3_no_legend 
P|legend_plot
plot_df0_no_legend +plot_df+plot_df2_no_legend +plot_df3_no_legend + plot_layout(ncol = 2, guides = "collect")
# 3. 使用 cowplot 包组合图表和图例，减少图之间的空间
library(cowplot)

# 组合所有图表（垂直排列），减少间距
plots_combined <- cowplot::plot_grid(
  plot_df0_no_legend,
  plot_df_no_legend, 
  plot_df2_no_legend, 
  plot_df3_no_legend, 
  ncol = 2,
  align = "l"
)


plots_combined
# 将图表和图例组合起来
final_plot <- cowplot::plot_grid(
  plots_combined, 
  legend_plot, 
  ncol = 2, 
  rel_widths = c(4, 1),
  align = "l",
  greedy = TRUE,
  hjust = -0.5  # Add this line to bring columns closer
)
# 4. 显示最终图
final_plot

# 1. 定义要分析的数据框列表
dataframes_list <- list(df = df, df0 = df0, df2 = df2, df3 = df3)

# 2. 定义要分析的列名
# 2. 定义要分析的 Mean ± SD 的列名
target_columns_mean_sd <- c("input length", "output length", "time", "Cosine", "Dice", "Levenshtein", "Jaccard")

# 3. 编写函数计算均值和标准差 (代码同前)
calculate_stats <- function(data, columns) {
  results_list <- list()
  for (col in columns) {
    # Ensure the column exists before trying to access it
    if (col %in% colnames(data)) {
      # Use backticks if column names have spaces or special chars
      col_data <- data[[col]]
      results_list[[col]] <- data.frame(
        Mean = mean(col_data, na.rm = TRUE),
        SD = sd(col_data, na.rm = TRUE)
      )
    } else {
      results_list[[col]] <- data.frame(Mean = NA, SD = NA)
      warning(paste("Column '", col, "' not found in dataframe: ", deparse(substitute(data)), sep=""))
    }
  }
  return(results_list)
}


# 4. 使用 lapply 循环处理数据框列表，计算 Mean ± SD
all_stats <- lapply(dataframes_list, calculate_stats, columns = target_columns_mean_sd)

# 5. 创建最终的汇总 dataframe
# Initialize list to store results row-wise
summary_list_rows <- list()

# Loop through each dataframe's results
for (df_name in names(all_stats)) {
  dataset_results <- all_stats[[df_name]]
  row_values <- c() # Initialize a named vector for the current row
  
  # --- Calculate Mean ± SD for target columns ---
  for (col_name in target_columns_mean_sd) {
    stats_df <- dataset_results[[col_name]]
    
    # Check for NA results (e.g., column missing or sd undefined)
    if (is.na(stats_df$Mean) || is.na(stats_df$SD)) {
      formatted_string <- "NA"
    } else {
      # Format the mean ± sd string (adjust rounding as needed)
      formatted_string <- sprintf("%.2f ± %.2f", stats_df$Mean, stats_df$SD)
    }
    # Add the formatted string to the row vector, named by the column name
    row_values[col_name] <- formatted_string
  }
  
  # --- Calculate Sum of Time separately ---
  current_df <- dataframes_list[[df_name]] # Get the original dataframe
  if ("time" %in% colnames(current_df)) {
    total_time <- sum(current_df[["time"]], na.rm = TRUE)
    # Format the sum (e.g., to 2 decimal places)
    formatted_sum_string <- sprintf("%.2f", total_time)
  } else {
    formatted_sum_string <- "NA" # Handle case where 'time' column is missing
    warning(paste("'time' column not found in dataframe:", df_name))
  }
  # Add the formatted sum to the row vector with a specific name
  row_values["Total Time"] <- formatted_sum_string
  
  
  # Add the completed row vector (including Mean±SD and Total Time) to the main list
  summary_list_rows[[df_name]] <- row_values
}

# Combine the list of row vectors into a dataframe
final_summary_df <- as.data.frame(do.call(rbind, summary_list_rows))

# Optional: Reorder columns if desired (e.g., put Total Time at the end)
# Identify the original Mean/SD columns and the new Total Time column
mean_sd_cols <- target_columns_mean_sd
all_cols <- c(mean_sd_cols, "Total Time")
# Ensure all expected columns are actually present before reordering
cols_to_keep <- intersect(all_cols, colnames(final_summary_df))
final_summary_df <- final_summary_df[, cols_to_keep, drop = FALSE]


# Print the final dataframe
print(final_summary_df)
write.xlsx(final_summary_df, "C:/Users/64291/Desktop/final_summary_df_文本分析.xlsx",rowNames=TRUE)


cor_summary_df0$Dataset <- "Medical history" # Or choose a more descriptive name if you like
cor_summary_df$Dataset  <- "Physical examination"
cor_summary_df2$Dataset <- "Clinical diagnosis"
cor_summary_df3$Dataset <- "Medical orders"

# 2. Combine the dataframes using rbind
combined_cor_summary <- rbind(cor_summary_df0, cor_summary_df, cor_summary_df2, cor_summary_df3)
write.xlsx(combined_cor_summary , "C:/Users/64291/Desktop/final_summary_df_文本分析相关性.xlsx",rowNames=TRUE)
