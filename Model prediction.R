# --- 0. 准备工作 ---

# 加载必要的 R 包
# install.packages(c("glmnet", "caret", "pROC", "boot", "ggplot2", "dplyr", "probably", "Metrics", "Matrix")) # 如果没安装，需要取消注释并运行来安装
library(glmnet)     # 用于 LASSO 回归
library(caret)      # 用于数据分割 (createDataPartition) 和混淆矩阵 (confusionMatrix)
library(pROC)       # 用于计算和绘制 ROC 曲线及 AUC
library(boot)       # 用于执行自助法 (boot)
library(ggplot2)    # 用于绘制图形 (如 ROC 曲线, 校准图, 直方图)
library(dplyr)      # 用于数据处理 (管道操作 %>%)
library(probably)   # 用于绘制校准图 (cal_plot_breaks)
library(Metrics)    # 用于计算 Brier 分数 (brier)
library(Matrix)     # Matrix包，用于处理稀疏矩阵，model.matrix 可能生成

# 假设 'final_data' 是你的数据框，已加载并准备好
# final_data <- read.csv("your_data.csv") # 你加载数据的方式

# 定义包含 *所有* 潜在预测变量的 *完整* 模型公式
formula_full_str <- "复诊标志 ~ 年龄 + 性别 + 省+市 + 治疗标志 + 医嘱复诊标志代码+LH基础值 + LH_FSH基础值 + Tanner分期代码 + 雌二醇E2检验结果+bmi_zscore"
formula_full_obj <- as.formula(formula_full_str) # 转换成公式对象

# 确保结果变量 '复诊标志' 是因子 (factor) 类型，并设置好阳性和阴性水平
positive_class_label <- "2" # 你的阳性类别标签，例如 "1" 或 "是"
negative_class_label <- "1"  # 你的阴性类别标签，例如 "0" 或 "否"
final_data$复诊标志 <- factor(final_data$复诊标志,
                              levels = c(negative_class_label, positive_class_label)) # 确保阴性在前

# 对于 glmnet 非常重要：在创建模型矩阵之前，严格处理缺失值 (NA)
# 选项 1: 如果可接受，删除包含 NA 的行 (在预测变量或结果变量中)
# final_data_clean <- na.omit(final_data[, all.vars(formula_full_obj)])
# 选项 2: 对缺失值进行插补 (例如使用 mice 包，或简单的均值/中位数插补)
# 为了演示，我们假设 final_data 是干净的，或使用 na.omit
final_data_clean <- final_data # 如果数据已干净，直接使用
na_rows <- which(!complete.cases(final_data_clean[, all.vars(formula_full_obj)])) # 找到包含NA的行索引
if (length(na_rows) > 0) {
  warning(paste("LASSO 前移除", length(na_rows), "行包含 NA 的数据。"))
  final_data_clean <- final_data_clean[complete.cases(final_data_clean[, all.vars(formula_full_obj)]), ] # 保留完整行
}
if (nrow(final_data_clean) < 50) { # 检查移除NA后是否还有足够的数据
  stop("移除 NA 后数据量不足，无法进行可靠建模。")
}

# 为 glmnet 准备数据：预测变量矩阵 (x) 和响应向量 (y)
# model.matrix 会自动为因子变量创建哑变量 (dummy variables)
# 使用完整的公式来创建包含所有潜在预测变量的矩阵
# [,-1] 移除了模型矩阵默认包含的截距列，因为 glmnet 会自己处理截距
x_full <- model.matrix(formula_full_obj, data = final_data_clean)[, -1]
# 响应变量 y (glmnet 对于二项分布可以处理因子类型的响应变量)
y_full <- final_data_clean$复诊标志

# 检查维度，确保数据准备正确
cat("LASSO 使用的数据维度:\n")
cat("预测变量矩阵 (x_full) 维度:", dim(x_full), "\n") # 行数 x 列数 (变量数)
cat("响应向量 (y_full) 长度:", length(y_full), "\n") # 应该等于 x_full 的行数


# --- 1. LASSO 变量筛选 ---
cat("\n--- 使用交叉验证进行 LASSO 变量筛选 ---\n")
set.seed(111) # 设置随机种子，保证交叉验证的分组可重复
# 使用 cv.glmnet 进行交叉验证来寻找最优的 lambda 值
cv_lasso <- cv.glmnet(x = x_full,             # 预测变量矩阵
                      y = y_full,             # 响应变量
                      family = "binomial",    # 指定逻辑回归 (二项分布)
                      alpha = 1,              # alpha=1 表示使用 LASSO (L1 正则化)；alpha=0 是岭回归(L2)
                      type.measure = "deviance",   # 交叉验证中用于评估模型性能的指标，这里选 AUC
                      # 可选项: "deviance" (默认), "class" (错分率), "mse", "mae"
                      nfolds = 10)            # 指定进行 10 折交叉验证

# 绘制交叉验证结果图 (通常是 AUC 或 Deviance vs. Log(Lambda))
plot(cv_lasso)
title("LASSO 交叉验证 (基于 AUC)", line = 2.5) # 添加标题
# --- 提取绘图所需数据 ---
cv_data <- data.frame(
  lambda = cv_lasso$lambda,         # Lambda 值序列
  cvm = cv_lasso$cvm,             # 平均交叉验证度量 (Mean CV metric)
  cvsd = cv_lasso$cvsd,           # 度量的标准差 (Standard Deviation)
  cvup = cv_lasso$cvup,           # 上界 = cvm + cvsd
  cvlo = cv_lasso$cvlo            # 下界 = cvm - cvsd
)

# 获取两个关键的 Lambda 值
lambda_min_val <- cv_lasso$lambda.min
lambda_1se_val <- cv_lasso$lambda.1se

# 找到这两个 Lambda 在数据中的对应 y 值 (cvm)，用于标注位置
cvm_at_min <- cv_data$cvm[cv_data$lambda == lambda_min_val]
cvm_at_1se <- cv_data$cvm[cv_data$lambda == lambda_1se_val]

# 找到 Y 轴的最大值，用于放置标注文本
y_max_limit <- max(cv_data$cvup, na.rm = TRUE)
y_annotation_pos <- y_max_limit * 1.01 # 稍微高于最高点

# --- 使用 ggplot2 绘制 ---
library(ggplot2)
library(scales) # 用于科学计数法标签

cv_plot_ggplot <- ggplot(cv_data, aes(x = lambda, y = cvm)) +
  
  # 添加误差棒 (类似图片中的红色竖线)
  geom_errorbar(aes(ymin = cvlo, ymax = cvup), color = "black", width = 0.025) + # width=0 移除横向帽子
  # --- 新增: 添加误差棒中心圆点 ---
  geom_point(color = "red", size = 1.5) + # 使用与线条相同的颜色，调整大小
  # 添加中心曲线 (类似图片中的蓝色曲线)
  
  # 添加标记 lambda.min (图片中的红色线, 标记为 λ_CV) 的垂直线
  geom_vline(xintercept = lambda_min_val, linetype = "solid", color = "red") +
  
  # 添加标记 lambda.1se (图片中的蓝色线, 标记为 λ_SE) 的垂直线
  geom_vline(xintercept = lambda_1se_val, linetype = "solid", color = "blue") +
  
  # 添加文本标注
  annotate("text", x = lambda_min_val+0.00005, y = y_annotation_pos-0.02, label = expression(lambda[CV]), parse = TRUE, color = "red", hjust = 0,vjust = 0, size=3.5) +
  annotate("text", x = lambda_1se_val+0.0005, y = y_annotation_pos-0.02, label = expression(lambda[SE]), parse = TRUE, color = "blue", hjust = 0, vjust = 0, size=3.5) +
  
  # 设置 X 轴为对数刻度 (移除 name 参数)
  scale_x_log10(
    breaks = scales::log_breaks(n = 7), # 尝试生成对数刻度, n 控制大致数量
    # labels = scales::label_number(accuracy = 0.0001), # 暂时移除自定义标签格式
    expand = expansion(mult = c(0.02, 0.02)) # 稍微增加扩展
  ) +
  # 设置 Y 轴标签和可能的 breaks
  scale_y_continuous(
    name = "Binomial deviance",
    n.breaks = 6 ,# 尝试建议 Y 轴刻度数量
    labels = scales::label_number(accuracy = 0.01)
    # limits = c(min(cv_data$cvlo)*0.98, y_annotation_pos * 1.02) # 可选：手动设置 Y 范围
  )+ #<-- 可以移除，在 labs() 中设置
  labs(
    x = expression(lambda[log]),                 #<-- 使用 Unicode λ
    y = "Cross-validation function",      #<-- 设置 Y 轴标签
    title = "A. Cross-validation plot" # 可选：设置标题
    # subtitle = NULL # 可选：设置副标题
  ) +
  # 设置 Y 轴范围，确保标注可见
  # expand_limits(y = y_annotation_pos * 1.02) + # 确保 Y 轴足够高以显示标注
  
  # 应用类似图片的主题
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(), # 移除次要网格线
    panel.grid.major.x = element_blank(), # 移除垂直主要网格线
    panel.grid.major.y = element_line(color = "grey90", linetype = "dotted"), # 水平主要网格线样式

    panel.border = element_rect(color = "black", fill = NA,size = 1), # 确保边框存在
    plot.title = element_text(hjust = 0), # 如果需要标题，居中
    axis.text = element_text(color="black"),
    axis.title = element_text(color="black"),
    plot.margin = margin(5, 20, 5, 5),  # 增加右侧边距
  )

# 显示图形
print(cv_plot_ggplot)
suppressWarnings(cv_plot_ggplot)
# 确定最优的 lambda 值
# lambda.1se: 在距离最小误差一个标准误范围内，给出最简洁模型（lambda 最大）的 lambda 值，通常更推荐，因为它更倾向于选择较少的变量
lambda_optimal <- cv_lasso$lambda.1se
# lambda.min: 给出交叉验证中指标（如此处的AUC）最优的 lambda 值，可能选择更多变量
# lambda_optimal <- cv_lasso$lambda.min
cat("最优 Lambda 值 (lambda.1se):", lambda_optimal, "\n")

# 获取在最优 lambda 值下的模型系数
lasso_coefs <- coef(cv_lasso, s = lambda_optimal) # coef() 是 S3 方法，用于提取系数
print("最优 Lambda 值下的 LASSO 系数:")
print(lasso_coefs) # 会显示一个稀疏矩阵对象

# 识别被选中的变量 (系数不为零的变量，去除截距项)
selected_vars_indices <- which(lasso_coefs != 0)               # 找到非零系数的索引
selected_vars_names <- rownames(lasso_coefs)[selected_vars_indices] # 获取对应的变量名
selected_vars_names <- selected_vars_names[selected_vars_names != "(Intercept)"] # 从选中的变量名中排除截距项

cat("\nLASSO 筛选出的变量 (不含截距):\n")
if (length(selected_vars_names) > 0) {
  print(selected_vars_names) # 打印选中的变量名 (这些可能是包含因子水平的名字)
} else {
  # 如果没有变量被选中，发出警告并停止，可能需要检查数据或调整lambda选择策略
  warning("LASSO 没有筛选出任何变量。请检查数据或 lambda 选择。程序停止。")
  stop("没有变量被 LASSO 选中。")
}

# --- 2. 创建筛选后的数据集和公式 ---
# 目标：根据 LASSO 筛选出的变量名（可能来自 model.matrix），构建一个新的、只包含这些变量的 glm 公式

# 策略：将 model.matrix 生成的变量名（如 "性别Male", "Tanner分期代码2"）映射回原始的变量名（如 "性别", "Tanner分期代码"）
# 这是因为后续的 glm 函数通常使用原始变量名的公式更方便

# 获取原始公式中的所有预测变量名
original_predictors <- all.vars(formula_full_obj)[-1] # [-1] 去除结果变量名

# 定义一个函数尝试将 model.matrix 的变量名映射回基础变量名
get_base_var <- function(var_name, all_original_vars) {
  # 简单情况：直接匹配原始变量名
  if (var_name %in% all_original_vars) return(var_name)
  # 因子水平情况：检查变量名是否以某个原始变量名开头
  # 这是一种启发式方法，可能需要根据你的变量命名调整
  for (orig_var in all_original_vars) {
    if (startsWith(var_name, orig_var)) {
      # 可以加更复杂的检查，比如检查是否真的是因子水平
      return(orig_var)
    }
  }
  # 处理交互项、多项式项等可能需要更复杂的逻辑 (此处未实现)
  # 如果没有明确匹配，返回 NULL
  return(NULL)
}

# 对每个被 LASSO 选中的变量名，应用映射函数，并去重
selected_base_vars <- unique(na.omit(sapply(selected_vars_names, get_base_var, all_original_vars = original_predictors)))

cat("\n与选中的 LASSO 项对应的原始变量:\n")
if(length(selected_base_vars) > 0) {
  print(selected_base_vars) # 打印映射后的基础变量名
  
  # 使用筛选后的基础变量名构建新的公式字符串
  formula_selected_str <- paste("复诊标志 ~", paste(selected_base_vars, collapse = " + "))
  formula_selected_obj <- as.formula(formula_selected_str) # 转换为公式对象
  cat("\n筛选后变量构成的新公式:\n", formula_selected_str, "\n")
  
  # 从进行 LASSO 的干净数据 (`final_data_clean`) 中选择必要的列：结果变量 + 筛选出的原始预测变量
  data_selected <- final_data_clean[, c("复诊标志", selected_base_vars)]
  
} else {
  # 如果映射失败，发出警告并停止
  warning("无法可靠地将选中的 LASSO 项映射回原始变量。程序停止。")
  stop("变量映射失败。")
}


# --- 3. 初始留出法分割 (在 data_selected 上进行) ---
# 现在对只包含筛选后变量的数据集进行训练集/测试集分割
set.seed(123) # 如果希望与之前不筛选时分割方式一致，使用相同种子
# 使用 caret::createDataPartition 进行分层抽样分割
train_indices <- createDataPartition(data_selected$复诊标志, p = 0.8, list = FALSE, times = 1)

train_data <- data_selected[train_indices, ]  # 训练集 (只含筛选后变量)
test_data  <- data_selected[-train_indices, ] # 测试集 (只含筛选后变量)

cat("\n数据分割结果 (使用筛选后的变量):\n")
cat("训练集大小:", nrow(train_data), "\n")
cat("测试集大小:", nrow(test_data), "\n")
cat("训练集结果变量分布:\n"); print(prop.table(table(train_data$复诊标志)))
cat("测试集结果变量分布:\n"); print(prop.table(table(test_data$复诊标志)))


# --- 4. 内部验证 (在 train_data 上进行 Bootstrap，使用筛选后的变量) ---

# 修改之前的 boot_statistic 函数，使其内部使用筛选后的公式 `formula_selected_obj`
# 使用之前修正过的包含稳健性检查的版本
boot_statistic_selected <- function(data, indices) {
  d_boot <- data[indices, ] # 创建自助样本
  
  # 在自助样本上拟合模型，使用筛选后的公式！
  fit_boot <- tryCatch({
    # 使用在外部定义的筛选后的公式对象
    glm(formula_selected_obj, data = d_boot, family = binomial(link = "logit"))
  }, error = function(e) { NULL }) # 出错返回 NULL
  
  # 如果模型拟合失败，返回 NA 向量 (长度与预期返回值一致，这里是6个指标)
  if (is.null(fit_boot)) { return(rep(NA, 6)) }
  
  # 使用拟合的模型，在 *原始* 训练集 `data` 上预测概率
  pred_prob_train <- tryCatch({
    predict(fit_boot, newdata = data, type = "response")
  }, error = function(e) { rep(NA_real_, nrow(data)) }) # 预测失败返回NA
  
  # 检查预测概率是否有效 (非NA，且有变化)
  if (anyNA(pred_prob_train) || length(unique(pred_prob_train[!is.na(pred_prob_train)])) <= 1) {
    return(rep(NA, 6)) # 预测无效则无法计算指标
  }
  
  # --- 计算各项评估指标 ---
  # ROC 和 AUC
  roc_obj_train <- tryCatch({
    roc(response = data$复诊标志, predictor = pred_prob_train,
        levels = levels(data$复诊标志), direction = "<", quiet = TRUE) # 明确levels和方向
  }, error = function(e) { NULL }) # ROC计算失败返回 NULL
  
  auc_val <- if (!is.null(roc_obj_train) && inherits(roc_obj_train, "roc")) {
    # 仅当 roc_obj_train 是有效的 roc 对象时，才计算 AUC
    tryCatch(auc(roc_obj_train), error = function(e) NA_real_) # AUC计算失败返回 NA
  } else { NA_real_ } # roc 对象无效，AUC 为 NA
  
  # 其他指标 (准确率, 精确率, 召回率, F1, Brier分数)
  accuracy_val <- NA; precision_val <- NA; recall_val <- NA; f1_val <- NA; brier_val <- NA
  # 只有在 AUC 有效时才计算后续指标 (因为它们通常依赖于有效预测)
  if (!is.na(auc_val)) {
    pred_class_train <- factor(ifelse(pred_prob_train > 0.5, positive_class_label, negative_class_label), levels = levels(data$复诊标志)) # 使用0.5阈值分类
    # 计算混淆矩阵
    cm_train <- tryCatch({
      confusionMatrix(data = pred_class_train, reference = data$复诊标志, positive = positive_class_label)
    }, error = function(e) { NULL }) # 混淆矩阵计算失败返回 NULL
    
    if (!is.null(cm_train)) { # 如果混淆矩阵有效
      accuracy_val <- cm_train$overall["Accuracy"]
      precision_val <- ifelse(is.nan(cm_train$byClass["Precision"]), 0, cm_train$byClass["Precision"]) # 处理NaN
      recall_val <- ifelse(is.nan(cm_train$byClass["Recall"]), 0, cm_train$byClass["Recall"]) # 处理NaN
      f1_val <- ifelse(is.nan(cm_train$byClass["F1"]), 0, cm_train$byClass["F1"]) # 处理NaN
    }
    # 计算 Brier 分数
    true_outcome_numeric <- as.numeric(data$复诊标志 == positive_class_label) # 真实结果转为0/1
    brier_val <- mean((pred_prob_train - true_outcome_numeric)^2, na.rm = TRUE) # na.rm 以防万一
  }
  
  # 返回所有指标的命名向量
  return(c(AUC = auc_val, Accuracy = accuracy_val, Precision = precision_val, Recall = recall_val, F1 = f1_val, Brier = brier_val))
}

# 执行 Bootstrap 验证
cat("\n--- 执行 Bootstrap 内部验证 (使用筛选后的变量) ---\n")
set.seed(456) # 设置随机种子
# 调用 boot 函数，使用修改后的统计函数 boot_statistic_selected
boot_results_selected <- boot(data = train_data,                   # 在筛选后的训练集上操作
                              statistic = boot_statistic_selected, # 使用适配筛选后变量的函数
                              R = 500,                            # Bootstrap 重复次数
                              strata = train_data$复诊标志         # 进行分层抽样
                              # 注意：formula_selected_obj 是在 boot_statistic_selected 函数 *内部* 使用的
)

# 汇总 Bootstrap 结果
cat("\nBootstrap 性能汇总 (筛选后变量模型):\n")
print(boot_results_selected) # 打印原始 boot 对象信息
# 计算各指标的均值和标准差
boot_summary_selected <- data.frame(
  Metric = names(boot_results_selected$t0),                # 指标名称
  Mean = colMeans(boot_results_selected$t, na.rm = TRUE),  # 计算均值 (忽略NA)
  StdDev = apply(boot_results_selected$t, 2, sd, na.rm = TRUE) # 计算标准差 (忽略NA)
)
print(boot_summary_selected) # 打印均值和标准差表格
# --- 新增：计算 Bootstrap AUC 的 95% 置信区间 ---
cat("\n--- 计算 Bootstrap AUC 的 95% 置信区间 ---\n")
# 检查 boot_results_selected 是否有效，以及是否有有效的 bootstrap 复制 (t)
if (exists("boot_results_selected") && !is.null(boot_results_selected$t) && nrow(boot_results_selected$t) > 1) {
  auc_boot_ci <- tryCatch({
    # 假设 AUC 是 boot_statistic_selected 返回的第一个指标 (index=1)
    # 使用 Percentile 方法计算置信区间
    boot.ci(boot_results_selected, type = "perc", index = 1, conf = 0.95)
  }, error = function(e) {
    warning("无法计算 Bootstrap AUC 置信区间 (可能由于 NA 过多): ", e$message)
    NULL
  })
  
  if (!is.null(auc_boot_ci)) {
    print(auc_boot_ci) # 打印 boot.ci 对象详细信息
    # 提取 percentile 置信区间的上下限 (通常是第4和第5个元素)
    ci_lower_boot <- auc_boot_ci$percent[4]
    ci_upper_boot <- auc_boot_ci$percent[5]
    cat(paste0("内部验证 Bootstrap 平均 AUC (Mean): ", round(boot_summary_selected$Mean[boot_summary_selected$Metric == 'AUC'], 3), "\n"))
    cat(paste0("内部验证 Bootstrap AUC 95% CI (Percentile): [", round(ci_lower_boot, 3), ", ", round(ci_upper_boot, 3), "]\n"))
  }
} else {
  warning("无法计算 Bootstrap AUC CI，因为 'boot_results_selected' 无效或 bootstrap 样本不足。")
}


# --- 5. 最终模型训练 (使用筛选后的变量) ---
cat("\n--- 训练最终模型 (使用筛选后的变量) ---\n")
# 在 *整个* 筛选后的训练集 (`train_data`) 上，使用筛选后的公式 (`formula_selected_obj`) 训练最终的 glm 模型
final_model_selected <- glm(formula_selected_obj, data = train_data, family = binomial(link = "logit"))
print(summary(final_model_selected)) # 打印最终模型的摘要信息
train_pred_prob_selected <- predict(final_model_selected, newdata = train_data, type = "response")
roc_obj_train_selected <- roc(response = train_data$复诊标志, predictor = train_pred_prob_selected, quiet=TRUE, levels=levels(test_data$复诊标志), direction="<")

ci.auc(roc_obj_train_selected, conf.level = 0.95)
roc_obj_train_selected

# --- 6. 在留出的测试集上进行最终评估 (使用筛选后的变量) ---
cat("\n--- 在测试集上评估最终模型 (使用筛选后的变量) ---\n")
# 使用最终训练好的筛选后变量模型 (`final_model_selected`) 在测试集 (`test_data`) 上预测概率
test_pred_prob_selected <- predict(final_model_selected, newdata = test_data, type = "response")

# --- 在测试集上计算评估指标 (基于筛选后变量模型) ---
# ROC 曲线和 AUC
roc_obj_test_selected <- roc(response = test_data$复诊标志, predictor = test_pred_prob_selected, quiet=TRUE, levels=levels(test_data$复诊标志), direction="<")
auc_test_selected <- auc(roc_obj_test_selected) # 计算 AUC
ci.auc(roc_obj_test_selected, conf.level = 0.95)
cat("测试集 AUC (筛选后变量模型):", auc_test_selected, "\n")

# 混淆矩阵及相关指标 (使用 0.5 阈值)
test_pred_class_selected <- factor(ifelse(test_pred_prob_selected > 0.5, positive_class_label, negative_class_label), levels = levels(test_data$复诊标志))
cm_test_selected <- confusionMatrix(data = test_pred_class_selected, reference = test_data$复诊标志, positive = positive_class_label)
# 打印感兴趣的指标
cat("测试集 Accuracy (筛选后变量模型):", cm_test_selected$overall["Accuracy"], "\n")
cat("测试集 Precision (筛选后变量模型):", cm_test_selected$byClass["Precision"], "\n")
cat("测试集 Recall (筛选后变量模型):", cm_test_selected$byClass["Sensitivity"], "\n")
cat("测试集 F1 (筛选后变量模型):", cm_test_selected$byClass["F1"], "\n")
cat("测试集 Specificity (筛选后变量模型):", cm_test_selected$byClass["Specificity"], "\n")

# Brier 分数
test_true_outcome_numeric <- as.numeric(test_data$复诊标志 == positive_class_label)
brier_test_selected <- mean((test_pred_prob_selected - test_true_outcome_numeric)^2)
cat("测试集 Brier 分数 (筛选后变量模型):", brier_test_selected, "\n")


# --- 7. 可视化 (手动创建标准的 FPR vs TPR ROC 图) ---
cat("\n--- 生成组合 ROC 图 (筛选后变量模型) ---\n")

# 计算最终模型在训练集上的性能 (筛选后变量)
train_pred_prob_final_selected <- predict(final_model_selected, newdata = train_data, type = "response")
roc_train_final_selected <- roc(response = train_data$复诊标志, predictor = train_pred_prob_final_selected, quiet=TRUE, levels=levels(train_data$复诊标志), direction="<")
auc_train_final_selected <- auc(roc_train_final_selected) # 训练集 AUC

# 准备 Bootstrap 结果的文本摘要 (筛选后模型)
bootstrap_auc_mean_selected <- boot_summary_selected$Mean[boot_summary_selected$Metric == "AUC"]
bootstrap_auc_sd_selected <- boot_summary_selected$StdDev[boot_summary_selected$Metric == "AUC"]
bootstrap_auc_text_selected <- paste0("内部验证 Bootstrap 平均 AUC (筛选后变量模型): ",
                                      round(bootstrap_auc_mean_selected, 3), " +/- ", round(bootstrap_auc_sd_selected, 3))

# 创建包含训练集和测试集 ROC 对象的列表 (筛选后模型)
roc_list_selected <- list(Training = roc_train_final_selected, Testing = roc_obj_test_selected)

cat("\n--- 手动生成标准的 1-Specificity vs Sensitivity ROC 图 ---\n")

# 1. & 2. & 3. 从 roc_list_selected 提取数据并计算 FPR
library(dplyr) # 用于 bind_rows
plot_data_list <- list() # 创建一个空列表来存储每个数据集的绘图数据

# 检查 roc_list_selected 的名称 (确保它们与下面 scale_color_manual 中使用的名称一致)
# 假设名称是 "训练集" 和 "测试集"
expected_names <- c("Training", "Testing")
if (!all(names(roc_list_selected) %in% expected_names)) {
  warning("roc_list_selected 中的名称与预期 ('训练集', '测试集') 不完全匹配，请检查 scale_color_manual")
  # 如果名称是 "Training", "Testing"，请修改下面的 scale_color_manual
}


for (name in names(roc_list_selected)) {
  roc_obj <- roc_list_selected[[name]]
  # 提取数据
  sensitivities <- roc_obj$sensitivities
  specificities <- roc_obj$specificities
  # 计算 FPR
  fpr <- 1 - specificities
  # 创建数据框
  temp_df <- data.frame(
    FPR = fpr,
    TPR = sensitivities,
    Dataset = name # 添加数据集标识列
  )
  # 添加到列表中
  plot_data_list[[name]] <- temp_df
}

# 将列表中的所有数据框合并成一个
plot_data <- bind_rows(plot_data_list)

# 4. 使用 ggplot 手动绘制

roc_plot_manual <- ggplot(plot_data, aes(x = FPR, y = TPR, color = Dataset)) +
  geom_line(size = 1) + # 使用从 roc 对象提取的数据绘制线条
  
  # 添加标准的对角线 (y = x for FPR vs TPR)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  
  # 设置图形的标题、副标题、坐标轴标签和图例
  labs(
    title = "B. ROC cruve",
    x = "1-Specificity", # 明确设置标准 X 轴标签
    y = "Sensitivity",     # 明确设置标准 Y 轴标签
  ) +
  
  # 手动设置颜色和图例标签 (确保名称与 plot_data$Dataset 中的值匹配)
  scale_color_manual(
    name = "Dataset", # 图例标题
    values = c("Training" = "#D55E00", "Testing" = "#0072B2"), # 确保名称与列表键/数据框值匹配
    labels = c("Training AUC=0.685 (0.671-0.699)","Validation AUC=0.705 (0.677-0.733)"), # 反转图例标签的顺序
    breaks = c("Training", "Testing")) + # 反转图例的顺序 
  
  # 设置坐标轴范围从 0 到 1，并移除边距使曲线接触坐标轴
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  
  # 使用简洁主题并进行自定义
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = c(0.75, 0.2), # 调整图例位置
    legend.text = element_text(size = 10), # 设置图例标签字体大小
    plot.title = element_text(hjust = 0), # 标题居中
    plot.subtitle = element_text(hjust = 0.5), # 副标题居中
    axis.ticks = element_line(color = "black"), # 设置所有轴刻度线为黑色
    axis.ticks.x = element_line(color = "black"), # 明确设置 X 轴刻度线为黑色
    axis.ticks.y = element_line(color = "black"), # 明确设置 Y 轴刻度线为黑色
    panel.grid.major = element_line(color = "grey90"), # 主要网格线
    axis.text = element_text(color = "black"),
    plot.margin = margin(5, 20, 5, 5),  # 增加右侧边距
    panel.grid.minor = element_blank() # 隐藏次要网格线
  )

# 显示绘制的图形
print(roc_plot_manual)
# --- 生成 LASSO 变量重要性图 ---

cat("\n--- 生成 LASSO 变量重要性图 ---\n")

# 1. 提取在最优 Lambda (lambda_optimal) 下的系数
#    coef() 返回的是一个稀疏矩阵 (dgCMatrix)
lasso_coefs_optimal <- coef(cv_lasso, s = lambda_optimal)

# 2. & 3. & 4. 转换为数据框，计算绝对值，移除截距，筛选非零系数
#    as.matrix() 转换为普通矩阵方便处理
importance_df <- as.data.frame(as.matrix(lasso_coefs_optimal))
names(importance_df) <- "Coefficient" # 重命名列
importance_df$Variable <- rownames(importance_df) # 将行名（变量名）转为一列

# 计算绝对系数作为重要性，并筛选
importance_df <- importance_df %>%
  filter(Variable != "(Intercept)") %>%      # 移除截距项
  mutate(Importance = abs(Coefficient)) %>% # 计算绝对值作为重要性
  filter(Importance > 1e-6) %>%            # 筛选出重要性大于一个很小的值（避免浮点数0的问题）
  select(Variable, Importance)             # 只保留变量名和重要性

# 检查是否有变量被选中
if (nrow(importance_df) == 0) {
  warning("在最优 Lambda 下没有变量的系数绝对值大于零，无法生成重要性图。")
} else {
  
  # 5. & 6. 按重要性降序排序，并设置因子顺序用于绘图
  importance_df <- importance_df %>%
    arrange(desc(Importance)) %>%
    mutate(Variable = factor(Variable, levels = rev(Variable))) # 设置因子顺序，rev() 使得 ggplot 纵向排序正确
  
  # 7. 使用 ggplot2 绘制条形图
  library(ggplot2)
  
  var_importance_plot <- ggplot(importance_df, aes(x = Variable, y = Importance)) +
    geom_col(fill = "steelblue") + # 或者使用 geom_bar(stat = "identity")
    coord_flip() + # 将坐标轴翻转，使条形图水平显示，更易阅读变量名
    labs(
      title = "LASSO 模型变量重要性",
      subtitle = paste("基于最优 Lambda (lambda.1se =", format(lambda_optimal, digits = 4), ") 的系数绝对值"),
      x = "预测变量",
      y = "重要性 (系数绝对值)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 10), # 调整 Y 轴（现在是变量名）字体大小
      axis.text.x = element_text(size = 10)
    )
  
  # 显示图形
  print(var_importance_plot)
  
}

cat("\n--- 变量重要性图生成完毕 ---\n")
# --- Forest Plot for Logistic Regression Results (LASSO Selected Variables) ---

cat("\n--- Generating Forest Plot for Final Logistic Regression Model ---\n")

# Ensure the final model object exists
if (!exists("final_model_selected") || !inherits(final_model_selected, "glm")) {
  stop("错误：找不到名为 'final_model_selected' 的 glm 模型对象。请确保之前的步骤已成功运行。")
}
if (final_model_selected$family$family != "binomial" || final_model_selected$family$link != "logit") {
  warning("警告：'final_model_selected' 可能不是一个标准的二项 logistic 回归模型。")
}


# 1. Extract Coefficients and Standard Errors
model_summary <- summary(final_model_selected)
coeffs <- model_summary$coefficients

# 2. Calculate ORs and CIs
log_odds <- coeffs[, "Estimate"]
std_errors <- coeffs[, "Std. Error"]
or <- exp(log_odds)
ci_lower_log <- log_odds - 1.96 * std_errors
ci_upper_log <- log_odds + 1.96 * std_errors
ci_lower_or <- exp(ci_lower_log)
ci_upper_or <- exp(ci_upper_log)

# 3. Prepare Data Frame for Plotting
forest_data <- data.frame(
  Variable = rownames(coeffs),
  OR = or,
  CI_Lower = ci_lower_or,
  CI_Upper = ci_upper_or,
  LogOdds = log_odds,       # Keep log-odds for potential checks
  StdError = std_errors   # Keep SE for potential checks
)

# Remove the intercept row
forest_data <- forest_data %>%
  filter(Variable != "(Intercept)")


  
  # Optional: Order variables for plotting (e.g., alphabetically)
  # Reverse order needed because ggplot plots factors bottom-up
  forest_data <- forest_data %>%
    mutate(Variable = factor(Variable, levels = rev(sort(Variable))))
  
  
  # 4. Create Forest Plot using ggplot2
  library(ggplot2)
  library(scales) # For log scale breaks/labels
  
  # --- Forest Plot with OR (95% CI) Text Labels ---
  
  cat("\n--- Generating Forest Plot with Text Labels ---\n")
  
  # Ensure the final model object exists and required data frame is prepared
  if (!exists("final_model_selected") || !inherits(final_model_selected, "glm")) {
    stop("错误：找不到名为 'final_model_selected' 的 glm 模型对象。")
  }
  if (!exists("forest_data") || !"CI_Upper" %in% names(forest_data)) {
    stop("错误：'forest_data' 数据框未准备好或缺少必要列 (CI_Upper)。请确保之前的步骤已成功运行。")
  }
  

    
  # --- Combined Importance and Forest Plot using Patchwork ---
  
  cat("\n--- Generating Combined Importance and Forest Plot ---\n")
  
  # Ensure required data frames exist
  if (!exists("importance_df") || !exists("forest_data")) {
    stop("错误：需要 'importance_df' 和 'forest_data' 数据框。请确保之前的步骤已成功运行。")
  }
  if (!"label_text" %in% names(forest_data)) {
    # Create label_text if it wasn't created in the previous step
    forest_data <- forest_data %>%
      mutate(label_text = paste0(sprintf("%.2f", OR),
                                 " (",
                                 sprintf("%.2f", CI_Lower),
                                 " - ",
                                 sprintf("%.2f", CI_Upper),
                                 ")"))
  }
  
  
  # 1. Ensure Data Consistency & Merge
  #    (Assuming both DFs use the same variable names from the model)
  #    Inner join ensures we only keep variables present in BOTH analyses
  combined_plot_data <- inner_join(importance_df, forest_data, by = "Variable")
  
  # Check if merge resulted in data
  if (nrow(combined_plot_data) == 0) {
    stop("错误：合并 'importance_df' 和 'forest_data' 后没有共同的变量。请检查变量名是否一致。")
  }
  
  # 2. Define Consistent Factor Order (based on Importance)
  #    Order descending by Importance, reverse levels for ggplot Y-axis plotting
  combined_plot_data <- combined_plot_data %>%
    arrange(desc(Importance)) %>%
    mutate(Variable = factor(Variable, levels = rev(Variable)))
  
  # 3. Create Plot 1: Importance Bar Chart
  library(ggplot2)
  library(patchwork)
  library(scales)
  
  # --- "Pseudo" Dual-Axis Plot: Importance (Left Y) & Forest Plot Info (Right Y-ish) ---
  
  cat("\n--- Generating Combined Plot (Var on X, Importance on Left Y, OR Info Overlaid) ---\n")
  
  # Ensure merged data exists and has necessary columns
  if (!exists("combined_plot_data") || !"label_text" %in% names(combined_plot_data)) {
    stop("错误：需要合并后的 'combined_plot_data' 数据框，并包含 'label_text' 列。")
  }
  
  # 1. Order Variables on X-axis (e.g., by Importance)
  plot_data_dual <- combined_plot_data %>%
    arrange(desc(Importance)) %>%
    mutate(Variable = factor(Variable, levels = Variable)) # Order by importance
  
  # 2. Create the plot
  library(ggplot2)
  library(scales)
  
  # Define colors
  col_importance <- "#D55E00"
  col_or <- "black"
  custom_fill_colors <- c(
    "医嘱复诊标志代码2" = "#1D7CA0", # Example color (blue)
    "治疗标志2"        = "#33a02c", # Example color (green)
    "雌二醇E2检验结果2" = "#1D7CA0", # Example color (blue)
    "市1"        = "#33a02c", # Example color (green)
    "年龄"        = "#1D7CA0" # Example color (green)
    # Add more variables and their colors here as needed
  )
  
  desired_variable_order <- c("医嘱复诊标志代码2", "治疗标志2", "雌二醇E2检验结果2", "市1", "年龄") # Your specific order
  new_variable_labels <- c("Follow-up visit order", "Treatment", "Estradiol level","City","Age") # 
  
  
  plot_data_dual_reordered <- plot_data_dual %>%
    # Filter to include only the variables you want to plot and order
    filter(Variable %in% desired_variable_order) %>%
    # Convert Variable to factor with the specified order
    mutate(Variable = factor(Variable, levels = desired_variable_order))
  
  
  # Check if plot_data_dual_reordered is empty
  if (nrow(plot_data_dual_reordered) == 0) {
    stop("错误：根据 'desired_variable_order' 筛选后没有数据可供绘图。")
  }
  
  
  pseudo_dual_plot <- ggplot(plot_data_dual_reordered, aes(x = Variable)) + # Use reordered data
    
    # Plot Importance (Bars for Left Y axis)
    geom_col(aes(y = Importance), fill = custom_fill_colors, width = 0.3) +

    # --- Apply New Labels in scale_x_discrete ---
    scale_x_discrete(
      expand = c(0.2, 0.2),
      labels = new_variable_labels # Use the new labels vector
    ) +
    # -------------------------------------------
  
  # Define the Primary Y Axis (Left - for Importance)
  scale_y_continuous(limits = c(0,0.84),
                     breaks = seq(0,0.84,0.16),
                     expand = c(0,0),
                     # Secondary axis definition (as provided by user)
                     sec.axis = sec_axis(~.*3,
                                         name = 'Logistic OR (95% CI)',
                                         breaks = seq(0,2.5,0.5)))+
    
    # Overlay Forest Plot elements (Points and Error Bars using transformed OR/CI)
    geom_point(aes(y = OR/3), color = col_or, size = 1.5) +
    geom_errorbar(aes(ymin = CI_Lower/3, ymax = CI_Upper/3), color = col_or, width = 0.05) +
    
    # Add Text Labels for OR (95% CI) near the points
    geom_text(
      # Ensure label_text column exists (assuming it was created before)
      aes(y = OR/3, label = label_text),
      color = "black",
      hjust = -0.1,
      vjust = 0.5,
      size = 3
    ) +
    
    # Add Title and Theme
    labs(
      title = "C. Variable importance and estimate",
      x = "Variable", # X-axis title remains "Variable" or set to NULL if labels are self-explanatory
      y = "Lasso β (absolute value)"
    ) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      axis.text = element_text(color = "black"),
      # Rotate X labels (using the new labels now)
      axis.text.x = element_text(angle = 45, hjust = 1, size=10),
      plot.title = element_text(hjust = 0),
      plot.subtitle = element_text(hjust = 0.5, size = 9), # Subtitle seems unused in labs()
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  # Print the adjusted plot
  print(pseudo_dual_plot)
  
  cat("\n--- X轴刻度名称和顺序已调整 ---\n")
library(patchwork)
cv_plot_ggplot/roc_plot_manual/pseudo_dual_plot

# ==============================================================================
# --- 6.2 校准度验证与 Brier Score (利用 rms::val.prob 函数) ---
# ==============================================================================
cat("\n--- 基于 rms::val.prob 进行训练集与测试集校准效能评估 ---\n")

# 1. 确保加载 rms 包 (如未安装请取消注释安装)
# if (!requireNamespace("rms", quietly = TRUE)) install.packages("rms")
library(rms)

# 2. 提取 0/1 数值型真实结局 (1: 阳性/复诊, 0: 阴性/未复诊)
# 自动与您的 positive_class_label ("2") 对齐
y_train <- ifelse(train_data$复诊标志 == positive_class_label, 1, 0)
y_test  <- ifelse(test_data$复诊标志 == positive_class_label, 1, 0)

# 提取预测概率向量
pred_train <- as.numeric(train_pred_prob_selected)
pred_test  <- as.numeric(test_pred_prob_selected)

# 剔除可能的缺失值 (防御性编程)
train_valid <- complete.cases(y_train, pred_train)
test_valid  <- complete.cases(y_test, pred_test)

y_train <- y_train[train_valid]; pred_train <- pred_train[train_valid]
y_test  <- y_test[test_valid];   pred_test  <- pred_test[test_valid]

# 3. 设置绘图参数 (1行2列并排展示训练集与测试集)
opar <- par(no.readonly = TRUE) # 备份原始绘图参数，防止影响后续 ggplot
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 2.5, 2.5))

# 4. 执行 val.prob 评估与绘图
# 注：m 为每组最小样本量(根据样本大小自适应，样本量较小时可设为 30-50，较大时设为 100)
m_train <- ifelse(length(y_train) > 500, 100, max(20, round(length(y_train)/10)))
m_test  <- ifelse(length(y_test) > 500, 100, max(20, round(length(y_test)/10)))

cat("\n>> 正在绘制训练集校准曲线...\n")
cal_metrics_train <- val.prob(
  p = pred_train, 
  y = y_train, 
  m = m_train, 
  pl = TRUE,
  xlab = "Predicted Probability",
  ylab = "Actual Probability",
  riskdist = "calibrated" # 在底部绘制预测概率分布直方/地毯图
)
title(main = "A. Calibration (Training Set)", line = 1)

cat("\n>> 正在绘制验证集校准曲线...\n")
cal_metrics_val <- val.prob(
  p = pred_test, 
  y = y_test, 
  m = m_test, 
  pl = TRUE,
  xlab = "Predicted Probability",
  ylab = "Actual Probability",
  riskdist = "calibrated"
)
title(main = "B. Calibration (Validation Set)", line = 1)

# 恢复默认绘图参数
par(opar)

# ==============================================================================
# --- 5. 提取并汇总关键校准指标表格 ---
# ==============================================================================
extract_val_prob_metrics <- function(cal_obj, dataset_name) {
  data.frame(
    Dataset = dataset_name,
    `Brier Score` = round(cal_obj["Brier"], 4),
    `Calibration Intercept` = round(cal_obj["Intercept"], 4), # 理想值为 0
    `Calibration Slope` = round(cal_obj["Slope"], 4),         # 理想值为 1
    `C-index (ROC)` = round(cal_obj["C (ROC)"], 4),
    `Emax (Max Error)` = round(cal_obj["Emax"], 4),          # 最大绝对校准误差
    `Eavg (Mean Error)` = round(cal_obj["Eavg"], 4),         # 平均绝对校准误差
    `HL p-value` = round(cal_obj["P"], 4),                   # Hosmer-Lemeshow p值
    check.names = FALSE
  )
}

cal_summary_df <- rbind(
  extract_val_prob_metrics(cal_metrics_train, "Training Set"),
  extract_val_prob_metrics(cal_metrics_val, "Validation Set")
)

cat("\n--- 校准度评估汇总表 (rms::val.prob) ---\n")
print(cal_summary_df, row.names = FALSE)