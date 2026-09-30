# 加载所需的包
if(!require(stringdist)) install.packages("stringdist")
library(stringdist)

data<- read.xlsx(".xlsx")

# 定义函数计算两两文本相似度cosine、jaccard或lv
calculate_similarity <- function(text1, text2, method = "jaccard") {
  stringdist(text1, text2, method = method)
}

# 使用函数计算 input-outputA, input-outputB 和 outputA-outputB 之间的相似度
similarity <- mapply(calculate_similarity, data$病史, data$output)
similarity<-as.data.frame(similarity)
# 计算相似度的均值和标准差
similarity_stats <- similarity%>%summarize(
    mean = mean(similarity),
    sd(similarity)
  )

# 进行配对 t 检验，比较两两相似度的差异
t_test_input_outputA_vs_outputB <- t.test(data$similarity_input_outputA, data$similarity_input_outputB, paired = TRUE)
t_test_input_outputA_vs_outputA_outputB <- t.test(data$similarity_input_outputA, data$similarity_outputA_outputB, paired = TRUE)
t_test_input_outputB_vs_outputA_outputB <- t.test(data$similarity_input_outputB, data$similarity_outputA_outputB, paired = TRUE)

# 打印结果
print(summary_stats)
print("t检验结果：input-outputA 与 input-outputB")
print(t_test_input_outputA_vs_outputB)

print("t检验结果：input-outputA 与 outputA-outputB")
print(t_test_input_outputA_vs_outputA_outputB)

print("t检验结果：input-outputB 与 outputA-outputB")
print(t_test_input_outputB_vs_outputA_outputB)
# 查看结果
print(data)
library(reticulate)
library(tiktoken)
use_python("D:/App/Anaconda/envs/NLP3.11", required = TRUE) # 指定 Python 可执行文件路径
use_condaenv("NLP3.11", required = TRUE)      # 指定 Conda 环境名称
if (!py_module_available("tiktoken")) {
  stop("Python 'tiktoken' 模块未在 R session 配置的 Python 环境中找到。\n",
       "请确保已在正确的 Python 环境中执行 'pip install tiktoken'，\n",
       "并确保 reticulate 正确配置以使用该环境 (例如使用 use_python() 或 use_condaenv())。")
}
encoding_name <- "cl100k_base" # 根据你的目标 OpenAI 模型选择
enc <- tryCatch({
  get_encoding(encoding_name)
}, error = function(e) {
  stop("无法获取 tiktoken 编码器 '", encoding_name, "'. ",
       "请检查 Python/tiktoken 安装和 reticulate 配置。\n原始错误: ", e$message)
}
)

count_openai_tokens <- function(text, encoder) {
  if (is.na(text)) {
    return(NA_integer_) # 对 NA 输入返回 NA
  }
  if (text == "") {
    return(0L) # 空字符串有 0 个 token
  }
  tryCatch({
    # encoder$encode 返回 token ID 列表 (整数向量)
    token_ids <- encoder$encode(text)
    # 计算 token ID 的数量
    length(token_ids)
  }, error = function(e) {
    warning("处理文本时发生错误: '", substr(text, 1, 50), "...' 错误: ", e$message)
    return(NA_integer_) # 编码失败时返回 NA
  })
}

# 5. 应用函数到 'text_column' 列
# 使用 sapply 将函数应用到列的每个元素
df$openai_token_count <- sapply(df$text_column, count_openai_tokens, encoder = enc)


