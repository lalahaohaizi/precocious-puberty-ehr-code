# 电子病历信息抽取性能评价

options(stringsAsFactors = FALSE)

resolve_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)))
  }

  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(source_file)) {
    return(dirname(normalizePath(source_file, mustWork = FALSE)))
  }

  normalizePath(getwd(), mustWork = FALSE)
}

PROJECT_ROOT <- Sys.getenv("EHR_EXTRACTION_ROOT", unset = resolve_script_dir())

ARTICLE_VARIABLES <- data.frame(
  No = paste0("n", seq_len(54)),
  Dataset_Key = c(
    rep("history", 38),
    rep("physical", 8),
    "diagnosis",
    rep("order", 7)
  ),
  Domain = c(
    rep("Medical history", 38),
    rep("Physical examination", 8),
    "Clinical diagnosis",
    rep("Medical order", 7)
  ),
  Variable_CN = c(
    "既往诊断", "既往用药名称", "肉类情况", "水果情况", "蔬菜情况",
    "牛奶情况", "零食情况", "饮料情况", "豆浆情况", "蜂蜜情况",
    "保健品是否服用情况", "补品是否服用情况", "是否挑食", "入睡时间",
    "睡眠质量", "是否开灯睡觉", "食欲情况", "运动情况", "体质情况",
    "智力情况", "学习情况", "大便情况", "小便情况", "家中有无避孕药",
    "避孕药接触情况", "生活居住环境情况", "是否足月出生", "先兆流产保胎史",
    "缺氧窒息史", "黄疸史", "其他出生缺陷和异常情况", "药物过敏情况",
    "食物过敏情况", "环境过敏情况", "其他过敏情况", "母亲初潮年龄",
    "母亲身高", "父亲身高",
    "身高", "体重", "腋毛情况", "左乳房情况", "右乳房情况",
    "阴毛情况", "睾丸情况", "阴茎情况",
    "诊断名称",
    "当前用药名称", "药物使用剂量", "药物使用剂型", "药物使用频次",
    "药物使用方式", "医嘱复查标志", "医嘱复查时间"
  ),
  Label_EN = c(
    "Previous diagnoses", "Previous medications", "Meat", "Fruits", "Vegetables",
    "Milk", "Snacks", "Beverages", "Soymilk", "Honey",
    "Health supplements", "Tonic food", "Picky eater", "Sleep time",
    "Sleep quality", "Sleep with the light", "Appetite", "Exercise",
    "Constitution", "Intelligence", "Learning", "Stool", "Urination",
    "Contraceptive pills at home", "Contraceptive pills exposure",
    "Living environment", "Term birth", "Threatened abortion", "Hypoxia",
    "Jaundice", "Other congenital disorders", "Drug allergies", "Food allergies",
    "Environmental allergies", "Other allergies", "Maternal age of menarche",
    "Maternal height", "Paternal height",
    "Child height", "Child weight", "Axillary hair", "Left breast",
    "Right breast", "Pubic hair", "Testes", "Penis",
    "Current diagnoses",
    "Current medications", "Drug dosages", "Dosage forms",
    "Administration frequency", "Administration routes",
    "Follow-up visit order", "Follow-up visit time order"
  ),
  Metric_Type = c(
    "multilabel",
    rep("categorical", 34),
    rep("continuous", 3),
    rep("continuous", 2),
    rep("categorical", 6),
    "multilabel",
    rep("categorical", 7)
  ),
  stringsAsFactors = FALSE
)

SECONDARY_NUMBERS <- c(
  "n1", "n6", "n14", paste0("n", 28:35), "n45", "n46",
  "n49", "n50", "n51", "n54"
)
ARTICLE_VARIABLES$Secondary_Eligible <- ARTICLE_VARIABLES$No %in% SECONDARY_NUMBERS

DATA_SOURCES <- list(
  history = list(
    file = file.path(PROJECT_ROOT, "数据集", "病史C", "病史c_唯一值映射.xlsx"),
    primary_sheet = "Sheet 1",
    secondary_sheet = "Sheet2"
  ),
  physical = list(
    file = file.path(PROJECT_ROOT, "数据集", "体格营养D", "体格检查D_唯一值映射.xlsx"),
    primary_sheet = "Sheet 1",
    secondary_sheet = "Sheet2"
  ),
  diagnosis = list(
    file = file.path(PROJECT_ROOT, "数据集", "诊断名称G", "诊断名称G_唯一值映射.xlsx"),
    primary_sheet = "Sheet 1",
    secondary_sheet = NA_character_
  ),
  order = list(
    file = file.path(PROJECT_ROOT, "数据集", "诊治意见H", "诊治意见H_唯一值映射.xlsx"),
    primary_sheet = "Sheet 1",
    secondary_sheet = "Sheet2"
  )
)

ABSENT_TOKENS <- c(
  "", "0", "9", "na", "n/a", "null", "none",
  "无", "未提及", "未记录", "未知"
)

CONTINUOUS_TOLERANCES <- c(
  "母亲初潮年龄" = NA_real_,
  "母亲身高" = NA_real_,
  "父亲身高" = NA_real_,
  "身高" = NA_real_,
  "体重" = NA_real_
)

METRIC_DEFINITIONS <- data.frame(
  Metric_Type = c("categorical", "continuous", "multilabel"),
  Recommended_Reporting = c(
    "Exact accuracy (95% Wilson CI), macro precision/recall/F1, weighted F1, Cohen's kappa; presence-detection metrics are supplementary.",
    "Presence precision/recall/F1 plus MAE, RMSE, median absolute error, bias, 95% limits of agreement and Lin's CCC on paired positive values.",
    "Exact-match ratio, micro and sample-averaged precision/recall/F1, and mean Jaccard similarity."
  ),
  Important_Note = c(
    "All observed classes are retained. Incorrect non-zero classes contribute both a false positive for the predicted class and a false negative for the reference class.",
    "All configured absence tokens, including 9 and missing cells, are recoded to 0 before analysis. Continuous-value agreement is calculated only when both reference and prediction are greater than 0. Tolerance accuracy is left blank until a clinically justified tolerance is supplied.",
    "Labels are compared as sets after delimiter splitting. Empty-empty pairs count as exact matches; duplicate labels are removed."
  ),
  stringsAsFactors = FALSE
)

safe_divide <- function(numerator, denominator) {
  if (length(denominator) != 1L || is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }
  numerator / denominator
}

divide_or_zero <- function(numerator, denominator) {
  if (is.na(denominator) || denominator == 0) 0 else numerator / denominator
}

f1_score <- function(precision, recall) {
  if (is.na(precision) || is.na(recall) || precision + recall == 0) {
    return(if (is.na(precision) || is.na(recall)) NA_real_ else 0)
  }
  2 * precision * recall / (precision + recall)
}

wilson_interval <- function(successes, total, confidence = 0.95) {
  if (is.na(total) || total <= 0) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(1 - (1 - confidence) / 2)
  p <- successes / total
  denominator <- 1 + z^2 / total
  centre <- (p + z^2 / (2 * total)) / denominator
  half_width <- z * sqrt((p * (1 - p) + z^2 / (4 * total)) / total) / denominator
  c(max(0, centre - half_width), min(1, centre + half_width))
}

canonical_scalar <- function(x) {
  result <- trimws(as.character(x))
  result[is.na(x)] <- ""
  absent <- tolower(result) %in% ABSENT_TOKENS
  numeric_like <- !absent & grepl("^[+-]?[0-9]+(?:\\.[0-9]+)?$", result)
  numeric_values <- suppressWarnings(as.numeric(result))
  result[numeric_like & is.finite(numeric_values)] <- format(
    numeric_values[numeric_like & is.finite(numeric_values)],
    scientific = FALSE,
    trim = TRUE,
    digits = 15
  )
  result[absent] <- "0"
  result
}

is_present_categorical <- function(x) {
  normalized <- tolower(canonical_scalar(x))
  !is.na(normalized) & !(normalized %in% ABSENT_TOKENS)
}

binary_presence_metrics <- function(reference_present, prediction_present) {
  reference_present <- as.logical(reference_present)
  prediction_present <- as.logical(prediction_present)
  valid <- !is.na(reference_present) & !is.na(prediction_present)
  reference_present <- reference_present[valid]
  prediction_present <- prediction_present[valid]

  tp <- sum(reference_present & prediction_present)
  tn <- sum(!reference_present & !prediction_present)
  fp <- sum(!reference_present & prediction_present)
  fn <- sum(reference_present & !prediction_present)
  precision <- safe_divide(tp, tp + fp)
  recall <- safe_divide(tp, tp + fn)

  list(
    Presence_Accuracy = safe_divide(tp + tn, tp + tn + fp + fn),
    Presence_Precision = precision,
    Presence_Recall = recall,
    Presence_F1 = f1_score(precision, recall),
    Presence_Specificity = safe_divide(tn, tn + fp),
    Presence_TN = tn,
    Presence_FP = fp,
    Presence_FN = fn,
    Presence_TP = tp
  )
}

evaluate_categorical <- function(truth, prediction) {
  if (length(truth) != length(prediction)) {
    stop("truth and prediction must have the same length")
  }

  truth_raw <- truth
  prediction_raw <- prediction
  truth <- canonical_scalar(truth)
  prediction <- canonical_scalar(prediction)

  valid <- !is.na(truth)
  truth <- truth[valid]
  prediction <- prediction[valid]
  prediction[is.na(prediction)] <- "<MISSING>"

  if (!length(truth)) {
    return(list(
      N_Total = length(truth_raw), N_Evaluable = 0L,
      Accuracy = NA_real_, Accuracy_CI_Low = NA_real_,
      Accuracy_CI_High = NA_real_, Macro_Precision = NA_real_,
      Macro_Recall = NA_real_, Macro_F1 = NA_real_,
      Weighted_F1 = NA_real_, Cohen_Kappa = NA_real_,
      N_Classes = 0L
    ))
  }

  classes <- sort(unique(c(truth, prediction)))
  confusion <- table(
    factor(prediction, levels = classes),
    factor(truth, levels = classes),
    dnn = c("Prediction", "Reference")
  )

  tp <- diag(confusion)
  predicted_count <- rowSums(confusion)
  reference_count <- colSums(confusion)
  precision <- mapply(divide_or_zero, tp, predicted_count)
  recall <- mapply(divide_or_zero, tp, reference_count)
  f1 <- mapply(f1_score, precision, recall)
  accuracy <- sum(tp) / sum(confusion)
  accuracy_ci <- wilson_interval(sum(tp), sum(confusion))

  expected_agreement <- sum(predicted_count * reference_count) / sum(confusion)^2
  kappa <- if (1 - expected_agreement == 0) {
    NA_real_
  } else {
    (accuracy - expected_agreement) / (1 - expected_agreement)
  }

  result <- list(
    N_Total = length(truth_raw),
    N_Evaluable = length(truth),
    N_Classes = length(classes),
    Accuracy = accuracy,
    Accuracy_CI_Low = accuracy_ci[[1]],
    Accuracy_CI_High = accuracy_ci[[2]],
    Macro_Precision = mean(precision),
    Macro_Recall = mean(recall),
    Macro_F1 = mean(f1),
    Weighted_F1 = sum(f1 * reference_count) / sum(reference_count),
    Cohen_Kappa = kappa
  )

  presence <- binary_presence_metrics(
    is_present_categorical(truth_raw[valid]),
    is_present_categorical(prediction_raw[valid])
  )
  c(result, presence)
}

evaluate_continuous <- function(truth, prediction, tolerance = NA_real_) {
  if (length(truth) != length(prediction)) {
    stop("truth and prediction must have the same length")
  }

  truth_numeric <- suppressWarnings(as.numeric(canonical_scalar(truth)))
  prediction_numeric <- suppressWarnings(as.numeric(canonical_scalar(prediction)))
  truth_present <- is.finite(truth_numeric) & truth_numeric > 0
  prediction_present <- is.finite(prediction_numeric) & prediction_numeric > 0
  paired <- truth_present & prediction_present
  presence <- binary_presence_metrics(truth_present, prediction_present)

  if (!any(paired)) {
    return(c(
      list(
        N_Total = length(truth_numeric),
        N_Reference_Present = sum(truth_present),
        N_Prediction_Present = sum(prediction_present),
        N_Paired = 0L,
        MAE = NA_real_, RMSE = NA_real_, Median_AE = NA_real_,
        Bias = NA_real_, SD_Error = NA_real_, LoA_Lower = NA_real_,
        LoA_Upper = NA_real_, Pearson_R = NA_real_, Lin_CCC = NA_real_,
        Exact_Match = NA_real_, Tolerance = tolerance,
        Within_Tolerance = NA_real_
      ),
      presence
    ))
  }

  reference <- truth_numeric[paired]
  predicted <- prediction_numeric[paired]
  error <- predicted - reference
  error_sd <- if (length(error) > 1L) stats::sd(error) else NA_real_
  reference_variance <- if (length(reference) > 1L) stats::var(reference) else NA_real_
  predicted_variance <- if (length(predicted) > 1L) stats::var(predicted) else NA_real_
  covariance <- if (length(reference) > 1L) stats::cov(reference, predicted) else NA_real_
  ccc_denominator <- reference_variance + predicted_variance +
    (mean(reference) - mean(predicted))^2
  lin_ccc <- if (is.na(ccc_denominator) || ccc_denominator == 0) {
    NA_real_
  } else {
    2 * covariance / ccc_denominator
  }

  pearson_r <- if (
    length(reference) >= 3L &&
    stats::sd(reference) > 0 &&
    stats::sd(predicted) > 0
  ) {
    stats::cor(reference, predicted, method = "pearson")
  } else {
    NA_real_
  }

  c(
    list(
      N_Total = length(truth_numeric),
      N_Reference_Present = sum(truth_present),
      N_Prediction_Present = sum(prediction_present),
      N_Paired = length(error),
      MAE = mean(abs(error)),
      RMSE = sqrt(mean(error^2)),
      Median_AE = stats::median(abs(error)),
      Bias = mean(error),
      SD_Error = error_sd,
      LoA_Lower = if (is.na(error_sd)) NA_real_ else mean(error) - 1.96 * error_sd,
      LoA_Upper = if (is.na(error_sd)) NA_real_ else mean(error) + 1.96 * error_sd,
      Pearson_R = pearson_r,
      Lin_CCC = lin_ccc,
      Exact_Match = mean(abs(error) < sqrt(.Machine$double.eps)),
      Tolerance = tolerance,
      Within_Tolerance = if (is.na(tolerance)) {
        NA_real_
      } else {
        mean(abs(error) <= tolerance)
      }
    ),
    presence
  )
}

split_labels <- function(value) {
  normalized <- canonical_scalar(value)
  if (is.na(normalized)) return(character())
  normalized_lower <- tolower(normalized)
  if (normalized_lower %in% ABSENT_TOKENS) return(character())

  labels <- unlist(strsplit(normalized, "[,，;；、|/]+", perl = TRUE), use.names = FALSE)
  labels <- trimws(tolower(labels))
  labels <- labels[nzchar(labels) & !(labels %in% ABSENT_TOKENS)]
  sort(unique(labels))
}

evaluate_multilabel <- function(truth, prediction) {
  if (length(truth) != length(prediction)) {
    stop("truth and prediction must have the same length")
  }

  truth_sets <- lapply(truth, split_labels)
  prediction_sets <- lapply(prediction, split_labels)
  n <- length(truth_sets)

  tp <- fp <- fn <- 0
  exact <- jaccard <- sample_precision <- sample_recall <- sample_f1 <- numeric(n)

  for (i in seq_len(n)) {
    reference <- truth_sets[[i]]
    predicted <- prediction_sets[[i]]
    intersection_n <- length(intersect(reference, predicted))
    union_n <- length(union(reference, predicted))

    tp <- tp + intersection_n
    fp <- fp + length(setdiff(predicted, reference))
    fn <- fn + length(setdiff(reference, predicted))
    exact[[i]] <- as.numeric(identical(reference, predicted))

    if (!length(reference) && !length(predicted)) {
      sample_precision[[i]] <- 1
      sample_recall[[i]] <- 1
      sample_f1[[i]] <- 1
      jaccard[[i]] <- 1
    } else {
      sample_precision[[i]] <- if (length(predicted)) intersection_n / length(predicted) else 0
      sample_recall[[i]] <- if (length(reference)) intersection_n / length(reference) else 0
      sample_f1[[i]] <- f1_score(sample_precision[[i]], sample_recall[[i]])
      jaccard[[i]] <- if (union_n) intersection_n / union_n else 1
    }
  }

  micro_precision <- safe_divide(tp, tp + fp)
  micro_recall <- safe_divide(tp, tp + fn)

  list(
    N_Total = n,
    N_Evaluable = n,
    Exact_Match = mean(exact),
    Micro_Precision = micro_precision,
    Micro_Recall = micro_recall,
    Micro_F1 = f1_score(micro_precision, micro_recall),
    Sample_Precision = mean(sample_precision),
    Sample_Recall = mean(sample_recall),
    Sample_F1 = mean(sample_f1),
    Mean_Jaccard = mean(jaccard),
    Label_TP = tp,
    Label_FP = fp,
    Label_FN = fn
  )
}

recommended_metric <- function(metric_type) {
  switch(
    metric_type,
    categorical = "Accuracy; macro-F1; Cohen's kappa",
    continuous = "MAE; RMSE; Lin's CCC; Bland-Altman limits",
    multilabel = "Exact match; micro-F1; mean Jaccard",
    NA_character_
  )
}

evaluate_variable <- function(truth, prediction, variable_name, metric_type) {
  result <- switch(
    metric_type,
    categorical = evaluate_categorical(truth, prediction),
    continuous = evaluate_continuous(
      truth,
      prediction,
      tolerance = unname(CONTINUOUS_TOLERANCES[[variable_name]])
    ),
    multilabel = evaluate_multilabel(truth, prediction),
    stop("Unsupported metric type: ", metric_type)
  )
  result$Recommended_Primary_Metric <- recommended_metric(metric_type)
  result
}

bind_rows_fill <- function(rows) {
  if (!length(rows)) return(data.frame())
  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  normalized <- lapply(rows, function(row) {
    missing_names <- setdiff(all_names, names(row))
    if (length(missing_names)) row[missing_names] <- NA
    as.data.frame(row[all_names], stringsAsFactors = FALSE, check.names = FALSE)
  })
  do.call(rbind, normalized)
}

read_mapping_sheet <- function(path, sheet) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Install it with install.packages('openxlsx').")
  }
  openxlsx::read.xlsx(
    xlsxFile = path,
    sheet = sheet,
    colNames = TRUE,
    check.names = FALSE,
    detectDates = TRUE,
    skipEmptyRows = FALSE
  )
}

evaluate_stage <- function(stage = c("primary", "secondary")) {
  stage <- match.arg(stage)
  variables <- ARTICLE_VARIABLES
  if (stage == "secondary") {
    variables <- variables[variables$Secondary_Eligible, , drop = FALSE]
  }

  result_rows <- list()
  issue_rows <- list()
  result_index <- 0L
  issue_index <- 0L

  for (dataset_key in unique(variables$Dataset_Key)) {
    config <- DATA_SOURCES[[dataset_key]]
    sheet <- config[[paste0(stage, "_sheet")]]
    dataset_variables <- variables[
      variables$Dataset_Key == dataset_key,
      ,
      drop = FALSE
    ]

    if (is.null(sheet) || is.na(sheet)) {
      for (i in seq_len(nrow(dataset_variables))) {
        issue_index <- issue_index + 1L
        issue_rows[[issue_index]] <- data.frame(
          Stage = stage,
          No = dataset_variables$No[[i]],
          Variable_CN = dataset_variables$Variable_CN[[i]],
          Issue = "No sheet configured for this stage",
          stringsAsFactors = FALSE
        )
      }
      next
    }

    if (!file.exists(config$file)) {
      stop("Input workbook not found: ", config$file)
    }

    message(sprintf("[%s] Reading %s / %s", stage, basename(config$file), sheet))
    dataset <- read_mapping_sheet(config$file, sheet)

    for (i in seq_len(nrow(dataset_variables))) {
      variable <- dataset_variables[i, , drop = FALSE]
      truth_column <- paste0(variable$Variable_CN, "_真实值")
      prediction_column <- paste0(variable$Variable_CN, "_预测值")

      if (!(truth_column %in% names(dataset)) || !(prediction_column %in% names(dataset))) {
        issue_index <- issue_index + 1L
        issue_rows[[issue_index]] <- data.frame(
          Stage = stage,
          No = variable$No,
          Variable_CN = variable$Variable_CN,
          Issue = sprintf(
            "Missing column(s): %s",
            paste(
              setdiff(c(truth_column, prediction_column), names(dataset)),
              collapse = ", "
            )
          ),
          stringsAsFactors = FALSE
        )
        next
      }

      metrics <- evaluate_variable(
        truth = dataset[[truth_column]],
        prediction = dataset[[prediction_column]],
        variable_name = variable$Variable_CN,
        metric_type = variable$Metric_Type
      )

      result_index <- result_index + 1L
      result_rows[[result_index]] <- c(
        list(
          Stage = stage,
          No = variable$No,
          Domain = variable$Domain,
          Variable_CN = variable$Variable_CN,
          Label_EN = variable$Label_EN,
          Metric_Type = variable$Metric_Type,
          Source_File = basename(config$file),
          Source_Sheet = sheet
        ),
        metrics
      )
    }
  }

  list(
    results = bind_rows_fill(result_rows),
    issues = bind_rows_fill(issue_rows)
  )
}

add_article_compatibility_columns <- function(results) {
  if (!nrow(results)) return(results)

  results$Article_Accuracy <- NA_real_
  results$Article_Precision <- NA_real_
  results$Article_Recall <- NA_real_
  results$Article_F1 <- NA_real_

  categorical <- results$Metric_Type == "categorical"
  multilabel <- results$Metric_Type == "multilabel"

  results$Article_Accuracy[categorical] <- results$Accuracy[categorical]
  results$Article_Precision[categorical] <- results$Macro_Precision[categorical]
  results$Article_Recall[categorical] <- results$Macro_Recall[categorical]
  results$Article_F1[categorical] <- results$Macro_F1[categorical]

  results$Article_Accuracy[multilabel] <- results$Exact_Match[multilabel]
  results$Article_Precision[multilabel] <- results$Micro_Precision[multilabel]
  results$Article_Recall[multilabel] <- results$Micro_Recall[multilabel]
  results$Article_F1[multilabel] <- results$Micro_F1[multilabel]

  results
}

round_results <- function(data, digits = 6L) {
  if (!nrow(data)) return(data)
  count_columns <- grepl(
    "^(N_|Presence_(TN|FP|FN|TP)$|Label_(TP|FP|FN)$)",
    names(data)
  )
  numeric_columns <- vapply(data, is.numeric, logical(1))
  columns_to_round <- numeric_columns & !count_columns
  data[columns_to_round] <- lapply(data[columns_to_round], round, digits = digits)
  data
}

write_results_workbook <- function(primary, secondary, issues, output_path) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  primary <- round_results(add_article_compatibility_columns(primary))
  secondary <- round_results(add_article_compatibility_columns(secondary))
  if (!nrow(issues)) {
    issues <- data.frame(Note = "No issues detected", stringsAsFactors = FALSE)
  }

  sheets <- list(
    Article_54_Primary = primary,
    Secondary_17 = secondary,
    Primary_Categorical = primary[
      primary$Metric_Type == "categorical",
      ,
      drop = FALSE
    ],
    Primary_Continuous = primary[
      primary$Metric_Type == "continuous",
      ,
      drop = FALSE
    ],
    Primary_Multilabel = primary[
      primary$Metric_Type == "multilabel",
      ,
      drop = FALSE
    ],
    Variable_Dictionary = ARTICLE_VARIABLES,
    Metric_Definitions = METRIC_DEFINITIONS,
    Issues = issues
  )

  writexl::write_xlsx(sheets, path = output_path, format_headers = TRUE)
  invisible(output_path)
}

run_article_evaluation <- function(output_path = NULL) {
  if (is.null(output_path) || !nzchar(output_path)) {
    output_path <- Sys.getenv(
      "EHR_METRICS_OUTPUT",
      unset = file.path(
        PROJECT_ROOT,
        "分析结果",
        "信息抽取评估指标_文章版.xlsx"
      )
    )
  }

  if (nrow(ARTICLE_VARIABLES) != 54L ||
      !identical(ARTICLE_VARIABLES$No, paste0("n", seq_len(54))) ||
      anyDuplicated(ARTICLE_VARIABLES$Variable_CN)) {
    stop("ARTICLE_VARIABLES must contain unique n1-n54 variables in article order.")
  }

  primary <- evaluate_stage("primary")
  secondary <- evaluate_stage("secondary")
  issues <- bind_rows_fill(c(
    split(primary$issues, seq_len(nrow(primary$issues))),
    split(secondary$issues, seq_len(nrow(secondary$issues)))
  ))

  if (nrow(primary$results) != 54L) {
    warning(sprintf(
      "Expected 54 primary variables but evaluated %d. See Issues sheet.",
      nrow(primary$results)
    ))
  }
  if (nrow(secondary$results) != 17L) {
    warning(sprintf(
      "Expected 17 secondary variables but evaluated %d. See Issues sheet.",
      nrow(secondary$results)
    ))
  }

  write_results_workbook(
    primary = primary$results,
    secondary = secondary$results,
    issues = issues,
    output_path = output_path
  )

  message("Evaluation completed: ", normalizePath(output_path, mustWork = FALSE))
  invisible(list(
    primary = primary$results,
    secondary = secondary$results,
    issues = issues,
    output_path = output_path
  ))
}

skip_main <- tolower(Sys.getenv("EHR_METRICS_SKIP_MAIN", unset = "false")) %in%
  c("1", "true", "yes")

if (!skip_main && identical(environment(), globalenv())) {
  run_article_evaluation()
}
