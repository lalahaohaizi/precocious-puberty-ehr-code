# Analysis code — LLM-based information extraction from CPP electronic health records

R scripts used to evaluate and visualise large-language-model (LLM) extraction of individual
risk factors from unstructured electronic health records (EHR) of children with central
precocious puberty (CPP).

This repository contains **analysis code only**. It does not contain patient records, the
curated dataset, or the model outputs; every script reads those from Excel workbooks that are
not distributed here (see [Data availability](#data-availability)).

## Repository contents

| Script | What it does |
| --- | --- |
| [`Evaluation indicator.r`](Evaluation%20indicator.r) | Scores reference vs. predicted values for all 54 extracted variables and writes one multi-sheet workbook of metrics. The only self-contained, headless-runnable script. |
| [`Model compare.r`](Model%20compare.r) | Compares five locally deployed models (GLM4:9B, Qwen2:7B, Yi:6B, Llama3.1:8B, Gemma2:9B) on overall and per-factor accuracy/precision/recall/F1 — bar plots, radar chart, heatmap. |
| [`Model prediction.R`](Model%20prediction.R) | LASSO variable selection and logistic regression for the return-visit flag, with 500-replicate bootstrap internal validation, an 80/20 external split, ROC, and `rms::val.prob` calibration. |
| [`Group correlation heatmap.R`](Group%20correlation%20heatmap.R) | Pearson correlations between text-similarity metrics (Cosine, Dice, Jaccard, Levenshtein) and length/time metrics, computed separately for each EHR section. |
| [`Circular heatmap.R`](Circular%20heatmap.R) | Radial heatmap (`coord_radial`) of extraction performance across risk-factor subcategories. |
| [`Text similarity and tokens.R`](Text%20similarity%20and%20tokens.R) | String-edit-distance similarity between source text and model output, paired *t*-tests, and `tiktoken` token counts. |

The scripts are independent of one another — none sources another, and each reloads its own
packages. `Evaluation indicator.r` is the upstream analysis: the metric workbook it produces is
what the `Circular heatmap.R` and `Group correlation heatmap.R` inputs are derived from, though
`Model compare.r` carries its five-model scores as literals instead of reading them.

## Requirements

R (tested on R 4.x) plus:

```r
install.packages(c(
  "tidyverse", "dplyr", "ggplot2", "tidyr", "patchwork", "cowplot", "gridExtra",
  "readxl", "openxlsx", "writexl",
  "glmnet", "caret", "pROC", "boot", "rms", "probably", "Metrics", "Matrix", "scales",
  "psych", "corrr", "linkET", "ggtext", "geomtextpath", "fmsb", "forcats", "viridis",
  "MetBrewer", "RColorBrewer", "showtext", "sysfonts",
  "stringdist", "reticulate"
))
```

`Text similarity and tokens.R` additionally needs a Python environment with `tiktoken`
(`reticulate::use_python()` / `use_condaenv()` point it at one — edit to your own path).
No script calls an LLM API: model outputs are expected to already be tabulated in the workbooks.

## Data availability

Input workbooks are not included. The visualisation scripts address them as placeholder paths
(`read.xlsx(".xlsx")`) that you substitute by hand; the evaluation script builds real paths under
`EHR_EXTRACTION_ROOT`, and `Model prediction.R` assumes `final_data` is already in the session.
Expected shapes:

- **Evaluation**: four per-domain mapping workbooks, looked up as
  `<root>/数据集/病史C/病史c_唯一值映射.xlsx`, `…/体格营养D/体格检查D_唯一值映射.xlsx`,
  `…/诊断名称G/诊断名称G_唯一值映射.xlsx`, `…/诊治意见H/诊治意见H_唯一值映射.xlsx`,
  each with a `Sheet 1` (primary extraction) and `Sheet2` (secondary/normalised mapping), and
  column pairs named `<变量名>_真实值` / `<变量名>_预测值`. Set `EHR_EXTRACTION_ROOT` to a tree
  laid out this way; the subdirectory names above are what the script looks for, and they do not
  match the workbook naming in this working copy, so leaving the default will raise
  `Input workbook not found`.
- **Group correlation**: one sheet per EHR section with columns `Cosine`, `Dice`, `Levenshtein`,
  `Jaccard`, `input length`, `output length`, `time`.
- **Circular heatmap**: long-format `id`, `label`, `Accuracy`, `Precision`, `Recall`, `F1-score`.

Because the records are clinical, share them only under your institution's data governance terms.

## Running the evaluation script

```bash
export EHR_EXTRACTION_ROOT=/path/to/parent        # directory containing 数据集/
export EHR_METRICS_OUTPUT=/path/to/metrics.xlsx   # optional
Rscript "Evaluation indicator.r"
```

Without those variables `PROJECT_ROOT` falls back to the script's own directory, so it only works
unattended if `数据集/` actually sits beside the script — usually you want to set
`EHR_EXTRACTION_ROOT`. Output defaults to `分析结果/信息抽取评估指标_文章版.xlsx` under that root.
`evaluate_stage()` and the rest can be sourced without triggering `main` by setting
`EHR_METRICS_SKIP_MAIN=true`.

Output sheets: `Article_54_Primary`, `Secondary_17`, per-metric-type splits of the primary
results, `Variable_Dictionary`, `Metric_Definitions`, and `Issues` (variables whose columns were
absent rather than silently dropped).

### Metric selection

Metrics are assigned by variable type, not applied uniformly — 47 categorical, 5 continuous,
2 multilabel across the 54 variables.

| Type | Variables | Primary metrics |
| --- | --- | --- |
| Categorical | single-answer history, examination and medication items | exact accuracy with Wilson 95% CI, macro precision/recall/F1, weighted F1, Cohen's kappa |
| Continuous | maternal menarche age, parental heights, child height/weight | presence precision/recall/F1, then MAE, RMSE, median absolute error, bias, 95% limits of agreement, Lin's CCC on paired positive values |
| Multilabel | previous diagnoses, current diagnoses | exact-match ratio, micro and sample-averaged precision/recall/F1, mean Jaccard |

Absence is normalised before comparison: empty cells and the tokens `0`, `9`, `无`, `未提及`,
`未记录`, `未知`, `none`, `null`, `na` all recode to `0`. Continuous agreement is computed only
where reference and prediction are both positive. Numeric-looking strings are canonicalised, so
`17.0` and `17` count as the same class rather than as a disagreement. Tolerance-based accuracy is
left blank (`NA`) until a clinically justified tolerance is supplied.

## Before running elsewhere

These scripts were written for one machine and one dataset, and the paths reflect that. To
reproduce on another setup:

- Replace every `".xlsx"` placeholder with a real path.
- `Group correlation heatmap.R` writes its two summary workbooks to `C:/Users/64291/Desktop/`
  (lines 332, 342) — change these.
- `Text similarity and tokens.R` hardcodes a conda environment path (lines 40–41).
- Font files are referenced by absolute Windows path in the plotting scripts
  (`msyh.ttc`, `simsun.ttc`, `arial.ttf`, `simhei.TTF`); substitute whatever CJK font your system has.

Known rough edges, left as-is because they are outside the scope of publishing the code:
`Model compare.r` references an undefined `feature_f1_long_df` in its first half (the working
name is `feature_f1_data_long`, used from line 512 on), and `Text similarity and tokens.R`
compares `data$similarity_*` columns and a `df` object that its own code never creates — both
blocks are leftovers from interactive sessions and will error if run top to bottom.

## Licence

Rights reserved; released for review of the associated manuscript. Open an issue if you would
like to reuse this code.
