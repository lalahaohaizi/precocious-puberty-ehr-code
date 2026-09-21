# Analysis code — LLM-based information extraction from CPP electronic health records

R scripts for a study extracting individual risk factors from unstructured electronic health
records (EHR) of children with central precocious puberty (CPP). Analysis code only; the record
data and model outputs are read from Excel workbooks that are not included here.

## Files

| Script | What it does |
| --- | --- |
| `Evaluation indicator.r` | Compares reference against predicted values for all 54 extracted variables and writes the performance metrics to a multi-sheet workbook. Categorical variables get accuracy, macro/weighted F1 and Cohen's kappa; continuous ones get MAE, RMSE, bias, limits of agreement and Lin's CCC; the two diagnosis fields get exact-match, micro-F1 and Jaccard. |
| `Model compare.r` | Compares five locally deployed models (GLM4:9B, Qwen2:7B, Yi:6B, Llama3.1:8B, Gemma2:9B) on overall and per-factor accuracy, precision, recall and F1 — bar charts, radar chart, heatmap. |
| `Model prediction.R` | Builds a return-visit prediction model: LASSO variable selection then logistic regression, with bootstrap internal validation, an external hold-out split, ROC and calibration curves. |
| `Group correlation heatmap.R` | Correlates text-similarity metrics (Cosine, Dice, Jaccard, Levenshtein) with input/output length and inference time, plotted separately for each EHR section. |
| `Circular heatmap.R` | Draws a radial heatmap of extraction performance across risk-factor subcategories. |
| `Text similarity and tokens.R` | Measures similarity between the source text and the model output, tests the differences with paired *t*-tests, and counts tokens with `tiktoken`. |

The scripts are independent of one another; each reads its own workbook, whose path has to be
filled in before running.
