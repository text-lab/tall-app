# ==============================================================================
# Generate LaTeX Table for TALL Benchmark Results
# ==============================================================================

library(dplyr)
library(kableExtra)

# ==============================================================================
# Prepare data
# ==============================================================================

# Create the benchmark data frame (based on your table)
benchmark_table <- data.frame(
  Setting = c(
    "10 docs, 1 sent",
    "10 docs, 10 sent",
    "10 docs, 100 sent",
    "100 docs, 1 sent",
    "100 docs, 10 sent",
    "100 docs, 100 sent",
    "1000 docs, 1 sent",
    "1000 docs, 10 sent",
    "1000 docs, 100 sent",
    "5000 docs, 1 sent",
    "5000 docs, 10 sent",
    "5000 docs, 100 sent",
    "10000 docs, 1 sent",
    "10000 docs, 10 sent"
  ),
  N_Tokens = c(
    78,
    1103,
    10436,
    1155,
    9920,
    106696,
    11372,
    106213,
    1064577,
    54460,
    532328,
    5331261,
    109237,
    1070462
  ),
  # Execution time (sec)
  Time_Preproc = c(
    0.53,
    0.6,
    1.31,
    0.62,
    1.15,
    7.96,
    0.96,
    6.94,
    73.21,
    2.79,
    32.44,
    398.78,
    5.56,
    64.06
  ),
  Time_Multi = c(
    0.01,
    0.01,
    0.04,
    0.01,
    0.04,
    0.27,
    0.04,
    0.25,
    1.7,
    0.15,
    0.88,
    7.77,
    0.24,
    1.65
  ),
  Time_Key = c(
    3.15,
    3.88,
    3.81,
    3.42,
    3.37,
    3.64,
    3.52,
    3.6,
    3.73,
    3.92,
    4.1,
    5.59,
    3.83,
    3.76
  ),
  Time_TM = c(
    0,
    0.01,
    0.05,
    0.01,
    0.06,
    0.46,
    0.06,
    0.53,
    4.79,
    0.29,
    2.54,
    23.98,
    0.58,
    5.48
  ),
  Time_Embed = c(
    0,
    0.01,
    0.02,
    0.01,
    0.02,
    0.23,
    0.02,
    0.22,
    2.33,
    0.12,
    1.19,
    11.61,
    0.24,
    2.4
  ),
  Time_Net = c(
    0.02,
    0.03,
    0.05,
    0.03,
    0.05,
    0.16,
    0.05,
    0.16,
    1.2,
    0.11,
    0.62,
    5.14,
    0.17,
    1.16
  ),
  Time_Polar = c(
    0.01,
    0.01,
    0.04,
    0.02,
    0.05,
    0.35,
    0.11,
    0.58,
    3.76,
    0.6,
    2.03,
    16.48,
    1.11,
    4.32
  ),
  # Memory (MB)
  Mem_Preproc = c(
    0.1,
    0.3,
    1.8,
    0.2,
    2,
    15.1,
    2.1,
    15.2,
    138.2,
    8.2,
    70.3,
    678.8,
    15.2,
    139.3
  ),
  Mem_Multi = c(
    1.1,
    1.6,
    8,
    1.6,
    8,
    64,
    8.1,
    64,
    555.3,
    33.6,
    284.5,
    2730.2,
    66.8,
    552.6
  ),
  Mem_Key = c(
    390.4,
    391.7,
    396.3,
    391.8,
    396.3,
    473.4,
    396.8,
    464.4,
    1153.5,
    427.8,
    766.8,
    4435.1,
    484.8,
    1156.5
  ),
  Mem_TM = c(
    0.5,
    1,
    4.4,
    1.1,
    5.5,
    36.6,
    6.1,
    45.6,
    344.5,
    27,
    219,
    1669.8,
    53.7,
    435.6
  ),
  Mem_Embed = c(
    0.1,
    0.3,
    2.5,
    0.3,
    2.5,
    24.3,
    2.6,
    24.5,
    249.9,
    12.7,
    125.2,
    1235.5,
    25.9,
    250.9
  ),
  Mem_Net = c(
    3.4,
    7.2,
    16.1,
    3,
    15.8,
    60.3,
    12.2,
    60.9,
    486.8,
    36.8,
    252.6,
    2405,
    66.9,
    492
  ),
  Mem_Polar = c(
    2.7,
    3.6,
    13.4,
    4.1,
    14.1,
    113,
    18.6,
    120.9,
    1115.6,
    83.9,
    595.4,
    5585.5,
    166.8,
    1192.4
  )
)

# Format numbers
benchmark_table <- benchmark_table %>%
  mutate(
    N_Tokens = format(N_Tokens, big.mark = ",", scientific = FALSE)
  )

# ==============================================================================
# OPTION 1: Full table with both Time and Memory
# ==============================================================================

latex_full <- benchmark_table %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    digits = 2,
    col.names = c(
      "Setting",
      "N. Tokens",
      "Pre-proc",
      "Multi",
      "Key",
      "TM",
      "Embed",
      "Net",
      "Polar",
      "Pre-proc",
      "Multi",
      "Key",
      "TM",
      "Embed",
      "Net",
      "Polar"
    ),
    align = c("l", "r", rep("r", 14)),
    caption = "TALL computational performance benchmarks across different corpus sizes.",
    label = "tab:performance"
  ) %>%
  add_header_above(c(
    " " = 2,
    "Execution Time (sec)" = 7,
    "Memory (MB)" = 7
  )) %>%
  kable_styling(
    latex_options = c("scale_down", "hold_position"),
    font_size = 9
  ) %>%
  row_spec(0, bold = TRUE)

# Save to file
writeLines(latex_full, "tall_benchmark_table_full.tex")

cat("=== LaTeX Table (Full) ===\n")
cat(latex_full)
cat("\n\n")

# ==============================================================================
# OPTION 2: Time only (cleaner for manuscript)
# ==============================================================================

benchmark_time <- benchmark_table %>%
  select(Setting, N_Tokens, starts_with("Time_"))

latex_time <- benchmark_time %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    digits = 2,
    col.names = c(
      "Setting",
      "N. Tokens",
      "Preprocessing",
      "Multiword",
      "Keyness",
      "Topic Model",
      "Embedding",
      "Network",
      "Polarity"
    ),
    align = c("l", "r", rep("r", 7)),
    caption = "TALL execution time (seconds) across different corpus sizes.",
    label = "tab:performance_time"
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 10
  ) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, width = "3cm") %>%
  column_spec(2, width = "1.5cm")

writeLines(latex_time, "/mnt/user-data/outputs/tall_benchmark_table_time.tex")

cat("=== LaTeX Table (Time Only) ===\n")
cat(latex_time)
cat("\n\n")

# ==============================================================================
# OPTION 3: Memory only
# ==============================================================================

benchmark_memory <- benchmark_table %>%
  select(Setting, N_Tokens, starts_with("Mem_"))

latex_memory <- benchmark_memory %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    digits = 1,
    col.names = c(
      "Setting",
      "N. Tokens",
      "Preprocessing",
      "Multiword",
      "Keyness",
      "Topic Model",
      "Embedding",
      "Network",
      "Polarity"
    ),
    align = c("l", "r", rep("r", 7)),
    caption = "TALL memory usage (MB) across different corpus sizes.",
    label = "tab:performance_memory"
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 10
  ) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, width = "3cm") %>%
  column_spec(2, width = "1.5cm")

writeLines(
  latex_memory,
  "/mnt/user-data/outputs/tall_benchmark_table_memory.tex"
)

cat("=== LaTeX Table (Memory Only) ===\n")
cat(latex_memory)
cat("\n\n")

# ==============================================================================
# OPTION 4: Compact combined format (time/memory in same cell)
# ==============================================================================

benchmark_compact <- benchmark_table %>%
  mutate(
    Preproc = sprintf("%.2f / %.1f", Time_Preproc, Mem_Preproc),
    Multi = sprintf("%.2f / %.1f", Time_Multi, Mem_Multi),
    Keyness = sprintf("%.2f / %.0f", Time_Key, Mem_Key),
    TM = sprintf("%.2f / %.1f", Time_TM, Mem_TM),
    Embed = sprintf("%.2f / %.1f", Time_Embed, Mem_Embed),
    Network = sprintf("%.2f / %.1f", Time_Net, Mem_Net),
    Polarity = sprintf("%.2f / %.1f", Time_Polar, Mem_Polar)
  ) %>%
  select(
    Setting,
    N_Tokens,
    Preproc,
    Multi,
    Keyness,
    TM,
    Embed,
    Network,
    Polarity
  )

latex_compact <- benchmark_compact %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    col.names = c(
      "Setting",
      "N. Tokens",
      "Preprocessing",
      "Multiword",
      "Keyness",
      "Topic Model",
      "Embedding",
      "Network",
      "Polarity"
    ),
    align = c("l", "r", rep("c", 7)),
    caption = "TALL performance benchmarks. Each cell shows execution time (seconds) / memory usage (MB).",
    label = "tab:performance_compact",
    escape = FALSE
  ) %>%
  kable_styling(
    latex_options = c("scale_down", "hold_position"),
    font_size = 9
  ) %>%
  row_spec(0, bold = TRUE)

writeLines(
  latex_compact,
  "/mnt/user-data/outputs/tall_benchmark_table_compact.tex"
)

cat("=== LaTeX Table (Compact) ===\n")
cat(latex_compact)
cat("\n\n")

# ==============================================================================
# OPTION 5: Professional manuscript table with custom LaTeX
# ==============================================================================

create_manuscript_latex <- function(data) {
  latex_code <- "\\begin{table}[htbp]
\\centering
\\caption{TALL computational performance benchmarks across different corpus sizes. Execution time (seconds) and memory usage (MB) for seven core operations.}
\\label{tab:tall_performance}
\\small
\\begin{tabular}{@{}lrrrrrrrrrrrrrrr@{}}
\\toprule
& & \\multicolumn{7}{c}{\\textbf{Execution Time (sec)}} & \\multicolumn{7}{c}{\\textbf{Memory (MB)}} \\\\
\\cmidrule(lr){3-9} \\cmidrule(lr){10-16}
\\textbf{Setting} & \\textbf{N. Tokens} & \\textbf{Prep.} & \\textbf{Multi} & \\textbf{Key} & \\textbf{TM} & \\textbf{Emb.} & \\textbf{Net} & \\textbf{Pol.} & \\textbf{Prep.} & \\textbf{Multi} & \\textbf{Key} & \\textbf{TM} & \\textbf{Emb.} & \\textbf{Net} & \\textbf{Pol.} \\\\
\\midrule
"

  for (i in 1:nrow(data)) {
    row <- data[i, ]
    latex_code <- paste0(
      latex_code,
      sprintf(
        "%s & %s & %.2f & %.2f & %.2f & %.2f & %.2f & %.2f & %.2f & %.1f & %.1f & %.0f & %.1f & %.1f & %.1f & %.1f \\\\\n",
        row$Setting,
        row$N_Tokens,
        row$Time_Preproc,
        row$Time_Multi,
        row$Time_Key,
        row$Time_TM,
        row$Time_Embed,
        row$Time_Net,
        row$Time_Polar,
        row$Mem_Preproc,
        row$Mem_Multi,
        row$Mem_Key,
        row$Mem_TM,
        row$Mem_Embed,
        row$Mem_Net,
        row$Mem_Polar
      )
    )

    # Add midrule after every 3 rows for readability
    if (i %% 3 == 0 && i < nrow(data)) {
      latex_code <- paste0(latex_code, "\\addlinespace\n")
    }
  }

  latex_code <- paste0(
    latex_code,
    "\\bottomrule
\\end{tabular}
\\begin{tablenotes}[flushleft]
\\small
\\item \\textit{Note:} Prep. = Preprocessing; Multi = Multiword extraction; Key = Keyness analysis; TM = Topic modeling; Emb. = Word embeddings; Net = Network analysis; Pol. = Polarity analysis. Tests conducted on [INSERT HARDWARE SPECS].
\\end{tablenotes}
\\end{table}
"
  )

  return(latex_code)
}

latex_manuscript <- create_manuscript_latex(benchmark_table)
writeLines(
  latex_manuscript,
  "/mnt/user-data/outputs/tall_benchmark_manuscript.tex"
)

cat("=== LaTeX Table (Manuscript Format) ===\n")
cat(latex_manuscript)

# ==============================================================================
# Summary
# ==============================================================================

cat("\n✓ LaTeX tables generated:\n")
cat("  1. tall_benchmark_table_full.tex - Full table with time and memory\n")
cat("  2. tall_benchmark_table_time.tex - Execution time only\n")
cat("  3. tall_benchmark_table_memory.tex - Memory usage only\n")
cat("  4. tall_benchmark_table_compact.tex - Combined format (time/memory)\n")
cat("  5. tall_benchmark_manuscript.tex - Professional manuscript format\n\n")

cat("To use in your LaTeX document:\n")
cat("  \\input{tall_benchmark_manuscript.tex}\n\n")
cat("Or copy the code directly into your manuscript.\n")
