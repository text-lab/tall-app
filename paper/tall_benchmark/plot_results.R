# ==============================================================================
# Create Line Plots for Execution Time and Memory vs N. Tokens
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ==============================================================================
# Create the data from your table
# ==============================================================================

# Assuming df_wide_ordered or similar exists, otherwise create from scratch
benchmark_data <- data.frame(
  Setting = c(
    "10 documents with 1 sentences each",
    "10 documents with 10 sentences each",
    "10 documents with 100 sentences each",
    "100 documents with 1 sentences each",
    "100 documents with 10 sentences each",
    "100 documents with 100 sentences each",
    "1000 documents with 1 sentences each",
    "1000 documents with 10 sentences each",
    "1000 documents with 100 sentences each",
    "5000 documents with 1 sentences each",
    "5000 documents with 10 sentences each",
    "5000 documents with 100 sentences each",
    "10000 documents with 1 sentences each",
    "10000 documents with 10 sentences each"
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
  Preprocessing = c(
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
  Multiword = c(
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
  Keyness = c(
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
  TM = c(
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
  Embedding = c(
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
  Network = c(
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
  Polarity = c(
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
  # Memory (MByte)
  Preprocessing_mem = c(
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
  Multiword_mem = c(
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
  Keyness_mem = c(
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
  TM_mem = c(
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
  Embedding_mem = c(
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
  Network_mem = c(
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
  Polarity_mem = c(
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

save(benchmark_data, file = "tall_benchmark_results.RData")

# ==============================================================================
# Transform to long format for plotting
# ==============================================================================

# For execution time
df_time_long <- benchmark_data %>%
  select(
    Setting,
    N_Tokens,
    Preprocessing,
    Multiword,
    Keyness,
    TM,
    Embedding,
    Network,
    Polarity
  ) %>%
  pivot_longer(
    cols = c(
      Preprocessing,
      Multiword,
      Keyness,
      TM,
      Embedding,
      Network,
      Polarity
    ),
    names_to = "Method",
    values_to = "Time_sec"
  ) %>%
  arrange(N_Tokens)

# For memory
df_memory_long <- benchmark_data %>%
  select(Setting, N_Tokens, ends_with("_mem")) %>%
  rename(
    Preprocessing = Preprocessing_mem,
    Multiword = Multiword_mem,
    Keyness = Keyness_mem,
    TM = TM_mem,
    Embedding = Embedding_mem,
    Network = Network_mem,
    Polarity = Polarity_mem
  ) %>%
  pivot_longer(
    cols = c(
      Preprocessing,
      Multiword,
      Keyness,
      TM,
      Embedding,
      Network,
      Polarity
    ),
    names_to = "Method",
    values_to = "Memory_MB"
  ) %>%
  arrange(N_Tokens)

# ==============================================================================
# PLOT 1: Execution Time vs N. Tokens
# ==============================================================================

# Define colors for methods
method_colors <- c(
  "Preprocessing" = "#E41A1C",
  "Multiword" = "#377EB8",
  "Keyness" = "#4DAF4A",
  "TM" = "#984EA3",
  "Embedding" = "#FF7F00",
  "Network" = "#FFFF33",
  "Polarity" = "#A65628"
)

p1 <- ggplot(
  df_time_long,
  aes(x = N_Tokens, y = Time_sec, color = Method, group = Method)
) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2.5, alpha = 0.9) +
  scale_x_log10(
    breaks = c(100, 1000, 10000, 100000, 1000000),
    labels = comma_format()
  ) +
  scale_y_log10(
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = comma_format()
  ) +
  scale_color_manual(values = method_colors) +
  labs(
    title = "TALL Performance: Execution Time by Method",
    subtitle = "Processing time increases with corpus size (number of tokens)",
    x = "Number of Tokens (log scale)",
    y = "Execution Time (seconds, log scale)",
    color = "Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_line(linetype = "dotted", color = "gray90"),
    panel.grid.major = element_line(color = "gray85"),
    axis.text = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 11)
  )

print(p1)
ggsave(
  "tall_execution_time_by_tokens.png",
  plot = p1,
  width = 12,
  height = 7,
  dpi = 300
)

# ==============================================================================
# PLOT 2: Memory Usage vs N. Tokens
# ==============================================================================

p2 <- ggplot(
  df_memory_long,
  aes(x = N_Tokens, y = Memory_MB, color = Method, group = Method)
) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2.5, alpha = 0.9) +
  scale_x_log10(
    breaks = c(100, 1000, 10000, 100000, 1000000),
    labels = comma_format()
  ) +
  scale_y_log10(
    breaks = c(0.1, 1, 10, 100, 1000, 10000),
    labels = comma_format()
  ) +
  scale_color_manual(values = method_colors) +
  labs(
    title = "TALL Performance: Memory Usage by Method",
    subtitle = "Memory consumption increases with corpus size (number of tokens)",
    x = "Number of Tokens (log scale)",
    y = "Memory Allocated (MB, log scale)",
    color = "Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_line(linetype = "dotted", color = "gray90"),
    panel.grid.major = element_line(color = "gray85"),
    axis.text = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 11)
  )

print(p2)
ggsave(
  "tall_memory_usage_by_tokens.png",
  plot = p2,
  width = 12,
  height = 7,
  dpi = 300
)

# ==============================================================================
# PLOT 3: Combined plot (Time and Memory side by side)
# ==============================================================================

library(patchwork)

p_combined <- p1 +
  p2 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "TALL Computational Performance Benchmarks",
    subtitle = "Execution time and memory usage across different corpus sizes",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 13, color = "gray40", hjust = 0.5)
    )
  )

print(p_combined)
ggsave(
  "tall_performance_combined.png",
  plot = p_combined,
  width = 18,
  height = 7,
  dpi = 300
)

# ==============================================================================
# Summary statistics
# ==============================================================================

cat("\n=== Performance Summary ===\n\n")

# Time summary
time_summary <- df_time_long %>%
  group_by(Method) %>%
  summarise(
    Min_Time = min(Time_sec),
    Max_Time = max(Time_sec),
    Range_Factor = Max_Time / Min_Time,
    .groups = "drop"
  ) %>%
  arrange(desc(Max_Time))

cat("Execution Time Summary:\n")
print(time_summary)

# Memory summary
memory_summary <- df_memory_long %>%
  group_by(Method) %>%
  summarise(
    Min_Memory = min(Memory_MB),
    Max_Memory = max(Memory_MB),
    Range_Factor = Max_Memory / Min_Memory,
    .groups = "drop"
  ) %>%
  arrange(desc(Max_Memory))

cat("\nMemory Usage Summary:\n")
print(memory_summary)

cat("\n✓ Plots created:\n")
cat("  1. tall_execution_time_by_tokens.png\n")
cat("  2. tall_memory_usage_by_tokens.png\n")
cat("  3. tall_performance_combined.png\n")
