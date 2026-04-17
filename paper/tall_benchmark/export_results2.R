# ==============================================================================
# Transform df_complete to WIDE format - Multiple Options
# ==============================================================================

library(dplyr)
library(tidyr)

# ==============================================================================
# OPTION 1: Wide format - Time only (one column per operation)
# ==============================================================================

df_wide_time <- df_complete %>%
  select(
    n_documents,
    n_sentences,
    setting_clean,
    operation,
    median_time_sec
  ) %>%
  pivot_wider(
    names_from = operation,
    values_from = median_time_sec,
    names_prefix = "time_"
  )

cat("=== OPTION 1: Wide format - Time only ===\n")
print(head(df_wide_time))
write.csv(
  df_wide_time,
  "/mnt/user-data/outputs/df_wide_time_only.csv",
  row.names = FALSE
)

# ==============================================================================
# OPTION 2: Wide format - Memory only (one column per operation)
# ==============================================================================

df_wide_memory <- df_complete %>%
  select(
    n_documents,
    n_sentences,
    setting_clean,
    operation,
    mem_alloc_bytes
  ) %>%
  mutate(mem_alloc_mb = mem_alloc_bytes / 1024^2) %>%
  select(-mem_alloc_bytes) %>%
  pivot_wider(
    names_from = operation,
    values_from = mem_alloc_mb,
    names_prefix = "memory_mb_"
  )

cat("\n=== OPTION 2: Wide format - Memory only ===\n")
print(head(df_wide_memory))
write.csv(
  df_wide_memory,
  "/mnt/user-data/outputs/df_wide_memory_only.csv",
  row.names = FALSE
)

# ==============================================================================
# OPTION 3: Wide format - Time AND Memory (two columns per operation)
# ==============================================================================

df_wide_both <- df_complete %>%
  select(
    n_documents,
    n_sentences,
    setting_clean,
    operation,
    median_time_sec,
    mem_alloc_bytes
  ) %>%
  mutate(mem_alloc_mb = mem_alloc_bytes / 1024^2) %>%
  select(-mem_alloc_bytes) %>%
  pivot_wider(
    names_from = operation,
    values_from = c(median_time_sec, mem_alloc_mb),
    names_glue = "{operation}_{.value}"
  )

# round time to 2 decimals and memory to 1 decimal for better readability
df_wide_both <- df_wide_both %>%
  mutate(across(ends_with("median_time_sec"), ~ round(.x, 2))) %>%
  mutate(across(ends_with("mem_alloc_mb"), ~ round(.x, 1)))


cat("\n=== OPTION 3: Wide format - Time AND Memory ===\n")
print(head(df_wide_both))
write.csv(
  df_wide_both,
  "/mnt/user-data/outputs/df_wide_time_and_memory.csv",
  row.names = FALSE
)

# ==============================================================================
# OPTION 4: Wide format - ALL metrics (comprehensive)
# ==============================================================================

df_wide_complete <- df_complete %>%
  select(
    n_documents,
    n_sentences,
    setting_clean,
    operation,
    median_time_sec,
    min_time_sec,
    itr_sec,
    mem_alloc_bytes,
    gc_sec,
    n_itr,
    n_gc
  ) %>%
  pivot_wider(
    names_from = operation,
    values_from = c(
      median_time_sec,
      min_time_sec,
      itr_sec,
      mem_alloc_bytes,
      gc_sec,
      n_itr,
      n_gc
    ),
    names_glue = "{operation}_{.value}"
  )

cat("\n=== OPTION 4: Wide format - ALL metrics ===\n")
print(head(df_wide_complete))
write.csv(
  df_wide_complete,
  "/mnt/user-data/outputs/df_wide_all_metrics.csv",
  row.names = FALSE
)

# ==============================================================================
# OPTION 5: Super wide - One row per document count, columns for everything
# ==============================================================================

df_super_wide <- df_complete %>%
  mutate(
    setting_label = paste0("sent_", n_sentences)
  ) %>%
  select(
    n_documents,
    setting_label,
    operation,
    median_time_sec,
    mem_alloc_bytes
  ) %>%
  pivot_wider(
    names_from = c(setting_label, operation),
    values_from = c(median_time_sec, mem_alloc_bytes),
    names_glue = "{setting_label}_{operation}_{.value}"
  )

cat("\n=== OPTION 5: Super wide - One row per document count ===\n")
print(df_super_wide)
write.csv(
  df_super_wide,
  "/mnt/user-data/outputs/df_super_wide.csv",
  row.names = FALSE
)

# ==============================================================================
# OPTION 6: Manuscript format - Operations as rows, settings as columns
# ==============================================================================

# Time table
df_manuscript_time <- df_complete %>%
  mutate(
    setting_col = paste0(n_documents, "d_", n_sentences, "s")
  ) %>%
  select(operation, setting_col, median_time_sec) %>%
  pivot_wider(
    names_from = setting_col,
    values_from = median_time_sec
  ) %>%
  arrange(operation)

cat("\n=== OPTION 6: Manuscript format - Time (operations as rows) ===\n")
print(df_manuscript_time)
write.csv(
  df_manuscript_time,
  "/mnt/user-data/outputs/df_manuscript_time.csv",
  row.names = FALSE
)

# Memory table
df_manuscript_memory <- df_complete %>%
  mutate(
    setting_col = paste0(n_documents, "d_", n_sentences, "s"),
    mem_mb = mem_alloc_bytes / 1024^2
  ) %>%
  select(operation, setting_col, mem_mb) %>%
  pivot_wider(
    names_from = setting_col,
    values_from = mem_mb
  ) %>%
  arrange(operation)

cat("\n=== OPTION 6: Manuscript format - Memory (operations as rows) ===\n")
print(df_manuscript_memory)
write.csv(
  df_manuscript_memory,
  "/mnt/user-data/outputs/df_manuscript_memory.csv",
  row.names = FALSE
)

# ==============================================================================
# OPTION 7: Custom wide with nice column ordering
# ==============================================================================

# Get unique operations and settings
operations <- unique(df_complete$operation)
doc_counts <- sort(unique(df_complete$n_documents))
sentence_counts <- sort(unique(df_complete$n_sentences))

# Create ordered column structure
df_wide_ordered <- df_complete %>%
  arrange(n_documents, n_sentences, operation) %>%
  mutate(
    mem_mb = mem_alloc_bytes / 1024^2,
    setting_id = paste0(n_documents, "docs_", n_sentences, "sent")
  ) %>%
  select(
    setting_id,
    n_documents,
    n_sentences,
    operation,
    median_time_sec,
    mem_mb
  ) %>%
  pivot_wider(
    id_cols = c(setting_id, n_documents, n_sentences),
    names_from = operation,
    values_from = c(median_time_sec, mem_mb),
    names_glue = "{operation}_{.value}",
    names_sort = TRUE
  )

cat("\n=== OPTION 7: Wide format with ordered columns ===\n")
print(head(df_wide_ordered))
write.csv(
  df_wide_ordered,
  "/mnt/user-data/outputs/df_wide_ordered.csv",
  row.names = FALSE
)

# ==============================================================================
# Summary of files created
# ==============================================================================

cat("\n✓ All wide format files created:\n")
cat("  1. df_wide_time_only.csv - Time metrics only\n")
cat("  2. df_wide_memory_only.csv - Memory metrics only\n")
cat("  3. df_wide_time_and_memory.csv - Both time and memory\n")
cat("  4. df_wide_all_metrics.csv - All benchmark metrics\n")
cat("  5. df_super_wide.csv - One row per document count\n")
cat("  6. df_manuscript_time.csv - Operations as rows (time)\n")
cat("  7. df_manuscript_memory.csv - Operations as rows (memory)\n")
cat("  8. df_wide_ordered.csv - Ordered columns\n")

# ==============================================================================
# Quick comparison of formats
# ==============================================================================

cat("\n=== Format Comparison ===\n")
cat(
  "Original (long):",
  nrow(df_complete),
  "rows x",
  ncol(df_complete),
  "cols\n"
)
cat(
  "Wide time only:",
  nrow(df_wide_time),
  "rows x",
  ncol(df_wide_time),
  "cols\n"
)
cat(
  "Wide time+memory:",
  nrow(df_wide_both),
  "rows x",
  ncol(df_wide_both),
  "cols\n"
)
cat(
  "Wide all metrics:",
  nrow(df_wide_complete),
  "rows x",
  ncol(df_wide_complete),
  "cols\n"
)
cat("Super wide:", nrow(df_super_wide), "rows x", ncol(df_super_wide), "cols\n")
cat(
  "Manuscript format:",
  nrow(df_manuscript_time),
  "rows x",
  ncol(df_manuscript_time),
  "cols\n"
)
``
`

**Qual è la differenza tra le opzioni?**

  | Opzione | Righe = | Colonne = | Best for |
  |---------|---------|-----------|----------|
  | **1. Time only** | Un setting (n_docs + n_sent) | Una per operazione | Analisi veloce dei tempi |
  | **2. Memory only** | Un setting | Una per operazione | Analisi memoria |
  | **3. Time + Memory** | Un setting | Due per operazione | Analisi completa |
  | **4. All metrics** | Un setting | Tutte le metriche | Analisi dettagliata |
  | **5. Super wide** | Un document count | Tutte combinazioni | Overview compatto |
  | **6. Manuscript** | Un'operazione | Un setting | **Tabella paper!** |
| **7. Ordered** | Un setting | Ordinate logicamente | Export Excel |

**La migliore per il manuscript è l'opzione 6!**

  Esempio output Opzione 6:
  `
``
operation
10
d_1s
10
d_10s
100
d_1s
...
10000
d_1s
10000
d_10s
preprocessing
0.53
0.54
7.32
...
73.2
398.8
network
0.16
0.16
1.20
...
163.7
514.1
polarity
0.58
0.58
3.76
...
...
...
