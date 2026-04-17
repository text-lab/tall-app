# ==============================================================================
# Extract TALL Benchmark Results - CORRECT VERSION
# ==============================================================================
# Element 3: 10, 100, 1000, 5000 documents
# Element 4: 10000 documents (only 1 and 10 sentences)
# ==============================================================================

library(dplyr)
library(tidyr)

# Function to extract key metrics from bench_mark object
extract_benchmark_metrics <- function(bench_obj) {
  if (is.null(bench_obj) || length(bench_obj) == 0) {
    return(data.frame(
      median_time_sec = NA,
      min_time_sec = NA,
      itr_sec = NA,
      mem_alloc_bytes = NA,
      gc_sec = NA,
      n_itr = NA,
      n_gc = NA
    ))
  }

  tryCatch(
    {
      median_time <- as.numeric(bench_obj$median)
      min_time <- as.numeric(bench_obj$min)
      mem_alloc <- as.numeric(bench_obj$mem_alloc)

      data.frame(
        median_time_sec = median_time,
        min_time_sec = min_time,
        itr_sec = bench_obj$`itr/sec`,
        mem_alloc_bytes = mem_alloc,
        gc_sec = bench_obj$`gc/sec`,
        n_itr = bench_obj$n_itr,
        n_gc = bench_obj$n_gc
      )
    },
    error = function(e) {
      data.frame(
        median_time_sec = NA,
        min_time_sec = NA,
        itr_sec = NA,
        mem_alloc_bytes = NA,
        gc_sec = NA,
        n_itr = NA,
        n_gc = NA
      )
    }
  )
}

# Parse setting name
parse_setting_name <- function(setting_name) {
  parts <- strsplit(setting_name, " ")[[1]]

  n_docs <- as.numeric(parts[1])
  n_sentences <- as.numeric(parts[4])

  data.frame(
    n_documents = n_docs,
    n_sentences = n_sentences,
    setting_clean = setting_name
  )
}

# Extract results from one setting
extract_setting_results <- function(setting_results, setting_name) {
  corpus_size <- if (!is.null(setting_results$corpus_size)) {
    setting_results$corpus_size
  } else {
    NA
  }

  # All possible operations
  operations <- c(
    "preprocessing",
    "ngrams",
    "reinert",
    "topic_modeling",
    "word_embeddings",
    "network",
    "polarity",
    "multiword",
    "keyness"
  )

  results_list <- lapply(operations, function(op) {
    if (op %in% names(setting_results) && !is.null(setting_results[[op]])) {
      metrics <- extract_benchmark_metrics(setting_results[[op]])
    } else {
      metrics <- data.frame(
        median_time_sec = NA,
        min_time_sec = NA,
        itr_sec = NA,
        mem_alloc_bytes = NA,
        gc_sec = NA,
        n_itr = NA,
        n_gc = NA
      )
    }

    cbind(
      setting = setting_name,
      corpus_size = corpus_size,
      operation = op,
      metrics,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results_list)
}

# Main extraction function - extracts from BOTH element 3 and 4
extract_from_benchmark_matrix <- function(results_matrix) {
  cat("=== Extracting from benchmark matrix ===\n")

  all_settings_data <- list()

  # Extract from element 3 (10, 100, 1000, 5000)
  if (length(results_matrix) >= 3 && is.list(results_matrix[[3]])) {
    elem3 <- results_matrix[[3]]
    cat("Element 3 contains", length(elem3), "settings\n")

    for (setting_name in names(elem3)) {
      setting_data <- elem3[[setting_name]]
      if (is.list(setting_data) && "corpus_size" %in% names(setting_data)) {
        all_settings_data[[setting_name]] <- setting_data
        cat("  - Found:", setting_name, "\n")
      }
    }
  }

  # Extract from element 4 (10000)
  if (length(results_matrix) >= 4 && is.list(results_matrix[[4]])) {
    elem4 <- results_matrix[[4]]
    cat("Element 4 contains", length(elem4), "settings\n")

    for (setting_name in names(elem4)) {
      setting_data <- elem4[[setting_name]]
      if (is.list(setting_data) && "corpus_size" %in% names(setting_data)) {
        all_settings_data[[setting_name]] <- setting_data
        cat("  - Found:", setting_name, "\n")
      }
    }
  }

  cat("\nTotal settings extracted:", length(all_settings_data), "\n\n")

  # Process all settings
  all_results_list <- lapply(names(all_settings_data), function(setting_name) {
    tryCatch(
      {
        extract_setting_results(all_settings_data[[setting_name]], setting_name)
      },
      error = function(e) {
        warning(paste("Error processing:", setting_name, "-", e$message))
        NULL
      }
    )
  })

  # Remove NULLs
  all_results_list <- all_results_list[!sapply(all_results_list, is.null)]

  if (length(all_results_list) > 0) {
    df <- do.call(rbind, all_results_list)
    rownames(df) <- NULL

    # Parse settings
    parsed <- do.call(rbind, lapply(df$setting, parse_setting_name))
    df <- cbind(parsed, df)

    # Calculate approximate total words
    df$approx_total_words <- df$n_documents * df$n_sentences * 100

    return(df)
  } else {
    return(data.frame())
  }
}

# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

# Extract all data including 10000 documents
df_all_results <- extract_from_benchmark_matrix(all_results_fino_a_10000)

# Summary
cat("=== EXTRACTION SUMMARY ===\n")
cat("Total rows:", nrow(df_all_results), "\n")

# Check document counts
doc_counts <- df_all_results %>%
  filter(!is.na(median_time_sec)) %>%
  group_by(n_documents, n_sentences) %>%
  summarise(
    n_operations = n(),
    .groups = "drop"
  ) %>%
  arrange(n_documents, n_sentences)

cat("\nData availability by document count and sentences:\n")
print(doc_counts)

# Verify 10000 documents
cat("\n=== 10000 DOCUMENTS ===\n")
df_10k <- df_all_results %>%
  filter(n_documents == 10000)

cat("Total rows with 10000 docs:", nrow(df_10k), "\n")
cat("Rows with data:", sum(!is.na(df_10k$median_time_sec)), "\n")

if (sum(!is.na(df_10k$median_time_sec)) > 0) {
  cat("\nAvailable operations:\n")
  print(
    df_10k %>%
      filter(!is.na(median_time_sec)) %>%
      select(n_sentences, operation, median_time_sec, mem_alloc_bytes) %>%
      arrange(n_sentences, operation)
  )
}

# Save complete results
write.csv(
  df_all_results,
  "/mnt/user-data/outputs/tall_benchmark_complete_all_data.csv",
  row.names = FALSE
)

# Save only complete observations
df_complete <- df_all_results %>%
  filter(!is.na(median_time_sec))

write.csv(
  df_complete,
  "/mnt/user-data/outputs/tall_benchmark_complete_only.csv",
  row.names = FALSE
)

cat("\n✓ Files saved:\n")
cat(
  "  - tall_benchmark_complete_all_data.csv (all:",
  nrow(df_all_results),
  "rows)\n"
)
cat(
  "  - tall_benchmark_complete_only.csv (complete:",
  nrow(df_complete),
  "rows)\n"
)

# ==============================================================================
# Create manuscript-ready tables
# ==============================================================================

# Table 1: Time in seconds
manuscript_time <- df_complete %>%
  mutate(
    setting_label = paste0(n_documents, " docs / ", n_sentences, " sent")
  ) %>%
  group_by(setting_label, operation) %>%
  summarise(
    median_time = median(median_time_sec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    time_formatted = sprintf("%.2f", median_time)
  ) %>%
  select(operation, setting_label, time_formatted) %>%
  pivot_wider(
    names_from = setting_label,
    values_from = time_formatted
  )

cat("\n=== Manuscript Table - Time (seconds) ===\n")
print(manuscript_time)

write.csv(
  manuscript_time,
  "/mnt/user-data/outputs/tall_manuscript_time.csv",
  row.names = FALSE
)

# Table 2: Memory in MB
manuscript_memory <- df_complete %>%
  mutate(
    setting_label = paste0(n_documents, " docs / ", n_sentences, " sent")
  ) %>%
  group_by(setting_label, operation) %>%
  summarise(
    median_mem = median(mem_alloc_bytes / 1024^2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mem_formatted = sprintf("%.0f", median_mem)
  ) %>%
  select(operation, setting_label, mem_formatted) %>%
  pivot_wider(
    names_from = setting_label,
    values_from = mem_formatted
  )

cat("\n=== Manuscript Table - Memory (MB) ===\n")
print(manuscript_memory)

write.csv(
  manuscript_memory,
  "/mnt/user-data/outputs/tall_manuscript_memory.csv",
  row.names = FALSE
)

# Table 3: Combined (Time / Memory)
manuscript_combined <- df_complete %>%
  mutate(
    setting_label = paste0(n_documents, " docs / ", n_sentences, " sent")
  ) %>%
  group_by(setting_label, operation) %>%
  summarise(
    median_time = median(median_time_sec, na.rm = TRUE),
    median_mem = median(mem_alloc_bytes / 1024^2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    combined = sprintf("%.2f s\n(%.0f MB)", median_time, median_mem)
  ) %>%
  select(operation, setting_label, combined) %>%
  pivot_wider(
    names_from = setting_label,
    values_from = combined
  )

cat("\n=== Manuscript Table - Combined ===\n")
print(manuscript_combined)

write.csv(
  manuscript_combined,
  "/mnt/user-data/outputs/tall_manuscript_combined.csv",
  row.names = FALSE
)

# ==============================================================================
# Visualizations including 10000 docs
# ==============================================================================

library(ggplot2)

# Plot 1: Time by operation
p1 <- ggplot(
  df_complete,
  aes(
    x = n_documents,
    y = median_time_sec,
    color = operation,
    shape = factor(n_sentences),
    group = interaction(operation, n_sentences)
  )
) +
  geom_line(alpha = 0.6) +
  geom_point(size = 3) +
  scale_x_log10(
    breaks = c(10, 100, 1000, 5000, 10000),
    labels = c("10", "100", "1K", "5K", "10K")
  ) +
  scale_y_log10() +
  labs(
    title = "TALL Performance Benchmarks",
    subtitle = "All corpus sizes including 10,000 documents",
    x = "Number of Documents (log scale)",
    y = "Median Time (seconds, log scale)",
    color = "Operation",
    shape = "Sentences\nper Document"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p1)
ggsave(
  "/mnt/user-data/outputs/tall_benchmark_time_complete.png",
  width = 14,
  height = 8,
  dpi = 300
)

# Plot 2: Memory usage
p2 <- ggplot(
  df_complete,
  aes(
    x = n_documents,
    y = mem_alloc_bytes / 1024^2,
    color = operation,
    shape = factor(n_sentences),
    group = interaction(operation, n_sentences)
  )
) +
  geom_line(alpha = 0.6) +
  geom_point(size = 3) +
  scale_x_log10(
    breaks = c(10, 100, 1000, 5000, 10000),
    labels = c("10", "100", "1K", "5K", "10K")
  ) +
  scale_y_log10() +
  labs(
    title = "TALL Memory Usage",
    subtitle = "All corpus sizes including 10,000 documents",
    x = "Number of Documents (log scale)",
    y = "Memory Allocated (MB, log scale)",
    color = "Operation",
    shape = "Sentences\nper Document"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p2)
ggsave(
  "/mnt/user-data/outputs/tall_benchmark_memory_complete.png",
  width = 14,
  height = 8,
  dpi = 300
)

# ==============================================================================
# Summary statistics
# ==============================================================================

cat("\n=== SUMMARY STATISTICS ===\n\n")

# By document count
summary_by_docs <- df_complete %>%
  group_by(n_documents) %>%
  summarise(
    n_settings = n_distinct(paste(n_documents, n_sentences)),
    n_operations = sum(!is.na(median_time_sec)),
    mean_time = mean(median_time_sec, na.rm = TRUE),
    mean_memory_mb = mean(mem_alloc_bytes / 1024^2, na.rm = TRUE),
    .groups = "drop"
  )

cat("Summary by document count:\n")
print(summary_by_docs)

# Detailed for 10000
cat("\n=== Detailed results for 10000 documents ===\n")
df_10k_detail <- df_complete %>%
  filter(n_documents == 10000) %>%
  arrange(n_sentences, operation) %>%
  select(n_sentences, operation, median_time_sec, mem_alloc_bytes, n_itr, n_gc)

print(df_10k_detail)

cat("\n✓ COMPLETE! All files saved including 10000 documents data.\n")
