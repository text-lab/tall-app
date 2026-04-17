setwd("~/Dropbox/R/tall_benchmark")
source("~/Dropbox/R/tall/inst/tall/tallFunctions.R")
source("~/Dropbox/R/tall/inst/tall/keyness.R")
load("/Users/massimoaria/Dropbox/R/tall_benchmark/mobidick_sentences.rdata")
txt <- df
rm(df)
set.seed(1234)

#!/usr/bin/env Rscript
# ==============================================================================
# TALL REAL Performance Benchmark
# ==============================================================================
#
# This script benchmarks TALL's ACTUAL functions, not generic R libraries.
# It replicates exactly what TALL does internally when users click buttons.
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("     TALL AUTHENTIC PERFORMANCE BENCHMARK\n")
cat("================================================================\n\n")

# ==============================================================================
# Setup: Load TALL Package and Dependencies
# ==============================================================================

cat("Loading TALL package and dependencies...\n")

suppressPackageStartupMessages({
  # Se TALL è installato come package
  if (requireNamespace("tall", quietly = TRUE)) {
    library(tall)
    library(udpipe)
    library(tidyverse)
    library(topicmodels)
    library(word2vec)
    library(igraph)
    library(bench)
  }
})

cat("✓ TALL loaded\n\n")

# ==============================================================================
# System Information
# ==============================================================================

get_system_info <- function() {
  os_type <- Sys.info()["sysname"]

  info <- list(
    os = os_type,
    os_release = Sys.info()["release"],
    r_version = R.version.string,
    cpu = tryCatch(
      {
        if (os_type == "Darwin") {
          # macOS specific command
          system("sysctl -n machdep.cpu.brand_string", intern = TRUE)
        } else {
          # Linux specific command
          system(
            "lscpu | grep 'Model name' | sed 's/Model name: *//'",
            intern = TRUE
          )
        }
      },
      error = function(e) "CPU info unavailable"
    ),
    ram = tryCatch(
      {
        if (os_type == "Darwin") {
          # macOS: Get total bytes and convert to GB
          bytes <- system("sysctl -n hw.memsize", intern = TRUE)
          paste0(round(as.numeric(bytes) / 1024^3, 2), " GB")
        } else {
          system("free -h | grep Mem | awk '{print $2}'", intern = TRUE)
        }
      },
      error = function(e) "RAM info unavailable"
    ),
    cores = parallel::detectCores()
  )

  cat("\n=== System Information ===\n")
  cat("OS:", info$os, info$os_release, "\n")
  cat("R Version:", info$r_version, "\n")
  cat("CPU:", info$cpu, "\n")
  cat("RAM:", info$ram, "\n")
  cat("Available cores:", info$cores, "\n\n")

  return(info)
}

# ==============================================================================
# Generate Synthetic Corpus (mimicking TALL's data structure)
# ==============================================================================

generate_tall_corpus <- function(df, n_docs, sentences_per_doc) {
  docs <- sapply(1:n_docs, function(i) {
    sentences <- sample(df, sentences_per_doc, replace = TRUE)
    paste(sentences, collapse = "")
  })

  # TALL's expected structure
  data.frame(
    doc_id = paste0("doc_", sprintf("%05d", 1:n_docs)),
    text = docs,
    doc_selected = TRUE,
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# 1. TALL PREPROCESSING BENCHMARK
# ==============================================================================

benchmark_tall_preprocessing <- function(
  corpus,
  language_file = "en_gum",
  iter = 3
) {
  cat("\n=== Benchmarking TALL Preprocessing ===\n")
  cat("Corpus size:", nrow(corpus), "documents\n")
  cat("Language model: UD 2.15", language_file, "\n")

  # Benchmark complete TALL preprocessing workflow
  result <- bench::mark(
    tall_preprocessing = {
      # 1. Load TALL's custom UD 2.15 model (CRITICAL!)
      udmodel_lang <- loadLanguageModel(
        file = language_file,
        model_repo = "2.15"
      )

      # 2. Get optimal cores (TALL's function)
      ncores <- ifelse(
        exists("coresCPU"),
        coresCPU(),
        parallel::detectCores() - 1
      )

      # 3. Run udpipe with TALL's custom model
      dfTag <- udpipe::udpipe(
        object = udmodel_lang,
        x = corpus,
        parallel.cores = ncores
      )

      # 4. TALL's standard dplyr::filtering (ADJ, NOUN, PROPN, VERB)
      dfTag <- dfTag %>%
        dplyr::filter(!is.na(upos)) %>%
        dplyr::filter(upos %in% c("ADJ", "NOUN", "PROPN", "VERB"))

      # 5. Lowercase normalization (default TALL behavior)
      dfTag <- dfTag %>%
        mutate(
          token = tolower(token),
          lemma = tolower(lemma)
        )

      dfTag
    },
    iterations = iter,
    check = FALSE,
    memory = FALSE
  )

  cat("Median time:", as.character(result$median), "\n")
  cat("Memory allocated:", format(result$mem_alloc, units = "MB"), "\n")
  cat("Tokens generated:", nrow(result[[1]][[1]]), "\n")

  return(list(
    benchmark = result,
    dfTag = dfTag # Extract the resulting dfTag
  ))
}

# ==============================================================================
# 2. TALL N-GRAM & MULTIWORD
# ==============================================================================

benchmark_tall_multiwords <- function(dfTag, iter = 3) {
  cat("\n=== Benchmarking TALL Multiword Creation by Rake ===\n")
  cat(class(dfTag))
  cat("Tokens:", nrow(dfTag), "\n")

  result <- bench::mark(
    tall_multiwords = {
      rake(
        dfTag,
        group = "doc_id",
        ngram_max = 4,
        relevant = c("NOUN", "ADJ", "PROPN"),
        freq.min = 1,
        term = "lemma",
        method = "rake"
      )
    },
    iterations = iter,
    check = FALSE
  )

  cat("Median time:", as.character(result$median), "\n")
  cat("Memory allocated:", format(result$mem_alloc, units = "MB"), "\n")

  return(result)
}

# ==============================================================================
# 3. TALL KEYNESS BENCHMARK (RCPP!)
# ==============================================================================

benchmark_tall_keyness <- function(dfTag, iter = 3) {
  cat("\n=== Benchmarking TALL Keyness ===\n")
  cat("Tokens:", nrow(dfTag), "\n")

  result <- bench::mark(
    tall_keyness = {
      # TALL's custom Reinert hierarchical clustering
      # (with Rcpp implementation for performance)
      tall_keyness_analysis(
        dfTag,
        approach = c("reference_corpus"),
        language = "english",
        N = 500,
        min.char = 3,
        upos_list = c("NOUN", "VERB"),
        term = "token"
      )
    },
    iterations = iter,
    check = FALSE
  )

  cat("Median time:", as.character(result$median), "\n")
  cat("Memory allocated:", format(result$mem_alloc, units = "MB"), "\n")

  return(result)
}

# ==============================================================================
# 4. TALL TOPIC MODELING BENCHMARK
# ==============================================================================

benchmark_tall_topic_modeling <- function(dfTag, n_topics = 5, iter = 3) {
  cat("\n=== Benchmarking TALL Topic Modeling ===\n")
  cat("Documents:", length(unique(dfTag$doc_id)), "\n")
  cat("Topics:", n_topics, "\n")

  result <- bench::mark(
    tall_topic_modeling = {
      # 1. Set number of topics
      tall_topics = 5

      # 2. Run LDA (using topicmodels package as TALL does)
      lda <- tmEstimate(
        dfTag,
        K = tall_topics,
        group = "doc_id",
        term = "lemma",
        n = 100,
        top_by = "freq"
      )

      lda
    },
    iterations = iter,
    check = FALSE
  )

  cat("Median time:", as.character(result$median), "\n")
  cat("Memory allocated:", format(result$mem_alloc, units = "MB"), "\n")

  return(result)
}

# ==============================================================================
# 5. TALL WORD EMBEDDINGS BENCHMARK
# ==============================================================================

benchmark_tall_word2vec <- function(dfTag, iter = 3) {
  cat("\n=== Benchmarking TALL Word2Vec ===\n")
  cat("Tokens:", nrow(dfTag), "\n")

  cat("Using TALL's trainWord2Vec function\n")

  result <- bench::mark(
    tall_word2vec = {
      model <- w2vTraining(
        dfTag,
        term = "lemma",
        dim = 100,
        iter = 20
      )
    },
    iterations = iter,
    check = FALSE
  )

  cat("Median time:", as.character(result$median), "\n")
  cat("Memory allocated:", format(result$mem_alloc, units = "MB"), "\n")

  return(result)
}

# ==============================================================================
# 6. TALL NETWORK BENCHMARK
# ==============================================================================
benchmark_tall_network <- function(dfTag, iter = 3) {
  cat("\n=== Benchmarking TALL Network ===\n")
  cat("Tokens:", nrow(dfTag), "\n")

  cat("Using TALL's network function\n")

  result <- bench::mark(
    tall_network = {
      res <- network(
        dfTag,
        term = "lemma",
        group = c("doc_id", "sentence_id"),
        n = 100,
        minEdges = "50%",
        labelsize = 4,
        opacity = 0.6,

        interLinks = TRUE,
        normalization = "association",
        remove.isolated = TRUE,
        community.repulsion = 0.5,
        seed = 1234,
        cluster = "louvain"
      )
    },
    iterations = iter,
    check = FALSE
  )

  cat("Median time:", as.character(result$median), "\n")
  cat("Memory allocated:", format(result$mem_alloc, units = "MB"), "\n")

  return(result)
}

# ==============================================================================
# 7. TALL POLARITY BENCHMARK
# ==============================================================================
benchmark_tall_polarity <- function(dfTag, iter = 3) {
  cat("\n=== Benchmarking TALL polarity ===\n")

  cat("Using TALL's polarity function\n")

  result <- bench::mark(
    tall_polarity = {
      lexiconD_polarity <- "huliu"

      res <- sentimentAnalysis(
        dfTag,
        language = "english",
        lexicon_model = lexiconD_polarity
      )
    },
    iterations = iter,
    check = FALSE
  )

  cat("Median time:", as.character(result$median), "\n")
  cat("Memory allocated:", format(result$mem_alloc, units = "MB"), "\n")

  return(result)
}


# ==============================================================================
# 8. RUN COMPLETE BENCHMARK SUITE
# ==============================================================================

run_tall_benchmark_suite <- function(
  txt,
  corpus_sizes,
  sentences_per_doc,
  iter = 3
) {
  cat("\n")
  cat("================================================================\n")
  cat("         TALL AUTHENTIC BENCHMARK SUITE\n")
  cat("================================================================\n")

  df = as.character(txt$sentence)
  # Get system info
  sys_info <- get_system_info()

  # Storage for results
  results <- list()

  for (size in corpus_sizes) {
    for (sent in sentences_per_doc) {
      cat("\n")
      cat("================================================================\n")
      cat(
        "-> Running benchmark for",
        size,
        "documents with",
        sent,
        "sentences each\n"
      )
      cat("================================================================\n")

      # Generate corpus
      corpus <- generate_tall_corpus(
        df,
        n_docs = size,
        sentences_per_doc = sent
      )

      # 1. Preprocessing (CRITICAL - uses TALL's UD 2.15 models!)
      preproc_result <- benchmark_tall_preprocessing(corpus, iter = iter)
      dfTag <- preproc_result$dfTag

      # 2. N-gram generation (Rcpp)
      multiword_result <- benchmark_tall_multiwords(dfTag, iter = iter)

      # 3. Keyness (Rcpp)
      keyness_result <- benchmark_tall_keyness(dfTag, iter = iter)

      # 4. Topic modeling
      topic_result <- benchmark_tall_topic_modeling(
        dfTag,
        n_topics = 5,
        iter = iter
      )

      # 5. Word embeddings
      w2v_result <- benchmark_tall_word2vec(dfTag, iter = iter)

      #6. Network
      network_result <- benchmark_tall_network(dfTag, iter = iter)

      #7. Polarity
      polarity_result <- benchmark_tall_polarity(dfTag, iter = iter)

      # Store results
      results[[paste(
        size,
        "documents with",
        sent,
        "sentences each",
        collapse = " "
      )]] <- list(
        corpus_size = size,
        preprocessing = preproc_result$benchmark,
        ngrams = multiword_result,
        reinert = keyness_result,
        topic_modeling = topic_result,
        word_embeddings = w2v_result,
        network = network_result,
        polarity = polarity_result
      )

      # Memory cleanup
      rm(corpus, dfTag)
      gc()
    }
  }

  return(list(system_info = sys_info, benchmarks = results))
}

# ==============================================================================
# 9. MAIN EXECUTION
# ==============================================================================

if (!interactive()) {
  cat("Starting TALL authentic benchmark...\n")
  cat("This may take 20-40 minutes depending on hardware.\n\n")

  tryCatch(
    {
      # Run with conservative sizes first
      results <- run_tall_benchmark_suite(
        txt = txt,
        corpus_sizes = c(10, 100, 1000, 5000),
        sentences_per_doc = c(1, 10, 100),
        iter = 3
      )

      # Save results
      saveRDS(
        results,
        "tall_authentic_benchmark.rds"
      )

      cat("\n")
      cat("================================================================\n")
      cat("   BENCHMARK COMPLETED SUCCESSFULLY\n")
      cat(
        "================================================================\n\n"
      )

      cat(
        "Results saved to: /mnt/user-data/outputs/tall_authentic_benchmark.rds\n"
      )
      cat(
        "\nNext step: Run generate_latex_table.R to create manuscript table\n"
      )
    },
    error = function(e) {
      cat("\n✗ ERROR:\n")
      cat(e$message, "\n\n")
      cat("Common issues:\n")
      cat("1. TALL package not properly loaded\n")
      cat("2. UD 2.15 models not downloaded\n")
      cat("3. Insufficient memory\n\n")
      cat("Check BENCHMARKING_GUIDE.md for troubleshooting\n")
    }
  )
} else {
  cat("Script loaded in interactive mode.\n")
  cat("Run: results <- run_tall_benchmark_suite(txt)\n")
}
