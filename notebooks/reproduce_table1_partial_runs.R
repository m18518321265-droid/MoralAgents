#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(jsonlite)
  library(ordinal)
  library(emmeans)
})

parse_kv_args <- function(args) {
  out <- list()
  for (arg in args) {
    if (!grepl("=", arg, fixed = TRUE)) next
    parts <- strsplit(arg, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) next
    key <- parts[1]
    val <- paste(parts[-1], collapse = "=")
    out[[key]] <- val
  }
  out
}

safe_int <- function(x) {
  suppressWarnings(as.integer(x))
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

clean_opinion <- function(x) {
  y <- tolower(as.character(x))
  y <- str_replace_all(y, "\\\\boxed\\{(\\d+)\\}", "\\1")
  y <- str_replace_all(y, "\\(.*?\\)", "")
  y <- str_replace_all(y, "\\s*\\([^)]*\\)", "")
  fixed_noise <- c(
    " out of 7", ".out.of.", "=out=of=7", "\tout\tof\t", "=", "/7", "/ 7", ", 7", "\"",
    "moral acceptability score:", "score:", "final answer", "final score:", "final rating:",
    "agreement rating:", "final agreement rating:", "the answer is", "- strongly disagree",
    "– strongly disagree", "– somewhat disagree", "– disagree", "- morally acceptable",
    "– completely acceptable", "–completely acceptable", "—completely acceptable",
    "— completely unacceptable", "– completely unacceptable", "– somewhat morally acceptable",
    "– somewhat morally unacceptable", "– mostly unacceptable.", "– completely unacceptable.",
    ": completely unacceptable", ": strongly acceptable", "— strongly morally acceptable"
  )
  for (token in fixed_noise) {
    y <- str_replace_all(y, fixed(token), "")
  }
  y <- str_trim(str_replace_all(y, "\\.+$", ""))
  num_txt <- str_extract(y, "-?\\d+(?:\\.\\d+)?")
  num <- safe_num(num_txt)
  num <- ifelse(is.na(num) | num < 1 | num > 7, NA_real_, num)
  as.integer(round(num))
}

load_dataset_types <- function(project_root) {
  datasets <- c("keshmirian", "greene", "korner", "oxford_utilitarianism_scale", "cni")
  all_rows <- vector("list", length(datasets))

  for (i in seq_along(datasets)) {
    dataset <- datasets[[i]]
    path <- file.path(project_root, "data", dataset, "data.json")
    if (!file.exists(path)) {
      all_rows[[i]] <- NULL
      next
    }
    df <- tryCatch(fromJSON(path), error = function(e) NULL)
    if (is.null(df) || !("index" %in% names(df)) || !("type" %in% names(df))) {
      all_rows[[i]] <- NULL
      next
    }
    all_rows[[i]] <- tibble(
      dataset = dataset,
      example_index = safe_int(df$index),
      type = as.character(df$type)
    )
  }

  bind_rows(all_rows) %>% distinct(dataset, example_index, .keep_all = TRUE)
}

discover_run_files <- function(runs_dir) {
  runs_dir_norm <- normalizePath(path.expand(runs_dir), winslash = "/", mustWork = FALSE)

  if (!dir.exists(runs_dir_norm)) {
    return(tibble(
      path = character(0),
      path_rel = character(0),
      dataset = character(0),
      model = character(0),
      group_folder = character(0),
      example_index = integer(0),
      ob = character(0),
      rep = integer(0),
      group_size = integer(0)
    ))
  }

  files <- list.files(runs_dir_norm, pattern = "\\.jsonl$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    return(tibble(
      path = character(0),
      path_rel = character(0),
      dataset = character(0),
      model = character(0),
      group_folder = character(0),
      example_index = integer(0),
      ob = character(0),
      rep = integer(0),
      group_size = integer(0)
    ))
  }

  files_norm <- normalizePath(files, winslash = "/", mustWork = FALSE)
  rel <- str_replace(files_norm, fixed(paste0(runs_dir_norm, "/")), "")
  parsed <- str_match(rel, "^([^/]+)/([^/]+)/([^/]+)/([^/_]+)_ob([^_]+)_([0-9]+)\\.jsonl$")

  tibble(
    path = files_norm,
    path_rel = rel,
    dataset = parsed[, 2],
    model = parsed[, 3],
    group_folder = parsed[, 4],
    example_index = safe_int(parsed[, 5]),
    ob = parsed[, 6],
    rep = safe_int(parsed[, 7])
  ) %>%
    mutate(
      group_size = safe_int(str_remove(group_folder, "n$"))
    )
}

read_one_run <- function(file_row) {
  dat <- tryCatch(
    stream_in(file(file_row$path), verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(dat) || nrow(dat) == 0) return(NULL)

  if (!("opinion" %in% names(dat))) return(NULL)

  group_size <- file_row$group_size
  if (is.na(group_size) || group_size <= 0) return(NULL)

  # Matches compile.ipynb convention:
  # single => step -1; groups => floor(index / group_size)
  step <- if (group_size == 1L) {
    rep(-1L, nrow(dat))
  } else {
    floor((seq_len(nrow(dat)) - 1L) / group_size)
  }

  tibble(
    dataset = file_row$dataset,
    model = file_row$model,
    group_size = group_size,
    example_index = file_row$example_index,
    ob = file_row$ob,
    rep = file_row$rep,
    phase = if ("phase" %in% names(dat)) as.character(dat$phase) else NA_character_,
    opinion_raw = as.character(dat$opinion),
    step = as.integer(step)
  )
}

model_to_short <- function(model_name) {
  recode(
    model_name,
    "gpt-4.1" = "GPT4.1",
    "llama3.3" = "Lamma3.3",
    "qwen2.5:32b-instruct" = "Qwen2.5",
    "qwen3:32b" = "Qwen3",
    "qwq" = "QWQ",
    "gemma3:27b" = "Gemma3",
    .default = model_name
  )
}

prepare_table1_data <- function(run_rows, dataset_types, target_model_short) {
  if (nrow(run_rows) == 0) {
    return(tibble(
      dataset = character(0),
      model = character(0),
      group_size = integer(0),
      example_index = integer(0),
      ob = character(0),
      rep = integer(0),
      phase = character(0),
      opinion_raw = character(0),
      step = integer(0),
      opinion = ordered(integer(0), levels = 1:7),
      model_short = character(0),
      Group = factor(character(0), levels = c("Solo", "Group")),
      item = factor(character(0)),
      type = character(0)
    ))
  }

  dat <- run_rows %>%
    mutate(
      opinion = clean_opinion(opinion_raw),
      model_short = model_to_short(model),
      Group = ifelse(step == -1L, "Solo", "Group"),
      item = as.factor(example_index)
    ) %>%
    left_join(dataset_types, by = c("dataset", "example_index")) %>%
    mutate(
      dataset = recode(dataset, "oxford_utilitarianism_scale" = "oxford"),
      type = recode(
        type,
        "Impartial Beneficence" = "Beneficence",
        "Instrumental Harm" = "Harm",
        .default = type
      )
    ) %>%
    filter(
      dataset == "oxford",
      model_short == target_model_short,
      type %in% c("Harm", "Beneficence"),
      step %in% c(-1L, 7L),
      !is.na(opinion)
    ) %>%
    mutate(
      Group = factor(Group, levels = c("Solo", "Group")),
      opinion = ordered(opinion, levels = 1:7)
    )

  dat
}

format_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.0001) return("<0.0001")
  format(p, digits = 4)
}

safe_contrast <- function(df_sub, condition_name, min_n = 10L) {
  result <- tibble(
    `Experimental Condition` = condition_name,
    `Effect Size` = NA_real_,
    SE = NA_real_,
    `z-value` = NA_real_,
    `p-value` = NA_character_,
    reason = NA_character_
  )

  if (nrow(df_sub) < min_n) {
    result$reason <- paste0("n<", min_n)
    return(result)
  }

  g_counts <- table(df_sub$Group)
  if (!all(c("Solo", "Group") %in% names(g_counts)) || any(g_counts[c("Solo", "Group")] == 0)) {
    result$reason <- "missing Solo/Group rows"
    return(result)
  }

  if (dplyr::n_distinct(df_sub$opinion) < 2) {
    result$reason <- "single opinion category"
    return(result)
  }

  fit_mod <- tryCatch(
    clmm(opinion ~ Group + (1 | item), data = df_sub, Hess = TRUE),
    error = function(e) e
  )
  if (inherits(fit_mod, "error")) {
    result$reason <- paste("clmm failed:", conditionMessage(fit_mod))
    return(result)
  }

  em <- tryCatch(
    emmeans(fit_mod, pairwise ~ Group, type = "response"),
    error = function(e) e
  )
  if (inherits(em, "error")) {
    result$reason <- paste("emmeans failed:", conditionMessage(em))
    return(result)
  }

  contr <- tryCatch(as.data.frame(summary(em$contrasts)), error = function(e) NULL)
  if (is.null(contr) || nrow(contr) == 0) {
    result$reason <- "no contrast returned"
    return(result)
  }

  row <- contr[1, , drop = FALSE]
  est <- safe_num(row$estimate[[1]])
  zr <- safe_num(row$z.ratio[[1]])
  se <- safe_num(row$SE[[1]])
  p <- safe_num(row$p.value[[1]])

  # Align with notebook convention: positive = utilitarian boost (Group - Solo).
  if (!is.na(est) && est < 0) {
    est <- -est
    zr <- -zr
  }

  result$`Effect Size` <- round(est, 3)
  result$SE <- round(se, 3)
  result$`z-value` <- round(zr, 2)
  result$`p-value` <- format_p(p)
  result$reason <- "ok"
  result
}

main <- function() {
  args <- parse_kv_args(commandArgs(trailingOnly = TRUE))
  project_root <- normalizePath(getwd())
  runs_dir <- args[["runs_dir"]] %||% file.path(project_root, "runs")
  runs_dir <- normalizePath(path.expand(runs_dir), winslash = "/", mustWork = FALSE)
  output_csv <- args[["output_csv"]] %||% file.path(project_root, "notebooks", "table1_oxford_partial_runs.csv")
  target_model_short <- args[["model_short"]] %||% "Gemma3"
  min_n <- safe_int(args[["min_n"]] %||% "10")
  if (is.na(min_n) || min_n < 1) min_n <- 10L

  cat("=== Reproduce Table 1 from partial runs ===\n")
  cat("runs_dir:", runs_dir, "\n")
  cat("output_csv:", output_csv, "\n")
  cat("model_short:", target_model_short, "\n")
  cat("min_n:", min_n, "\n\n")
  cat("runs_dir exists:", dir.exists(runs_dir), "\n\n")

  dataset_types <- load_dataset_types(project_root)
  file_meta <- discover_run_files(runs_dir)
  parsed_ok <- file_meta %>% filter(!is.na(dataset), !is.na(model), !is.na(group_size), !is.na(example_index))
  parsed_bad_n <- nrow(file_meta) - nrow(parsed_ok)

  cat("Run files discovered:", nrow(file_meta), "\n")
  cat("Run files parsed:", nrow(parsed_ok), "\n")
  cat("Run files unparsed:", parsed_bad_n, "\n")
  if (parsed_bad_n > 0) {
    cat("Sample unparsed paths (expect '*_ob*_*.jsonl'):\n")
    print(head(file_meta$path_rel[is.na(file_meta$dataset)], 5))
  }

  run_rows <- purrr::map_dfr(split(parsed_ok, seq_len(nrow(parsed_ok))), read_one_run)
  cat("Raw rows loaded:", nrow(run_rows), "\n")

  table1_input <- prepare_table1_data(run_rows, dataset_types, target_model_short)
  cat("Rows after Oxford/Table1 filters:", nrow(table1_input), "\n\n")

  if (nrow(table1_input) == 0) {
    coverage <- tibble(ob = character(0), Group = character(0), n = integer(0))
    cat("Coverage by ob x Group: no rows available.\n")
  } else {
    coverage <- table1_input %>%
      count(ob, Group, name = "n") %>%
      arrange(ob, Group)
    cat("Coverage by ob x Group:\n")
    print(coverage, n = Inf)
  }
  cat("\n")

  df_overall <- table1_input %>% filter(ob %in% c("n", "nn", "nnn"))
  df_dyads <- table1_input %>% filter(ob %in% c("n", "nn"))
  df_triads <- table1_input %>% filter(ob %in% c("n", "nnn"))

  table1 <- bind_rows(
    safe_contrast(df_overall, "Overall Group", min_n = min_n),
    safe_contrast(df_dyads, "Dyads (2 agents)", min_n = min_n),
    safe_contrast(df_triads, "Triads (3 agents)", min_n = min_n)
  )

  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
  write.csv(table1, output_csv, row.names = FALSE)

  cat("Table 1 output:\n")
  print(table1, n = Inf)
  cat("\nSaved:", output_csv, "\n")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

main()
