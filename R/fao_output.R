# What the analysis functions show when they finish: a short summary of the results in the
# console (the setting show_data, TRUE by default) and, with plot_show = TRUE, each chart in the
# plot window as it is saved. Only the function called by the user shows its summary: the
# functions it calls in turn (e.g. those run by fao_report()) stay quiet. The argument plot_show
# is documented with the other common arguments (summary_params).

# Called at the start of an analysis function, with on.exit(fao_run_end(run), add = TRUE)
fao_run_begin <- function(plot_show = fao_setting("plot_show")) {
  if (!(is.logical(plot_show) && length(plot_show) == 1 && !is.na(plot_show))) {
    stop("`plot_show` must be TRUE or FALSE.", call. = FALSE)
  }
  depth <- (if (is.null(.fao_cache$depth)) 0 else .fao_cache$depth) + 1
  .fao_cache$depth <- depth
  if (depth == 1) {
    .fao_cache$saved <- character(0)
    .fao_cache$wd <- getwd()
  }
  list(depth = depth, options = options(faofishingreport.plot_show = plot_show))
}

fao_run_end <- function(run) {
  options(run$options)
  .fao_cache$depth <- run$depth - 1
  invisible(NULL)
}

# Draws a chart in the plot window (plot_show = TRUE)
fao_show_plot <- function(plot) {
  print(plot)
  invisible(plot)
}

# Notes a file saved by fao_save_plot() or fao_write_csv(), for the summary
fao_run_saved <- function(file) {
  if (!is.null(.fao_cache$depth) && .fao_cache$depth > 0) {
    .fao_cache$saved <- c(.fao_cache$saved, normalizePath(file, winslash = "/", mustWork = FALSE))
  }
  invisible(file)
}

# Returns the result of an analysis function invisibly, after showing its summary when the
# function was called by the user. `main` is the table to show (its first `rows` rows, or its last
# ones with from = "bottom" for tables by year), `note` a line about what was analysed. `quiet`
# is the `silent` argument of the CMSY++ functions.
fao_run_result <- function(result, run, title, main = NULL, note = NULL, rows = 10,
                           from = c("top", "bottom"), quiet = FALSE) {
  if (run$depth == 1 && isTRUE(fao_setting("show_data")) && !isTRUE(quiet)) {
    fao_show_summary(result, title, main, note, rows, match.arg(from))
  }
  invisible(result)
}

fao_show_summary <- function(result, title, main = NULL, note = NULL, rows = 10, from = "top") {
  dash <- if (isTRUE(l10n_info()[["UTF-8"]])) "\u2500" else "-"
  width <- max(40, min(getOption("width", 80), 100))
  cat("\n", strrep(dash, 2), " ", title, " ", strrep(dash, max(3, width - nchar(title) - 4)), "\n", sep = "")
  if (!is.null(note)) cat(note, "\n", sep = "")
  if (is.data.frame(main) && nrow(main) > 0) {
    shown <- if (from == "top") utils::head(main, rows) else utils::tail(main, rows)
    print(fao_display_table(shown), row.names = FALSE)
    more <- nrow(main) - nrow(shown)
    if (more > 0) {
      cat("  ... ", format(more, big.mark = ","), if (from == "top") " more rows" else " earlier rows",
          " (", format(nrow(main), big.mark = ","), " in all)\n", sep = "")
    }
  }
  saved <- unique(.fao_cache$saved)
  charts <- saved[grepl("[.]png$", saved)]
  tables <- saved[grepl("[.]csv$", saved)]
  if (length(charts) > 0) cat(paste0("Charts saved in ", fao_files_text(charts), "\n"), sep = "")
  if (length(tables) > 0) cat(paste0("Tables saved in ", fao_files_text(tables), "\n"), sep = "")
  parts <- if (is.list(result) && !is.data.frame(result)) names(result) else NULL
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) > 0) cat("Results returned: ", paste0("$", parts, collapse = ", "), "\n", sep = "")
  invisible(NULL)
}

# The files grouped by folder ("folder/: file, file"), relative to the working folder of the call
fao_files_text <- function(files) {
  wd <- normalizePath(if (is.null(.fao_cache$wd)) getwd() else .fao_cache$wd, winslash = "/", mustWork = FALSE)
  inside <- startsWith(files, paste0(wd, "/"))
  files[inside] <- substring(files[inside], nchar(wd) + 2)
  folders <- dirname(files)
  parts <- vapply(unique(folders), function(folder) {
    names <- basename(files[folders == folder])
    shown <- if (length(names) > 4) c(names[1:3], paste0("... (", length(names), " files)")) else names
    paste0(folder, "/: ", paste(shown, collapse = ", "))
  }, character(1))
  unname(parts)
}

# Rounds the numbers of a table for the console, value by value (a column can mix scales, e.g.
# r and k of CMSY++): whole numbers and values of at least 100 without decimals, with thousands
# separators when the column reaches 10,000 (so years stay plain), other values with three
# significant digits; text is padded so that it stays aligned to the left
fao_display_table <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (name in names(x)) {
    v <- x[[name]]
    if (is.factor(v)) v <- as.character(v)
    if (is.logical(v)) v <- ifelse(is.na(v), "NA", as.character(v))
    if (is.numeric(v)) {
      big <- suppressWarnings(max(abs(v[is.finite(v)])))
      mark <- if (is.finite(big) && big >= 10000) "," else ""
      v <- vapply(v, function(value) {
        if (is.na(value)) return("NA")
        if (!is.finite(value)) return(format(value))
        if (value == round(value) || abs(value) >= 100) return(formatC(round(value), format = "d", big.mark = mark))
        sub("[.]$", "", formatC(signif(value, 3), format = "fg", digits = 3, flag = "#"))
      }, character(1))
    } else if (is.character(v)) {
      v[is.na(v)] <- "NA"
      # padded by display width, so that Chinese names (two columns per character) stay aligned
      width <- max(nchar(v, type = "width"), nchar(name))
      v <- paste0(v, strrep(" ", width - nchar(v, type = "width")))
    } else if (inherits(v, "Date")) {
      v <- format(v)
    } else if (!is.atomic(v)) {
      v <- rep("<list>", nrow(x))
    }
    x[[name]] <- v
  }
  x
}

# Title of a summary: the function and what it analysed (the fishing area or country of the data)
fao_run_title <- function(fn, data = NULL, label = NULL, what = NULL) {
  what <- c(what, if (!is.null(data) || !is.null(label)) fao_summary_label(data, label))
  paste0(fn, "()", if (length(what) > 0) paste0(": ", paste(what, collapse = ", ")))
}

# The columns of a table that exist, in the order given
fao_cols <- function(x, cols) {
  if (!is.data.frame(x)) return(NULL)
  as.data.frame(x)[, intersect(cols, names(x)), drop = FALSE]
}

# A table of the decades and years of species_group_composition() and the like, cut to its
# first column, the last two decades, the last year and the share in the last year
fao_decade_brief <- function(x) {
  if (!is.data.frame(x)) return(NULL)
  decades <- grep("^[0-9]{4}s$", names(x), value = TRUE)
  years <- grep("^[0-9]{4}$", names(x), value = TRUE)
  share <- grep("^share_", names(x), value = TRUE)
  fao_cols(x, c(names(x)[1], utils::tail(decades, 2), utils::tail(years, 1), share))
}

# "1 to 10" for consecutive ranks, else the ranks listed ("5, 6")
fao_ranks_text <- function(ranks) {
  if (length(ranks) > 1 && all(diff(ranks) == 1)) paste(min(ranks), "to", max(ranks)) else paste(ranks, collapse = ", ")
}

# The rows chosen with `rank_range` as a table with their ranks: `cols` maps the column names
# shown to those of `selected`
fao_rank_table <- function(selected, ranks, what, name_col, cols) {
  selected <- as.data.frame(selected)
  out <- data.frame(rank = ranks[seq_len(nrow(selected))], name = as.character(selected[[name_col]]),
                    stringsAsFactors = FALSE)
  names(out)[2] <- what
  for (shown in names(cols)) out[[shown]] <- selected[[cols[[shown]]]]
  out
}
