# Writing the LaTeX (and PDF) version of fao_report(): the same paragraphs, figures, tables and
# references as the Word file, in a .tex file with its figures in a folder next to it. The
# report is compiled with XeLaTeX (ctexart for Chinese reports), with fonts chosen from those
# installed on the computer.

fao_latex_new <- function(state, file) {
  x <- structure(list(), class = "fao_latex")
  state$latex <- c("\\begin{titlepage}", "\\centering")
  state$latex_file <- file
  state$latex_figures <- paste0(tools::file_path_sans_ext(basename(file)), "_figures")
  state$latex_bookmarks <- 0L
  attr(x, "fao_report") <- state
  x
}

fao_latex_add <- function(x, ...) {
  state <- attr(x, "fao_report")
  state$latex <- c(state$latex, ...)
  x
}

# Special characters of LaTeX in plain text
fao_latex_escape <- function(s) {
  s <- gsub("\\", "\001", s, fixed = TRUE)
  s <- gsub("([#$%&_{}])", "\\\\\\1", s)
  s <- gsub("~", "\\textasciitilde{}", s, fixed = TRUE)
  s <- gsub("^", "\\textasciicircum{}", s, fixed = TRUE)
  gsub("\001", "\\textbackslash{}", s, fixed = TRUE)
}

# Plain text with its web addresses as \url{} (so that they break at the end of a line)
fao_latex_text <- function(s) {
  # an address ends before a space, a bracket or Chinese text and punctuation
  pattern <- "https?://[^ \u3000-\u303f\u4e00-\u9fff\uff00-\uffef)\\]]*[^ .,;:\u3000-\u303f\u4e00-\u9fff\uff00-\uffef)\\]]"
  urls <- regmatches(s, gregexpr(pattern, s, perl = TRUE))[[1]]
  if (length(urls) == 0) return(fao_latex_escape(s))
  parts <- regmatches(s, gregexpr(pattern, s, perl = TRUE), invert = TRUE)[[1]]
  out <- fao_latex_escape(parts[1])
  for (i in seq_along(urls)) {
    out <- paste0(out, "\\url{", gsub("([#%])", "\\\\\\1", urls[i]), "}", fao_latex_escape(parts[i + 1]))
  }
  out
}

# Marked text (citations, *italics*, ^superscripts^, ~subscripts~) as LaTeX
fao_latex_runs <- function(x, text, numbered = FALSE, spacing = TRUE) {
  runs <- fao_report_runs(x, text, numbered, spacing)
  paste(vapply(runs, function(run) {
    t <- fao_latex_text(run$text)
    if (isTRUE(run$italic)) t <- paste0("\\textit{", t, "}")
    if (isTRUE(run$sup)) t <- paste0("\\textsuperscript{", t, "}")
    if (isTRUE(run$sub)) t <- paste0("\\textsubscript{", t, "}")
    t
  }, character(1)), collapse = "")
}

fao_latex_par <- function(x, text, style = "FAOBody", bold_prefix = NULL) {
  state <- attr(x, "fao_report")
  numbered <- style %in% c("FAOHeading1", "FAOHeading2", "FAOCaption", "FAOTableCaption")
  body <- fao_latex_runs(x, text, numbered, spacing = !style %in% c("FAONote", "FAOReference"))
  if (!is.null(bold_prefix)) body <- paste0("\\textbf{", fao_latex_escape(bold_prefix), "}", body)
  bookmark <- function(level) {
    state$latex_bookmarks <- state$latex_bookmarks + 1L
    plain <- fao_latex_escape(gsub("[*^~]", "", text))
    sprintf("\\faobookmark{%d}{%s}{fao%d}", level, plain, state$latex_bookmarks)
  }
  out <- switch(style,
    FAOBody = paste0(body, "\\par"),
    FAONote = paste0("\\textcolor{faogray}{", body, "}\\par"),
    FAOTitle = paste0("{\\centering\\faotitlefont ", body, "\\par}"),
    FAOAuthor = paste0("{\\centering ", body, "\\par}"),
    FAOAbstractHeading = paste0("{\\centering\\faoabstractfont ", body, "\\par}"),
    FAOHeading1 = paste0("\\faoheading{", bookmark(1), "}{\\faoheadingonefont ", body, "}"),
    FAOHeading2 = paste0("\\faoheading{", bookmark(2), "}{\\faoheadingtwofont ", body, "}"),
    FAOCaption = paste0("{\\centering\\faocaptionfont ", body, "\\par}"),
    FAOTableCaption = paste0("{\\centering\\faocaptionfont ", body, "\\par}\\nopagebreak[4]"),
    FAOReference = paste0("\\faoreference{", body, "}"),
    FAOCoverProject = paste0("{\\faocoverprojectfont ", body, "\\par}"),
    FAOCoverTitle = paste0("{\\faocovertitlefont ", body, "\\par}"),
    FAOCoverText = paste0("{\\faocovertextfont ", body, "\\par}"),
    paste0(body, "\\par")
  )
  fao_latex_add(x, out, "")
}

# An empty line of the given paragraph style
fao_latex_blank <- function(x, style) {
  size <- c(FAOCoverText = 24, FAOAuthor = 18)[style]
  fao_latex_add(x, sprintf("\\vspace{%gpt}", if (is.na(size)) 18 else size), "")
}

# A figure (copied into the figure folder) with its caption below, kept on one page
fao_latex_figure <- function(x, file, number, caption) {
  state <- attr(x, "fao_report")
  caption <- fao_latex_caption(x, "fig_prefix", number, caption)
  if (is.na(file) || !file.exists(file)) {
    return(fao_latex_add(x, paste0("\\textcolor{faogray}{", fao_latex_text(fao_report_text("figure_missing", state$lang)),
                                   "}\\par"), caption, ""))
  }
  dir <- file.path(dirname(state$latex_file), state$latex_figures)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  target <- paste0("figure_", gsub("[^0-9A-Za-z]+", "_", number), ".", tolower(tools::file_ext(file)))
  file.copy(file, file.path(dir, target), overwrite = TRUE)
  fao_latex_add(x, "\\begin{figure}[H]", "\\centering",
                sprintf("\\includegraphics[width=\\linewidth,height=0.62\\textheight,keepaspectratio]{%s/%s}\\par",
                        state$latex_figures, target),
                "\\vspace{0.4em}", caption, "\\end{figure}", "")
}

# The caption of a figure as LaTeX: "图1-1 ..." in Chinese, "Figure 1-1. ..." with a bold label
fao_latex_caption <- function(x, prefix_key, number, caption) {
  state <- attr(x, "fao_report")
  label <- fao_report_text(prefix_key, state$lang, list(no = number))
  body <- if (state$lang == "zh") {
    fao_latex_runs(x, paste(label, caption), numbered = TRUE)
  } else {
    paste0("\\textbf{", fao_latex_escape(paste0(label, ".")), "}", fao_latex_runs(x, paste0(" ", caption), numbered = TRUE))
  }
  paste0("{\\centering\\faocaptionfont ", body, "\\par}")
}

# A three-line table (booktabs) that may run over pages; column widths follow the longest cell
fao_latex_table <- function(x, table) {
  cell <- function(s) {
    s[is.na(s)] <- ""
    parts <- strsplit(s, "*", fixed = TRUE)
    vapply(parts, function(p) {
      if (length(p) == 0) return("")
      italic <- seq_along(p) %% 2 == 0
      paste(ifelse(italic & nzchar(p), paste0("\\textit{", fao_latex_text_vec(p), "}"), fao_latex_text_vec(p)), collapse = "")
    }, character(1))
  }
  # Column widths, in units of half an em of the table text (a Chinese character is two): every
  # column gets at least its longest word (the first column its longest cell), the rest of the
  # line is shared in proportion to the length of the longest cells
  units <- function(s) nchar(gsub("[*]", "", s), type = "width") + 1
  n <- ncol(table)
  cells <- lapply(seq_len(n), function(j) c(names(table)[j], as.character(table[[j]])))
  full <- vapply(cells, function(v) max(units(v[!is.na(v)]), 4), numeric(1))
  shortest <- vapply(seq_len(n), function(j) {
    if (j == 1) return(min(full[j], 24))
    words <- unlist(strsplit(gsub("[*]", "", cells[[j]][!is.na(cells[[j]])]), " ", fixed = TRUE))
    max(units(words), 4)
  }, numeric(1))
  available <- (415 - 12 * n) / 5.25
  w <- if (sum(full) <= available) {
    full
  } else if (sum(shortest) <= available) {
    shortest + (full - shortest) * (available - sum(shortest)) / sum(full - shortest)
  } else {
    shortest
  }
  w <- w / sum(w)
  spec <- vapply(seq_len(n), function(j) {
    align <- if (j == 1) "\\raggedright" else "\\centering"
    sprintf(">{%s\\arraybackslash}p{%.3f\\dimexpr\\linewidth-%d\\tabcolsep\\relax}", align, w[j], 2 * n)
  }, character(1))
  row <- function(v) paste0(paste(v, collapse = " & "), " \\\\")
  body <- vapply(seq_len(nrow(table)), function(i) row(cell(as.character(unlist(table[i, ])))), character(1))
  fao_latex_add(x, "{\\faotablefont",
                paste0("\\begin{longtable}{@{}", paste(spec, collapse = ""), "@{}}"),
                "\\toprule", row(cell(names(table))), "\\midrule", "\\endhead",
                "\\bottomrule", "\\endlastfoot",
                body,
                "\\end{longtable}}", "")
}

fao_latex_text_vec <- function(s) vapply(s, fao_latex_text, character(1), USE.NAMES = FALSE)

# The end of the cover page; pages are numbered from 1 after it
fao_latex_end_cover <- function(x) {
  fao_latex_add(x, "\\end{titlepage}", "\\setcounter{page}{1}", "")
}

# The preamble: page, fonts and the commands of the paragraph styles
fao_latex_preamble <- function(state, meta) {
  zh <- state$lang == "zh"
  families <- tryCatch(unique(systemfonts::system_fonts()$family), error = function(e) character(0))
  first_font <- function(candidates) {
    hit <- candidates[candidates %in% families]
    if (length(hit) > 0) hit[1] else NA_character_
  }
  latin <- first_font(c(state$font, if (zh || identical(state$font, "Times New Roman")) "Liberation Serif" else "Liberation Sans"))
  size <- function(pt, extra = "") sprintf("\\fontsize{%gpt}{%gpt}\\selectfont%s", pt, pt * 1.5, extra)
  lines <- if (zh) {
    # CJK fonts: the font asked for, else those of Windows or macOS, else the Fandol fonts of TeX Live
    cjk <- function(candidates, fandol) {
      font <- first_font(candidates)
      if (is.na(font)) sprintf("%s.otf", fandol) else font
    }
    fang <- cjk(c(state$body_ea, "FangSong", "STFangsong", "FangSong_GB2312"), "FandolFang-Regular")
    hei <- cjk(c("SimHei", "Heiti SC", "STHeiti", "Noto Sans CJK SC", "Source Han Sans SC"), "FandolHei-Regular")
    kai <- cjk(c("KaiTi", "Kaiti SC", "STKaiti", "KaiTi_GB2312"), "FandolKai-Regular")
    c("\\documentclass[a4paper,zihao=-4,linespread=1.5,fontset=none]{ctexart}",
      # Chinese fonts have no bold face: bold is made by thickening the strokes
      sprintf("\\setCJKmainfont[BoldFont=%s,BoldFeatures={FakeBold=3}]{%s}", fang, fang),
      sprintf("\\setCJKsansfont[BoldFont=%s,BoldFeatures={FakeBold=3}]{%s}", hei, hei),
      sprintf("\\setCJKmonofont{%s}", fang),
      sprintf("\\setCJKfamilyfont{faohei}[BoldFont=%s,BoldFeatures={FakeBold=3}]{%s}", hei, hei),
      sprintf("\\setCJKfamilyfont{faokai}[BoldFont=%s,BoldFeatures={FakeBold=3}]{%s}", kai, kai),
      "\\newcommand{\\faohei}{\\CJKfamily{faohei}}",
      "\\newcommand{\\faokai}{\\CJKfamily{faokai}}",
      if (!is.na(latin)) sprintf("\\setmainfont{%s}", latin),
      "\\newcommand{\\faotitlefont}{\\fontsize{18pt}{27pt}\\selectfont\\bfseries}",
      paste0("\\newcommand{\\faoabstractfont}{", size(15), "\\faohei}"),
      paste0("\\newcommand{\\faoheadingonefont}{", size(14), "\\faohei}"),
      paste0("\\newcommand{\\faoheadingtwofont}{", size(14), "\\faokai}"),
      paste0("\\newcommand{\\faocaptionfont}{", size(12), "\\faohei}"),
      "\\newcommand{\\faotablefont}{\\linespread{1}\\fontsize{10.5pt}{14pt}\\selectfont}",
      paste0("\\newcommand{\\faocoverprojectfont}{", size(26), "\\faohei\\bfseries}"),
      paste0("\\newcommand{\\faocovertitlefont}{", size(22), "\\bfseries}"),
      paste0("\\newcommand{\\faocovertextfont}{", size(16), "}"),
      "\\newcommand{\\faoreference}[1]{\\par\\noindent\\hangindent=2em\\hangafter=1 #1\\par}")
  } else {
    c("\\documentclass[a4paper,12pt]{article}",
      "\\usepackage{fontspec}",
      if (!is.na(latin)) sprintf("\\setmainfont{%s}", latin),
      "\\usepackage{setspace}", "\\onehalfspacing",
      "\\setlength{\\parindent}{24pt}",
      "\\newcommand{\\faotitlefont}{\\fontsize{18pt}{27pt}\\selectfont\\bfseries}",
      paste0("\\newcommand{\\faoabstractfont}{", size(15), "\\bfseries}"),
      paste0("\\newcommand{\\faoheadingonefont}{", size(14), "\\bfseries}"),
      paste0("\\newcommand{\\faoheadingtwofont}{", size(12), "\\bfseries}"),
      paste0("\\newcommand{\\faocaptionfont}{", size(12), "}"),
      "\\newcommand{\\faotablefont}{\\linespread{1}\\fontsize{10.5pt}{13pt}\\selectfont}",
      paste0("\\newcommand{\\faocoverprojectfont}{", size(26), "\\bfseries}"),
      paste0("\\newcommand{\\faocovertitlefont}{", size(22), "\\bfseries}"),
      paste0("\\newcommand{\\faocovertextfont}{", size(16), "}"),
      sprintf("\\newcommand{\\faoreference}[1]{\\par\\noindent\\hangindent=%s\\hangafter=1 #1\\par}",
              if (state$citation == "numeric") "24pt" else "36pt"))
  }
  c(lines,
    "\\usepackage[a4paper,top=2.54cm,bottom=2.54cm,left=3.18cm,right=3.18cm]{geometry}",
    "\\usepackage{graphicx}", "\\usepackage{float}", "\\usepackage{array}", "\\usepackage{longtable}",
    "\\usepackage{booktabs}", "\\usepackage{needspace}", "\\usepackage{xcolor}",
    "\\definecolor{faogray}{HTML}{7F7F7F}",
    "\\usepackage[hidelinks,bookmarks=true]{hyperref}",
    sprintf("\\hypersetup{pdftitle={%s},pdfauthor={%s}}", fao_latex_escape(meta$title), fao_latex_escape(meta$author)),
    "\\usepackage{xurl}", "\\urlstyle{same}",
    "\\pagestyle{plain}",
    "\\setlength{\\LTpre}{0.3em}", "\\setlength{\\LTpost}{0.8em}",
    "\\newcommand{\\faobookmark}[3]{\\pdfbookmark[#1]{#2}{#3}}",
    "\\newcommand{\\faoheading}[2]{\\par\\Needspace{5\\baselineskip}#1\\noindent{#2\\par}\\nopagebreak[4]}")
}

# Writes the .tex file, and compiles it to PDF with XeLaTeX when asked
fao_latex_save <- function(x, meta, pdf = FALSE) {
  state <- attr(x, "fao_report")
  tex <- c(fao_latex_preamble(state, meta), "\\begin{document}", state$latex, "\\end{document}")
  writeLines(enc2utf8(tex), state$latex_file, useBytes = TRUE)
  out <- c(latex = state$latex_file)
  if (pdf) out <- c(out, pdf = fao_latex_compile(state$latex_file))
  out
}

fao_latex_compile <- function(file) {
  pdf <- sub("\\.tex$", ".pdf", file)
  if (requireNamespace("tinytex", quietly = TRUE)) {
    tinytex::latexmk(file, engine = "xelatex", clean = TRUE)
  } else {
    if (!nzchar(Sys.which("xelatex"))) {
      stop("A PDF report needs XeLaTeX. Install it with install.packages(\"tinytex\"); tinytex::install_tinytex(), ",
           "or use format = \"latex\" and compile ", basename(file), " yourself.", call. = FALSE)
    }
    owd <- setwd(dirname(file))
    on.exit(setwd(owd), add = TRUE)
    for (i in 1:2) system2("xelatex", c("-interaction=nonstopmode", "-halt-on-error", shQuote(basename(file))),
                           stdout = FALSE, stderr = FALSE)
    unlink(sub("\\.tex$", c(".aux", ".log", ".out"), basename(file)))
  }
  if (!file.exists(pdf)) stop("The PDF report could not be made from ", file, call. = FALSE)
  pdf
}
