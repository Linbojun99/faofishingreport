# Chinese function reference of the website: one page per help page (vignettes/articles/zh/reference/),
# made from the help pages (man/*.Rd) and their Chinese translations (pkgdown/zh-reference/*.json).
# Each section is given in Chinese with the English original below it; usage, examples and
# references are taken from the help page as they are.
#
# Run from the package root, in a UTF-8 locale:
#   LC_ALL=en_US.UTF-8 Rscript data-raw/make_zh_reference.R extract   # English text to translate
#   LC_ALL=en_US.UTF-8 Rscript data-raw/make_zh_reference.R build     # Chinese pages
#
# `extract` keeps the existing translations. When a help page has changed since it was translated,
# its file is marked "stale": true; translate the changed parts and set it back to false.

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop("Run in a UTF-8 locale: LC_ALL=en_US.UTF-8 Rscript data-raw/make_zh_reference.R ...")
}
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) > 0) args[1] else "build"
json_dir <- file.path("pkgdown", "zh-reference")
page_dir <- file.path("vignettes", "articles", "zh", "reference")

# ---- Help pages to Markdown ------------------------------------------------------------------
tag <- function(x) {
  t <- attr(x, "Rd_tag")
  if (is.null(t)) "" else t
}

# Rd escapes (\\%, \\{, \\}, \\\\) as plain characters
rd_unescape <- function(x) gsub("\\\\([%{}\\\\])", "\\1", x)

rd_text <- function(x) {
  if (is.character(x)) return(rd_unescape(paste(x, collapse = "")))
  paste(vapply(x, rd_text, character(1)), collapse = "")
}

rd_md <- function(x) {
  t <- tag(x)
  if (is.character(x) && !t %in% c("\\dots", "\\ldots", "USERMACRO")) return(rd_unescape(paste(x, collapse = "")))
  inner <- function(i = 1) rd_md(x[[i]])
  switch(t,
    "\\code" = , "\\verb" = , "\\samp" = , "\\env" = , "\\option" = , "\\command" = , "\\file" = , "\\pkg" =
      paste0("`", gsub("`", "", rd_text(x)), "`"),
    "\\link" = rd_md_list(x),
    "\\linkS4class" = rd_text(x),
    "\\emph" = , "\\var" = , "\\dfn" = paste0("*", rd_md_list(x), "*"),
    "\\bold" = , "\\strong" = paste0("**", rd_md_list(x), "**"),
    "\\sQuote" = paste0("‘", rd_md_list(x), "’"),
    "\\dQuote" = paste0("“", rd_md_list(x), "”"),
    "\\href" = paste0("[", rd_md(x[[2]]), "](", rd_text(x[[1]]), ")"),
    "\\url" = paste0("<", rd_text(x), ">"),
    "\\email" = paste0("<", rd_text(x), ">"),
    "\\doi" = paste0("[doi:", rd_text(x), "](https://doi.org/", rd_text(x), ")"),
    "\\eqn" = , "\\deqn" = paste0("`", rd_text(x[[if (length(x) > 1) 2 else 1]]), "`"),
    "\\dots" = , "\\ldots" = "...",
    "\\R" = "R",
    "\\cr" = "  \n",
    "\\tab" = " | ",
    "\\itemize" = rd_md_items(x, numbered = FALSE),
    "\\enumerate" = rd_md_items(x, numbered = TRUE),
    "\\describe" = rd_md_describe(x),
    "\\preformatted" = paste0("\n```\n", rd_text(x), "\n```\n"),
    "\\tabular" = rd_md_table(x[[2]]),
    "\\Sexpr" = "",
    # \doi{} and other user macros are given as the macro and its argument, then the expansion
    # (a \Sexpr, dropped above)
    "USERMACRO" = if (identical(attr(x, "macro"), "\\doi")) {
      paste0("[doi:", x[[2]], "](https://doi.org/", x[[2]], ")")
    } else {
      ""
    },
    "COMMENT" = "",
    rd_md_list(x)
  )
}

rd_md_list <- function(x) paste(vapply(x, rd_md, character(1)), collapse = "")

# \tabular{}{}: a Markdown table, the first row as header
rd_md_table <- function(x) {
  rows <- list()
  row <- character(0)
  cell <- ""
  for (e in x) {
    t <- tag(e)
    if (t == "\\tab") {
      row <- c(row, cell); cell <- ""
    } else if (t == "\\cr") {
      rows[[length(rows) + 1]] <- c(row, cell); row <- character(0); cell <- ""
    } else {
      cell <- paste0(cell, rd_md(e))
    }
  }
  if (length(row) > 0 || nzchar(trimws(cell))) rows[[length(rows) + 1]] <- c(row, cell)
  rows <- lapply(rows, function(r) gsub("\\s+", " ", trimws(r)))
  rows <- Filter(function(r) any(nzchar(r)), rows)
  if (length(rows) == 0) return("")
  line <- function(r) paste0("| ", paste(gsub("\\|", "\\\\|", r), collapse = " | "), " |")
  paste0("\n\n", paste(c(line(rows[[1]]), paste0("|", strrep("---|", length(rows[[1]]))),
                         vapply(rows[-1], line, character(1))), collapse = "\n"), "\n\n")
}

rd_md_items <- function(x, numbered) {
  out <- character(0)
  current <- NULL
  for (e in x) {
    if (tag(e) == "\\item") {
      if (!is.null(current)) out <- c(out, current)
      current <- ""
    } else if (!is.null(current)) {
      current <- paste0(current, rd_md(e))
    }
  }
  if (!is.null(current)) out <- c(out, current)
  out <- gsub("\\s+", " ", trimws(out))
  prefix <- if (numbered) paste0(seq_along(out), ". ") else "- "
  paste0("\n\n", paste0(prefix, out, collapse = "\n"), "\n\n")
}

rd_md_describe <- function(x) {
  items <- Filter(function(e) tag(e) == "\\item", x)
  out <- vapply(items, function(e) {
    paste0("- **", gsub("\\s+", " ", trimws(rd_md(e[[1]]))), "**: ", gsub("\\s+", " ", trimws(rd_md(e[[2]]))))
  }, character(1))
  paste0("\n\n", paste(out, collapse = "\n"), "\n\n")
}

# Tidy Markdown: no indentation from the Rd source, at most one blank line
md_clean <- function(s) {
  s <- gsub("\\[([A-Za-z_.][A-Za-z0-9_.]*\\(\\))\\]", "`\\1`", s)
  s <- gsub("[ \t]+\n", "\n", s)
  s <- gsub("\n[ \t]+", "\n", s)
  s <- gsub("\n{3,}", "\n\n", s)
  trimws(s)
}

rd_sections <- function(file) {
  rd <- tools::parse_Rd(file, encoding = "UTF-8")
  get <- function(name) {
    hit <- Filter(function(e) tag(e) == name, rd)
    if (length(hit) == 0) NULL else hit[[1]]
  }
  arguments <- list()
  arg_block <- get("\\arguments")
  if (!is.null(arg_block)) {
    for (e in Filter(function(e) tag(e) == "\\item", arg_block)) {
      arguments[[trimws(rd_text(e[[1]]))]] <- md_clean(rd_md(e[[2]]))
    }
  }
  value <- get("\\value")
  sections <- list()
  for (e in Filter(function(e) tag(e) == "\\section", rd)) {
    sections[[trimws(rd_text(e[[1]]))]] <- md_clean(rd_md(e[[2]]))
  }
  list(
    name = trimws(rd_text(get("\\name"))),
    title = gsub("\\s+", " ", trimws(rd_text(get("\\title")))),
    description = if (!is.null(get("\\description"))) md_clean(rd_md(get("\\description"))) else "",
    details = if (!is.null(get("\\details"))) md_clean(rd_md(get("\\details"))) else "",
    value = if (!is.null(value)) md_clean(if (any(vapply(value, tag, "") == "\\item")) rd_md_describe(value) else rd_md(value)) else "",
    arguments = arguments,
    sections = sections,
    references = if (!is.null(get("\\references"))) md_clean(rd_md(get("\\references"))) else "",
    seealso = if (!is.null(get("\\seealso"))) md_clean(rd_md(get("\\seealso"))) else "",
    usage = trimws(rd_text(get("\\usage"))),
    examples = if (!is.null(get("\\examples"))) rd_examples(get("\\examples")) else ""
  )
}

# Examples: the code inside \dontrun{} is shown as it is
rd_examples <- function(x) {
  code <- paste(vapply(x, function(e) if (tag(e) %in% c("\\dontrun", "\\donttest")) rd_text(e) else rd_text(e), character(1)), collapse = "")
  trimws(gsub("\n{3,}", "\n\n", code))
}

topics <- function() {
  files <- list.files("man", pattern = "[.]Rd$", full.names = TRUE)
  files[!vapply(files, function(f) any(grepl("\\\\keyword\\{internal\\}", readLines(f, warn = FALSE))), logical(1))]
}

# ---- extract: English text to translate -------------------------------------------------------
extract <- function() {
  dir.create(json_dir, showWarnings = FALSE, recursive = TRUE)
  for (file in topics()) {
    s <- rd_sections(file)
    out_file <- file.path(json_dir, paste0(s$name, ".json"))
    md5 <- unname(tools::md5sum(file))
    old <- if (file.exists(out_file)) jsonlite::read_json(out_file) else NULL
    empty <- function(x) if (is.list(x)) lapply(x, function(i) "") else ""
    zh <- if (!is.null(old)) old$zh else list()
    for (field in c("title", "description", "details", "value")) if (is.null(zh[[field]])) zh[[field]] <- ""
    for (group in c("arguments", "sections", "section_titles")) {
      keep <- if (is.null(zh[[group]])) list() else zh[[group]]
      from <- if (group == "section_titles") "sections" else group
      zh[[group]] <- stats::setNames(lapply(names(s[[from]]), function(n) if (is.null(keep[[n]])) "" else keep[[n]]), names(s[[from]]))
      if (length(zh[[group]]) == 0) zh[[group]] <- structure(list(), names = character(0))
    }
    en <- s[c("title", "description", "details", "value", "arguments", "sections")]
    for (group in c("arguments", "sections")) if (length(en[[group]]) == 0) en[[group]] <- structure(list(), names = character(0))
    stale <- !is.null(old) && !identical(old$rd_md5, md5) && any(nzchar(unlist(zh)))
    jsonlite::write_json(list(topic = s$name, rd_md5 = md5, stale = stale || isTRUE(old$stale), en = en, zh = zh),
                         out_file, auto_unbox = TRUE, pretty = TRUE)
  }
  message("Wrote ", length(topics()), " files in ", json_dir)
}

# ---- build: Chinese pages ----------------------------------------------------------------------
# Groups and order of the reference index, from _pkgdown.yml
groups_zh <- c("💾 FAO data" = "💾 FAO数据", "🌊 Fishing areas" = "🌊 渔区分析",
               "🐟 Species and countries" = "🐟 物种与国家", "📊 Summary analyses" = "📊 汇总分析",
               "🔬 Literature-based analyses" = "🔬 文献方法分析", "🌡️ FishBase and temperature" = "🌡️ FishBase与温度",
               "📉 CMSY++ stock assessment" = "📉 CMSY++资源评估", "📝 Reports" = "📝 报告",
               "🎨 Settings and language" = "🎨 设置与语言")

# "<label>" in file names is written as text, not read as an HTML tag (code spans and <https://...>
# links are kept)
md_escape <- function(s) {
  if (is.null(s) || !nzchar(s)) return(s)
  parts <- strsplit(s, "`", fixed = TRUE)[[1]]
  plain <- seq_along(parts) %% 2 == 1
  parts[plain] <- gsub("<(?!https?://|mailto:|/?details|/?summary)([A-Za-z_][^<>\\s]*)>", "&lt;\\1&gt;", parts[plain], perl = TRUE)
  out <- paste(parts, collapse = "`")
  if (endsWith(s, "`")) paste0(out, "`") else out
}

translated <- function(zh) !is.null(zh) && nzchar(trimws(zh))
text_or_en <- function(zh, en) if (translated(zh)) zh else en

# The English original below the Chinese text, folded (shown only when the text is translated)
en_block <- function(text, zh, indent = "") {
  if (!nzchar(trimws(text)) || !translated(zh)) return("")
  lines <- c("<details class=\"fao-en\"><summary>English</summary>", "", strsplit(text, "\n")[[1]], "", "</details>")
  paste0(paste0(ifelse(nzchar(lines), indent, ""), lines, collapse = "\n"), "\n")
}

page <- function(s, tr) {
  esc <- function(x) if (is.list(x)) lapply(x, esc) else if (is.character(x)) md_escape(x) else x
  for (field in c("description", "details", "value", "arguments", "sections", "references", "seealso")) {
    s[[field]] <- esc(s[[field]])
  }
  zh <- esc(tr$zh)
  yaml_text <- function(x) gsub("\"", "\\\\\"", x)
  section <- function(heading, zh_text, en_text) {
    if (!nzchar(trimws(en_text))) return("")
    paste0("## ", heading, "\n\n", text_or_en(zh_text, en_text), "\n\n", en_block(en_text, zh_text), "\n")
  }
  args <- ""
  if (length(s$arguments) > 0) {
    items <- vapply(names(s$arguments), function(n) {
      zh_text <- text_or_en(zh$arguments[[n]], s$arguments[[n]])
      indent <- function(x) gsub("\n", "\n    ", x)
      paste0("`", n, "`\n:   ", indent(zh_text), "\n\n", en_block(s$arguments[[n]], zh$arguments[[n]], "    "))
    }, character(1))
    args <- paste0("## 参数\n\n", paste(items, collapse = "\n"), "\n")
  }
  extra <- paste(vapply(names(s$sections), function(n) {
    section(text_or_en(zh$section_titles[[n]], n), zh$sections[[n]], s$sections[[n]])
  }, character(1)), collapse = "")
  seealso <- if (nzchar(s$seealso)) paste0("## 另请参阅\n\n", s$seealso, "\n\n") else ""
  references <- if (nzchar(s$references)) paste0("## 参考文献\n\n", s$references, "\n\n") else ""
  examples <- if (nzchar(s$examples)) paste0("## 示例\n\n```r\n", s$examples, "\n```\n\n") else ""
  paste0(
    "---\n",
    "title: \"", yaml_text(paste0(s$name, "(): ", text_or_en(zh$title, s$title))), "\"\n",
    "description: \"", yaml_text(s$title), "\"\n",
    "---\n\n",
    "<!-- Generated by data-raw/make_zh_reference.R from man/", s$name, ".Rd and pkgdown/zh-reference/",
    s$name, ".json: do not edit by hand. -->\n\n",
    "::: {.fao-ref-source}\n英文原页：[`", s$name, "()`](../../../reference/", s$name, ".html)",
    if (isTRUE(tr$stale)) "（英文说明已更新，中文翻译待更新）" else "", "\n:::\n\n",
    section("说明", zh$description, s$description),
    "## 用法\n\n```r\n", s$usage, "\n```\n\n",
    args,
    section("返回值", zh$value, s$value),
    section("详细说明", zh$details, s$details),
    extra, references, seealso, examples
  )
}

build <- function() {
  dir.create(page_dir, showWarnings = FALSE, recursive = TRUE)
  unlink(list.files(page_dir, pattern = "[.]Rmd$", full.names = TRUE))
  titles <- list()
  missing <- character(0)
  for (file in topics()) {
    s <- rd_sections(file)
    json <- file.path(json_dir, paste0(s$name, ".json"))
    tr <- if (file.exists(json)) jsonlite::read_json(json) else list(zh = list())
    if (!nzchar(text_or_en(tr$zh$title, ""))) missing <- c(missing, s$name)
    writeLines(enc2utf8(page(s, tr)), file.path(page_dir, paste0(s$name, ".Rmd")), useBytes = TRUE)
    titles[[s$name]] <- text_or_en(tr$zh$title, s$title)
  }
  # index: the groups of _pkgdown.yml
  config <- yaml::read_yaml("_pkgdown.yml")
  lines <- c("---", "title: \"函数参考\"", "description: \"faofishingreport全部函数的中文说明\"", "---", "",
             "<!-- Generated by data-raw/make_zh_reference.R: do not edit by hand. -->", "",
             "每个函数的中文说明，附英文原文对照；用法、示例和参考文献与英文帮助页面相同。在R中可以用`?函数名`查看英文帮助。", "")
  for (group in config$reference) {
    title <- if (!is.null(groups_zh[group$title])) groups_zh[[group$title]] else group$title
    lines <- c(lines, paste("##", title), "", "| 函数 | 说明 |", "|---|---|")
    for (name in unlist(group$contents)) {
      if (!is.null(titles[[name]])) lines <- c(lines, paste0("| [`", name, "()`](", name, ".html) | ", titles[[name]], " |"))
    }
    lines <- c(lines, "")
  }
  writeLines(enc2utf8(lines), file.path(page_dir, "index.Rmd"), useBytes = TRUE)
  message("Wrote ", length(titles), " pages and the index in ", page_dir,
          if (length(missing) > 0) paste0("; not yet translated: ", paste(missing, collapse = ", ")))
}

if (mode == "extract") extract() else build()
