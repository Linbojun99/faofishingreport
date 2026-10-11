# Writing the Word file of fao_report(): styles, paragraphs with citations and italics, figures,
# tables, captions, sections and the final clean-up of the file.

# A new Word document with the styles of the report
fao_report_new <- function(lang, font, font_zh, citation, report_no, references) {
  zh <- lang == "zh"
  body_ea <- if (zh) font_zh else font
  heading_ea <- if (zh) "SimHei" else font
  x <- officer::read_docx()
  style <- function(x, id, size, ea = body_ea, align = "justify", line = 1.5, bold = FALSE,
                    first_line = 0, hanging = 0, keep = FALSE, color = "black") {
    officer::docx_set_paragraph_style(
      x, style_id = id, style_name = fao_report_style_name(id), base_on = "Normal",
      fp_p = officer::fp_par(text.align = align, line_spacing = line, padding.top = 0, padding.bottom = 0,
                             padding.left = hanging, padding.right = 0, first_line = first_line,
                             hanging = hanging, keep_with_next = keep),
      fp_t = officer::fp_text_lite(font.family = font, hansi.family = font, cs.family = font,
                                   eastasia.family = ea, font.size = size, bold = bold, color = color)
    )
  }
  x <- style(x, "FAOBody", 12, first_line = 24)
  x <- style(x, "FAONote", 12, first_line = 24, color = "#7F7F7F")
  x <- style(x, "FAOTitle", 18, align = "center", bold = TRUE)
  x <- style(x, "FAOAuthor", 12, align = "center")
  x <- style(x, "FAOAbstractHeading", 15, ea = heading_ea, align = "center", bold = !zh)
  x <- style(x, "FAOHeading1", 14, ea = heading_ea, align = "left", bold = !zh, keep = TRUE)
  x <- style(x, "FAOHeading2", if (zh) 14 else 12, ea = if (zh) "KaiTi" else font, align = "left",
             bold = !zh, keep = TRUE)
  x <- style(x, "FAOFigure", 12, align = "center", line = 1, keep = TRUE)
  x <- style(x, "FAOCaption", 12, ea = heading_ea, align = "center")
  x <- style(x, "FAOTableCaption", 12, ea = heading_ea, align = "center", keep = TRUE)
  x <- style(x, "FAOTableText", 10.5, align = "center", line = 1)
  x <- style(x, "FAOReference", 12, align = "left", hanging = if (citation == "numeric") 24 else 36)
  x <- style(x, "FAOCoverProject", 26, ea = heading_ea, align = "center", bold = TRUE)
  x <- style(x, "FAOCoverTitle", 22, align = "center", bold = TRUE)
  x <- style(x, "FAOCoverText", 16, align = "center")

  # Report state: language, fonts, numbering of figures and tables, and the citations
  state <- new.env()
  state$lang <- lang
  state$font <- font
  state$body_ea <- body_ea
  state$citation <- citation
  state$report_no <- report_no
  state$figures <- character()
  state$tables <- character()
  state$cited <- character()
  state$references <- references
  state$suffix <- fao_ref_suffixes(references, citation)
  attr(x, "fao_report") <- state
  x
}

# Letters after the year for references of the same authors and year (2026a, 2026b)
fao_ref_suffixes <- function(references, citation) {
  suffix <- stats::setNames(rep("", length(references)), names(references))
  if (citation == "numeric") return(suffix)
  id <- vapply(references, function(r) paste(fao_ref_short(r$author, citation), r$year), character(1))
  for (same in split(names(references), id)) {
    if (length(same) > 1) {
      titles <- vapply(references[same], function(r) tolower(r$title), character(1))
      suffix[same[order(titles)]] <- letters[seq_along(same)]
    }
  }
  suffix
}

# The number of a figure or table ("1-3"), given when it is first referred to
fao_report_number_of <- function(x, kind, id) {
  state <- attr(x, "fao_report")
  ids <- state[[kind]]
  if (!id %in% ids) state[[kind]] <- c(ids, id)
  paste0(state$report_no, "-", match(id, state[[kind]]))
}

# "Figure 1-3" or "Figures 1-3 and 1-4" for the text
fao_report_ref <- function(x, kind, ids) {
  state <- attr(x, "fao_report")
  numbers <- vapply(ids, function(id) fao_report_number_of(x, kind, id), character(1))
  key <- if (kind == "figures") "fig_prefix" else "tab_prefix"
  if (state$lang == "zh") {
    paste(vapply(numbers, function(n) fao_report_text(key, "zh", list(no = n)), character(1)), collapse = "\u3001")
  } else {
    label <- fao_report_text(key, "en", list(no = ""))
    if (length(numbers) > 1) paste0(trimws(label), "s ", fao_report_list(numbers, "en")) else paste0(label, numbers)
  }
}

# The text of citations: superscript numbers, or author and year
fao_report_cite <- function(x, keys) {
  state <- attr(x, "fao_report")
  for (key in keys) {
    if (is.null(state$references[[key]])) stop("Unknown reference: ", key, call. = FALSE)
    if (!key %in% state$cited) state$cited <- c(state$cited, key)
  }
  if (state$citation == "numeric") {
    n <- sort(match(keys, state$cited))
    label <- if (length(n) > 2 && all(diff(n) == 1)) paste0(n[1], "-", n[length(n)]) else paste(n, collapse = ",")
    list(list(text = paste0("[", label, "]"), sup = TRUE))
  } else {
    parts <- vapply(keys, function(k) {
      paste0(fao_ref_short(state$references[[k]]$author, state$citation), ", ",
             state$references[[k]]$year, state$suffix[[k]])
    }, character(1))
    list(list(text = paste0("(", paste(sort(parts), collapse = "; "), ")")))
  }
}

# Splits marked text into runs: citations, *italics*, ^superscripts^ and ~subscripts~
fao_report_runs <- function(x, text, numbered = FALSE, spacing = TRUE) {
  state <- attr(x, "fao_report")
  if (state$citation == "numeric") text <- gsub("\\s+\\{cite:", "{cite:", text)
  if (state$lang == "zh" && spacing) text <- fao_report_zh_spacing(text, numbered)
  pattern <- "\\{cite:[^}]+\\}|\\*[^*]+\\*|\\^[^^]+\\^|~[^~]+~"
  marks <- gregexpr(pattern, text)[[1]]
  runs <- list()
  pos <- 1
  if (marks[1] != -1) {
    for (i in seq_along(marks)) {
      start <- marks[i]
      end <- start + attr(marks, "match.length")[i] - 1
      if (start > pos) runs <- c(runs, list(list(text = substr(text, pos, start - 1))))
      mark <- substr(text, start, end)
      inner <- substr(mark, 2, nchar(mark) - 1)
      runs <- c(runs, switch(substr(mark, 1, 1),
        "{" = fao_report_cite(x, trimws(strsplit(sub("^cite:", "", inner), ",")[[1]])),
        "*" = list(list(text = inner, italic = TRUE)),
        "^" = list(list(text = inner, sup = TRUE)),
        "~" = list(list(text = inner, sub = TRUE))
      ))
      pos <- end + 1
    }
  }
  if (pos <= nchar(text)) runs <- c(runs, list(list(text = substr(text, pos, nchar(text)))))
  runs
}

# Removes spaces next to Chinese characters and punctuation, except the space after the number
# of a heading ("1.1 ") or of a figure or table ("1-1 ")
fao_report_zh_spacing <- function(text, numbered = FALSE) {
  number <- if (numbered) regmatches(text, regexpr("^(\u9644\u8868|[\u56fe\u8868])?[0-9]+([.-][0-9]+)* ", text)) else ""
  if (length(number) == 0) number <- ""
  text <- substr(text, nchar(number) + 1, nchar(text))
  cjk <- "[\u3000-\u303f\u4e00-\u9fff\uff00-\uffef]"
  text <- gsub(paste0("(", cjk, ") +"), "\\1", text, perl = TRUE)
  paste0(number, gsub(paste0(" +(", cjk, ")"), "\\1", text, perl = TRUE))
}

# Adds a paragraph of marked text
fao_report_par <- function(x, text, style = "FAOBody", bold_prefix = NULL) {
  numbered <- style %in% c("FAOHeading1", "FAOHeading2", "FAOCaption", "FAOTableCaption")
  # notes and references are written as they are
  runs <- fao_report_runs(x, text, numbered, spacing = !style %in% c("FAONote", "FAOReference"))
  pieces <- lapply(runs, function(run) {
    if (isTRUE(run$italic) || isTRUE(run$sup) || isTRUE(run$sub)) {
      officer::ftext(run$text, officer::fp_text_lite(
        italic = isTRUE(run$italic),
        vertical.align = if (isTRUE(run$sup)) "superscript" else if (isTRUE(run$sub)) "subscript" else "baseline"))
    } else {
      run$text
    }
  })
  if (!is.null(bold_prefix)) pieces <- c(list(officer::ftext(bold_prefix, officer::fp_text_lite(bold = TRUE))), pieces)
  officer::body_add_fpar(x, do.call(officer::fpar, pieces), style = fao_report_style_name(style))
}

fao_report_style_name <- function(id) gsub("([a-z])([A-Z0-9])", "\\1 \\2", sub("^FAO", "FAO ", id))

# Adds a figure with its caption below; the size follows the PNG, within the text width
fao_report_figure <- function(x, id, file, caption) {
  state <- attr(x, "fao_report")
  number <- fao_report_number_of(x, "figures", id)
  if (is.na(file) || !file.exists(file)) {
    x <- fao_report_par(x, fao_report_text("figure_missing", state$lang), "FAONote")
  } else {
    header <- readBin(file, "raw", 24)
    size <- c(sum(as.integer(header[17:20]) * 256^(3:0)), sum(as.integer(header[21:24]) * 256^(3:0)))
    width <- (11906 - 2 * 1800) / 1440
    height <- width * size[2] / size[1]
    if (height > 7.5) {
      width <- width * 7.5 / height
      height <- 7.5
    }
    x <- officer::body_add_img(x, file, style = fao_report_style_name("FAOFigure"), width = width, height = height)
  }
  fao_report_caption(x, "fig_prefix", number, caption, "FAOCaption")
}

fao_report_caption <- function(x, prefix_key, number, caption, style) {
  state <- attr(x, "fao_report")
  label <- fao_report_text(prefix_key, state$lang, list(no = number))
  if (state$lang == "zh") {
    fao_report_par(x, paste(label, caption), style)
  } else {
    fao_report_par(x, paste0(" ", caption), style, bold_prefix = paste0(label, "."))
  }
}

# Adds a table with its caption above (three-line borders are added when the file is saved)
fao_report_table <- function(x, id, table, caption, appendix = FALSE) {
  if (appendix) {
    state <- attr(x, "fao_report")
    state$appendix <- c(state$appendix, id)
    x <- fao_report_caption(x, "app_tab_prefix", length(state$appendix), caption, "FAOTableCaption")
  } else {
    x <- fao_report_caption(x, "tab_prefix", fao_report_number_of(x, "tables", id), caption, "FAOTableCaption")
  }
  table[] <- lapply(table, as.character)
  if (attr(x, "fao_report")$lang == "zh") {
    table[] <- lapply(table, fao_report_zh_spacing)
    names(table) <- fao_report_zh_spacing(names(table))
  }
  styles <- list(names(table))
  names(styles) <- fao_report_style_name("FAOTableText")
  x <- officer::body_add_table(x, table, style = "Normal Table", header = TRUE,
                               stylenames = officer::table_stylenames(styles), align_table = "center")
  fao_report_table_italics(x)
  officer::body_add_par(x, "", style = fao_report_style_name("FAOTableText"))
}

# Text between * marks in the cells of the last table becomes italic (scientific names)
fao_report_table_italics <- function(x) {
  w <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  ns <- c(w = w)
  body <- officer::docx_body_xml(x)
  tables <- xml2::xml_find_all(body, ".//w:tbl", ns)
  if (length(tables) == 0) return(invisible(x))
  escape <- function(s) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", s, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  for (run in xml2::xml_find_all(tables[[length(tables)]], ".//w:r[w:t[contains(., '*')]]", ns)) {
    text <- xml2::xml_text(xml2::xml_find_first(run, "w:t", ns))
    parts <- strsplit(text, "*", fixed = TRUE)[[1]]
    italic <- seq_along(parts) %% 2 == 0
    for (i in seq_along(parts)) {
      if (parts[i] == "") next
      new <- xml2::read_xml(sprintf('<w:r xmlns:w="%s">%s<w:t xml:space="preserve">%s</w:t></w:r>', w,
                                    if (italic[i]) "<w:rPr><w:i/></w:rPr>" else "", escape(parts[i])))
      xml2::xml_add_sibling(run, new, .where = "before")
    }
    xml2::xml_remove(run)
  }
  invisible(x)
}

# Page size, margins and page numbers of the report sections
fao_report_section <- function(state, footer = TRUE) {
  page_number <- officer::block_list(officer::fpar(
    officer::run_word_field("PAGE", prop = officer::fp_text_lite(font.family = state$font, font.size = 10.5)),
    fp_p = officer::fp_par(text.align = "center")))
  officer::prop_section(
    page_size = officer::page_size(width = 11906 / 1440, height = 16838 / 1440, orient = "portrait"),
    page_margins = officer::page_mar(top = 1, bottom = 1, left = 1.25, right = 1.25,
                                     header = 851 / 1440, footer = 992 / 1440, gutter = 0),
    type = "nextPage",
    footer_default = if (footer) page_number else NULL
  )
}

# Saves the document: three-line tables, page numbers from 1 after the cover, and explicit
# fonts (no theme fonts) and outline levels in the styles
fao_report_save <- function(x, file, meta) {
  state <- attr(x, "fao_report")
  w <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  node <- function(xml) xml2::read_xml(sprintf(xml, w))
  body <- officer::docx_body_xml(x)
  ns <- c(w = w)
  for (tbl in xml2::xml_find_all(body, ".//w:tbl", ns)) {
    pr <- xml2::xml_find_first(tbl, "w:tblPr", ns)
    xml2::xml_remove(xml2::xml_find_all(pr, "w:tblBorders", ns))
    borders <- node(paste0('<w:tblBorders xmlns:w="%s">',
                           '<w:top w:val="single" w:sz="12" w:space="0" w:color="000000"/>',
                           '<w:left w:val="nil"/>',
                           '<w:bottom w:val="single" w:sz="12" w:space="0" w:color="000000"/>',
                           '<w:right w:val="nil"/><w:insideH w:val="nil"/><w:insideV w:val="nil"/>',
                           '</w:tblBorders>'))
    look <- xml2::xml_find_first(pr, "w:tblLook", ns)
    if (inherits(look, "xml_missing")) xml2::xml_add_child(pr, borders) else xml2::xml_add_sibling(look, borders, .where = "before")
    header <- xml2::xml_find_first(tbl, "w:tr", ns)
    for (tc in xml2::xml_find_all(header, "w:tc", ns)) {
      if (inherits(xml2::xml_find_first(tc, "w:tcPr", ns), "xml_missing")) {
        xml2::xml_add_child(tc, node('<w:tcPr xmlns:w="%s"/>'), .where = 0)
      }
      xml2::xml_add_child(xml2::xml_find_first(tc, "w:tcPr", ns),
                          node('<w:tcBorders xmlns:w="%s"><w:bottom w:val="single" w:sz="6" w:space="0" w:color="000000"/></w:tcBorders>'))
    }
  }
  sections <- xml2::xml_find_all(body, ".//w:sectPr", ns)
  last <- sections[[length(sections)]]
  start <- node('<w:pgNumType xmlns:w="%s" w:start="1"/>')
  cols <- xml2::xml_find_first(last, "w:cols", ns)
  if (inherits(cols, "xml_missing")) xml2::xml_add_child(last, start) else xml2::xml_add_sibling(cols, start, .where = "before")

  styles_file <- file.path(x$package_dir, "word", "styles.xml")
  if (file.exists(styles_file)) {
    s <- paste(readLines(styles_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    s <- gsub(" w:(asciiTheme|eastAsiaTheme|hAnsiTheme|cstheme)=\"[^\"]*\"", "", s)
    fonts <- sprintf(" w:ascii=\"%s\" w:hAnsi=\"%s\" w:eastAsia=\"%s\" w:cs=\"%s\"",
                     state$font, state$font, state$body_ea, state$font)
    s <- sub("(?s)(<w:docDefaults>.*?<w:rFonts)[^/>]*", paste0("\\1", fonts), s, perl = TRUE)
    if (state$lang == "zh") {
      s <- sub("(?s)(<w:docDefaults>.*?<w:lang )[^/>]*", "\\1w:val=\"en-US\" w:eastAsia=\"zh-CN\" w:bidi=\"ar-SA\"", s, perl = TRUE)
    }
    for (level in 1:2) {
      s <- sub(sprintf("(?s)(<w:style [^>]*w:styleId=\"FAOHeading%d\".*?)(</w:pPr>)", level),
               sprintf("\\1<w:outlineLvl w:val=\"%d\"/>\\2", level - 1), s, perl = TRUE)
    }
    writeLines(enc2utf8(s), styles_file, useBytes = TRUE)
  }
  x <- officer::set_doc_properties(x, title = meta$title, creator = meta$author)
  print(x, target = file)
  invisible(file)
}
