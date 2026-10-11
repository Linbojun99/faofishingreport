#' Assessment Report of an FAO Fishing Area
#'
#' This function runs the analyses of the package for one FAO major fishing area and writes a
#' Word report in Chinese or English: cover, title, abstract, data sources and methods, results
#' and conclusions with charts and tables, references and appendix tables. The text is written
#' from templates filled in with the results, as a first draft: check it, and complete the parts
#' marked in grey, such as other data sources, the discussion and other references.
#'
#' Chinese reports follow the format of project reports: A4 pages; body text in FangSong and
#' Times New Roman, size 12 (xiao si), 1.5 line spacing and a first-line indent of two
#' characters; title in bold FangSong, size 18; "abstract" in SimHei, size 15; section headings
#' in SimHei and subsection headings in KaiTi, size 14; captions in SimHei, size 12, numbered by
#' report (figure 1-1, table 1-1), below figures and above tables; three-line tables with text
#' in FangSong, size 10.5; numbered references in superscript square brackets, in the order of
#' citation (GB/T 7714). English reports have the same structure, in one font (Arial by
#' default), with author-date citations in APA style or in the style of FAO publications.
#'
#' The text templates are in `inst/extdata/report_text.csv` and the references in
#' `inst/extdata/report_references.csv` of the package source: edit them to change the wording.
#'
#' @param area The FAO major fishing area: its name, e.g. "Atlantic, Eastern Central", or its
#'        code, e.g. 34.
#' @param file The Word file to write. Default is `reports/FAO<code>_report_<lang>.docx` in the
#'        working directory.
#' @param lang The language of the report: "zh" (Chinese) or "en" (English). Default is NULL,
#'        the language set with `fao_settings()` ("both" gives a Chinese report).
#' @param title The report title. Default is generated from the area, e.g. "Assessment of
#'        Marine Capture Production in FAO Major Fishing Area 34 (Eastern Central Atlantic)".
#' @param report_no The number of the report, shown as "Report 1" (in Chinese reports with a
#'        Chinese numeral) and used in the figure and table numbers (1-1, 1-2, ...). Default is 1.
#' @param author,affiliation The author and affiliation on the cover and title page.
#' @param project The text at the top of the cover. Default is "Project Report".
#' @param date The date on the cover. Default is today.
#' @param font The font of the text in Latin script. Default is "Times New Roman" in Chinese
#'        reports and "Arial" in English reports.
#' @param font_zh The font of the Chinese body text. Default is "FangSong".
#' @param citation The citation style of English reports: "apa" (default) or "fao" (the style
#'        of FAO publications). Chinese reports use numbered citations (GB/T 7714).
#' @param top_n The number of main species and countries in the charts and tables. Default is 10.
#' @param data_version The FAO release to use, as in `load_fao_data()`. Default is "latest".
#' @param cmsy A logical value. If TRUE, the main species are also assessed with CMSY++
#'        ([cmsy_area()]), and the report ends with a section on the assessment, with the results
#'        of every species. Default is FALSE. The result of an earlier [cmsy_area()] run can also
#'        be given, to write the report without running CMSY++ again. CMSY++ needs JAGS: see
#'        [cmsy_setup()].
#' @param cmsy_top The number of species with the largest catch of the last ten years assessed
#'        with CMSY++. Default is `top_n`, the number of main species of the report.
#' @return Invisibly, a list of class `fao_report` with the path of the Word file (`file`) and
#'         the tables of the report as data frames: `main_species` and `main_countries`, and with
#'         CMSY++ `cmsy_reference_points` (r, k, MSY, Fmsy, Bmsy, B/Bmsy and F/Fmsy with 95%
#'         confidence limits), `cmsy_priors` (resilience, r, k and B/k priors, their sources and
#'         the links to FishBase or SeaLifeBase, the FishBase data, the CMSY++ code and the FAO
#'         data) and the full CMSY++ result `cmsy`. Assign it, e.g. `report <- fao_report(...)`,
#'         to look at the tables (`View(report$cmsy_priors)`). With `cmsy = TRUE`, all CMSY++
#'         results (runs, charts and tables) are kept in the folder `cmsy/` next to the report,
#'         with `cmsy_result.rds` to write the report again without running CMSY++
#'         (`cmsy = readRDS("cmsy/<area>/cmsy_result.rds")`).
#' @family report
#' @export
#' @examples
#' \dontrun{
#' fao_report(34)                                  # Chinese or English, from fao_settings()
#' fao_report("Atlantic, Eastern Central", lang = "zh", author = "Hongyu Lin")
#' fao_report(34, lang = "en", font = "Times New Roman", citation = "fao")
#'
#' # With a CMSY++ assessment of the 10 main species (a few minutes)
#' report <- fao_report(34, lang = "zh", cmsy = TRUE)
#' report                                         # the tables it returns
#' View(report$cmsy_reference_points)             # r, k, MSY, B/Bmsy, ... with 95% limits
#' View(report$cmsy_priors)                       # priors, sources and links
#' fao_report(34, lang = "zh", top_n = 15, cmsy = TRUE, cmsy_top = 5)
#'
#' # Reuse CMSY++ results to rewrite the report
#' area34 <- cmsy_area("Atlantic, Eastern Central", top = 10, lang = "zh")
#' fao_report(34, lang = "zh", cmsy = area34)
#' }
fao_report <- function(area, file = NULL, lang = NULL, title = NULL, report_no = 1,
                       author = NULL, affiliation = NULL, project = NULL, date = Sys.Date(),
                       font = NULL, font_zh = "FangSong", citation = c("apa", "fao"),
                       top_n = 10, data_version = "latest", cmsy = FALSE, cmsy_top = top_n) {
  fao_check_report_packages()
  lang <- fao_lang(lang)
  if (lang == "both") lang <- "zh"
  citation <- if (lang == "zh") "numeric" else match.arg(citation)
  if (is.null(font)) font <- if (lang == "zh") "Times New Roman" else "Arial"
  area <- fao_report_area(area, lang)
  if (is.null(file)) file <- file.path("reports", paste0("FAO", area$code, "_report_", lang, ".docx"))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  file <- file.path(normalizePath(dirname(file)), basename(file))

  message("Analysing ", area$name, " ...")
  results <- fao_report_analyses(area, lang, top_n, data_version)
  info <- fao_data_info(data_version)
  values <- fao_report_values(results, area, lang, top_n)
  values$release <- info$release
  values$release_date <- fao_report_date(info$release_date, lang)
  values$download_date <- fao_report_date(info$download_date, lang)
  values$url <- info$url

  cmsy_result <- if (inherits(cmsy, "cmsy_result")) cmsy else if (isTRUE(cmsy)) {
    fao_report_cmsy_run(results$area_data, cmsy_top, lang, file.path(dirname(file), "cmsy"))
  }
  values <- c(values, fao_report_cmsy_values(cmsy_result, lang, values))
  # values given in the call (e.g. the years of one stock) come before the report values
  text <- function(.key, ...) fao_report_text(.key, lang, c(list(...), values))

  meta <- list(
    title = if (is.null(title)) text("title") else title,
    report_no = text("report_no", no = fao_report_number(report_no, lang)),
    author = if (is.null(author)) text("author_default") else author,
    affiliation = affiliation,
    project = if (is.null(project)) text("project_default") else project,
    date = format(as.Date(date), "%Y-%m-%d")
  )
  message("Writing ", file, " ...")
  doc <- fao_report_new(lang, font, font_zh, citation, report_no, fao_report_references(info, date, cmsy_result))
  doc <- fao_report_body(doc, meta, results, values, text, cmsy_result)
  fao_report_save(doc, file, meta)
  message("Report saved: ", file)

  # The tables of the report as data frames, to look at or reuse in R
  species <- values$species_table
  countries <- values$country_table
  out <- structure(list(
    file = file,
    main_species = data.frame(species = values$species_names$plain, species_en = species$name_en,
                              scientific_name = values$species_names$scientific, mean_catch_t = species$mean,
                              share_pct = species$share, peak_year = species$peak_year,
                              pct_of_peak = species$pct_of_peak, trend_10_years = species$trend,
                              stringsAsFactors = FALSE),
    main_countries = data.frame(country = values$country_names, country_en = countries$name_en,
                                mean_catch_t = countries$mean, share_pct = countries$share,
                                peak_year = countries$peak_year, pct_of_peak = countries$pct_of_peak,
                                trend_10_years = countries$trend, stringsAsFactors = FALSE),
    cmsy = cmsy_result,
    cmsy_reference_points = values$cmsy_reference_points,
    cmsy_priors = values$cmsy_priors
  ), class = "fao_report")
  invisible(out)
}

#' @export
print.fao_report <- function(x, ...) {
  cat("FAO report: ", x$file, "\n", sep = "")
  tables <- c(main_species = "main species of the last five years",
              main_countries = "main fishing countries of the last five years",
              cmsy_reference_points = "CMSY++ biological reference points with 95% confidence limits",
              cmsy_priors = "CMSY++ priors, their sources and links")
  for (name in names(tables)) {
    if (!is.null(x[[name]])) cat(sprintf("  $%-22s %s (%d rows)\n", name, tables[[name]], nrow(x[[name]])))
  }
  if (!is.null(x$cmsy)) cat("  $cmsy                   CMSY++ result (folder ", x$cmsy$dir, ")\n", sep = "")
  invisible(x)
}

# Runs CMSY++ on the main species of the area, keeping its results in `out_dir`
fao_report_cmsy_run <- function(area_data, top, lang, out_dir) {
  system <- cmsy_system(silent = TRUE)
  if (is.na(system$jags) || !all(system$packages$ok)) {
    stop("CMSY++ needs JAGS and the R packages ", paste(system$packages$package[!system$packages$ok], collapse = ", "),
         ": run cmsy_setup() once, or use cmsy = FALSE.", call. = FALSE)
  }
  message("  CMSY++ for the ", top, " main species (a few minutes)")
  old_options <- options(faofishingreport.dpi = 300, faofishingreport.pptx = FALSE)
  on.exit(options(old_options), add = TRUE)
  result <- suppressWarnings(suppressMessages(cmsy_area(
    area_data, top = top, lang = lang, out_dir = out_dir, forecast = FALSE,
    charts = c("management", "kobe_all", "b_bmsy_all"), silent = TRUE)))
  # Saved to write the report again without running CMSY++: cmsy = readRDS(<file>)
  saved <- file.path(result$dir, "cmsy_result.rds")
  saveRDS(result, saved)
  message("  CMSY++ results: ", result$dir, " (reuse them with cmsy = readRDS(\"", saved, "\"))")
  result
}

# Values of the CMSY++ text: counts of the stocks, and the text of every species
fao_report_cmsy_values <- function(result, lang, values) {
  if (is.null(result)) return(list(abstract_cmsy = "", has_cmsy = FALSE))
  text <- function(.key, ...) fao_report_text(.key, lang, list(...))
  list_text <- function(x) fao_report_list(x, lang)
  s <- result$summary
  ok <- s$run_status %in% "ok"
  names <- fao_report_species_names(s$species_en, lang, fao_group_table()$species)
  b <- s$B_Bmsy
  f <- s$F_Fmsy
  overfished <- ok & b < 0.8
  missing <- c(as.character(result$input$excluded$species_name), names$plain[!ok])
  counts <- list(n_ok = sum(ok), n_overfished = sum(overfished), n_msf = sum(ok & b >= 0.8),
                 n_green = sum(ok & b >= 1 & f <= 1, na.rm = TRUE), n_red = sum(ok & b < 1 & f > 1, na.rm = TRUE))
  number <- function(x) sprintf("%.2f", x)
  catch <- function(x) fao_report_catch(x, lang)
  status <- function(i) {
    label <- cmsy_label(if (b[i] >= 0.8) "status_msf" else "status_overfished", lang)
    if (lang == "zh") label else tolower(label)
  }
  species <- lapply(seq_len(nrow(s)), function(i) {
    if (!ok[i]) return(list(ok = FALSE, values = list(species = names$full[i], message = s$message[i])))
    list(ok = TRUE, values = list(
      species = names$full[i], first_year = s$first_year[i], last_year = s$last_year[i],
      r = number(s$r[i]), r_lcl = number(s$r_lcl[i]), r_ucl = number(s$r_ucl[i]),
      k = catch(s$k[i]), k_range = fao_report_catch_range(s$k_lcl[i], s$k_ucl[i], lang),
      msy = catch(s$MSY[i]), msy_range = fao_report_catch_range(s$MSY_lcl[i], s$MSY_ucl[i], lang),
      bbmsy = number(b[i]), bbmsy_lcl = number(s$B_Bmsy_lcl[i]), bbmsy_ucl = number(s$B_Bmsy_ucl[i]),
      ffmsy = number(f[i]), ffmsy_lcl = number(s$F_Fmsy_lcl[i]), ffmsy_ucl = number(s$F_Fmsy_ucl[i]),
      catch_last = catch(s$catch_last[i]), catch_msy = fao_report_pct(100 * s$catch_last[i] / s$MSY[i]),
      status = status(i), p_green = fao_report_pct(100 * s$p_green[i]), p_orange = fao_report_pct(100 * s$p_orange[i]),
      p_yellow = fao_report_pct(100 * s$p_yellow[i]), p_red = fao_report_pct(100 * s$p_red[i])
    ))
  })
  table <- data.frame(
    species = names$plain, scientific = names$marked, years = paste0(s$first_year, "\u2013", s$last_year),
    r = ifelse(ok, number(s$r), "-"),
    k = ifelse(ok, sprintf("%.1f", s$k / if (lang == "zh") 1e4 else 1e3), "-"),
    msy = ifelse(ok, sprintf("%.1f", s$MSY / if (lang == "zh") 1e4 else 1e3), "-"),
    bbmsy = ifelse(ok, number(b), "-"), ffmsy = ifelse(ok, number(f), "-"),
    status = ifelse(ok, vapply(seq_len(nrow(s)), function(i) if (ok[i]) cmsy_label(if (b[i] >= 0.8) "status_msf" else "status_overfished", lang) else "-", character(1)), "-"),
    stringsAsFactors = FALSE
  )
  word <- function(key) fao_report_text(key, lang)
  names(table) <- c(word("col_species"), word("col_scientific"), word("col_years"), word("col_r"), word("col_k"),
                    word("col_msy"), word("col_bbmsy"), word("col_ffmsy"), word("col_status"))
  tables <- fao_report_cmsy_tables(result, lang, names)
  c(counts, tables, list(
    has_cmsy = TRUE, cmsy_top = nrow(s) + nrow(result$input$excluded),
    not_assessed = if (length(missing) > 0) text("cmsy_not_assessed", list = list_text(missing)) else "",
    overfished_list = if (any(overfished)) text("cmsy_overfished_list", list = list_text(names$plain[overfished])) else "",
    abstract_cmsy = do.call(text, c(list("abstract_cmsy"), counts)),
    cmsy_species = species, cmsy_table = table,
    cmsy_charts = list(
      kobe = file.path(result$dir, "figures", "summary", "kobe_all.png"),
      bbmsy = file.path(result$dir, "figures", "summary", "b_bmsy_all.png"),
      species = file.path(result$dir, "figures", cmsy_safe_name(s$Stock), "management.png"))
  ))
}

# Biological reference points, priors and their sources: data frames for R (with all values
# and links) and tables for the appendix of the report
fao_report_cmsy_tables <- function(result, lang, names) {
  s <- result$summary
  stocks <- result$input$stocks[match(s$Stock, result$input$stocks$Stock), ]
  ok <- s$run_status %in% "ok"
  word <- function(key) fao_report_text(key, lang)
  scale <- if (lang == "zh") 1e4 else 1e3
  ci <- function(x, low, high, digits = 2, divide = 1) {
    f <- function(v) formatC(v / divide, format = "f", digits = digits)
    ifelse(ok, paste0(f(x), " (", f(low), "\u2013", f(high), ")"), "-")
  }
  range <- function(low, high, digits = 2, divide = 1) {
    f <- function(v) formatC(v / divide, format = "f", digits = digits)
    ifelse(is.na(low) | is.na(high), "-", paste0(f(low), "\u2013", f(high)))
  }
  bk_source <- function(x) {
    x <- ifelse(x %in% "user", word("source_user"), ifelse(is.na(x), "-", word("source_network")))
    x
  }
  resilience <- ifelse(is.na(stocks$Resilience), "-", vapply(stocks$Resilience, function(r) {
    tryCatch(fao_report_text(paste0("resilience_", r), lang), error = function(e) r)
  }, character(1)))
  r_source <- fao_report_cmsy_source(stocks$r_source, lang)

  # Data frames for R: one row per stock, with every value and link
  reference_points <- data.frame(
    Stock = s$Stock, species = names$plain, species_en = s$species_en, scientific_name = s$scientific_name,
    first_year = s$first_year, last_year = s$last_year, run_status = s$run_status,
    r = s$r, r_lcl = s$r_lcl, r_ucl = s$r_ucl, k_t = s$k, k_lcl_t = s$k_lcl, k_ucl_t = s$k_ucl,
    MSY_t = s$MSY, MSY_lcl_t = s$MSY_lcl, MSY_ucl_t = s$MSY_ucl, Fmsy = s$Fmsy, Fmsy_lcl = s$Fmsy_lcl,
    Fmsy_ucl = s$Fmsy_ucl, Bmsy_t = s$Bmsy, Bmsy_lcl_t = s$Bmsy_lcl, Bmsy_ucl_t = s$Bmsy_ucl,
    B_Bmsy = s$B_Bmsy, B_Bmsy_lcl = s$B_Bmsy_lcl, B_Bmsy_ucl = s$B_Bmsy_ucl,
    F_Fmsy = s$F_Fmsy, F_Fmsy_lcl = s$F_Fmsy_lcl, F_Fmsy_ucl = s$F_Fmsy_ucl, catch_last_t = s$catch_last,
    p_green = s$p_green, p_orange = s$p_orange, p_yellow = s$p_yellow, p_red = s$p_red, status = s$status,
    stringsAsFactors = FALSE
  )
  priors <- data.frame(
    Stock = s$Stock, species = names$plain, species_en = s$species_en, scientific_name = s$scientific_name,
    resilience = stocks$Resilience, resilience_source = stocks$resilience_source,
    r_prior_low = s$r_prior_low, r_prior_high = s$r_prior_high, r_prior_source = stocks$r_source,
    k_prior_low_t = s$k_prior_low, k_prior_high_t = s$k_prior_high, MSY_prior_t = s$MSY_prior,
    start_bk_prior_low = s$start_bk_prior_low, start_bk_prior_high = s$start_bk_prior_high,
    start_bk_prior_source = s$start_bk_prior_source, int_year = s$int_year,
    int_bk_prior_low = s$int_bk_prior_low, int_bk_prior_high = s$int_bk_prior_high,
    int_bk_prior_source = s$int_bk_prior_source, end_bk_prior_low = s$end_bk_prior_low,
    end_bk_prior_high = s$end_bk_prior_high, end_bk_prior_source = s$end_bk_prior_source,
    species_page_url = stocks$url, fishbase_data_url = stocks$data_url, cmsy_guide_url = stocks$rule_url,
    fao_data_url = stocks$fao_data_url, stringsAsFactors = FALSE
  )

  # Tables of the appendix
  points_table <- data.frame(names$plain, ci(s$r, s$r_lcl, s$r_ucl),
                             ci(s$k, s$k_lcl, s$k_ucl, 1, scale), ci(s$MSY, s$MSY_lcl, s$MSY_ucl, 1, scale),
                             ci(s$Fmsy, s$Fmsy_lcl, s$Fmsy_ucl), ci(s$Bmsy, s$Bmsy_lcl, s$Bmsy_ucl, 1, scale),
                             ci(s$B_Bmsy, s$B_Bmsy_lcl, s$B_Bmsy_ucl), ci(s$F_Fmsy, s$F_Fmsy_lcl, s$F_Fmsy_ucl),
                             stringsAsFactors = FALSE)
  names(points_table) <- c(word("col_species"), word("col_r"), word("col_k"), word("col_msy"), word("col_fmsy"),
                           word("col_bmsy"), word("col_bbmsy"), word("col_ffmsy"))
  priors_table <- data.frame(names$plain, resilience, range(s$r_prior_low, s$r_prior_high, 3),
                             range(s$k_prior_low, s$k_prior_high, 1, scale),
                             range(s$start_bk_prior_low, s$start_bk_prior_high),
                             ifelse(is.na(s$int_year), "-", paste0(s$int_year, ": ", range(s$int_bk_prior_low, s$int_bk_prior_high))),
                             range(s$end_bk_prior_low, s$end_bk_prior_high), stringsAsFactors = FALSE)
  names(priors_table) <- c(word("col_species"), word("col_resilience"), word("col_r_prior"), word("col_k_prior"),
                           word("col_bk_start"), word("col_bk_int"), word("col_bk_end"))
  sources_table <- data.frame(names$plain, r_source, bk_source(s$end_bk_prior_source),
                              ifelse(is.na(stocks$url), "-", stocks$url), stringsAsFactors = FALSE)
  names(sources_table) <- c(word("col_species"), word("col_r_source"), word("col_bk_source"), word("col_page"))
  first <- function(x) { x <- unique(stats::na.omit(x)); if (length(x) > 0) x[1] else "-" }
  list(cmsy_reference_points = reference_points, cmsy_priors = priors,
       cmsy_appendix = list(points = points_table, priors = priors_table, sources = sources_table),
       fishbase_data = first(stocks$data_url), cmsy_code = first(stocks$rule_url), fao_data = first(stocks$fao_data_url))
}

# The source of the r prior in the report language
fao_report_cmsy_source <- function(x, lang) {
  if (lang != "zh") return(ifelse(is.na(x), "-", x))
  x <- sub("^(\\S+) prior r, 95% CL \\(Based on ([0-9]+) full stock assessments?\\.?\\)$",
           "\\1 \u5148\u9a8c r\uff0895% \u7f6e\u4fe1\u9650\uff0c\u57fa\u4e8e \\2 \u4e2a\u5b8c\u6574\u8d44\u6e90\u8bc4\u4f30\uff09", x)
  x <- sub("^(\\S+) prior r, 95% CL \\(Based on ([0-9]+) data-limited stock assessments?\\.?\\)$",
           "\\1 \u5148\u9a8c r\uff0895% \u7f6e\u4fe1\u9650\uff0c\u57fa\u4e8e \\2 \u4e2a\u6570\u636e\u6709\u9650\u7684\u8d44\u6e90\u8bc4\u4f30\uff09", x)
  x <- sub("^(\\S+) resilience \\(([^)]*)\\), CMSY\\+\\+ user guide Table 2$",
           "\\1 \u6062\u590d\u529b\uff08\\2\uff09\uff0cCMSY++ \u7528\u6237\u6307\u5357\u8868 2", x)
  ifelse(is.na(x), "-", x)
}

# officer writes the Word file and xml2 edits it; both are suggested packages
fao_check_report_packages <- function() {
  missing <- c("officer", "xml2")[!vapply(c("officer", "xml2"), requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Reports need the ", paste(missing, collapse = " and "), " package",
         if (length(missing) > 1) "s", ": install.packages(c(",
         paste0("\"", missing, "\"", collapse = ", "), "))", call. = FALSE)
  }
  invisible(TRUE)
}

# The area from its name or code, with its label in the report language
fao_report_area <- function(area, lang) {
  areas <- fao_group_table()$area
  if (is.numeric(area) || grepl("^[0-9]+$", area)) {
    row <- match(sprintf("%02d", as.integer(area)), areas$area_code)
  } else {
    row <- match(area, areas$name_en)
  }
  if (is.na(row)) {
    marine <- areas[areas$inland_marine_en == "Marine areas", ]
    stop("Unknown FAO major fishing area: ", area, ". Marine areas: ",
         paste0(as.integer(marine$area_code), " (", marine$name_en, ")", collapse = ", "),
         call. = FALSE)
  }
  name <- areas$name_en[row]
  code <- as.character(as.integer(areas$area_code[row]))
  local_name <- if (lang == "zh") fao_translate(name, "area", "zh") else name
  list(name = name, code = code, marine = areas$inland_marine_en[row] == "Marine areas",
       label = fao_report_text("area_label", lang, list(code = code, name = local_name)))
}

# Report text from inst/extdata/report_text.csv with {placeholders} filled in. Citations
# ({cite:key}) and the marks for italics (*...*), superscripts (^...^) and subscripts (~...~)
# are kept, and turned into formatted text when the paragraph is written.
fao_report_text <- function(key, lang, values = list()) {
  texts <- fao_extdata_csv("report_text.csv")
  text <- texts[[lang]][match(key, texts$key)]
  if (is.na(text)) stop("Unknown report text: ", key, call. = FALSE)
  names <- unique(regmatches(text, gregexpr("\\{[a-z0-9_]+\\}", text))[[1]])
  for (name in names) {
    value <- values[[substr(name, 2, nchar(name) - 1)]]
    if (is.null(value)) stop("No value for ", name, " in report text ", key, call. = FALSE)
    text <- gsub(name, paste(value, collapse = ""), text, fixed = TRUE)
  }
  text
}

# Chinese numerals for report numbers up to 99
fao_report_number <- function(n, lang) {
  if (lang != "zh" || !is.numeric(n) || n < 1 || n > 99 || n != round(n)) return(as.character(n))
  digits <- c("\u4e00", "\u4e8c", "\u4e09", "\u56db", "\u4e94", "\u516d", "\u4e03", "\u516b", "\u4e5d")
  tens <- n %/% 10
  ones <- n %% 10
  paste0(if (tens > 1) digits[tens], if (tens > 0) "\u5341", if (ones > 0) digits[ones])
}

# Formatting of numbers and lists in the report text
fao_report_pct <- function(x) sprintf("%.1f", x)

fao_report_catch <- function(x, lang) {
  if (lang == "zh") {
    if (x >= 1e4) paste0(sprintf("%.1f", x / 1e4), " \u4e07 t") else paste0(round(x), " t")
  } else if (x >= 1e6) {
    sprintf("%.2f million t", x / 1e6)
  } else if (x >= 1e3) {
    sprintf("%.1f thousand t", x / 1e3)
  } else {
    paste0(round(x), " t")
  }
}

# A range of catches in one unit, chosen from the upper limit: "561.8-991.5 wan t"
fao_report_catch_range <- function(low, high, lang) {
  unit <- fao_report_catch(high, lang)
  value <- sub("^([0-9.]+).*$", "\\1", unit)
  scale <- high / as.numeric(value)
  digits <- nchar(sub("^[0-9]*[.]?", "", value))
  paste0(formatC(low / scale, format = "f", digits = digits), "\u2013", unit)
}

fao_report_list <- function(x, lang) {
  n <- length(x)
  if (n <= 1) return(paste(x, collapse = ""))
  if (lang == "zh") {
    paste0(paste(x[-n], collapse = "\u3001"), "\u548c", x[n])
  } else {
    paste0(paste(x[-n], collapse = ", "), " and ", x[n])
  }
}

fao_report_date <- function(date, lang) {
  if (is.na(date)) return("-")
  date <- as.Date(date)
  day <- as.integer(format(date, "%d"))
  month <- as.integer(format(date, "%m"))
  if (lang == "zh") {
    paste0(format(date, "%Y"), " \u5e74 ", month, " \u6708 ", day, " \u65e5")
  } else {
    paste(day, month.name[month], format(date, "%Y"))
  }
}

fao_report_p <- function(p, lang) {
  space <- if (lang == "zh") "" else " "
  if (is.na(p)) return(paste0("*P*", space, "=", space, "NA"))
  if (p < 0.001) paste0("*P*", space, "<", space, "0.001") else paste0("*P*", space, "=", space, sprintf("%.3f", p))
}

# Runs the analyses in a temporary folder (charts at 300 dpi, no PowerPoint) and keeps their
# results and the charts each one saved
fao_report_analyses <- function(area, lang, top_n, data_version) {
  work <- tempfile("fao_report_")
  dir.create(work)
  old_wd <- setwd(work)
  on.exit(setwd(old_wd), add = TRUE)
  old_options <- options(faofishingreport.dpi = 300, faofishingreport.pptx = FALSE)
  on.exit(options(old_options), add = TRUE)

  charts <- function() {
    files <- list.files(file.path(work, "figures", "png"), pattern = "[.]png$",
                        recursive = TRUE, full.names = TRUE)
    stats::setNames(file.mtime(files), files)
  }
  # Each step keeps the charts it saved: new files, or files written again
  run <- function(step, expr) {
    message("  ", step)
    before <- charts()
    value <- NULL
    utils::capture.output(value <- suppressWarnings(suppressMessages(expr)))
    after <- charts()
    saved <- after[is.na(before[names(after)]) | after > before[names(after)]]
    list(value = value, files = names(sort(saved)))
  }

  fa <- run("total catch", fishing_area_data(area$name, lang = lang, data_version = data_version))
  area_data <- fa$value$area_data
  out <- list(
    area_data = area_data,
    total = fa,
    species = run("main species", fishing_area_species(area_data, rank_range = c(1, top_n), table = FALSE, lang = lang)),
    countries = run("main countries", fishing_area_country(area_data, rank_range = c(1, top_n), table = FALSE, lang = lang)),
    groups = run("species groups", species_group_composition(area_data, table = FALSE, lang = lang)),
    status = run("stock status", stock_status(area_data, table = FALSE, lang = lang)),
    quality = run("data quality", data_quality(area_data, table = FALSE, lang = lang)),
    diversity = run("diversity", diversity_concentration(area_data, table = FALSE, lang = lang)),
    fleets = run("coastal and distant-water fleets", coastal_distant(area_data, table = FALSE, lang = lang)),
    species_trend = run("trends", peak_trend(area_data, by = "species", top_n = Inf, plot = FALSE, table = FALSE, lang = "en"))$value,
    country_trend = suppressMessages(peak_trend(area_data, by = "country", top_n = Inf, plot = FALSE, table = FALSE, lang = "en")),
    world = suppressMessages(load_fao_data(data_version))
  )
  pick <- function(step, pattern = NULL, n = 1) {
    files <- out[[step]]$files
    if (!is.null(pattern)) files <- files[grepl(pattern, files)]
    if (length(files) >= n) files[n] else NA_character_
  }
  out$charts <- list(
    total = pick("total", "fishing_trend"), share = pick("total", "fishing_percentage"),
    map = pick("total", "area_country_map"),
    species = pick("species", "area_species[.]png$"), species_wrap = pick("species", "area_species_wrap"),
    countries = pick("countries", "area_country[.]png$"), countries_wrap = pick("countries", "area_country_wrap"),
    groups = pick("groups", "_share[.]png$"),
    status = pick("status", n = 1), status_catch = pick("status", n = 2),
    quality = pick("quality", n = 1), quality_taxon = pick("quality", n = 2),
    diversity = pick("diversity"), fleets = pick("fleets", n = 1)
  )
  out
}

# Values of the report text, computed from the results
fao_report_values <- function(results, area, lang, top_n) {
  data <- results$area_data
  groups <- fao_group_table()
  first_year <- min(data$Year)
  last_year <- max(data$Year)
  y5 <- last_year - 4
  catch_text <- function(x) fao_report_catch(x, lang)
  list_text <- function(x) fao_report_list(x, lang)
  text <- function(.key, ...) fao_report_text(.key, lang, list(...))
  word <- function(key) fao_report_text(key, lang)

  # Total catch, peak, trend and stages
  total <- tapply(data$Production, data$Year, sum)
  years <- as.integer(names(total))
  total_test <- suppressWarnings(stats::cor.test(years, as.numeric(total), method = "kendall", exact = FALSE))
  overall <- if (total_test$p.value < 0.05) ifelse(total_test$estimate > 0, "overall_up", "overall_down") else "overall_none"
  recent <- results$species_trend[1, ]
  coefs <- results$total$value$coefs_data
  stages <- if (!is.null(coefs) && nrow(coefs) > 1) {
    data.frame(from = coefs$start_year, to = c(coefs$start_year[-1] - 1, last_year), mean = coefs$intercept)
  } else {
    NULL
  }

  # Share of the world catch of marine areas (or inland waters for an inland area)
  same_water <- groups$area$name_en[groups$area$inland_marine_en ==
                                      groups$area$inland_marine_en[match(area$name, groups$area$name_en)]]
  world <- results$world[results$world$FAO.major.fishing.area..Name. %in% same_water, ]
  world_total <- tapply(world$Production, world$Year, sum)
  share <- 100 * as.numeric(total) / as.numeric(world_total[names(total)])

  # Main species and countries of the last five years
  window <- data[data$Year >= y5, ]
  top_table <- function(column, trend_table) {
    sums <- sort(tapply(window$Production, window[[column]], sum), decreasing = TRUE)
    sums <- sums[sums > 0]
    top <- utils::head(sums, top_n)
    trend <- trend_table[match(names(top), trend_table$name_en), ]
    data.frame(name_en = names(top), mean = as.numeric(top) / 5,
               share = 100 * as.numeric(top) / sum(window$Production),
               peak_year = trend$peak_year, pct_of_peak = trend$pct_of_peak, trend = trend$trend,
               stringsAsFactors = FALSE)
  }
  species <- top_table("ASFIS.species..Name.", results$species_trend)
  countries <- top_table("Country..Name.", results$country_trend)
  species_names <- fao_report_species_names(species$name_en, lang, groups$species)
  country_names <- if (lang == "zh") fao_translate(countries$name_en, "country", "zh") else countries$name_en
  trend_parts <- function(kind, trend, names) {
    up <- names[trend %in% "up"]
    down <- names[trend %in% "down"]
    list(up_part = if (length(up) > 0) text(paste0(kind, "_up"), n = length(up), list = list_text(up)) else word(paste0(kind, "_up_none")),
         down_part = if (length(down) > 0) text(paste0(kind, "_down"), n = length(down), list = list_text(down)) else word(paste0(kind, "_down_none")))
  }
  species_trends <- trend_parts("species", species$trend, species_names$plain)
  country_trends <- trend_parts("countries", countries$trend, country_names)

  # ISSCAAP groups of the last year
  last <- data[data$Year == last_year, ]
  group_of <- groups$species[[paste0("isscaap_", if (lang == "zh") "zh" else "en")]][
    match(last$ASFIS.species..Name., groups$species$name_en)]
  group_sums <- sort(tapply(last$Production, group_of, sum), decreasing = TRUE)
  group_share <- 100 * group_sums / sum(last$Production)
  group_value <- function(i, what) {
    if (length(group_sums) < i) return("-")
    if (what == "name") names(group_sums)[i] else fao_report_pct(group_share[i])
  }

  # Stock status of the last year
  status <- results$status$value
  status_last <- status$yearly[status$yearly$Year == last_year, ]
  status_last <- status_last[order(as.integer(status_last$status_label)), ]
  oc <- as.integer(status_last$status_label) %in% c(3, 4)
  status_pcts <- function(column) list_text(paste0(fao_report_pct(status_last[[column]]), "%"))

  # Data quality of the last year
  level <- groups$species$taxon_level[match(last$ASFIS.species..Name., groups$species$name_en)]
  pct_of_last <- function(keep) 100 * sum(last$Production[keep], na.rm = TRUE) / sum(last$Production)

  # Diversity and concentration
  diversity <- results$diversity$value
  first_div <- diversity[diversity$Year == min(diversity$Year), ]
  last_div <- diversity[diversity$Year == last_year, ]
  concentration <- function(hhi) word(if (hhi < 1500) "conc_low" else if (hhi < 2500) "conc_moderate" else "conc_high")

  # Coastal countries and distant-water fleets
  fleets <- results$fleets$value
  fleet_levels <- levels(fleets$yearly$category)
  fleet_year <- function(category) tapply(fleets$yearly$Tonnes[fleets$yearly$category == category],
                                          fleets$yearly$Year[fleets$yearly$category == category], sum)
  fleet_total <- tapply(fleets$yearly$Tonnes, fleets$yearly$Year, sum)
  coastal_share <- 100 * fleet_year(fleet_levels[1]) / fleet_total
  distant_share <- 100 * fleet_year(fleet_levels[2]) / fleet_total
  distant_peak <- which.max(distant_share)
  distant <- fleets$distant[fleets$distant$Year >= y5 & as.character(fleets$distant$category) != fao_label("other_distant", lang), ]
  distant_sums <- utils::head(sort(tapply(distant$Tonnes, as.character(distant$category), sum), decreasing = TRUE), 3)

  down_species <- species_names$plain[species$trend %in% "down"]
  values <- list(
    area = area$label,
    first_year = first_year, last_year = last_year, y5 = y5, y10 = last_year - 9, top_n = top_n,
    n_countries = length(unique(data$Country..Name.[data$Production > 0])),
    n_species = length(unique(data$ASFIS.species..Name.[data$Production > 0])),
    total_first = catch_text(total[1]), total_last = catch_text(total[length(total)]),
    peak_year = years[which.max(total)], peak_catch = catch_text(max(total)),
    pct_peak = fao_report_pct(100 * total[length(total)] / max(total)),
    overall = word(overall),
    growth10 = sprintf("%.1f", recent$growth_pct_per_year),
    trend10 = text(paste0("trend10_", recent$trend), p = fao_report_p(recent$p_value, lang)),
    n_stages = if (is.null(stages)) 1 else nrow(stages),
    stage_years = if (is.null(stages)) paste0(first_year, "\u2013", last_year) else
      list_text(paste0(stages$from, "\u2013", stages$to, if (lang == "zh") " \u5e74")),
    break_years = if (is.null(stages)) "" else list_text(stages$from[-1]),
    stages = if (is.null(stages)) "" else paste(vapply(seq_len(nrow(stages)), function(i) {
      text("stage", from = stages$from[i], to = stages$to[i], mean = catch_text(stages$mean[i]))
    }, character(1)), collapse = if (lang == "zh") "\uff1b" else "; "),
    has_stages = !is.null(stages),
    share_last = fao_report_pct(share[length(share)]), share_mean = fao_report_pct(mean(share, na.rm = TRUE)),
    top_species = list_text(species_names$full[seq_len(min(5, nrow(species)))]),
    top5_species_share = fao_report_pct(sum(utils::head(species$share, 5))),
    top1_species_plain = species_names$plain[1], top1_species_mean = catch_text(species$mean[1]),
    top1_species_share = fao_report_pct(species$share[1]),
    species_up_part = species_trends$up_part, species_down_part = species_trends$down_part,
    n_countries_last = sum(tapply(last$Production, last$Country..Name., sum) > 0),
    top_countries = list_text(utils::head(country_names, 5)),
    top5_countries_share = fao_report_pct(sum(utils::head(countries$share, 5))),
    top1_country = country_names[1], top1_country_mean = catch_text(countries$mean[1]),
    top1_country_share = fao_report_pct(countries$share[1]),
    countries_up_part = country_trends$up_part, countries_down_part = country_trends$down_part,
    g1 = group_value(1, "name"), g1_share = group_value(1, "share"),
    g2 = group_value(2, "name"), g2_share = group_value(2, "share"),
    g3 = group_value(3, "name"), g3_share = group_value(3, "share"),
    n_stocks = nrow(status$stocks),
    stock_pcts = status_pcts("stocks_pct"), catch_pcts = status_pcts("catch_pct"),
    oc_pct = fao_report_pct(sum(status_last$stocks_pct[oc])),
    oc_catch_pct = fao_report_pct(sum(status_last$catch_pct[oc])),
    official_pct = fao_report_pct(pct_of_last(last$Production.Type == "actual")),
    est_pct = fao_report_pct(pct_of_last(last$Production.Type == "estimate")),
    species_pct = fao_report_pct(pct_of_last(level %in% "species")),
    nonspecies_pct = fao_report_pct(pct_of_last(!level %in% "species")),
    h_first = sprintf("%.2f", first_div$shannon_species), h_last = sprintf("%.2f", last_div$shannon_species),
    hhi_species = round(last_div$hhi_species), hhi_countries = round(last_div$hhi_countries),
    conc_species = concentration(last_div$hhi_species), conc_countries = concentration(last_div$hhi_countries),
    top5_last = fao_report_pct(last_div$top5_species_pct),
    coastal_pct = fao_report_pct(coastal_share[length(coastal_share)]),
    distant_pct = fao_report_pct(distant_share[length(distant_share)]),
    distant_peak_year = names(distant_share)[distant_peak],
    distant_peak_pct = fao_report_pct(distant_share[distant_peak]),
    distant_change = word(if (distant_share[length(distant_share)] < 0.8 * distant_share[distant_peak]) "distant_down" else "distant_same"),
    top_distant = list_text(names(distant_sums)),
    status_note = if (length(down_species) > 0) text("concl_3_down", list = list_text(utils::head(down_species, 3))) else word("concl_3_none")
  )
  values$species_table <- species
  values$species_names <- species_names
  values$country_table <- countries
  values$country_names <- country_names
  values
}

# Species names for the report: the name, and the name with the scientific name in brackets
# (in italics for species and genera)
fao_report_species_names <- function(en, lang, species_groups) {
  asfis <- new.env()
  load(system.file("extdata", "ASFIS_sp.rda", package = "faofishingreport"), envir = asfis)
  scientific <- asfis$ASFIS_sp$Scientific_Name[match(en, asfis$ASFIS_sp$English_name)]
  level <- species_groups$taxon_level[match(en, species_groups$name_en)]
  genus <- sub(" spp[.]?$", "", scientific)
  marked <- ifelse(is.na(scientific), NA,
                   ifelse(level %in% "species", paste0("*", scientific, "*"),
                          ifelse(level %in% "genus", paste0("*", genus, "* spp."), scientific)))
  plain_scientific <- ifelse(level %in% "genus", paste0(genus, " spp."), scientific)
  plain <- if (lang == "zh") fao_translate(en, "species", "zh") else en
  full <- ifelse(is.na(marked), plain,
                 if (lang == "zh") paste0(plain, "\uff08", marked, "\uff09") else paste0(plain, " (", marked, ")"))
  list(plain = plain, full = full, scientific = ifelse(is.na(plain_scientific), "", plain_scientific),
       marked = ifelse(is.na(marked), "", marked))
}

# References of the report: the FAO data used, R and the package (made from the data and the
# session), and the references in inst/extdata/report_references.csv
fao_report_references <- function(info, date, cmsy_result = NULL) {
  accessed <- format(as.Date(info$download_date), "%Y-%m-%d")
  today <- format(as.Date(date), "%Y-%m-%d")
  release_year <- substr(info$release_date, 1, 4)
  refs <- list(
    fao_capture = list(type = "DB", author = "FAO", year = release_year,
                       title = paste0("FishStat: Global capture production ", info$years[1], "\u2013", info$years[2]),
                       version = paste("release", info$release), place = "Rome", publisher = "FAO", accessed = accessed,
                       url = info$url, fao_url = "https://www.fao.org/fishery/statistics/software/fishstatj/en"),
    r_core = list(type = "CP", author = "R Core Team", year = R.version$year,
                  title = "R: A language and environment for statistical computing",
                  place = "Vienna", publisher = "R Foundation for Statistical Computing",
                  accessed = today, url = "https://www.R-project.org/"),
    faofishingreport = list(type = "CP", author = "Lin, H.", year = substr(today, 1, 4),
                            title = paste0("faofishingreport: Analyse and report FAO Global Capture Production statistics. R package version ",
                                           utils::packageVersion("faofishingreport")),
                            accessed = today, url = "https://linbojun99.github.io/faofishingreport/")
  )
  if (!is.null(cmsy_result)) {
    sources <- cmsy_result$sources
    fishbase <- sources$version[sources$component == "FishBase"]
    version <- if (length(fishbase) == 1 && grepl("^[0-9]{2}[.][0-9]{2}$", fishbase)) {
      paste0(substr(fishbase, 4, 5), "/20", substr(fishbase, 1, 2))
    } else {
      fishbase[1]
    }
    refs$fishbase <- list(type = "DB", author = "Froese, R.; Pauly, D.", editors = TRUE,
                          year = if (grepl("/", version)) sub(".*/", "", version) else substr(today, 1, 4),
                          title = "FishBase", version = paste("version", version), accessed = today,
                          url = "https://www.fishbase.se")
  }
  table <- fao_extdata_csv("report_references.csv")
  for (i in seq_len(nrow(table))) {
    ref <- as.list(table[i, setdiff(names(table), "key")])
    refs[[table$key[i]]] <- ref[!is.na(ref) & ref != ""]
  }
  refs
}

# Authors of a reference: "Family, I. I." for persons separated by ";", or an organisation
fao_ref_authors <- function(author, style) {
  a <- trimws(strsplit(author, ";")[[1]])
  person <- grepl(",", a)
  family <- ifelse(person, sub(",.*", "", a), a)
  initials <- ifelse(person, trimws(sub("^[^,]*,", "", a)), "")
  n <- length(a)
  if (style == "numeric") {
    x <- ifelse(person, paste(family, trimws(gsub("[.]", " ", initials))), a)
    x <- gsub("\\s+", " ", x)
    if (n > 3) paste0(paste(x[1:3], collapse = ", "), ", et al") else paste(x, collapse = ", ")
  } else if (style == "apa") {
    x <- ifelse(person, paste0(family, ", ", initials), a)
    if (n == 1) x else if (n == 2) paste0(x[1], ", & ", x[2]) else paste0(paste(x[-n], collapse = ", "), ", & ", x[n])
  } else {
    x <- ifelse(person, paste0(family, ", ", gsub(" ", "", initials)), a)
    if (n == 1) x else paste0(paste(x[-n], collapse = ", "), " & ", x[n])
  }
}

# Short author names for author-date citations
fao_ref_short <- function(author, style) {
  a <- trimws(strsplit(author, ";")[[1]])
  family <- sub(",.*", "", a)
  if (length(a) == 1) family else if (length(a) == 2) {
    paste(family[1], if (style == "apa") "&" else "and", family[2])
  } else {
    paste(family[1], "et al.")
  }
}

fao_ref_date <- function(date) {
  date <- as.Date(date)
  paste(as.integer(format(date, "%d")), month.name[as.integer(format(date, "%m"))], format(date, "%Y"))
}

# A reference in the list of references, with *...* for italics
fao_ref_entry <- function(ref, style, suffix = "") {
  dot <- function(x) if (grepl("[.]$", x)) x else paste0(x, ".")
  pages <- if (!is.null(ref$pages)) gsub("-", "\u2013", ref$pages) else NULL
  doi <- if (!is.null(ref$doi)) paste0("https://doi.org/", ref$doi) else NULL
  edition <- if (!is.null(ref$edition)) {
    n <- as.integer(ref$edition)
    paste0(n, c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
  } else {
    NULL
  }
  year <- paste0(ref$year, suffix)
  if (style == "numeric") {
    authors <- fao_ref_authors(ref$author, style)
    if (isTRUE(ref$editors)) authors <- paste0(authors, ", ed")
    switch(ref$type,
      J = paste0(authors, ". ", ref$title, "[J]. ", ref$container, ", ", ref$year, ", ", ref$volume,
                 if (!is.null(ref$issue)) paste0("(", ref$issue, ")"), ": ", ref$pages, ".",
                 if (!is.null(ref$doi)) paste0(" DOI: ", ref$doi, ".")),
      M = paste0(authors, ". ", ref$title, "[M]. ", if (!is.null(edition)) paste0(edition, " ed. "),
                 ref$place, ": ", ref$publisher, ", ", ref$year, ".",
                 if (!is.null(ref$doi)) paste0(" DOI: ", ref$doi, ".")),
      C = paste0(authors, ". ", ref$title, ": ", ref$container, "[C]. ", ref$place, ", ", ref$year,
                 if (!is.null(ref$pages)) paste0(", ", ref$pages), "."),
      DB = paste0(authors, ". ", ref$title, if (!is.null(ref$version)) paste0(" (", ref$version, ")"), "[DB/OL]. ",
                  if (!is.null(ref$place)) paste0(ref$place, ": ", ref$publisher, ", "), ref$year,
                  "[", ref$accessed, "]. ", ref$url, "."),
      CP = paste0(authors, ". ", ref$title, "[CP/OL]. ",
                  if (!is.null(ref$place)) paste0(ref$place, ": ", ref$publisher, ", "), ref$year,
                  "[", ref$accessed, "]. ", ref$url, ".")
    )
  } else if (style == "apa") {
    authors <- dot(fao_ref_authors(ref$author, style))
    if (isTRUE(ref$editors)) authors <- paste0(authors, " (Eds.).")
    version <- if (!is.null(ref$version)) paste0(" (", toupper(substr(ref$version, 1, 1)), substring(ref$version, 2), ")") else ""
    switch(ref$type,
      J = paste0(authors, " (", year, "). ", ref$title, ". *", ref$container, "*, *", ref$volume, "*",
                 if (!is.null(ref$issue)) paste0("(", ref$issue, ")"), ", ", pages, ".",
                 if (!is.null(doi)) paste0(" ", doi)),
      M = paste0(authors, " (", year, "). *", ref$title, "*", if (!is.null(edition)) paste0(" (", edition, " ed.)"), ".",
                 if (!identical(ref$publisher, ref$author)) paste0(" ", ref$publisher, "."),
                 if (!is.null(doi)) paste0(" ", doi)),
      C = paste0(authors, " (", year, "). ", ref$title, ". In *", ref$container, "*",
                 if (!is.null(pages)) paste0(" (pp. ", pages, ")"), ".", if (!is.null(ref$url)) paste0(" ", ref$url)),
      DB = paste0(authors, " (", year, "). *", ref$title, "*", version, " [Data set]. ", ref$url),
      CP = paste0(authors, " (", year, "). *", ref$title, "* [Computer software].",
                  if (!is.null(ref$publisher)) paste0(" ", ref$publisher, "."), " ", ref$url)
    )
  } else {
    authors <- dot(fao_ref_authors(ref$author, style))
    switch(ref$type,
      J = paste0(authors, " ", year, ". ", ref$title, ". *", ref$container, "*, ", ref$volume,
                 if (!is.null(ref$issue)) paste0("(", ref$issue, ")"), ": ", pages, ".",
                 if (!is.null(doi)) paste0(" ", doi)),
      M = paste0(authors, " ", year, ". *", ref$title, "*. ",
                 if (!is.null(edition)) paste0(edition, " edition. "),
                 if (!is.null(ref$series)) paste0(ref$series, ". "), ref$place,
                 if (!identical(ref$publisher, ref$author)) paste0(", ", ref$publisher), ".",
                 if (!is.null(doi)) paste0(" ", doi)),
      C = paste0(authors, " ", year, ". ", ref$title, ". In: *", ref$container, "*",
                 if (!is.null(pages)) paste0(", pp. ", pages), ". ", ref$place, ".",
                 if (!is.null(ref$url)) paste0(" ", ref$url)),
      DB = if (!is.null(ref$fao_url)) {
        paste0(authors, " ", year, ". ", ref$title, " (", ref$version, "). [Accessed on ",
               fao_ref_date(ref$accessed), "]. In: FishStatJ. Available at: ", ref$fao_url,
               ". Licence: CC-BY-4.0.")
      } else {
        paste0(authors, if (isTRUE(ref$editors)) ", eds.", " ", year, ". ", ref$title,
               ". World Wide Web electronic publication, ", ref$version, ". ", ref$url)
      },
      CP = paste0(authors, " ", year, ". *", ref$title, "*. ",
                  if (!is.null(ref$place)) paste0(ref$place, ", ", ref$publisher, ". "), ref$url)
    )
  }
}

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

# The sections of the report
fao_report_body <- function(x, meta, results, values, text, cmsy_result = NULL) {
  state <- attr(x, "fao_report")
  lang <- state$lang
  par <- function(x, key, style = "FAOBody") fao_report_par(x, text(key), style)
  heading <- function(x, key, level = 1) fao_report_par(x, text(key), paste0("FAOHeading", level))
  blank <- function(x, style = "FAOAuthor") officer::body_add_par(x, "", style = fao_report_style_name(style))
  charts <- results$charts
  figure <- function(x, id, caption_key) fao_report_figure(x, id, charts[[id]], text(caption_key))
  fig <- function(...) fao_report_ref(x, "figures", c(...))
  tab <- function(...) fao_report_ref(x, "tables", c(...))

  # Cover, on its own page without page number
  x <- blank(x, "FAOCoverText")
  x <- blank(x, "FAOCoverText")
  x <- fao_report_par(x, meta$project, "FAOCoverProject")
  x <- blank(x, "FAOCoverText")
  x <- blank(x, "FAOCoverText")
  x <- fao_report_par(x, meta$title, "FAOCoverTitle")
  for (i in 1:6) x <- blank(x, "FAOCoverText")
  x <- fao_report_par(x, text("cover_no", value = meta$report_no), "FAOCoverText")
  x <- fao_report_par(x, text("cover_author", value = meta$author), "FAOCoverText")
  if (!is.null(meta$affiliation)) x <- fao_report_par(x, text("cover_affiliation", value = meta$affiliation), "FAOCoverText")
  x <- fao_report_par(x, text("cover_date", value = meta$date), "FAOCoverText")
  x <- officer::body_end_block_section(x, officer::block_section(fao_report_section(state, footer = FALSE)))

  # Title and abstract
  x <- fao_report_par(x, meta$report_no, "FAOTitle")
  x <- fao_report_par(x, meta$title, "FAOTitle")
  x <- fao_report_par(x, meta$author, "FAOAuthor")
  x <- blank(x)
  x <- par(x, "abstract_heading", "FAOAbstractHeading")
  x <- blank(x)
  x <- par(x, "abstract")

  # 1 Data and methods
  x <- heading(x, "h_data_methods")
  x <- heading(x, "h_data", 2)
  x <- par(x, "data_p1")
  x <- par(x, "data_p2")
  x <- par(x, "data_note", "FAONote")
  x <- heading(x, "h_methods", 2)
  methods <- c("methods_bp", "methods_status", "methods_trend", "methods_diversity",
               if (isTRUE(values$has_cmsy)) "methods_cmsy", "methods_software")
  for (key in methods) x <- par(x, key)

  # 2 Results and conclusions
  x <- heading(x, "h_results")
  x <- heading(x, "h_total", 2)
  x <- fao_report_par(x, text("total_p1", fig_total = fig("total")))
  x <- figure(x, "total", "fig_total")
  x <- par(x, if (values$has_stages) "total_p2" else "total_p2_none")
  x <- fao_report_par(x, text("total_p3", fig_share = fig("share")))
  x <- figure(x, "share", "fig_share")

  x <- heading(x, "h_species", 2)
  x <- fao_report_par(x, text("species_p1", tab_species = tab("species"), fig_species = fig("species")))
  x <- fao_report_table(x, "species", fao_report_top_table(values, "species", lang), text("tab_species"))
  x <- figure(x, "species", "fig_species")
  x <- fao_report_par(x, text("species_p2", up_part = values$species_up_part, down_part = values$species_down_part,
                              fig_species_wrap = fig("species_wrap")))
  x <- figure(x, "species_wrap", "fig_species_wrap")

  x <- heading(x, "h_countries", 2)
  x <- fao_report_par(x, text("countries_p1", tab_countries = tab("countries"), fig_countries = fig("countries"),
                              fig_map = fig("map")))
  x <- fao_report_table(x, "countries", fao_report_top_table(values, "countries", lang), text("tab_countries"))
  x <- figure(x, "countries", "fig_countries")
  x <- figure(x, "map", "fig_map")
  x <- fao_report_par(x, text("countries_p2", up_part = values$countries_up_part, down_part = values$countries_down_part,
                              fig_countries_wrap = fig("countries_wrap")))
  x <- figure(x, "countries_wrap", "fig_countries_wrap")

  x <- heading(x, "h_groups", 2)
  x <- fao_report_par(x, text("groups_p1", fig_groups = fig("groups")))
  x <- figure(x, "groups", "fig_groups")

  x <- heading(x, "h_status", 2)
  x <- fao_report_par(x, text("status_p1", fig_status = fig("status", "status_catch")))
  x <- figure(x, "status", "fig_status")
  x <- figure(x, "status_catch", "fig_status_catch")
  x <- par(x, "status_p2")

  x <- heading(x, "h_quality", 2)
  x <- fao_report_par(x, text("quality_p1", fig_quality = fig("quality", "quality_taxon")))
  x <- figure(x, "quality", "fig_quality")
  x <- figure(x, "quality_taxon", "fig_quality_taxon")
  x <- par(x, "quality_p2")
  x <- fao_report_par(x, text("diversity_p1", fig_diversity = fig("diversity")))
  x <- figure(x, "diversity", "fig_diversity")

  x <- heading(x, "h_fleets", 2)
  x <- fao_report_par(x, text("fleets_p1", fig_fleets = fig("fleets")))
  x <- figure(x, "fleets", "fig_fleets")

  x <- heading(x, "h_conclusions", 2)
  for (key in c("concl_1", "concl_2", "concl_3", "concl_4")) x <- par(x, key)
  x <- par(x, "concl_note", "FAONote")

  # 3 CMSY++ assessment of the main species, one subsection per species
  if (isTRUE(values$has_cmsy)) x <- fao_report_cmsy_section(x, values, text)

  # References, in the order of citation or alphabetical
  x <- heading(x, "h_references")
  cited <- state$cited
  entries <- vapply(cited, function(k) fao_ref_entry(state$references[[k]], state$citation, state$suffix[[k]]), character(1))
  if (state$citation == "numeric") {
    entries <- paste0("[", seq_along(entries), "] ", entries)
  } else {
    entries <- entries[order(tolower(gsub("[*]", "", entries)))]
  }
  for (entry in entries) x <- fao_report_par(x, entry, "FAOReference")
  x <- par(x, paste0("ref_note_", state$citation), "FAONote")

  # Appendix: catch by decade
  x <- heading(x, "h_appendix")
  x <- fao_report_table(x, "app_species", fao_report_decade_table(results$area_data, values, "species", lang),
                        text("app_species"), appendix = TRUE)
  x <- fao_report_table(x, "app_countries", fao_report_decade_table(results$area_data, values, "countries", lang),
                        text("app_countries"), appendix = TRUE)
  if (isTRUE(values$has_cmsy)) {
    x <- fao_report_table(x, "app_cmsy_points", values$cmsy_appendix$points, text("app_cmsy_points"), appendix = TRUE)
    x <- fao_report_table(x, "app_cmsy_priors", values$cmsy_appendix$priors, text("app_cmsy_priors"), appendix = TRUE)
    x <- fao_report_table(x, "app_cmsy_sources", values$cmsy_appendix$sources, text("app_cmsy_sources"), appendix = TRUE)
    x <- fao_report_par(x, text("app_cmsy_note"), "FAONote")
  }
  officer::body_set_default_section(x, fao_report_section(state, footer = TRUE))
}

# The CMSY++ section: overview with the results table and the charts of all stocks, then one
# subsection per species and a summary
fao_report_cmsy_section <- function(x, values, text) {
  charts <- values$cmsy_charts
  fig <- function(...) fao_report_ref(x, "figures", c(...))
  x <- fao_report_par(x, text("h_cmsy"), "FAOHeading1")
  x <- fao_report_par(x, text("h_cmsy_overview"), "FAOHeading2")
  x <- fao_report_par(x, text("cmsy_p1", tab_cmsy = fao_report_ref(x, "tables", "cmsy"),
                              fig_cmsy = fig("cmsy_kobe", "cmsy_bbmsy")))
  x <- fao_report_table(x, "cmsy", values$cmsy_table, text("tab_cmsy"))
  x <- fao_report_figure(x, "cmsy_kobe", charts$kobe, text("fig_cmsy_kobe"))
  x <- fao_report_figure(x, "cmsy_bbmsy", charts$bbmsy, text("fig_cmsy_bbmsy"))
  for (i in seq_along(values$cmsy_species)) {
    stock <- values$cmsy_species[[i]]
    x <- fao_report_par(x, text("h_cmsy_species", no = i + 1, species = stock$values$species), "FAOHeading2")
    if (!stock$ok) {
      x <- fao_report_par(x, do.call(text, c(list("cmsy_species_failed"), stock$values)))
      next
    }
    id <- paste0("cmsy_", i)
    x <- fao_report_par(x, do.call(text, c(list("cmsy_species_p1", fig = fig(id)), stock$values)))
    x <- fao_report_figure(x, id, charts$species[i], text("fig_cmsy_species", species = stock$values$species))
    x <- fao_report_par(x, text("cmsy_species_note"), "FAONote")
  }
  x <- fao_report_par(x, text("h_cmsy_summary", no = length(values$cmsy_species) + 2), "FAOHeading2")
  fao_report_par(x, text("cmsy_summary"))
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

# Table of the main species or countries of the last five years
fao_report_top_table <- function(values, kind, lang) {
  word <- function(key) fao_report_text(key, lang)
  top <- if (kind == "species") values$species_table else values$country_table
  scale <- if (lang == "zh") 1e4 else 1e3
  trend <- c(up = word("trend_up"), down = word("trend_down"), none = word("trend_none"))[top$trend]
  table <- data.frame(rank = seq_len(nrow(top)), stringsAsFactors = FALSE)
  if (kind == "species") {
    table$name <- values$species_names$plain
    table$scientific <- values$species_names$marked
  } else {
    table$name <- values$country_names
  }
  table$mean <- sprintf("%.1f", top$mean / scale)
  table$share <- sprintf("%.1f", top$share)
  table$peak_year <- top$peak_year
  table$pct_of_peak <- sprintf("%.1f", top$pct_of_peak)
  table$trend <- ifelse(is.na(trend), "-", trend)
  names(table) <- c(word("col_rank"), word(if (kind == "species") "col_species" else "col_country"),
                    if (kind == "species") word("col_scientific"),
                    word("col_mean"), word("col_share"), word("col_peak_year"), word("col_pct_peak"),
                    word("col_trend"))
  table
}

# Appendix table: mean yearly catch by decade of the main species or countries
fao_report_decade_table <- function(data, values, kind, lang) {
  column <- if (kind == "species") "ASFIS.species..Name." else "Country..Name."
  top <- if (kind == "species") values$species_table$name_en else values$country_table$name_en
  names <- if (kind == "species") values$species_names$plain else values$country_names
  scale <- if (lang == "zh") 1e4 else 1e3
  data <- data[data[[column]] %in% top, ]
  decade <- 10 * (data$Year %/% 10)
  sums <- tapply(data$Production, list(data[[column]], decade), sum)
  sums[is.na(sums)] <- 0
  years <- tapply(data$Year, decade, function(y) length(unique(y)))
  means <- sweep(sums, 2, years[colnames(sums)], "/")[top, , drop = FALSE]
  table <- data.frame(name = names, matrix(sprintf("%.1f", means / scale), nrow = nrow(means)),
                      stringsAsFactors = FALSE)
  names(table) <- c(fao_report_text(if (kind == "species") "col_species" else "col_country", lang),
                    vapply(colnames(sums), function(d) fao_report_text("decade", lang, list(decade = d)), character(1)))
  table
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
