#' Assessment Report of an FAO Fishing Area
#'
#' This function runs the analyses of the package for one FAO major fishing area and writes a
#' Word report in Chinese or English: cover, title, abstract, data sources and methods, results
#' and conclusions with charts and tables, references and appendix tables. The text is written
#' from templates filled in with the results, as a first draft: check it, and complete the parts
#' marked in grey, such as other data sources, the discussion and other references.
#'
#' Every analysis is a section that can be switched on or off (`total`, `species`, ...,
#' `cmsy`). The settings of the function of a section are given with the section name as
#' prefix, e.g. `species_h_value = 0.2` is `h_value = 0.2` of [fishing_area_species()] and
#' `cmsy_top = 5` is `top = 5` of [cmsy_area()]. The abstract, the methods, the conclusions and
#' the references follow the sections that are switched on.
#'
#' Chinese reports follow the format of project reports: A4 pages; body text in FangSong and
#' Times New Roman, size 12 (xiao si), 1.5 line spacing and a first-line indent of two
#' characters; title in bold FangSong, size 18; "abstract" in SimHei, size 15; section headings
#' in SimHei and subsection headings in KaiTi, size 14; captions in SimHei, size 12, numbered by
#' report (figure 1-1, table 1-1), below figures and above tables; three-line tables with text
#' in FangSong, size 10.5; numbered references in superscript square brackets, in the order of
#' citation (GB/T 7714). English reports have the same structure, in one font (Arial by
#' default), with author-date citations in APA style or in the style of FAO publications.
#' Scientific names of species and genera are in italics.
#'
#' The text templates are in `inst/extdata/report_text.csv` and the references in
#' `inst/extdata/report_references.csv` of the package source: edit them to change the wording.
#'
#' @param area The FAO major fishing area: its name, e.g. "Atlantic, Eastern Central", or its
#'        code, e.g. 34.
#' @param ... Settings of the analyses, named with the section as prefix: `total_` for
#'        [fishing_area_data()], `species_` for [fishing_area_species()], `countries_` for
#'        [fishing_area_country()], `groups_` for [species_group_composition()], `status_` for
#'        [stock_status()], `quality_` for [data_quality()], `diversity_` for
#'        [diversity_concentration()], `fleets_` for [coastal_distant()], `shocks_` for
#'        [catch_shocks()], `collapse_` for [collapse_analysis()] (and `collapse_seed`, the seed of
#'        its random series, default 1), `synchrony_` for [catch_synchrony()], `pelagic_` for
#'        [pelagic_demersal()], `mtc_` for [mean_temperature_catch()] (the report uses
#'        `mtc_version = "builtin"`, the FishBase data bundled with the package, unless another
#'        version is given), `distant_` for [distant_water_profile()] (and `distant_countries`,
#'        the number of distant-water countries profiled, default 3, or their names) and
#'        `cmsy_` for [cmsy_area()] (and [cmsy_input()]). For example `species_h_value = 0.2`,
#'        `countries_line_linewidth = 1`, `status_smooth = TRUE`, `cmsy_top = 5` or
#'        `cmsy_settings = list(n.iter = 100000)`. A name that is not an argument of the
#'        function gives an error listing its arguments. The arguments the report sets itself
#'        (`area_data`, `area_name`, `area`, `lang`, `table` and `pptx`) cannot be changed, nor
#'        can `pelagic_by` and `mtc_by` (the analyses by division use the data of
#'        `load_fao_divisions()`, an extension that is not part of the report).
#' @param file The file to write. Default is `reports/FAO<code>_report_<lang>.docx` in the
#'        working directory. The other formats get the same name with `.tex` and `.pdf`.
#' @param format The formats to write, one or more of "docx" (default: Word), "latex" (a `.tex`
#'        file with its figures in the folder `<name>_figures/` next to it, compiled with XeLaTeX;
#'        Chinese reports use the ctexart class) and "pdf" (the LaTeX file compiled to PDF with
#'        XeLaTeX, through the tinytex package when it is installed, which also installs missing
#'        LaTeX packages). Install TinyTeX once with `install.packages("tinytex")` and
#'        `tinytex::install_tinytex()`; an old TinyTeX is updated with
#'        `tinytex::reinstall_tinytex()`. The analyses run once for all formats, and the formats have the same
#'        text, figures, tables and references. Fonts are taken from those of the computer: for
#'        Chinese, FangSong, SimHei and KaiTi on Windows, STFangsong, Heiti SC and Kaiti SC on macOS,
#'        else the Fandol fonts of TeX Live.
#' @param lang The language of the report: "zh" (Chinese) or "en" (English). Default is NULL,
#'        the language set with `fao_settings()` ("both" gives a Chinese report).
#' @param title The report title. Default is generated from the area, e.g. "Assessment of
#'        Marine Capture Production in FAO Major Fishing Area 34 (Atlantic, Eastern Central)".
#' @param report_no The number of the report, shown as "Report 1" (in Chinese reports with a
#'        Chinese numeral) and used in the figure and table numbers (1-1, 1-2, ...). Default is 1.
#' @param author The author on the cover and below the title. Default is NULL, which writes
#'        "Author" ("作者" in Chinese reports) as a placeholder.
#' @param affiliation The affiliation on the cover. Default is NULL, none.
#' @param project The text at the top of the cover. Default is NULL, which writes "Project Report"
#'        ("课题报告" in Chinese reports).
#' @param date The date on the cover. Default is today.
#' @param font The font of the text in Latin script. Default is "Times New Roman" in Chinese
#'        reports and "Arial" in English reports.
#' @param font_zh The font of the Chinese body text. Default is "FangSong".
#' @param citation The citation style of English reports: "apa" (default) or "fao" (the style
#'        of FAO publications). Chinese reports use numbered citations (GB/T 7714).
#' @param top_n The number of main species and countries in the charts and tables, and of
#'        species assessed with CMSY++ (unless `cmsy_top` is given). Default is 10.
#' @param data_version The FAO release to use, as in `load_fao_data()`. Default is "latest".
#' @param total,species,countries,groups,status,quality,diversity,fleets Logical values: whether
#'        the report has the section on the total catch and its stages ([fishing_area_data()]),
#'        the main species ([fishing_area_species()]), the main fishing countries
#'        ([fishing_area_country()]), the ISSCAAP groups ([species_group_composition()]), the
#'        stock status plots ([stock_status()]), the data quality ([data_quality()]), the
#'        diversity and concentration of the catch ([diversity_concentration()]) and the
#'        coastal and distant-water fleets ([coastal_distant()]). All TRUE by default.
#' @param shocks,collapse,synchrony,pelagic,mtc,distant Logical values: whether the report has
#'        the sections on catch shocks ([catch_shocks()]), fishery collapses
#'        ([collapse_analysis()]), the synchrony of the catch and the portfolio effect
#'        ([catch_synchrony()]), the pelagic to demersal ratio ([pelagic_demersal()]), the mean
#'        temperature of the catch ([mean_temperature_catch()]) and the profiles of the main
#'        distant-water fleets in the area ([distant_water_profile()]). All FALSE by default.
#' @param cmsy A logical value. If TRUE, the main species are also assessed with CMSY++
#'        ([cmsy_area()]), and the report has a part on the assessment, with the results of
#'        every species and appendix tables of the reference points, priors and their sources.
#'        Default is FALSE. The result of an earlier [cmsy_area()] run can also be given, to
#'        write the report without running CMSY++ again; it must be of the same area and FAO
#'        release as the report (`data_version`), else the report stops. CMSY++ needs JAGS: see
#'        [cmsy_setup()].
#' @return Invisibly, a list of class `fao_report` with the paths of the files written (`file`,
#'         named by format: `docx`, `latex`, `pdf`), the
#'         sections in the report (`sections`) and
#'         the tables of the report as data frames: `main_species` and `main_countries`, the
#'         species of the mean temperature of the catch with their FishBase matches
#'         (`mtc_species`), the results of all analyses (`analyses`), and with
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
#' # The other analyses of the package, with their settings
#' fao_report(34, lang = "zh", shocks = TRUE, collapse = TRUE, synchrony = TRUE, pelagic = TRUE,
#'            mtc = TRUE, distant = TRUE, collapse_method = "worm2006", distant_countries = 5)
#'
#' # Only some sections, with settings of their functions
#' fao_report(34, lang = "zh", quality = FALSE, diversity = FALSE,
#'            species_h_value = 0.2, status_smooth = TRUE)
#'
#' # With a CMSY++ assessment of the 10 main species (a few minutes)
#' report <- fao_report(34, lang = "zh", cmsy = TRUE)
#' report                                         # the tables it returns
#' View(report$cmsy_reference_points)             # r, k, MSY, B/Bmsy, ... with 95% limits
#' View(report$cmsy_priors)                       # priors, sources and links
#' fao_report(34, lang = "zh", top_n = 15, cmsy = TRUE, cmsy_top = 5)
#'
#' # Reuse CMSY++ results to rewrite the report
#' fao_report(34, lang = "zh", cmsy = readRDS(file.path("reports", "cmsy", "Atlantic__Eastern_Central",
#'                                                       "cmsy_result.rds")))
#' }
fao_report <- function(area, ..., file = NULL, format = "docx", lang = NULL, title = NULL, report_no = 1,
                       author = NULL, affiliation = NULL, project = NULL, date = Sys.Date(),
                       font = NULL, font_zh = "FangSong", citation = c("apa", "fao"),
                       top_n = 10, data_version = "latest",
                       total = TRUE, species = TRUE, countries = TRUE, groups = TRUE, status = TRUE,
                       quality = TRUE, diversity = TRUE, fleets = TRUE, shocks = FALSE, collapse = FALSE,
                       synchrony = FALSE, pelagic = FALSE, mtc = FALSE, distant = FALSE, cmsy = FALSE) {
  format <- match.arg(format, c("docx", "latex", "pdf"), several.ok = TRUE)
  if ("docx" %in% format) fao_check_report_packages()
  sections <- fao_report_sections()
  args <- fao_report_args(list(...), sections)
  lang <- fao_lang(lang)
  if (lang == "both") lang <- "zh"
  citation <- if (lang == "zh") "numeric" else match.arg(citation)
  if (is.null(font)) font <- if (lang == "zh") "Times New Roman" else "Arial"
  area <- fao_report_area(area, lang)
  if (is.null(file)) file <- file.path("reports", paste0("FAO", area$code, "_report_", lang, ".docx"))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  file <- file.path(normalizePath(dirname(file)), basename(file))
  # The file names of the formats: the same name with .docx, .tex (and .pdf)
  stem <- sub("\\.(docx|tex|pdf)$", "", file, ignore.case = TRUE)

  # The switched-on sections; cmsy may be the result of an earlier cmsy_area() run
  switches <- list(total = total, species = species, countries = countries, groups = groups, status = status,
                   quality = quality, diversity = diversity, fleets = fleets, shocks = shocks,
                   collapse = collapse, synchrony = synchrony, pelagic = pelagic, mtc = mtc, distant = distant,
                   cmsy = isTRUE(cmsy) || inherits(cmsy, "cmsy_result"))
  ctx <- new.env()
  ctx$sections <- sections
  ctx$on <- names(sections)[vapply(names(sections), function(n) isTRUE(switches[[n]]), logical(1))]
  ctx$args <- args
  ctx$area <- area
  ctx$lang <- lang
  ctx$top_n <- top_n
  ctx$data_version <- data_version
  ctx$cmsy_result <- if (inherits(cmsy, "cmsy_result")) cmsy
  ctx$cmsy_dir <- file.path(dirname(file), "cmsy")
  info <- fao_data_info(data_version)
  # A CMSY++ result given to the report must be of this area and of the FAO release of the report
  if (!is.null(ctx$cmsy_result)) fao_report_check_cmsy(ctx$cmsy_result, area$name, info$release)

  message("Analysing ", area$name, " ...")
  fao_report_run(ctx)
  values <- fao_report_base_values(ctx)
  for (name in ctx$on) values <- c(values, ctx$sections[[name]]$values(ctx))
  values$release <- info$release
  values$release_date <- fao_report_date(info$release_date, lang)
  values$download_date <- fao_report_date(info$download_date, lang)
  values$url <- info$url
  ctx$values <- values
  # values given in the call (e.g. the years of one stock) come before the report values
  ctx$text <- function(.key, ...) fao_report_text(.key, lang, c(list(...), values))

  meta <- list(
    title = if (is.null(title)) ctx$text("title") else title,
    report_no = ctx$text("report_no", no = fao_report_number(report_no, lang)),
    report_number = as.character(report_no),
    author = if (is.null(author)) ctx$text("author_default") else author,
    affiliation = affiliation,
    project = if (is.null(project)) ctx$text("project_default") else project,
    date = format(as.Date(date), "%Y-%m-%d")
  )
  cmsy_result <- if ("cmsy" %in% ctx$on) ctx$results$cmsy$value
  fishbase <- fao_report_fishbase_version(ctx, cmsy_result)
  references <- fao_report_references(info, date, fishbase)
  files <- character()
  if ("docx" %in% format) {
    message("Writing ", stem, ".docx ...")
    doc <- fao_report_new(lang, font, font_zh, citation, report_no, references)
    doc <- fao_report_body(doc, meta, ctx)
    files["docx"] <- fao_report_save(doc, paste0(stem, ".docx"), meta)
  }
  if (any(c("latex", "pdf") %in% format)) {
    message("Writing ", stem, ".tex", if ("pdf" %in% format) " and the PDF", " ...")
    doc <- fao_report_new(lang, font, font_zh, citation, report_no, references, format = "latex",
                          file = paste0(stem, ".tex"))
    doc <- fao_report_body(doc, meta, ctx)
    files <- c(files, fao_latex_save(doc, meta, pdf = "pdf" %in% format))
  }
  message("Report saved: ", paste(files, collapse = ", "))

  # The tables of the report as data frames, to look at or reuse in R
  out <- list(file = files, sections = ctx$on)
  if ("species" %in% ctx$on) {
    s <- values$species_table
    out$main_species <- data.frame(species = values$species_names$plain, species_en = s$name_en,
                                   scientific_name = values$species_names$scientific, mean_catch_t = s$mean,
                                   share_pct = s$share, peak_year = s$peak_year, pct_of_peak = s$pct_of_peak,
                                   trend_10_years = s$trend, stringsAsFactors = FALSE)
  }
  if ("countries" %in% ctx$on) {
    s <- values$country_table
    out$main_countries <- data.frame(country = values$country_names, country_en = s$name_en, mean_catch_t = s$mean,
                                     share_pct = s$share, peak_year = s$peak_year, pct_of_peak = s$pct_of_peak,
                                     trend_10_years = s$trend, stringsAsFactors = FALSE)
  }
  if ("mtc" %in% ctx$on) out$mtc_species <- values$mtc_species
  out$analyses <- lapply(ctx$results[ctx$on], `[[`, "value")
  if ("cmsy" %in% ctx$on) {
    out$cmsy <- cmsy_result
    out$cmsy_reference_points <- values$cmsy_reference_points
    out$cmsy_priors <- values$cmsy_priors
  }
  invisible(structure(out, class = "fao_report"))
}

#' @export
print.fao_report <- function(x, ...) {
  cat("FAO report: ", paste(x$file, collapse = "\n            "), "\n", "Sections: ", paste(x$sections, collapse = ", "), "\n", sep = "")
  tables <- c(main_species = "main species of the last five years",
              main_countries = "main fishing countries of the last five years",
              cmsy_reference_points = "CMSY++ biological reference points with 95% confidence limits",
              cmsy_priors = "CMSY++ priors, their sources and links",
              mtc_species = "species of the mean temperature of the catch, with FishBase matches")
  for (name in names(tables)) {
    if (!is.null(x[[name]])) cat(sprintf("  $%-22s %s (%d rows)\n", name, tables[[name]], nrow(x[[name]])))
  }
  if (!is.null(x$cmsy)) cat("  $cmsy                   CMSY++ result (folder ", x$cmsy$dir, ")\n", sep = "")
  if (length(x$analyses) > 0) cat("  $analyses               results of the analyses: ", paste(names(x$analyses), collapse = ", "), "\n", sep = "")
  invisible(x)
}

# The FishBase version cited by the report: from the CMSY++ run, or of the FishBase data used for
# the mean temperature of the catch (bundled with the package unless mtc_version is given)
fao_report_fishbase_version <- function(ctx, cmsy_result = NULL) {
  if (!is.null(cmsy_result)) {
    version <- cmsy_result$sources$version[cmsy_result$sources$component == "FishBase"]
    if (length(version) == 1) return(version)
  }
  if (!"mtc" %in% ctx$on) return(NULL)
  version <- ctx$args$mtc$version
  if (is.null(version) || identical(version, "builtin")) {
    bundled <- new.env()
    load(system.file("extdata", "fishbase_traits.rda", package = "faofishingreport"), envir = bundled)
    version <- attr(bundled$fishbase_traits, "fishbase_version")[["fishbase"]]
  }
  version
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
  # the years of each decade are those of the data of the area, the same whatever items are chosen
  years <- tapply(data$Year, 10 * (data$Year %/% 10), function(y) length(unique(y)))
  data <- data[data[[column]] %in% top, ]
  decade <- 10 * (data$Year %/% 10)
  sums <- tapply(data$Production, list(data[[column]], decade), sum)
  sums[is.na(sums)] <- 0
  means <- sweep(sums, 2, years[colnames(sums)], "/")[top, , drop = FALSE]
  table <- data.frame(name = names, matrix(sprintf("%.1f", means / scale), nrow = nrow(means)),
                      stringsAsFactors = FALSE)
  names(table) <- c(fao_report_text(if (kind == "species") "col_species" else "col_country", lang),
                    vapply(colnames(sums), function(d) fao_report_text("decade", lang, list(decade = d)), character(1)))
  table
}
