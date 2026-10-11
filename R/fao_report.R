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
