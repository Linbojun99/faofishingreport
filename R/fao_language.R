#' Translate FAO Country, Species or Fishing Area Names
#'
#' This function translates English names, as used in the FAO data, to Chinese
#' or to bilingual "Chinese English" names. Translations come from the FAO
#' official code lists (country, species and water area lists published with
#' the FAO data, and the ASFIS species list). Names without an official Chinese
#' name are filled from the tables bundled with the package, and otherwise kept
#' in English.
#'
#' Many FAO species items have no Chinese name in the FAO lists. For those identified to one
#' species, the Chinese common name of the species in FishBase or SeaLifeBase is used (Froese and
#' Pauly 2026; Palomares and Pauly 2026): the name given for China in Mandarin Chinese, in
#' simplified characters, preferring the name FishBase marks as preferred and leaving out names
#' applied to synonyms and species with several equally used names. The table, with the other
#' names and the source of each, is `inst/extdata/species_names_zh_fishbase.csv`, made by
#' `data-raw/build_species_names_zh.R`. FAO names always come first.
#'
#' @param x A character vector of English names.
#' @param type The kind of name: "country", "species" or "area".
#' @param lang The output language: "en" (English), "zh" (Chinese) or "both" (Chinese and English).
#' @return A character vector of translated names.
#' @references
#' Froese, R. and Pauly, D. (Eds.) (2026). FishBase. World Wide Web electronic publication, version
#' 06/2026. \url{https://www.fishbase.se}
#'
#' Palomares, M.L.D. and Pauly, D. (Eds.) (2026). SeaLifeBase. World Wide Web electronic
#' publication, version 06/2026. \url{https://www.sealifebase.se}
#' @family settings and language
#' @export
#' @examples
#' \dontrun{
#' fao_translate(c("Morocco", "Spain"), "country", "both")
#' }
fao_translate <- function(x, type = c("country", "species", "area"), lang = "zh") {
  type <- match.arg(type)
  lang <- fao_lang(lang)
  x <- as.character(x)
  if (lang == "en") return(x)

  names_table <- fao_name_table()
  names_table <- names_table[names_table$type == type, ]
  en <- unique(x[!is.na(x)])
  zh <- names_table$name_zh[match(en, names_table$name_en)]

  # Different English names that share one Chinese name keep their English name, so they stay apart
  shared <- !is.na(zh) & (duplicated(zh) | duplicated(zh, fromLast = TRUE))
  if (lang == "zh") {
    out <- ifelse(is.na(zh), en, ifelse(shared, paste(zh, en), zh))
  } else {
    out <- ifelse(is.na(zh), en, paste(zh, en))
  }
  out[match(x, en)]
}


#' Font Used for Chart Text
#'
#' This function returns the font used for all chart text. The font is chosen
#' for the operating system so that Chinese text displays correctly: "Songti SC"
#' on macOS, "SimSun" on Windows and "Noto Serif CJK SC" on Linux. If that font
#' is not installed, the next available Chinese font is used.
#'
#' @param family Optional font family to use from now on, e.g. "Microsoft YaHei".
#'   The choice can also be set with `options(faofishingreport.font = "...")`.
#' @return The font family name.
#' @family settings and language
#' @export
#' @examples
#' \dontrun{
#' fao_font()                    # font chosen for this computer
#' fao_font("Microsoft YaHei")   # use another font
#' }
fao_font <- function(family = NULL) {
  if (!is.null(family)) {
    options(faofishingreport.font = family)
  }
  font <- getOption("faofishingreport.font")
  if (is.null(font)) {
    font <- fao_detect_font()
    options(faofishingreport.font = font)
  }
  font
}

# Chinese fonts to try on each operating system, in order of preference
fao_font_candidates <- list(
  macOS = c("Songti SC", "STSong", "PingFang SC", "Heiti SC", "Hiragino Sans GB", "Arial Unicode MS"),
  Windows = c("SimSun", "NSimSun", "Microsoft YaHei", "SimHei", "DengXian", "KaiTi"),
  Linux = c("Noto Serif CJK SC", "Noto Sans CJK SC", "Source Han Serif SC", "Source Han Sans SC",
            "WenQuanYi Zen Hei", "WenQuanYi Micro Hei", "AR PL UMing CN")
)

fao_os <- function() {
  switch(Sys.info()[["sysname"]], Darwin = "macOS", Windows = "Windows", "Linux")
}

# First installed Chinese font for the operating system, or "sans" if none is found
fao_detect_font <- function(os = fao_os(), installed = NULL) {
  if (is.null(installed)) {
    installed <- tryCatch(unique(systemfonts::system_fonts()$family), error = function(e) character(0))
  }
  candidates <- fao_font_candidates[[os]]
  # If the installed fonts cannot be listed, trust the system's usual font
  if (length(installed) == 0) return(candidates[1])
  found <- candidates[candidates %in% installed]
  if (length(found) > 0) found[1] else "sans"
}

# Writes a CSV file as UTF-8 with a byte order mark, so Excel shows Chinese names correctly
fao_write_csv <- function(x, file) {
  readr::write_excel_csv(x, file)
}

# Resolves the output language: the `lang` argument, else ZH = TRUE (the older way of asking
# for Chinese), else the language set with fao_settings()
fao_lang <- function(lang = NULL, ZH = FALSE) {
  if (is.null(lang)) lang <- if (isTRUE(ZH)) "zh" else fao_setting("lang")
  match.arg(lang, c("en", "zh", "both"))
}

# Session cache for tables read from inst/extdata
.fao_cache <- new.env(parent = emptyenv())

# Reads a UTF-8 CSV file (with or without the byte order mark that Excel writes) in any locale: the
# text is marked as UTF-8 instead of being converted to the native encoding, which fails for
# Chinese text in a locale that is not UTF-8 (e.g. "C" on some Linux servers)
fao_read_csv_utf8 <- function(file, ...) {
  # in such a locale R notes that the text is kept in UTF-8, which is what is wanted
  x <- withCallingHandlers(utils::read.csv(file, encoding = "UTF-8", check.names = FALSE, ...),
                           warning = function(w) {
                             if (grepl("not representable in native encoding", conditionMessage(w))) invokeRestart("muffleWarning")
                           })
  names(x)[1] <- sub("^\xef\xbb\xbf", "", names(x)[1], useBytes = TRUE)
  Encoding(names(x)) <- "UTF-8"
  x
}

# Reads a UTF-8 CSV file bundled with the package, once per session
fao_extdata_csv <- function(file) {
  if (is.null(.fao_cache[[file]])) {
    .fao_cache[[file]] <- fao_read_csv_utf8(system.file("extdata", file, package = "faofishingreport"),
                                            colClasses = "character")
  }
  .fao_cache[[file]]
}

# Chart text in the requested language; "both" gives "Chinese (English)".
# Labels are in inst/extdata/chart_labels.csv; "{n}" in a label is replaced by `n`.
fao_label <- function(key, lang, n = NULL) {
  labels <- fao_extdata_csv("chart_labels.csv")
  label <- labels[labels$key == key, c("en", "zh")]
  if (nrow(label) == 0) stop("Unknown chart label: ", key, call. = FALSE)
  text <- switch(lang,
                 en = label$en,
                 zh = label$zh,
                 both = paste0(label$zh, " (", label$en, ")"))
  if (!is.null(n)) text <- gsub("{n}", n, text, fixed = TRUE)
  text
}

# Former countries that have no Chinese name in the FAO code lists
fao_former_countries <- data.frame(
  name_en = c("Un. Sov. Soc. Rep.", "Yugoslavia SFR", "Czechoslovakia",
              "Serbia and Montenegro", "Ethiopia PDR"),
  name_zh = c("\u82cf\u8054",                                  # 苏联
              "\u5357\u65af\u62c9\u592b",                      # 南斯拉夫
              "\u6377\u514b\u65af\u6d1b\u4f10\u514b",          # 捷克斯洛伐克
              "\u585e\u5c14\u7ef4\u4e9a\u548c\u9ed1\u5c71",    # 塞尔维亚和黑山
              "\u57c3\u585e\u4fc4\u6bd4\u4e9a\u4eba\u6c11\u6c11\u4e3b\u5171\u548c\u56fd"),  # 埃塞俄比亚人民民主共和国
  stringsAsFactors = FALSE
)

# English -> Chinese names: the newer of the downloaded and bundled tables first, then the other.
fao_name_table <- function(data_dir = fao_data_dir()) {
  bundled <- new.env()
  load(system.file("extdata", "fao_names.rda", package = "faofishingreport"), envir = bundled)
  names_table <- bundled$fao_names

  versions <- fao_local_versions(data_dir)
  if (length(versions) > 0) {
    newest <- versions[length(versions)]
    names_file <- fao_names_file(newest, data_dir)
    if (file.exists(names_file)) {
      downloaded <- readRDS(names_file)
      names_table <- if (numeric_version(newest) > numeric_version(fao_bundled_release())) {
        rbind(downloaded, names_table)
      } else {
        rbind(names_table, downloaded)
      }
    }
  }
  # Species items without an FAO Chinese name: the Chinese common name of the species in FishBase
  # or SeaLifeBase (records for China, simplified characters; data-raw/build_species_names_zh.R)
  names_table <- rbind(names_table, fao_fishbase_names_zh())
  names_table[!duplicated(names_table[c("type", "name_en")]), ]
}

fao_fishbase_names_zh <- function() {
  if (is.null(.fao_cache$fishbase_names_zh)) {
    x <- fao_read_csv_utf8(system.file("extdata", "species_names_zh_fishbase.csv", package = "faofishingreport"),
                           stringsAsFactors = FALSE)
    .fao_cache$fishbase_names_zh <- data.frame(type = "species", name_en = x$name_en, name_zh = x$name_zh,
                                              stringsAsFactors = FALSE)
  }
  .fao_cache$fishbase_names_zh
}

# Builds the English -> Chinese name table from the code lists of a Capture release.
# FAO official Chinese names come first; the package's own tables fill the gaps.
# `asfis` is an optional ASFIS species list (data frame) with more official Chinese names.
fao_build_name_table <- function(capture, lookup = fao_lookup_tables(), asfis = NULL) {
  blank_to_na <- function(x) {
    x <- trimws(x)
    x[!is.na(x) & x == ""] <- NA
    x
  }
  first_available <- function(...) {
    values <- list(...)
    out <- values[[1]]
    for (v in values[-1]) out[is.na(out)] <- v[is.na(out)]
    out
  }

  countries <- capture$countries
  country_zh <- first_available(
    blank_to_na(countries$Name_Cn),
    fao_former_countries$name_zh[match(fao_country_names(countries$UN_Code, countries, lookup),
                                       fao_former_countries$name_en)],
    blank_to_na(lookup$name_mapping$Short.name.ZH[match(as.integer(countries$UN_Code), lookup$name_mapping$UN)])
  )
  country <- data.frame(
    type = "country",
    name_en = c(fao_country_names(countries$UN_Code, countries, lookup),
                fao_former_countries$name_en, lookup$name_mapping$Short.name),
    name_zh = c(country_zh, fao_former_countries$name_zh,
                blank_to_na(lookup$name_mapping$Short.name.ZH)),
    stringsAsFactors = FALSE
  )

  species <- capture$species
  species_code <- species$`3A_Code`
  species_zh <- blank_to_na(species$Name_Cn)
  if (!is.null(asfis)) {
    species_zh <- first_available(species_zh,
                                  blank_to_na(asfis$Chinese_name[match(species_code, asfis$Alpha3_Code)]))
  }
  species_zh <- first_available(species_zh,
                                blank_to_na(lookup$asfis$Chinese_name[match(species_code, lookup$asfis$Alpha3_Code)]))
  species_names <- data.frame(
    type = "species",
    name_en = c(fao_species_names(species_code, species, lookup), lookup$asfis$English_name),
    name_zh = c(species_zh, blank_to_na(lookup$asfis$Chinese_name)),
    stringsAsFactors = FALSE
  )

  areas <- capture$areas
  area <- data.frame(type = "area", name_en = areas$Name_En, name_zh = blank_to_na(areas$Name_Cn),
                     stringsAsFactors = FALSE)

  names_table <- rbind(country, species_names, area)
  names_table <- names_table[!is.na(names_table$name_en) & !is.na(names_table$name_zh), ]
  names_table <- names_table[!duplicated(names_table[c("type", "name_en")]), ]
  rownames(names_table) <- NULL
  names_table
}

# Creates output folders, including missing parent folders.
fao_ensure_dirs <- function(...) {
  for (dir in c(...)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

# PowerPoint output needs the eoffice package (in Suggests, since on macOS it needs XQuartz)
fao_check_eoffice <- function() {
  if (!requireNamespace("eoffice", quietly = TRUE)) {
    stop("PowerPoint output needs the eoffice package: install.packages(\"eoffice\")",
         if (Sys.info()[["sysname"]] == "Darwin") ". On macOS it also needs XQuartz (https://www.xquartz.org)." else ".",
         " Or use fao_settings(pptx = FALSE) to save PNG files only.", call. = FALSE)
  }
  invisible(TRUE)
}

# Saves a chart as PNG in figures/png/, and also as editable PowerPoint in figures/pptx/ when pptx = TRUE.
# `name` is the path inside those folders without extension, e.g. "species/Bonga_shad_country_ranked".
fao_save_plot <- function(plot, name, width, height, pptx = FALSE) {
  # No background grid lines unless options(faofishingreport.grid = TRUE); a wider right margin
  # keeps the label of the last year, at the right edge of the axis, inside the image
  if (!isTRUE(fao_setting("grid"))) {
    plot <- plot + ggplot2::theme(panel.grid = ggplot2::element_blank(),
                                  panel.grid.major = ggplot2::element_blank(),
                                  panel.grid.minor = ggplot2::element_blank())
  }
  plot <- plot + ggplot2::theme(plot.margin = ggplot2::margin(5.5, 18, 5.5, 5.5))
  png_file <- file.path("figures", "png", paste0(name, ".png"))
  fao_ensure_dirs(dirname(png_file))
  ggplot2::ggsave(filename = png_file, plot = plot, device = ragg::agg_png, dpi = fao_setting("dpi"),
                  width = width, height = height)
  if (isTRUE(pptx)) {
    fao_check_eoffice()
    pptx_file <- file.path("figures", "pptx", paste0(name, ".pptx"))
    fao_ensure_dirs(dirname(pptx_file))
    eoffice::topptx(figure = plot, filename = pptx_file, width = width, height = height)
  }
  invisible(png_file)
}

# Country or species name made safe for file names
fao_file_name <- function(x) {
  gsub("[[:punct:][:space:]]", "_", x)
}

# Axis breaks every `by` years from 1950, ending at the last year of the data. The last year
# replaces the previous break when the two are too close for both labels to fit.
fao_year_breaks <- function(years, by = 5) {
  last <- max(years, na.rm = TRUE)
  breaks <- seq(1950, last, by = by)
  if (last - breaks[length(breaks)] < by / 2 + 0.5) {
    breaks[length(breaks)] <- last
  } else {
    breaks <- c(breaks, last)
  }
  breaks
}

# Decade labels ("1990s"), as used in the tables
fao_decade <- function(year) {
  paste0(floor(year / 10) * 10, "s")
}

# Periods for the pie charts: whole decades, and single years for the latest, unfinished decade
fao_pie_period <- function(year, last_year) {
  ifelse(year < floor(last_year / 10) * 10, fao_decade(year), as.character(year))
}

# Runs the breakpoint analysis for each name. A series that cannot be analysed (e.g. too few
# years for h_value) is skipped with a message instead of stopping the whole function.
fao_breakpoints_by <- function(series, get_bp_coefs, data, h_value) {
  lapply(series, function(name) {
    tryCatch(get_bp_coefs(name, data = data, h_value = h_value), error = function(e) {
      message("Breakpoint analysis skipped for ", name, ": ", conditionMessage(e),
              ". A larger h_value (e.g. 3) may work for short series.")
      list(coefs_data = NULL, bps = NULL)
    })
  })
}
