# Priors for the intrinsic growth rate r of each FAO species, from FishBase and SeaLifeBase:
# a built-in table (inst/extdata/cmsy_priors.csv, made by data-raw/cmsy_priors.R) that can be
# searched offline, with the link to the FishBase or SeaLifeBase page of every species.

#' FishBase and SeaLifeBase Priors for CMSY++
#'
#' This function searches the built-in table of CMSY++ priors for the FAO species items: the
#' resilience of the species and, where available, the prior range of r ("Prior r = ..., 95% CL
#' = ...", from the "Estimates based on models" section of the FishBase or SeaLifeBase species
#' page). The table works offline. Every row gives the sources of its values as links, so each
#' value can be checked online (`open = TRUE` opens the species pages):
#' \describe{
#'   \item{url}{the FishBase or SeaLifeBase page of the species, which shows its resilience and
#'     prior r (or a search of the database when the species was not found);}
#'   \item{data_url}{the table of the FishBase or SeaLifeBase release the value was read from
#'     (`estimate` for prior r, `stocks` for resilience);}
#'   \item{rule_url}{the CMSY++ user guide, whose Tables 1 and 2 turn these values into the r range.}
#' }
#'
#' Species can be given by their English or Chinese FAO name, both together as shown with
#' `lang = "both"` (e.g. "Chinese (English)"), scientific name, FishBase valid name or FAO 3-alpha
#' code. Each value is first matched exactly (ignoring case); otherwise every species whose names
#' contain it is returned.
#'
#' The table was made from FishBase and SeaLifeBase release 26.06 for the species-level items of
#' the FAO capture statistics. Each FAO scientific name is looked up in FishBase (fishes) or
#' SeaLifeBase (other animals) as a valid species name, then as a synonym in the database's own
#' synonym table; ambiguous synonyms and genus, family or higher groups get no values. The
#' `match_type` column tells how each item was matched.
#'
#' The r range given to CMSY++ follows its user guide (Froese et al. 2021):
#' \itemize{
#'   \item the 95% confidence limits of FishBase's prior r when the species has one
#'         ("Alternatively and better, get the prior range of r from the section 'Estimates
#'         based on models'", user guide Table 1);
#'   \item otherwise the range of the species' resilience (user guide Table 2): High 0.6-1.5,
#'         Medium 0.2-0.8, Low 0.05-0.5, Very low 0.015-0.1;
#'   \item otherwise none: the user guide gives no default, so the prior has to come from the user
#'         (e.g. r close to 2 M or 2 Fmsy, user guide Table 1); see `priors` in [cmsy_input()].
#' }
#' When FishBase lists several stocks of a species with different resilience, the value of its
#' species-wide stock (the first stock record, shown on the species page) is used and the other
#' values are given in `resilience_note`.
#'
#' @param species Optional names to search for: English, Chinese or scientific names, or FAO
#'        3-alpha codes, or parts of them (e.g. "Atlantic cod", "Gadus morhua", "COD", "cod" or the
#'        Chinese name). Default is NULL, all rows.
#' @param open A logical value. If TRUE, the FishBase or SeaLifeBase pages of the rows found (at
#'        most 10) are opened in the browser. Default is FALSE.
#' @param lang The language of the `species_name` column: "en" (English), "zh" (Chinese) or
#'        "both". Default is NULL, the language set with `fao_settings()`. The `species_en` and
#'        `species_zh` columns always give both names.
#' @return A data frame with one row per FAO species item.
#' @references
#' Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for CMSY++. GEOMAR,
#' Germany, 17 pp. \url{https://oceanrep.geomar.de/id/eprint/52147/}
#'
#' Froese, R. and Pauly, D. (Eds.) (2026). FishBase. Version (06/2026). \url{https://www.fishbase.se}
#'
#' Palomares, M.L.D. and Pauly, D. (Eds.) (2026). SeaLifeBase. Version (06/2026).
#' \url{https://www.sealifebase.se}
#' @export
#' @examples
#' cmsy_priors("Gadus morhua")
#' cmsy_priors(c("COD", "Atlantic cod"), lang = "both")
#' head(cmsy_priors()[, c("alpha3", "scientific_name", "resilience", "r_low", "r_high", "r_source")])
cmsy_priors <- function(species = NULL, open = FALSE, lang = NULL) {
  lang <- fao_lang(lang)
  priors <- cmsy_priors_table()
  lookup <- cmsy_species_lookup()
  if (!is.null(species)) {
    codes <- unlist(lapply(species, function(s) {
      tryCatch(cmsy_match_species(s), error = function(e) {
        text <- cmsy_normalise(paste(lookup$alpha3, lookup$species_en, lookup$species_zh,
                                     lookup$scientific_name, lookup$matched_name, sep = " | "))
        lookup$alpha3[grepl(cmsy_normalise(s), text, fixed = TRUE)]
      })
    }))
    priors <- priors[priors$alpha3 %in% codes, ]
    if (nrow(priors) == 0) message("No species matches \"", paste(species, collapse = "\", \""), "\".")
  }
  priors <- cbind(species_name = fao_translate(priors$species_en, "species", lang),
                  species_zh = lookup$species_zh[match(priors$alpha3, lookup$alpha3)], priors)
  priors <- priors[, c("species_name", "alpha3", "species_en", "species_zh",
                       setdiff(names(priors), c("species_name", "alpha3", "species_en", "species_zh")))]
  rownames(priors) <- NULL
  if (open && nrow(priors) > 0) {
    for (url in utils::head(priors$url, 10)) utils::browseURL(url)
  }
  priors
}

# The built-in table, read once per session
cmsy_priors_table <- function() {
  if (is.null(.cmsy_cache$priors)) {
    file <- system.file("extdata", "cmsy_priors.csv", package = "faofishingreport")
    priors <- utils::read.csv(file, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
    # Links to the sources of the values: the release table and the CMSY++ user guide
    code <- c(FishBase = "fb", SeaLifeBase = "slb")[priors$database]
    version <- sub("^.* ", "", priors$release)
    table <- ifelse(!is.na(priors$prior_r), "estimate", "stocks")
    priors$data_url <- ifelse(is.na(priors$database) | is.na(priors$r_source), NA_character_,
                              paste0("https://data.source.coop/cboettig/fishbase/", code, "/v", version,
                                     "/parquet/", table, ".parquet"))
    priors$rule_url <- ifelse(is.na(priors$r_source), NA_character_, cmsy_oceanrep_page)
    .cmsy_cache$priors <- priors
  }
  .cmsy_cache$priors
}

.cmsy_cache <- new.env(parent = emptyenv())

# r prior ranges of the resilience categories (User Guide for CMSY++, Table 2)
cmsy_resilience_r <- data.frame(
  resilience = c("High", "Medium", "Low", "Very low"),
  r_low = c(0.6, 0.2, 0.05, 0.015),
  r_high = c(1.5, 0.8, 0.5, 0.1),
  stringsAsFactors = FALSE
)

# Builds the priors table from FishBase and SeaLifeBase tables (data-raw/cmsy_priors.R).
#   items:  FAO species items with name_en, alpha3, scientific_name, major_en
#   tables: list(fishbase = list(species, synonyms, stocks, estimate), sealifebase = list(...))
#   versions: named character vector of the releases, e.g. c(fishbase = "26.06", sealifebase = "26.06")
cmsy_build_priors <- function(items, tables, versions) {
  dbs <- lapply(names(tables), function(server) cmsy_prior_db(server, tables[[server]], versions[[server]]))
  names(dbs) <- names(tables)

  rows <- lapply(seq_len(nrow(items)), function(i) {
    order <- if (identical(items$major_en[i], "PISCES")) c("fishbase", "sealifebase") else c("sealifebase", "fishbase")
    cmsy_prior_match(items$scientific_name[i], dbs[intersect(order, names(dbs))])
  })
  result <- cbind(items[, c("alpha3", "species_en", "scientific_name", "major_en")],
                  do.call(rbind, rows), stringsAsFactors = FALSE)

  # r range given to CMSY++ (user guide Tables 1 and 2)
  from_r <- !is.na(result$prior_r_lcl) & !is.na(result$prior_r_ucl)
  category <- match(result$resilience, cmsy_resilience_r$resilience)
  result$r_low <- ifelse(from_r, round(result$prior_r_lcl, 3), cmsy_resilience_r$r_low[category])
  result$r_high <- ifelse(from_r, round(result$prior_r_ucl, 3), cmsy_resilience_r$r_high[category])
  result$r_source <- ifelse(from_r, paste0(result$database, " prior r, 95% CL (", result$prior_r_note, ")"),
                            ifelse(!is.na(category), paste0(result$database, " resilience (",
                                                            result$resilience, "), CMSY++ user guide Table 2"),
                                   NA_character_))
  result[order(result$species_en), ]
}

cmsy_prior_db <- function(server, t, version) {
  key <- function(genus, species) tolower(paste(trimws(genus), trimws(species)))
  species <- data.frame(key = key(t$species$Genus, t$species$Species), SpecCode = t$species$SpecCode,
                        name = paste(t$species$Genus, t$species$Species), stringsAsFactors = FALSE)
  syn <- t$synonyms[t$synonyms$Status %in% c("synonym", "ambiguous synonym"), ]
  syn <- data.frame(key = key(syn$SynGenus, syn$SynSpecies), SpecCode = syn$SpecCode, status = syn$Status,
                    stringsAsFactors = FALSE)
  # Resilience per species: one value, or the first stock record's when stocks differ
  st <- t$stocks[!is.na(t$stocks$Resilience), c("SpecCode", "StockCode", "Resilience")]
  st <- st[order(st$SpecCode, st$StockCode), ]
  first <- st[!duplicated(st$SpecCode), ]
  others <- tapply(st$Resilience, st$SpecCode, function(v) {
    if (length(unique(v)) > 1) paste0("other stocks of the species: ", paste(unique(v[-1]), collapse = ", ")) else NA_character_
  })
  resilience <- data.frame(SpecCode = first$SpecCode, resilience = first$Resilience,
                           resilience_note = unname(others[as.character(first$SpecCode)]), stringsAsFactors = FALSE)
  est <- t$estimate[!is.na(t$estimate$prior_r), c("SpecCode", "prior_r", "lcl_r", "ucl_r", "n_r", "Comment_r")]
  list(server = server, version = version, species = species, synonyms = syn,
       resilience = resilience, estimate = est,
       name = c(fishbase = "FishBase", sealifebase = "SeaLifeBase")[[server]],
       web = c(fishbase = "https://www.fishbase.se", sealifebase = "https://www.sealifebase.se")[[server]])
}

# Looks up one FAO scientific name in the databases, in order
cmsy_prior_match <- function(scientific_name, dbs) {
  name <- trimws(gsub("\\s*\\([^)]*\\)", "", scientific_name))   # drop a subgenus in brackets
  words <- strsplit(name, "\\s+")[[1]]
  empty <- function(type, note, db = dbs[[1]]) {
    genus <- words[1]
    species <- if (length(words) > 1) words[2] else ""
    data.frame(database = NA_character_, SpecCode = NA_integer_, matched_name = NA_character_,
               match_type = type, match_note = note, resilience = NA_character_, resilience_note = NA_character_,
               prior_r = NA_real_, prior_r_lcl = NA_real_, prior_r_ucl = NA_real_, prior_r_n = NA_integer_,
               prior_r_note = NA_character_,
               url = paste0(db$web, "/Nomenclature/ScientificNameSearchList.php?Genus=",
                            utils::URLencode(genus), "&Species=", utils::URLencode(species)),
               release = paste(vapply(dbs, function(d) paste(d$name, d$version), character(1)), collapse = "; "),
               stringsAsFactors = FALSE)
  }
  if (length(words) < 2 || grepl("\\bspp\\b|\\bnei\\b|\\.", name)) return(empty("not a species", ""))
  key <- tolower(paste(words[1], words[2]))
  subspecies <- length(words) > 2
  for (db in dbs) {
    code <- db$species$SpecCode[db$species$key == key]
    type <- "exact"
    note <- ""
    if (length(code) == 0) {
      hits <- db$synonyms[db$synonyms$key == key, ]
      codes <- unique(hits$SpecCode[hits$status == "synonym"])
      if (length(codes) == 1 && !any(hits$status == "ambiguous synonym" & hits$SpecCode != codes)) {
        code <- codes
        type <- "synonym"
      } else if (length(unique(hits$SpecCode)) > 0) {
        return(empty("ambiguous synonym", paste0(db$name, " SpecCode ", paste(unique(hits$SpecCode), collapse = ", ")), db))
      }
    }
    if (length(code) >= 1) {
      code <- code[1]
      if (subspecies) note <- "FAO name is a subspecies; the species is used"
      res <- db$resilience[db$resilience$SpecCode == code, ]
      est <- db$estimate[db$estimate$SpecCode == code, ]
      return(data.frame(
        database = db$name, SpecCode = as.integer(code),
        matched_name = db$species$name[db$species$SpecCode == code][1],
        match_type = type, match_note = note,
        resilience = if (nrow(res) > 0) res$resilience[1] else NA_character_,
        resilience_note = if (nrow(res) > 0) res$resilience_note[1] else NA_character_,
        prior_r = if (nrow(est) > 0) round(est$prior_r[1], 3) else NA_real_,
        prior_r_lcl = if (nrow(est) > 0) round(est$lcl_r[1], 3) else NA_real_,
        prior_r_ucl = if (nrow(est) > 0) round(est$ucl_r[1], 3) else NA_real_,
        prior_r_n = if (nrow(est) > 0) as.integer(est$n_r[1]) else NA_integer_,
        prior_r_note = if (nrow(est) > 0) est$Comment_r[1] else NA_character_,
        url = paste0(db$web, "/summary/", code),
        release = paste(db$name, db$version),
        stringsAsFactors = FALSE))
    }
  }
  empty("not found", "")
}
