# Finding FAO species items by any of their names: English or Chinese FAO name (or both, as
# shown with lang = "both"), scientific name (also the FishBase valid name of a synonym) or FAO
# 3-alpha code. Used by cmsy_priors(), cmsy_input(), cmsy_run() and cmsy_plot().

# Names of every species item of the priors table
cmsy_species_lookup <- function() {
  if (is.null(.cmsy_cache$lookup)) {
    p <- cmsy_priors_table()
    zh <- fao_translate(p$species_en, "species", "zh")
    zh[zh == p$species_en] <- NA_character_
    .cmsy_cache$lookup <- data.frame(alpha3 = p$alpha3, species_en = p$species_en, species_zh = zh,
                                     scientific_name = p$scientific_name, matched_name = p$matched_name,
                                     stringsAsFactors = FALSE)
  }
  .cmsy_cache$lookup
}

cmsy_normalise <- function(x) tolower(gsub("\\s+", " ", trimws(x)))

# FAO 3-alpha codes of the species named in `x`. Each value is matched exactly (ignoring case and
# spaces) against the code, English, Chinese, scientific and FishBase names, and "Chinese
# (English)"; if there is no exact match, as part of a name, which must then point to one item.
cmsy_match_species <- function(x, among = NULL) {
  lookup <- cmsy_species_lookup()
  if (!is.null(among)) lookup <- lookup[lookup$alpha3 %in% among, ]
  # "Chinese English" as in charts with lang = "both", or "Chinese (English)"
  both <- ifelse(is.na(lookup$species_zh), NA_character_, paste(lookup$species_zh, lookup$species_en))
  both_brackets <- ifelse(is.na(lookup$species_zh), NA_character_, paste0(lookup$species_zh, " (", lookup$species_en, ")"))
  keys <- list(lookup$alpha3, lookup$species_en, lookup$species_zh, lookup$scientific_name,
               lookup$matched_name, both, both_brackets)
  unique(unlist(lapply(x, function(value) {
    v <- cmsy_normalise(value)
    exact <- unique(unlist(lapply(keys, function(k) lookup$alpha3[!is.na(k) & cmsy_normalise(k) == v])))
    if (length(exact) > 0) return(exact)
    text <- cmsy_normalise(paste(lookup$alpha3, lookup$species_en, lookup$species_zh, lookup$scientific_name,
                                 lookup$matched_name, sep = " | "))
    partial <- unique(lookup$alpha3[grepl(v, text, fixed = TRUE)])
    if (length(partial) == 1) {
      row <- lookup[lookup$alpha3 == partial, ]
      message("\"", value, "\" matched ", row$species_en, if (!is.na(row$species_zh)) paste0(" / ", row$species_zh),
              " (", row$scientific_name, ", ", row$alpha3, ").")
      return(partial)
    }
    if (length(partial) == 0) stop("No species matches \"", value, "\".", call. = FALSE)
    rows <- lookup[lookup$alpha3 %in% partial, ]
    shown <- utils::head(paste0(rows$alpha3, ": ", rows$species_en,
                                ifelse(is.na(rows$species_zh), "", paste0(" / ", rows$species_zh)),
                                " (", rows$scientific_name, ")"), 15)
    stop("\"", value, "\" matches ", length(partial), " species; give one of these names or codes:\n  ",
         paste(shown, collapse = "\n  "), if (length(partial) > 15) "\n  ...", call. = FALSE)
  })))
}

# Stock identifiers of cmsy_input() or cmsy_run() named by `x`: Stock identifiers, or species
# names in any form (all stocks of those species)
cmsy_match_stocks <- function(x, stocks) {
  direct <- x[x %in% stocks$Stock]
  species <- setdiff(x, direct)
  from_species <- if (length(species) > 0) {
    codes <- cmsy_match_species(species, among = unique(stocks$alpha3))
    stocks$Stock[stocks$alpha3 %in% codes]
  } else {
    character(0)
  }
  unique(c(direct, from_species))
}
