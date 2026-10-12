#' Match FAO Species Items to FishBase and SeaLifeBase
#'
#' This function goes through the FAO species items of the data one by one, looks each up in
#' FishBase (fishes) or SeaLifeBase (other aquatic animals and plants) by the scientific name in
#' the FAO species list, and returns the matched record with its trophic level, temperature
#' preference, habitat, maximum length and vulnerability to fishing. Only matches that follow a
#' published rule are made; every row gives the rule and its source, and links to the FishBase,
#' SeaLifeBase or WoRMS pages, so each match can be checked and, if wrong, corrected (see
#' Corrections). Items that cannot be matched by these rules are not filled in: they are marked
#' as failed, with links for searching the name online.
#'
#' @section Matching rules:
#' Fishes (FAO major group PISCES) are looked up in FishBase first and all other items in
#' SeaLifeBase first, following Branch et al. (2010, Supplementary Information p. 1).
#' \enumerate{
#'   \item \strong{exact}: the FAO scientific name is a valid species name in the database.
#'   \item \strong{synonym}: the FAO name is listed as a synonym (status "synonym") of one valid
#'     species in the database's synonym table. Misapplied names are never used, since they are
#'     misidentifications and not aliases (Froese and Pauly 2000, "The SYNONYMS table"); names that the
#'     table marks as ambiguous, or that point to several species, are not matched.
#'   \item \strong{WoRMS valid name}: a name not found by rules 1-2 is looked up online in the
#'     World Register of Marine Species (WoRMS), which the FAO ASFIS list follows for most
#'     groups (FAO 2026, pp. 1-3). Only an exact WoRMS record with status
#'     "accepted" or with a single valid name is used; the valid name is then matched by rules
#'     1-2 or 4. Results are saved in `worms_lookup.csv` in the `fishbase` folder of `data_dir`
#'     and reused when the computer is offline.
#'   \item \strong{genus, family, order, class, phylum}: an FAO item above species level (e.g.
#'     "Sardinella spp" or "Sparidae") takes the unweighted mean trophic level of the species of
#'     that taxon in the database (Branch et al. 2010, Supplementary Information p. 1; Liang
#'     and Pauly 2017). Temperature preference, habitat, length and vulnerability are left
#'     empty for these items, since studies of the mean temperature of the catch exclude items
#'     above species level (Tsikliras and Stergiou 2014; Liang et al. 2018).
#'   \item \strong{plant (TL 1)}: aquatic plants (FAO major group PLANTAE AQUATICAE) have a
#'     trophic level of 1 (Pauly et al. 1998).
#' }
#' Items that name several taxa (e.g. "Sepiidae, Sepiolidae") are not matched, since there is no
#' published rule for them. Species not found by these rules are not given the values of their
#' genus. The `status` column is "matched", "failed" or "pending online check" (offline and not
#' looked up in WoRMS before).
#'
#' The trophic levels are the FishBase and SeaLifeBase estimates (table "estimate", column
#' Troph), which come from diet studies or, when these are missing, from related species and
#' body size (Froese and Pauly 2000, "The FOOD ITEMS table", boxes 25-26). Temperature preferences (column TempPrefMean) are means of the
#' AquaMaps distribution of the species (FishBase reference 123201); note that Cheung et al.
#' (2013) used medians.
#'
#' Rows are sorted by catch, with the share and cumulative share of the catch, so checking the
#' first rows covers most of the catch. The table is saved in `tables/`, with empty `checked` and
#' `note` columns for recording the checks.
#'
#' @section Corrections:
#' Wrong or missing matches can be corrected in a CSV file (by default `corrections.csv` in the
#' `fishbase` folder of `data_dir`; a template is created there the first time). Each row has the
#' `alpha3` code of the FAO item and either a `database` ("FishBase" or "SeaLifeBase") and
#' `SpecCode` of the record to use, or values to use directly (`trophic_level`,
#' `temp_pref_mean`, `habitat`, `max_length_cm`, `vulnerability`), or both; `note` should give
#' the source (e.g. "TL 3.5 for Marine fishes nei, Pauly et al. 1998"). Corrections are applied
#' every time the function runs, so a team sharing `data_dir` shares them.
#'
#' @inheritParams summary_params
#' @param area_data FAO catch data: `fishing_area_data()$area_data`, or any subset of
#'        `load_fao_data()`. Default is NULL, all species in `load_fao_data()`.
#' @param version The FishBase and SeaLifeBase release, e.g. "26.06". Default is "latest": the
#'        newest online release when online (its tables are downloaded as needed), else the newest
#'        downloaded release, else the table built into the package. "builtin" always uses the
#'        built-in table, made with the same rules (see `fishbase_sources()` for its release).
#' @param corrections Path of the corrections file (see Corrections), or FALSE for none.
#' @param data_dir The directory where downloaded data is stored. Default is `fao_data_dir()`.
#' @return Invisibly, a data frame with one row per FAO species item.
#' @references
#' Branch, T.A., Watson, R., Fulton, E.A. et al. (2010). The trophic fingerprint of marine
#' fisheries. Nature 468: 431-435. \doi{10.1038/nature09528}
#'
#' Cheung, W.W.L., Watson, R. and Pauly, D. (2013). Signature of ocean warming in global
#' fisheries catch. Nature 497: 365-368. \doi{10.1038/nature12156}
#'
#' Liang, C. and Pauly, D. (2017). Fisheries impacts on China's coastal ecosystems: unmasking a
#' pervasive 'fishing down' effect. PLoS ONE 12: e0173296. \doi{10.1371/journal.pone.0173296}
#'
#' Liang, C., Xian, W. and Pauly, D. (2018). Impacts of ocean warming on China's fisheries
#' catches: an application of "mean temperature of the catch" concept. Frontiers in Marine
#' Science 5: 26. \doi{10.3389/fmars.2018.00026}
#'
#' Pauly, D., Christensen, V., Dalsgaard, J., Froese, R. and Torres, F. (1998). Fishing down
#' marine food webs. Science 279: 860-863. \doi{10.1126/science.279.5352.860}
#'
#' Tsikliras, A.C. and Stergiou, K.I. (2014). Mean temperature of the catch increases quickly in
#' the Mediterranean Sea. Marine Ecology Progress Series 515: 281-284. \doi{10.3354/meps11005}
#'
#' FAO (2026). Nomenclature, revision, and references of the ASFIS list (version 2026). Rome, FAO.
#' \url{https://www.fao.org/fishery/static/ASFIS/ASFIS_References_2026_EN.pdf}
#'
#' Froese, R. and Pauly, D. (Eds.) (2000). FishBase 2000: concepts, design and data sources. ICLARM,
#' Los Baños, Philippines. Chapters "The SYNONYMS table"
#' (\url{https://www.fishbase.se/manual/fishbasethe_synonyms_table.htm}) and "The FOOD ITEMS
#' table", boxes 25 and 26 on trophic levels
#' (\url{https://www.fishbase.se/manual/fishbasethe_food_items_table.htm}).
#'
#' Froese, R. and Pauly, D. (Eds.) FishBase. World Wide Web electronic publication.
#' \url{https://www.fishbase.se}
#'
#' Palomares, M.L.D. and Pauly, D. (Eds.) SeaLifeBase. World Wide Web electronic publication.
#' \url{https://www.sealifebase.se}
#'
#' WoRMS Editorial Board. World Register of Marine Species. Flanders Marine Institute (VLIZ).
#' \url{https://www.marinespecies.org}, \doi{10.14284/170}
#' @export
#' @examples
#' \dontrun{
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' match <- fishbase_match(FAO34$area_data, lang = "zh")
#' head(match[, c("species_name", "status", "match_type", "trophic_level", "url")])
#' }
fishbase_match <- function(area_data = NULL, version = "latest", corrections = NULL, table = TRUE,
                           lang = NULL, label = NULL, data_dir = fao_data_dir()) {
  lang <- fao_lang(lang)
  if (is.null(area_data)) {
    area_data <- load_fao_data(data_dir = data_dir)
    if (is.null(label)) label <- "all_species"
  }
  name <- fao_summary_name(fao_summary_label(area_data, label), "fishbase_match")

  items <- fb_species_catalog(area_data)
  dbs <- list()
  if (!identical(version, "builtin")) {
    for (server in c("fishbase", "sealifebase")) {
      dbs[[server]] <- tryCatch(fb_match_db(server, version, data_dir), error = function(e) {
        warning(fb_server_name(server), " is not available, so items are not looked up there: ",
                conditionMessage(e), call. = FALSE)
        NULL
      })
    }
    dbs <- Filter(Negate(is.null), dbs)
  }
  if (length(dbs) == 0) {
    builtin <- fb_builtin_table()
    if (!identical(version, "builtin")) {
      message("FishBase and SeaLifeBase cannot be reached and have not been downloaded: using the table built ",
              "into the package (", paste(fb_server_name(names(attr(builtin, "fishbase_version"))),
                                          attr(builtin, "fishbase_version"), collapse = ", "),
              ", made on ", attr(builtin, "built"), "). Run download_fishbase_data() when online for the latest release.")
    }
    result <- fb_builtin_match(items, builtin)
  } else {
    result <- fb_match_with(items, dbs, data_dir)
  }

  corrections_file <- if (isFALSE(corrections)) NULL else if (is.null(corrections)) fb_corrections_file(data_dir) else corrections
  if (!is.null(corrections_file)) result <- fb_apply_corrections(result, corrections_file, dbs, is.null(corrections))

  result$checked <- ""
  result$note <- ""
  result <- cbind(species_name = fao_translate(result$species_en, "species", lang), result)
  rownames(result) <- NULL
  attr(result, "fishbase_version") <- if (length(dbs) == 0) attr(builtin, "fishbase_version") else
    vapply(dbs, function(db) db$version, character(1))

  if (table) fao_save_table(result, name)
  counts <- base::table(factor(result$status, levels = c("matched", "failed", "pending online check")))
  message("FAO species items: ", nrow(result), "; ", paste(names(counts), counts, sep = " ", collapse = ", "),
          ". Matched items cover ", round(sum(result$catch_pct[result$status == "matched"]), 1), "% of the catch.")
  invisible(result)
}


# Matches the items with the downloaded (or just downloaded) databases: rules 1 to 5
fb_match_with <- function(items, dbs, data_dir) {
  parsed <- lapply(seq_len(nrow(items)), function(i) fb_parse_name(items$fao_scientific_name[i], items$taxon_level[i]))
  db_order <- lapply(items$major_group, function(major) {
    dbs[if (identical(major, "PISCES") || is.na(major)) c("fishbase", "sealifebase") else c("sealifebase", "fishbase")]
  })

  # Rules 1, 2, 4 and 5 with the downloaded databases
  matches <- do.call(rbind, lapply(seq_len(nrow(items)), function(i) {
    if (identical(items$major_group[i], "PLANTAE AQUATICAE")) return(fb_plant_row(parsed[[i]], db_order[[i]]))
    fb_match_local(parsed[[i]], db_order[[i]])
  }))

  # Rule 3: names not found are looked up in WoRMS, and their valid names matched again
  lookup <- matches$status == "failed" & matches$worms_lookup
  if (any(lookup)) {
    worms <- fb_worms_lookup(vapply(parsed[lookup], `[[`, character(1), "name"), data_dir)
    for (k in seq_len(nrow(worms))) {
      i <- which(lookup)[k]
      matches$worms_url[i] <- worms$worms_url[k]
      if (worms$result[k] == "pending") {
        matches$status[i] <- "pending online check"
        matches$match_note[i] <- "Not found in the downloaded databases; look up in WoRMS when online"
      } else if (worms$result[k] == "valid name") {
        retry <- fb_match_local(fb_parse_name(worms$valid_name[k], parsed[[i]]$level), db_order[[i]])
        if (retry$status == "matched") {
          retry$match_type <- paste0("WoRMS valid name (", retry$match_type, ")")
          retry$rule_source <- paste0("WoRMS ", worms$lookup_date[k], ", as used by FAO ASFIS; then ", retry$rule_source)
          retry$worms_url <- worms$worms_url[k]
          retry$search_url <- matches$search_url[i]
          retry$worms_lookup <- FALSE
          matches[i, ] <- retry
        } else {
          matches$match_note[i] <- paste0(matches$match_note[i], "; WoRMS valid name ", worms$valid_name[k],
                                          " (", worms$status[k], ") is not in FishBase or SeaLifeBase either")
        }
      } else {
        matches$match_note[i] <- paste0(matches$match_note[i], "; ", worms$note[k])
      }
    }
  }
  matches$worms_lookup <- NULL
  cbind(items, matches)
}

# The table built into the package (data-raw/build_fishbase_traits.R): fishbase_match() for every
# species item of the bundled FAO data, without manual corrections
fb_builtin_table <- function() {
  file <- system.file("extdata", "fishbase_traits.rda", package = "faofishingreport")
  if (file == "") stop("The package has no built-in FishBase table.", call. = FALSE)
  if (is.null(.fb_cache$builtin)) {
    env <- new.env()
    load(file, envir = env)
    .fb_cache$builtin <- env$fishbase_traits
  }
  .fb_cache$builtin
}

# Matches the items with the built-in table, by FAO 3-alpha code (or name). Items that are not in
# it (e.g. from a newer FAO release) are pending an online check.
fb_builtin_match <- function(items, builtin) {
  id_columns <- c("species_en", "alpha3", "fao_scientific_name", "taxon_level", "major_group")
  i <- match(items$alpha3, builtin$alpha3)
  by_name <- is.na(i)
  i[by_name] <- match(items$species_en[by_name], builtin$species_en)
  m <- builtin[i, setdiff(names(builtin), id_columns)]
  m$source <- ifelse(is.na(m$source), NA_character_, paste(m$source, "(built-in table)"))
  missing <- is.na(i)
  if (any(missing)) {
    m$status[missing] <- "pending online check"
    m$match_type[missing] <- "not matched"
    m$match_note[missing] <- "Not in the built-in table; look up in FishBase when online"
    m$search_url[missing] <- vapply(which(missing), function(k) {
      server <- if (identical(items$major_group[k], "PISCES") || is.na(items$major_group[k])) "fishbase" else "sealifebase"
      fb_search_urls(fb_parse_name(items$fao_scientific_name[k], items$taxon_level[k])$names, list(list(server = server)))
    }, character(1))
  }
  rownames(m) <- NULL
  cbind(items, m)
}


fb_web <- function(server) c(fishbase = "https://www.fishbase.se", sealifebase = "https://www.sealifebase.se")[[server]]

fb_corrections_file <- function(data_dir) file.path(data_dir, "fishbase", "corrections.csv")

fb_trait_columns <- c("trophic_level", "trophic_level_se", "temp_pref_mean", "habitat", "max_length_cm", "vulnerability")

# Short sources of the matching rules (full references in ?fishbase_match)
fb_rule_sources <- c(
  exact = "Valid name in FishBase (fishes) or SeaLifeBase (others): Branch et al. 2010 Nature 468:431, SI p. 1",
  synonym = "Synonym (status 'synonym') in the FishBase/SeaLifeBase synonym table; misapplied names excluded (FishBase manual)",
  group = "Unweighted mean trophic level of the species of the taxon: Branch et al. 2010 Nature 468:431, SI p. 1; Liang & Pauly 2017 PLoS ONE 12:e0173296",
  plant = "Trophic level 1 for plants: Pauly et al. 1998 Science 279:860"
)

# FAO species items of the data with their scientific name, taxonomic level, major group and catch,
# sorted by catch
fb_species_catalog <- function(area_data) {
  catch <- tapply(area_data$Production, area_data$ASFIS.species..Name., sum, na.rm = TRUE)
  catch <- sort(catch, decreasing = TRUE)
  items <- data.frame(species_en = names(catch), stringsAsFactors = FALSE)

  groups <- fao_group_table()$species
  g <- groups[match(items$species_en, groups$name_en), ]
  items$alpha3 <- g$alpha3
  items$fao_scientific_name <- g$scientific_name
  items$taxon_level <- g$taxon_level
  items$major_group <- g$major_en

  # Group tables saved by older versions of the package have no scientific names: use the
  # bundled ASFIS list, or the name itself for items shown as [Scientific name]
  missing <- is.na(items$fao_scientific_name)
  if (any(missing)) {
    asfis <- fao_lookup_tables()$asfis
    a <- asfis[match(items$species_en[missing], asfis$English_name), ]
    items$alpha3[missing] <- a$Alpha3_Code
    items$fao_scientific_name[missing] <- a$Scientific_Name
    items$taxon_level[missing] <- ifelse(is.na(items$taxon_level[missing]), fao_taxon_level(a$Taxonomic_Code),
                                         items$taxon_level[missing])
    bracket <- missing & is.na(items$fao_scientific_name) & grepl("^\\[.*\\]$", items$species_en)
    items$fao_scientific_name[bracket] <- gsub("^\\[|\\]$", "", items$species_en[bracket])
  }

  total <- sum(catch)
  items$catch_tonnes <- as.numeric(catch)
  items$catch_pct <- if (total > 0) 100 * items$catch_tonnes / total else 0
  items$cumulative_pct <- cumsum(items$catch_pct)
  items
}

# Tables of one database needed for matching, with indexes of species by name and taxon
fb_match_db <- function(server, version, data_dir) {
  version <- fb_resolve_version(server, version, data_dir, "species")
  get <- function(table, columns) {
    x <- fishbase_table(table, server, version, data_dir)
    x[setdiff(columns, names(x))] <- NA
    x[columns]
  }
  species <- get("species", c("SpecCode", "Genus", "Species", "FamCode", "DemersPelag", "Length", "Vulnerability"))
  estimate <- get("estimate", c("SpecCode", "Troph", "seTroph", "TempPrefMean"))
  families <- get("families", c("FamCode", "Family", "Order", "Class", "Phylum"))
  synonyms <- get("synonyms", c("SpecCode", "SynGenus", "SynSpecies", "Status"))

  traits <- species
  e <- estimate[match(species$SpecCode, estimate$SpecCode), ]
  traits$Troph <- e$Troph
  traits$seTroph <- e$seTroph
  traits$TempPrefMean <- e$TempPrefMean
  f <- families[match(species$FamCode, families$FamCode), ]
  traits$Family <- f$Family
  traits$Order <- f$Order
  traits$Class <- f$Class
  traits$Phylum <- f$Phylum
  traits$key <- tolower(paste(traits$Genus, traits$Species))

  # Misapplied names are misidentifications, not aliases (FishBase manual): never used
  synonyms <- synonyms[tolower(synonyms$Status) %in%
                         c("synonym", "ambiguous synonym", "accepted name", "provisionally accepted name"), ]
  synonyms$key <- tolower(paste(synonyms$SynGenus, synonyms$SynSpecies))
  index <- function(x) split(seq_along(x), tolower(x))
  # FishBase orders such as "Perciformes/Percoidei" are found by their first name only
  order <- sub("/.*$", "", as.character(traits$Order))
  list(server = server, version = version, traits = traits, synonyms = synonyms,
       by_rank = list(genus = index(traits$Genus), family = index(traits$Family), order = index(order),
                      class = index(traits$Class), phylum = index(traits$Phylum)),
       synonym_index = index(synonyms$key))
}

# Reads an FAO scientific name: a species ("Genus species"), a taxon above species ("Sardinella
# spp", "Sparidae"), or several taxa ("Sepiidae, Sepiolidae"). Names in brackets (subgenera, former
# names) are kept only for the search links.
fb_parse_name <- function(scientific_name, taxon_level) {
  if (is.na(scientific_name) || trimws(scientific_name) == "") {
    return(list(kind = "none", name = NA_character_, names = character(0), level = taxon_level))
  }
  inner <- regmatches(scientific_name, gregexpr("\\([^)]*\\)", scientific_name))[[1]]
  main <- trimws(gsub("\\s+", " ", gsub("\\([^)]*\\)", " ", scientific_name)))
  first_word <- function(x) vapply(strsplit(trimws(x), " "), `[`, character(1), 1)
  inner_names <- first_word(trimws(gsub("[()=]", " ", inner)))
  if (grepl(",|;| or ", main)) {
    # Species keep both words; an abbreviated genus ("Auxis thazard, A. rochei") is written out
    parts <- strsplit(trimws(strsplit(main, ",|;| or ")[[1]]), " ")
    genus <- parts[[1]][1]
    names <- vapply(parts, function(p) {
      if (grepl("\\.$", p[1])) p[1] <- genus
      if (length(p) >= 2 && !tolower(p[2]) %in% c("spp", "spp.", "sp", "sp.", "nei")) paste(p[1], p[2]) else p[1]
    }, character(1))
    return(list(kind = "several", name = main, names = unique(c(names, inner_names)), level = taxon_level))
  }
  words <- strsplit(main, " ")[[1]]
  if (identical(taxon_level, "species") && length(words) >= 2 &&
      !tolower(words[2]) %in% c("spp", "spp.", "sp", "sp.", "nei")) {
    name <- paste(words[1], words[2])
    return(list(kind = "species", name = name, names = name, level = taxon_level))
  }
  list(kind = "taxon", name = words[1], names = unique(c(words[1], inner_names)), level = taxon_level)
}

# Names in an FAO scientific name of a taxon above species, the main name first
# ("Clupeiformes (=Clupeoidei)" -> "Clupeiformes", "Clupeoidei"). Used by mean_temperature_catch().
fb_candidate_names <- function(scientific_name) fb_parse_name(scientific_name, "higher")$names

# Rules 1, 2 and 4 with the downloaded databases (`dbs` in the order to search)
fb_match_local <- function(parsed, dbs) {
  dbs <- Filter(Negate(is.null), dbs)
  search <- fb_search_urls(parsed$names, dbs)
  if (parsed$kind == "none") return(fb_failed_row(dbs, "No FAO scientific name", search, worms = FALSE))
  if (parsed$kind == "several") {
    return(fb_failed_row(dbs, "The FAO item names several taxa; there is no published rule for these", search, worms = FALSE))
  }

  if (parsed$kind == "species") {
    key <- tolower(parsed$name)
    for (db in dbs) {
      i <- match(key, db$traits$key)
      if (!is.na(i)) return(fb_species_row(db, i, "exact", fb_rule_sources[["exact"]], search))
    }
    for (db in dbs) {
      rows <- db$synonym_index[[key]]
      if (length(rows) == 0) next
      codes <- unique(db$synonyms$SpecCode[rows])
      if (length(codes) > 1 || any(tolower(db$synonyms$Status[rows]) == "ambiguous synonym")) {
        note <- paste0("Ambiguous synonym in ", fb_server_name(db$server), " (SpecCode ", paste(codes, collapse = ", "), ")")
        return(fb_failed_row(dbs, note, search, worms = TRUE))
      }
      i <- match(codes, db$traits$SpecCode)
      if (!is.na(i)) return(fb_species_row(db, i, "synonym", fb_rule_sources[["synonym"]], search))
    }
    return(fb_failed_row(dbs, "Species not found in FishBase or SeaLifeBase", search, worms = TRUE))
  }

  for (db in dbs) {
    for (rank in c("genus", "family", "order", "class", "phylum")) {
      rows <- db$by_rank[[rank]][[tolower(parsed$name)]]
      if (length(rows) > 0) return(fb_group_row(db, rows, rank, parsed$name, search))
    }
  }
  fb_failed_row(dbs, "Taxon not found in FishBase or SeaLifeBase", search, worms = TRUE)
}

fb_species_row <- function(db, i, type, rule_source, search) {
  t <- db$traits[i, ]
  fb_row(db, "matched", type, matched_name = paste(t$Genus, t$Species), SpecCode = t$SpecCode, n_species = 1,
         trophic_level = t$Troph, trophic_level_se = t$seTroph, temp_pref_mean = t$TempPrefMean,
         habitat = t$DemersPelag, max_length_cm = t$Length, vulnerability = t$Vulnerability,
         url = paste0(fb_web(db$server), "/summary/", t$Genus, "-", t$Species, ".html"),
         search_url = search, note = "", rule_source = rule_source)
}

# Rule 4: unweighted mean trophic level of the species of the taxon; no other values
fb_group_row <- function(db, rows, rank, taxon, search) {
  t <- db$traits[rows, ]
  web <- fb_web(db$server)
  url <- switch(rank,
    genus = paste0(web, "/identification/SpeciesList.php?genus=", t$Genus[1]),
    family = paste0(web, "/Summary/FamilySummary.php?ID=", t$FamCode[1]),
    order = if (db$server == "fishbase") paste0(web, "/Summary/OrdersSummary.php?order=", taxon) else
      paste0(web, "/Nomenclature/ScientificNameSearchList.php?Genus=", taxon),
    paste0(web, "/Nomenclature/ScientificNameSearchList.php?Genus=", taxon))
  with_tl <- sum(!is.na(t$Troph))
  fb_row(db, "matched", rank, matched_name = taxon, SpecCode = NA, n_species = nrow(t),
         trophic_level = if (with_tl > 0) mean(t$Troph, na.rm = TRUE) else NA_real_, trophic_level_se = NA_real_,
         temp_pref_mean = NA_real_, habitat = NA_character_, max_length_cm = NA_real_, vulnerability = NA_real_,
         url = url, search_url = search,
         note = paste0("Mean trophic level of the ", with_tl, " species of ", rank, " ", taxon, " with one (",
                       nrow(t), " species in ", fb_server_name(db$server), ")"),
         rule_source = fb_rule_sources[["group"]])
}

# Rule 5: aquatic plants
fb_plant_row <- function(parsed, dbs) {
  dbs <- Filter(Negate(is.null), dbs)
  search <- fb_search_urls(parsed$names, dbs)
  db <- if (length(dbs) > 0) dbs[[1]] else list(server = NA, version = NA)
  fb_row(db, "matched", "plant (TL 1)", matched_name = parsed$name, SpecCode = NA, n_species = NA,
         trophic_level = 1, trophic_level_se = NA_real_, temp_pref_mean = NA_real_, habitat = NA_character_,
         max_length_cm = NA_real_, vulnerability = NA_real_, url = search, search_url = search, note = "",
         rule_source = fb_rule_sources[["plant"]])
}

fb_failed_row <- function(dbs, note, search, worms) {
  db <- if (length(dbs) > 0) dbs[[1]] else list(server = NA, version = NA)
  row <- fb_row(db, "failed", "not matched", matched_name = NA_character_, SpecCode = NA, n_species = NA,
                trophic_level = NA_real_, trophic_level_se = NA_real_, temp_pref_mean = NA_real_,
                habitat = NA_character_, max_length_cm = NA_real_, vulnerability = NA_real_, url = NA_character_,
                search_url = search, note = note, rule_source = "")
  row$database <- NA_character_
  row$worms_lookup <- worms
  row
}

fb_row <- function(db, status, type, matched_name, SpecCode, n_species, trophic_level, trophic_level_se,
                   temp_pref_mean, habitat, max_length_cm, vulnerability, url, search_url, note, rule_source) {
  data.frame(
    status = status, match_type = type,
    database = if (is.na(db$server)) NA_character_ else fb_server_name(db$server),
    matched_name = matched_name, SpecCode = as.integer(SpecCode), n_species = as.integer(n_species),
    trophic_level = round(as.numeric(trophic_level), 2), trophic_level_se = round(as.numeric(trophic_level_se), 2),
    temp_pref_mean = round(as.numeric(temp_pref_mean), 1), habitat = as.character(habitat),
    max_length_cm = round(as.numeric(max_length_cm), 1), vulnerability = round(as.numeric(vulnerability), 1),
    url = url, search_url = search_url, worms_url = NA_character_, match_note = note, rule_source = rule_source,
    source = if (is.na(db$server)) NA_character_ else paste(fb_server_name(db$server), db$version),
    worms_lookup = FALSE, stringsAsFactors = FALSE
  )
}

# Links that search FishBase or SeaLifeBase (the first database to search) for each name
fb_search_urls <- function(names, dbs) {
  if (length(names) == 0 || length(dbs) == 0) return(NA_character_)
  urls <- vapply(names, function(nm) {
    words <- strsplit(nm, " ")[[1]]
    paste0(fb_web(dbs[[1]]$server), "/Nomenclature/ScientificNameSearchList.php?Genus=", words[1],
           if (length(words) >= 2) paste0("&Species=", words[2]) else "")
  }, character(1))
  paste(urls, collapse = " | ")
}

fb_worms_rest <- "https://www.marinespecies.org/rest/AphiaRecordsByMatchNames"

# Rule 3: looks names up in WoRMS (50 per request). Online, results replace those saved in
# worms_lookup.csv; offline, saved results are used and other names are "pending".
# `result` is "valid name", "no match" or "pending".
fb_worms_lookup <- function(names, data_dir) {
  cache_file <- file.path(data_dir, "fishbase", "worms_lookup.csv")
  columns <- c("name", "result", "status", "valid_name", "worms_url", "note", "lookup_date")
  cache <- if (file.exists(cache_file)) {
    fao_read_csv_utf8(cache_file, colClasses = "character")[columns]
  } else {
    as.data.frame(stats::setNames(rep(list(character(0)), length(columns)), columns))
  }
  unique_names <- unique(names)
  fresh <- NULL
  if (isTRUE(fao_setting("online"))) {
    fresh <- tryCatch(do.call(rbind, lapply(split(unique_names, ceiling(seq_along(unique_names) / 50)), fb_worms_query)),
                      error = function(e) {
                        message("Could not connect to WoRMS (", conditionMessage(e), "); using saved WoRMS results.")
                        NULL
                      })
  }
  if (!is.null(fresh)) {
    cache <- rbind(cache[!cache$name %in% fresh$name, ], fresh)
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    fao_write_csv(cache[order(cache$name), ], cache_file)
  }
  out <- cache[match(names, cache$name), ]
  pending <- is.na(out$name)
  out$name[pending] <- names[pending]
  out$result[pending] <- "pending"
  out$worms_url[pending] <- paste0("https://www.marinespecies.org/aphia.php?p=taxlist&tName=",
                                   utils::URLencode(names[pending], reserved = TRUE))
  rownames(out) <- NULL
  out
}

fb_worms_query <- function(names) {
  url <- paste0(fb_worms_rest, "?", paste0("scientificnames%5B%5D=", vapply(names, utils::URLencode, character(1),
                                                                             reserved = TRUE), collapse = "&"),
                "&marine_only=false")
  old <- options(timeout = 30)
  on.exit(options(old))
  text <- suppressWarnings(readLines(url, warn = FALSE, encoding = "UTF-8"))
  # WoRMS answers "204 No Content" when none of the names has a record
  records <- if (length(text) == 0) vector("list", length(names)) else jsonlite::fromJSON(paste(text, collapse = ""), simplifyVector = FALSE)
  do.call(rbind, lapply(seq_along(names), function(k) fb_worms_result(names[k], records[[k]])))
}

# Only an exact record with status "accepted", or with a single valid name, is used
fb_worms_result <- function(name, records) {
  search <- paste0("https://www.marinespecies.org/aphia.php?p=taxlist&tName=", utils::URLencode(name, reserved = TRUE))
  row <- function(result, status = NA_character_, valid = NA_character_, url = search, note = "") {
    data.frame(name = name, result = result, status = status, valid_name = valid, worms_url = url, note = note,
               lookup_date = as.character(Sys.Date()), stringsAsFactors = FALSE)
  }
  get <- function(r, field) if (is.null(r[[field]])) NA else r[[field]]
  exact <- Filter(function(r) identical(get(r, "match_type"), "exact"), records)
  if (length(exact) == 0) return(row("no match", note = "No exact WoRMS record"))
  valid_ids <- unique(stats::na.omit(vapply(exact, function(r) as.character(get(r, "valid_AphiaID")), character(1))))
  if (length(valid_ids) == 0) return(row("no match", note = "Exact WoRMS record without a valid name"))
  if (length(valid_ids) > 1) {
    return(row("no match", note = paste0("Several WoRMS records (valid AphiaID ", paste(valid_ids, collapse = ", "), ")")))
  }
  r <- exact[[1]]
  status <- get(r, "status")
  usable <- c("accepted", "unaccepted", "superseded combination", "superseded rank", "alternative representation")
  if (!status %in% usable) {
    return(row("no match", status, get(r, "valid_name"), get(r, "url"),
               paste0("WoRMS status '", status, "': not used automatically")))
  }
  row("valid name", status, get(r, "valid_name"), get(r, "url"))
}

# Applies the manual corrections (see ?fishbase_match). A template is created at the default
# location if there is no file yet.
fb_apply_corrections <- function(result, file, dbs, is_default) {
  columns <- c("alpha3", "species_en", "database", "SpecCode", setdiff(fb_trait_columns, "trophic_level_se"), "note")
  if (!file.exists(file)) {
    if (!is_default) stop("Corrections file not found: ", file, call. = FALSE)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    template <- as.data.frame(stats::setNames(rep(list(character(0)), length(columns)), columns))
    fao_write_csv(template, file)
    message("To correct a match, add a row to ", file, " (see ?fishbase_match).")
    return(result)
  }
  corr <- fao_read_csv_utf8(file, colClasses = "character")
  if (!"alpha3" %in% names(corr)) stop("The corrections file needs an alpha3 column: ", file, call. = FALSE)
  corr[setdiff(columns, names(corr))] <- ""
  corr[is.na(corr)] <- ""
  corr <- corr[corr$alpha3 != "", ]
  applied <- 0
  for (k in seq_len(nrow(corr))) {
    i <- which(result$alpha3 == corr$alpha3[k])
    if (length(i) == 0) next
    if (corr$SpecCode[k] != "") {
      server <- c(fishbase = "fishbase", sealifebase = "sealifebase")[tolower(corr$database[k])]
      if (is.na(server)) server <- if (identical(result$major_group[i[1]], "PISCES")) "fishbase" else "sealifebase"
      db <- dbs[[server]]
      j <- if (is.null(db)) NA else match(as.integer(corr$SpecCode[k]), db$traits$SpecCode)
      if (is.null(db) && !any(unlist(corr[k, setdiff(fb_trait_columns, "trophic_level_se")]) != "")) {
        warning("Correction for ", corr$alpha3[k], ": ", fb_server_name(server), " is not available, so SpecCode ",
                corr$SpecCode[k], " cannot be looked up, and the file gives no values; the correction is skipped.",
                call. = FALSE)
        next
      } else if (is.null(db)) {
        warning("Correction for ", corr$alpha3[k], ": ", fb_server_name(server), " is not available, so SpecCode ",
                corr$SpecCode[k], " cannot be looked up; only the values given in the file are applied.", call. = FALSE)
      } else if (is.na(j)) {
        warning("Correction for ", corr$alpha3[k], ": SpecCode ", corr$SpecCode[k], " not found in ",
                fb_server_name(server), ".", call. = FALSE)
        next
      } else {
        row <- fb_species_row(db, j, "manual", "", result$search_url[i[1]])
        row$worms_lookup <- NULL
        row$worms_url <- result$worms_url[i[1]]
        result[i, names(row)] <- row
      }
    }
    for (column in setdiff(fb_trait_columns, "trophic_level_se")) {
      if (corr[[column]][k] != "") {
        result[[column]][i] <- if (column == "habitat") corr[[column]][k] else as.numeric(corr[[column]][k])
      }
    }
    result$status[i] <- "matched"
    result$match_type[i] <- "manual"
    result$rule_source[i] <- paste0("Manual correction (", basename(file), ")", if (corr$note[k] != "") paste0(": ", corr$note[k]))
    result$match_note[i] <- ""
    applied <- applied + 1
  }
  if (applied > 0) message("Applied ", applied, " correction(s) from ", file)
  result
}
