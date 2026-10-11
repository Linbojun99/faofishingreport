#' Sources, Licences and Matching Rules of the FishBase Data
#'
#' This function lists where the FishBase, SeaLifeBase and WoRMS data used by the package come
#' from, under which licence, how to cite them, which tables are downloaded and what they hold,
#' and the rules (with their published sources) by which `fishbase_match()` matches FAO species
#' items to them. `download_fishbase_data()` prints a summary of the same information.
#'
#' @param print A logical value. If TRUE (default), the list is printed.
#' @return Invisibly, a data frame with one row per data source, table or rule: `type`, `item`,
#'   `description`, `source` (citation or licence) and `link`.
#' @references
#' Boettiger, C., Lang, D.T. and Wainwright, P.C. (2012). rfishbase: exploring, manipulating and
#' visualizing FishBase data from R. Journal of Fish Biology 81(6): 2030-2039.
#' \doi{10.1111/j.1095-8649.2012.03464.x}. Source code: \url{https://github.com/ropensci/rfishbase}
#'
#' Branch, T.A., Watson, R., Fulton, E.A. et al. (2010). The trophic fingerprint of marine
#' fisheries. Nature 468: 431-435. \doi{10.1038/nature09528}
#'
#' FAO (2026). Nomenclature, revision, and references of the ASFIS list (version 2026). Rome, FAO.
#' \url{https://www.fao.org/fishery/static/ASFIS/ASFIS_References_2026_EN.pdf}
#'
#' Froese, R. and Pauly, D. (Eds.) (2000). FishBase 2000: concepts, design and data sources. ICLARM,
#' Los Baños, Philippines. \url{https://www.fishbase.se/manual/fishbasethe_food_items_table.htm},
#' \url{https://www.fishbase.se/manual/fishbasethe_synonyms_table.htm}
#'
#' Froese, R. and Pauly, D. (Eds.) FishBase. World Wide Web electronic publication.
#' \url{https://www.fishbase.se}
#'
#' Liang, C. and Pauly, D. (2017). Fisheries impacts on China's coastal ecosystems: unmasking a
#' pervasive 'fishing down' effect. PLoS ONE 12: e0173296. \doi{10.1371/journal.pone.0173296}
#'
#' Liang, C., Xian, W. and Pauly, D. (2018). Impacts of ocean warming on China's fisheries
#' catches: an application of "mean temperature of the catch" concept. Frontiers in Marine
#' Science 5: 26. \doi{10.3389/fmars.2018.00026}
#'
#' Palomares, M.L.D. and Pauly, D. (Eds.) SeaLifeBase. World Wide Web electronic publication.
#' \url{https://www.sealifebase.se}
#'
#' Pauly, D., Christensen, V., Dalsgaard, J., Froese, R. and Torres, F. (1998). Fishing down
#' marine food webs. Science 279: 860-863. \doi{10.1126/science.279.5352.860}
#'
#' Tsikliras, A.C. and Stergiou, K.I. (2014). Mean temperature of the catch increases quickly in
#' the Mediterranean Sea. Marine Ecology Progress Series 515: 281-284. \doi{10.3354/meps11005}
#'
#' WoRMS Editorial Board. World Register of Marine Species. Flanders Marine Institute (VLIZ).
#' \url{https://www.marinespecies.org}, \doi{10.14284/170}
#' @export
#' @examples
#' \dontrun{
#' fishbase_sources()
#' }
fishbase_sources <- function(print = TRUE) {
  builtin <- tryCatch(fb_builtin_table(), error = function(e) NULL)
  builtin_text <- if (is.null(builtin)) "not available" else
    paste0(paste(fb_server_name(names(attr(builtin, "fishbase_version"))), attr(builtin, "fishbase_version"), collapse = ", "),
           ", made on ", attr(builtin, "built"), " for the ", nrow(builtin), " species items of FAO release ",
           attr(builtin, "fao_release"))
  rows <- list(
    c("data", "FishBase", "Fishes: names, classification, ecology, trophic level, temperature preference, resilience",
      "Froese, R. and Pauly, D. (Eds.) FishBase. www.fishbase.se. Licence CC BY-NC 4.0 (FishBase.org team): cite, no commercial use",
      "https://www.fishbase.se"),
    c("data", "SeaLifeBase", "Other aquatic animals and plants: the same tables as FishBase",
      "Palomares, M.L.D. and Pauly, D. (Eds.) SeaLifeBase. www.sealifebase.se. Licence CC BY-NC 4.0 (FishBase.org team)",
      "https://www.sealifebase.se"),
    c("data", "Snapshots", "Versioned copies of the FishBase and SeaLifeBase tables as parquet files, the files downloaded by the package",
      "Published by the rfishbase project of rOpenSci (Boettiger et al. 2012, J. Fish Biol. 81(6): 2030-2039, doi:10.1111/j.1095-8649.2012.03464.x)",
      "https://data.source.coop/cboettig/fishbase/"),
    c("data", "rfishbase", "R package that publishes the snapshots and reads them; faofishingreport reads the same files directly, without depending on it",
      "Boettiger, C., Lang, D.T. and Wainwright, P.C. (2012). J. Fish Biol. 81(6): 2030-2039. Source code and documentation: https://github.com/ropensci/rfishbase, https://docs.ropensci.org/rfishbase/",
      "https://github.com/ropensci/rfishbase"),
    c("data", "WoRMS", "Valid names of species and taxa, looked up online for names not found in FishBase or SeaLifeBase",
      "WoRMS Editorial Board. World Register of Marine Species. VLIZ. doi:10.14284/170. Licence CC BY",
      "https://www.marinespecies.org"),
    c("table", "species", "Genus, species, family code, habitat (DemersPelag), maximum length (Length), vulnerability to fishing", "FishBase/SeaLifeBase", ""),
    c("table", "estimate", "Trophic level (Troph, seTroph) from diet studies or, when missing, related species and size; preferred temperature (TempPrefMean, AquaMaps distribution; FishBase ref. 123201); prior r",
      "Froese & Pauly (Eds.) 2000, FishBase 2000: concepts, design and data sources, 'The FOOD ITEMS table', boxes 25-26",
      "https://www.fishbase.se/manual/fishbasethe_food_items_table.htm"),
    c("table", "families", "Family, order, class and phylum", "FishBase/SeaLifeBase", ""),
    c("table", "synonyms", "Former and other names with their status (accepted name, synonym, misapplied name, ambiguous synonym)", "FishBase/SeaLifeBase", ""),
    c("table", "stocks", "Resilience (High, Medium, Low, Very low)", "FishBase/SeaLifeBase", ""),
    c("table", "faoareas", "Occurrence of each species by FAO major fishing area", "FishBase/SeaLifeBase", ""),
    c("rule", "1. exact", "Fishes looked up in FishBase first, others in SeaLifeBase first; the FAO scientific name is a valid species name", fb_rule_sources[["exact"]], ""),
    c("rule", "2. synonym", "The FAO name is a synonym (status 'synonym') of one species; misapplied names never used, ambiguous synonyms not matched", fb_rule_sources[["synonym"]], ""),
    c("rule", "3. WoRMS valid name", "Names not found by rules 1-2 are looked up online in WoRMS; only an exact record with status 'accepted' or a single valid name is used, then matched by rules 1, 2 or 4",
      "The FAO ASFIS list follows WoRMS for most groups: FAO (2026) Nomenclature, revision, and references of the ASFIS list (version 2026), pp. 1-3",
      "https://www.fao.org/fishery/static/ASFIS/ASFIS_References_2026_EN.pdf"),
    c("rule", "4. taxon above species", "Unweighted mean trophic level of the species of the genus, family, order, class or phylum; other values left empty",
      paste0(fb_rule_sources[["group"]], "; MTC studies exclude items above species: Tsikliras & Stergiou 2014 MEPS 515:281; Liang et al. 2018 Front Mar Sci 5:26"), ""),
    c("rule", "5. plants", "Aquatic plants have trophic level 1", fb_rule_sources[["plant"]], ""),
    c("rule", "not matched", "Species not found (no values borrowed from the genus), items naming several taxa, uncertain or several WoRMS records: marked as failed, with search links",
      "No published rule for these cases", ""),
    c("rule", "corrections", "Manual corrections in corrections.csv in the fishbase folder of fao_data_dir(), applied every time; the note column should give the source", "User", ""),
    c("data", "Built-in table", paste0("Used only when FishBase cannot be reached and has not been downloaded: ", builtin_text),
      "Made by data-raw/build_fishbase_traits.R with the rules above; CC BY-NC 4.0 (FishBase.org team)", "")
  )
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("type", "item", "description", "source", "link")
  if (print) {
    for (type in c("data", "table", "rule")) {
      cat(c(data = "\nData sources\n", table = "\nTables downloaded (download_fishbase_data(tables = \"needed\"))\n",
            rule = "\nMatching rules of fishbase_match()\n")[[type]])
      x <- out[out$type == type, ]
      for (i in seq_len(nrow(x))) {
        cat("  ", x$item[i], ": ", x$description[i], "\n", sep = "")
        if (x$source[i] != "") cat("      Source: ", x$source[i], "\n", sep = "")
        if (x$link[i] != "") cat("      Link:   ", x$link[i], "\n", sep = "")
      }
    }
  }
  invisible(out)
}

# Summary printed before FishBase data are downloaded
fb_download_notice <- function(server, version, tables, data_dir) {
  message("FishBase data to download")
  message("  Databases: ", paste(paste(fb_server_name(server), version), collapse = ", "),
          " (FishBase: fishes; SeaLifeBase: other aquatic animals and plants)")
  message("  Tables:    ", if (is.null(tables)) "all" else paste(tables, collapse = ", "))
  message("  From:      https://data.source.coop/cboettig/fishbase/ (snapshots published by the rfishbase project, ",
          "https://github.com/ropensci/rfishbase)")
  message("  Licence:   CC BY-NC 4.0, FishBase.org team: cite FishBase (Froese & Pauly, Eds.) and SeaLifeBase ",
          "(Palomares & Pauly, Eds.) with the release; no commercial use")
  message("  Used by:   fishbase_match(), which matches FAO species items only by published rules ",
          "(exact name, official synonym, WoRMS valid name, mean trophic level of a taxon, trophic level 1 for plants)")
  message("  Saved in:  ", file.path(data_dir, "fishbase"))
  message("  Details:   fishbase_sources()")
}
