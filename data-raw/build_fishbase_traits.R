# Builds inst/extdata/fishbase_traits.rda: the result of fishbase_match() for every species item
# of the FAO data bundled with the package, without manual corrections. fishbase_match() uses it
# only when FishBase cannot be reached and has not been downloaded (or with version = "builtin").
#
# Rebuild it when FishBase publishes a new release or the bundled FAO data is updated. It must be
# run online, so that names can be looked up in WoRMS. Run from the package root:
#   Rscript data-raw/build_fishbase_traits.R
#
# The values come from FishBase and SeaLifeBase (CC BY-NC 4.0, FishBase.org team) and WoRMS (CC BY);
# see inst/COPYRIGHTS and fishbase_sources().

pkgload::load_all(".")

fao <- load_fao_data("bundled")
match <- fishbase_match(fao, corrections = FALSE, table = FALSE, label = "builtin")
if (any(grepl("built-in", match$source)) || any(match$status == "pending online check")) {
  stop("FishBase or WoRMS could not be reached: run this script online.")
}

keep <- c("alpha3", "species_en", "fao_scientific_name", "taxon_level", "major_group", "status", "match_type",
          "database", "matched_name", "SpecCode", "n_species", "trophic_level", "trophic_level_se",
          "temp_pref_mean", "habitat", "max_length_cm", "vulnerability", "url", "search_url", "worms_url",
          "match_note", "rule_source", "source")
fishbase_traits <- match[keep]
rownames(fishbase_traits) <- NULL
attr(fishbase_traits, "fishbase_version") <- attr(match, "fishbase_version")
attr(fishbase_traits, "fao_release") <- fao_bundled_release()
attr(fishbase_traits, "built") <- as.character(Sys.Date())
save(fishbase_traits, file = "inst/extdata/fishbase_traits.rda", compress = "xz")

message("Built-in FishBase table: ", nrow(fishbase_traits), " FAO species items, ",
        paste(names(attr(fishbase_traits, "fishbase_version")), attr(fishbase_traits, "fishbase_version"), collapse = ", "),
        ", FAO release ", fao_bundled_release(), "; ",
        paste(names(table(fishbase_traits$status)), table(fishbase_traits$status), collapse = ", "))
