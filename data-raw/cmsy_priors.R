# Builds inst/extdata/cmsy_priors.csv: resilience and prior r of the species-level FAO items of the
# capture statistics, from the FishBase and SeaLifeBase releases recorded in cmsy_sources().
# Run from the package root: Rscript data-raw/cmsy_priors.R (needs nanoparquet and the FAO data).

pkgload::load_all(".", quiet = TRUE)

version <- "26.06"
tables <- c("species", "synonyms", "stocks", "estimate")
dir <- Sys.getenv("FB_PARQUET_DIR", file.path(tempdir(), "fishbase"))   # reuse earlier downloads if set
dir.create(dir, showWarnings = FALSE)
options(timeout = 1200)

read_tables <- function(code) {
  out <- lapply(tables, function(table) {
    url <- paste0("https://data.source.coop/cboettig/fishbase/", code, "/v", version, "/parquet/", table, ".parquet")
    file <- file.path(dir, paste0(code, "_", table, ".parquet"))
    if (!file.exists(file)) utils::download.file(url, file, mode = "wb", quiet = TRUE)
    as.data.frame(nanoparquet::read_parquet(file))
  })
  names(out) <- tables
  out
}
fb <- list(fishbase = read_tables("fb"), sealifebase = read_tables("slb"))

# Species-level items that have catch in the FAO capture statistics
capture <- load_fao_data()
groups <- fao_group_table()$species
items <- groups[groups$taxon_level == "species" & groups$name_en %in% unique(capture$ASFIS.species..Name.),
                c("name_en", "alpha3", "scientific_name", "major_en")]
items <- items[!duplicated(items$alpha3), ]
names(items)[names(items) == "name_en"] <- "species_en"

priors <- cmsy_build_priors(items, fb, c(fishbase = version, sealifebase = version))
attr_line <- paste0("# FAO capture ", attr(capture, "fao_version"), "; FishBase and SeaLifeBase ", version,
                    "; built ", Sys.Date())
message(attr_line)
print(table(priors$match_type))
print(table(is.na(priors$r_source)))

utils::write.csv(priors, "inst/extdata/cmsy_priors.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")
