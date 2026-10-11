# Chinese names of the FAO species items that have none in the FAO code lists, from the common
# names of FishBase and SeaLifeBase: the names given for China (country code 156) in Mandarin
# Chinese, written in simplified characters. Only items matched to one species in the table
# bundled with the package (inst/extdata/fishbase_traits.rda) get a name. Names that FishBase
# marks as applied to a synonym ("Name applied to ...") are left out. The name used is the
# preferred one of FishBase (Rank 1), else the most frequent of the simplified names (Rank 2;
# Rank 3 names are the traditional forms); when three or more names are equally frequent (e.g.
# the regional names of species that FishBase has merged), no name is used. The other names are
# kept in `other_names`. The tables are the parquet files of FishBase and SeaLifeBase that the
# rfishbase package reads (https://github.com/ropensci/rfishbase), hosted on Source Cooperative.
#
# Run from the package root: Rscript data-raw/build_species_names_zh.R
# Writes inst/extdata/species_names_zh_fishbase.csv, used by fao_translate() after the FAO names.

pkgload::load_all(".", quiet = TRUE)
release <- "26.06"
tmp <- tempfile("comnames_")
dir.create(tmp)
comnames <- function(server) {
  url <- sprintf("https://data.source.coop/cboettig/fishbase/%s/v%s/parquet/comnames.parquet", server, release)
  file <- file.path(tmp, paste0(server, ".parquet"))
  utils::download.file(url, file, mode = "wb", quiet = TRUE)
  x <- nanoparquet::read_parquet(file)
  x <- x[x$Language == "Mandarin Chinese" & x$C_Code %in% "156" & x$Script == "Chinese" &
           !grepl("^Name applied to", x$Remarks), c("SpecCode", "ComName", "Rank")]
  # Chinese characters only (no Latin letters or garbled text), and all of them simplified
  # characters: names that cannot be written in GB 2312 hold traditional characters
  x[grepl("^[\\u4e00-\\u9fff]+$", x$ComName) & !is.na(iconv(x$ComName, "UTF-8", "GB2312")), ]
}
names_fb <- list(FishBase = comnames("fb"), SeaLifeBase = comnames("slb"))

traits_env <- new.env()
load(file.path("inst", "extdata", "fishbase_traits.rda"), envir = traits_env)
traits <- traits_env[[ls(traits_env)[1]]]
traits <- traits[!is.na(traits$SpecCode) & traits$taxon_level == "species" & traits$database %in% names(names_fb), ]

official <- fao_name_table()
official <- official$name_en[official$type == "species" & !is.na(official$name_zh)]
traits <- traits[!traits$species_en %in% official, ]

rows <- lapply(seq_len(nrow(traits)), function(i) {
  hits <- names_fb[[traits$database[i]]]
  hits <- hits[hits$SpecCode == traits$SpecCode[i], ]
  if (nrow(hits) == 0) return(NULL)
  counts <- table(factor(hits$ComName, levels = unique(hits$ComName)))
  preferred <- unique(hits$ComName[hits$Rank %in% 1])
  if (length(preferred) > 0) {
    ranked <- c(preferred[1], setdiff(names(counts), preferred[1]))
  } else {
    if (sum(counts == max(counts)) >= 3) return(NULL)
    ranked <- names(sort(counts, decreasing = TRUE))
  }
  data.frame(alpha3 = traits$alpha3[i], name_en = traits$species_en[i],
             scientific_name = traits$fao_scientific_name[i], database = traits$database[i],
             SpecCode = traits$SpecCode[i], name_zh = ranked[1],
             other_names = paste(ranked[-1], collapse = "/"), stringsAsFactors = FALSE)
})
out <- do.call(rbind, rows)
out <- out[order(out$name_en), ]
out$source <- paste0(out$database, " ", release, ", common names (China, Mandarin Chinese)")
fao_write_csv(out, file.path("inst", "extdata", "species_names_zh_fishbase.csv"))
message(nrow(out), " Chinese names of ", nrow(traits), " species items without an FAO Chinese name")
