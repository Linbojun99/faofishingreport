# Matching rules of fishbase_match(), tested offline on a small fake database

fake_db <- function(server = "fishbase") {
  traits <- data.frame(
    SpecCode = 1:5,
    Genus = c("Sardina", "Sardinella", "Sardinella", "Rhombosepion", "Dentex"),
    Species = c("pilchardus", "aurita", "maderensis", "orbignyanum", "dentex"),
    FamCode = c(1L, 1L, 1L, 2L, 3L),
    DemersPelag = c("pelagic-neritic", "pelagic-neritic", "pelagic-neritic", "demersal", "demersal"),
    Length = c(27.5, 41, 37, 12, 100), Vulnerability = c(37, 30, 31, 20, 60),
    Troph = c(3.06, 3.40, 3.20, 3.80, 4.50), seTroph = c(0.1, 0.4, 0.3, 0.5, 0.6),
    TempPrefMean = c(10.3, 18.8, 21.0, 15.0, 17.0),
    Family = c("Clupeidae", "Clupeidae", "Clupeidae", "Sepiidae", "Sparidae"),
    Order = c("Clupeiformes", "Clupeiformes", "Clupeiformes", "Sepiida", "Perciformes/Percoidei"),
    Class = c("Teleostei", "Teleostei", "Teleostei", "Cephalopoda", "Teleostei"),
    Phylum = NA, stringsAsFactors = FALSE
  )
  traits$key <- tolower(paste(traits$Genus, traits$Species))
  synonyms <- data.frame(SpecCode = c(4L, 2L, 3L, 1L), SynGenus = c("Sepia", "Clupea", "Clupea", "Sardina"),
                         SynSpecies = c("orbignyana", "aurita", "aurita", "pilchardus"),
                         Status = c("synonym", "synonym", "synonym", "accepted name"), stringsAsFactors = FALSE)
  synonyms$key <- tolower(paste(synonyms$SynGenus, synonyms$SynSpecies))
  index <- function(x) split(seq_along(x), tolower(x))
  list(server = server, version = "26.06", traits = traits, synonyms = synonyms,
       by_rank = list(genus = index(traits$Genus), family = index(traits$Family),
                      order = index(sub("/.*$", "", traits$Order)), class = index(traits$Class),
                      phylum = index(traits$Phylum)),
       synonym_index = index(synonyms$key))
}

match_name <- function(name, level = "species") fb_match_local(fb_parse_name(name, level), list(fake_db()))

test_that("FAO scientific names are read as species, taxa or several taxa", {
  expect_equal(fb_parse_name("Sardina pilchardus", "species")$kind, "species")
  expect_equal(fb_parse_name("Penaeus (Melicertus) kerathurus", "species")$name, "Penaeus kerathurus")
  expect_equal(fb_parse_name("Sardinella spp", "genus")[c("kind", "name")], list(kind = "taxon", name = "Sardinella"))
  expect_equal(fb_parse_name("Clupeiformes (=Clupeoidei)", "higher")$name, "Clupeiformes")
  several <- fb_parse_name("Auxis thazard, A. rochei", "species")
  expect_equal(several$kind, "several")
  expect_equal(several$names, c("Auxis thazard", "Auxis rochei"))
  expect_equal(fb_parse_name(NA, "species")$kind, "none")
})

test_that("rule 1: exact valid names give the species values and page", {
  row <- match_name("Sardina pilchardus")
  expect_equal(row$status, "matched")
  expect_equal(row$match_type, "exact")
  expect_equal(row$trophic_level, 3.06)
  expect_equal(row$temp_pref_mean, 10.3)
  expect_equal(row$url, "https://www.fishbase.se/summary/Sardina-pilchardus.html")
  expect_match(row$rule_source, "Branch et al. 2010")
})

test_that("rule 2: synonyms are used, ambiguous synonyms are not", {
  row <- match_name("Sepia orbignyana")
  expect_equal(row$match_type, "synonym")
  expect_equal(row$matched_name, "Rhombosepion orbignyanum")
  ambiguous <- match_name("Clupea aurita")
  expect_equal(ambiguous$status, "failed")
  expect_match(ambiguous$match_note, "Ambiguous synonym")
  expect_true(ambiguous$worms_lookup)
})

test_that("species not in the database are not given the values of their genus", {
  row <- match_name("Sardinella longiceps")
  expect_equal(row$status, "failed")
  expect_true(is.na(row$trophic_level))
  expect_true(row$worms_lookup)
  expect_match(row$search_url, "Genus=Sardinella&Species=longiceps")
})

test_that("rule 4: taxa above species take the unweighted mean trophic level only", {
  row <- match_name("Sardinella spp", "genus")
  expect_equal(row$match_type, "genus")
  expect_equal(row$trophic_level, round(mean(c(3.40, 3.20)), 2))
  expect_true(is.na(row$temp_pref_mean))
  expect_true(is.na(row$habitat))
  expect_equal(row$n_species, 2L)
  expect_equal(match_name("Clupeidae", "family")$trophic_level, round(mean(c(3.06, 3.40, 3.20)), 2))
  expect_equal(match_name("Perciformes", "higher")$match_type, "order")
  expect_equal(match_name("Percoidei", "higher")$status, "failed")
})

test_that("items naming several taxa are not matched", {
  row <- match_name("Sepiidae, Sepiolidae", "family")
  expect_equal(row$status, "failed")
  expect_false(row$worms_lookup)
  expect_match(row$search_url, "Sepiidae.*\\|.*Sepiolidae")
})

test_that("rule 5: aquatic plants have trophic level 1", {
  row <- fb_plant_row(fb_parse_name("Rhodophyta", "higher"), list(fake_db("sealifebase")))
  expect_equal(row$trophic_level, 1)
  expect_match(row$rule_source, "Pauly et al. 1998")
})

test_that("rule 3: only exact WoRMS records with a usable status are used", {
  record <- function(status, valid, id, match = "exact") {
    list(match_type = match, status = status, valid_name = valid, valid_AphiaID = id, url = "https://www.marinespecies.org/x")
  }
  expect_equal(fb_worms_result("Liza aurata", list(record("unaccepted", "Chelon auratus", 1L)))$valid_name, "Chelon auratus")
  expect_equal(fb_worms_result("Liza", list(record("uncertain", "Chelon", 2L)))$result, "no match")
  expect_equal(fb_worms_result("Sardina pilchardux", list(record("accepted", "Sardina pilchardus", 3L, "phonetic")))$result,
               "no match")
  expect_equal(fb_worms_result("Xus", list(record("accepted", "Xus", 4L), record("accepted", "Xus", 5L)))$result, "no match")
  expect_equal(fb_worms_result("Yus", list())$note, "No exact WoRMS record")
})

test_that("offline, names not looked up in WoRMS before are pending", {
  dir <- withr::local_tempdir()
  withr::local_options(faofishingreport.online = FALSE)
  out <- fb_worms_lookup(c("Polybius navigator", "Liza"), dir)
  expect_equal(out$result, c("pending", "pending"))
  expect_match(out$worms_url[1], "taxlist&tName=Polybius%20navigator")
})

test_that("without FishBase, the built-in table is used; items not in it are pending", {
  dir <- withr::local_tempdir()
  withr::local_options(faofishingreport.online = FALSE, faofishingreport.data_dir = dir)
  withr::local_dir(withr::local_tempdir())
  area <- local({
    data <- load_fao_data("bundled")
    data[data$FAO.major.fishing.area..Name. == "Atlantic, Eastern Central", ]
  })
  expect_message(m <- suppressWarnings(fishbase_match(area, table = FALSE, corrections = FALSE)), "built into the package")
  pilchard <- m[m$species_en == "European pilchard(=Sardine)", ]
  expect_equal(pilchard$status, "matched")
  expect_match(pilchard$source, "built-in table")
  expect_false(is.na(pilchard$trophic_level))
  forced <- suppressMessages(fishbase_match(area, version = "builtin", table = FALSE, corrections = FALSE))
  expect_equal(forced$trophic_level, m$trophic_level)
  new_item <- area[1, ]
  new_item$ASFIS.species..Name. <- "A species added in a later FAO release"
  pending <- suppressMessages(fishbase_match(rbind(area, new_item), version = "builtin", table = FALSE, corrections = FALSE))
  expect_equal(pending$status[pending$species_en == new_item$ASFIS.species..Name.], "pending online check")
  expect_length(list.files(), 0)
})

test_that("the built-in table was made with the matching rules", {
  builtin <- fb_builtin_table()
  expect_true(all(c("alpha3", "status", "match_type", "rule_source", "url") %in% names(builtin)))
  expect_false(any(builtin$status == "pending online check"))
  expect_true(all(builtin$rule_source[builtin$status == "matched"] != ""))
  expect_true(all(is.na(builtin$temp_pref_mean[builtin$match_type %in% c("genus", "family", "order", "class", "phylum")])))
  expect_named(attr(builtin, "fishbase_version"), c("fishbase", "sealifebase"))
})

test_that("fishbase_sources() lists data, tables and every rule with a source", {
  sources <- fishbase_sources(print = FALSE)
  expect_true(all(c("data", "table", "rule") %in% sources$type))
  expect_true(all(c("FishBase", "SeaLifeBase", "WoRMS") %in% sources$item))
  expect_true(all(fb_needed_tables %in% sources$item[sources$type == "table"]))
  expect_match(sources$source[sources$item == "FishBase"], "CC BY-NC 4.0")
  rules <- sources[sources$type == "rule", ]
  expect_true(all(rules$source != ""))
})
