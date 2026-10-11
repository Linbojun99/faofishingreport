# FishBase data layer, tested offline on a small fake copy of two releases

local_fishbase_copy <- function(env = parent.frame()) {
  skip_if_not_installed("nanoparquet")
  dir <- withr::local_tempdir(.local_envir = env)
  write <- function(version, table, data) {
    path <- file.path(dir, "fishbase", "fb", version)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    nanoparquet::write_parquet(data, file.path(path, paste0(table, ".parquet")))
  }
  write("25.04", "estimate", data.frame(SpecCode = 1:2, Troph = c(3.1, 4.2)))
  write("25.04", "ecology", data.frame(SpecCode = 1L, Neritic = -1L))
  write("26.06", "estimate", data.frame(SpecCode = 1:3, Troph = c(3.0, 4.2, 2.5)))
  withr::local_options(faofishingreport.online = FALSE, faofishingreport.data_dir = dir, .local_envir = env)
  .fb_cache$tables <- list()
  .fb_cache$releases <- list()
  dir
}

test_that("offline, the newest downloaded release that has the table is used", {
  local_fishbase_copy()
  estimate <- suppressMessages(fishbase_table("estimate"))
  expect_equal(attr(estimate, "fishbase_version"), "26.06")
  expect_equal(nrow(estimate), 3)
  expect_match(attr(estimate, "source_url"), "fb/v26.06/parquet/estimate.parquet$")
  ecology <- suppressMessages(fishbase_table("ecology"))
  expect_equal(attr(ecology, "fishbase_version"), "25.04")
  expect_equal(nrow(suppressMessages(fishbase_table("estimate", version = "25.04"))), 2)
})

test_that("offline, a missing table or release gives a clear error", {
  local_fishbase_copy()
  expect_error(suppressMessages(fishbase_table("diet")), "download_fishbase_data")
  expect_error(suppressMessages(fishbase_table("estimate", version = "24.07")), "has not been downloaded")
  expect_error(suppressMessages(fishbase_table("estimate", server = "sealifebase")), "SeaLifeBase")
})

test_that("downloaded releases are listed in order", {
  dir <- local_fishbase_copy()
  expect_equal(fb_local_versions("fishbase", dir), c("25.04", "26.06"))
  expect_equal(fb_local_versions("fishbase", dir, "ecology"), "25.04")
  expect_equal(sort(fb_local_tables("fishbase", "25.04", dir)), c("ecology", "estimate"))
})

test_that("S3 listings are parsed", {
  xml <- paste0("<ListBucketResult><IsTruncated>false</IsTruncated>",
                "<Contents><Key>cboettig/fishbase/fb/v26.06/parquet/species.parquet</Key><Size>3469911</Size></Contents>",
                "<CommonPrefixes><Prefix>cboettig/fishbase/fb/v26.06/</Prefix></CommonPrefixes></ListBucketResult>")
  expect_equal(fb_xml_values(xml, "Key"), "cboettig/fishbase/fb/v26.06/parquet/species.parquet")
  expect_equal(fb_xml_values(xml, "Size"), "3469911")
  expect_equal(fb_xml_values(xml, "IsTruncated"), "false")
  expect_equal(fb_file_url("sealifebase", "26.06", "species"),
               "https://data.source.coop/cboettig/fishbase/slb/v26.06/parquet/species.parquet")
})

test_that("the online setting only accepts TRUE or FALSE", {
  withr::defer(fao_settings(reset = TRUE))
  expect_error(fao_settings(online = "no"), "TRUE or FALSE")
  fao_settings(online = FALSE)
  expect_false(fao_setting("online"))
  expect_null(fb_online_releases("fishbase"))
})

test_that("the data folder comes from the setting, else the environment variable", {
  withr::local_options(faofishingreport.data_dir = NULL)
  withr::local_envvar(FAOFISHINGREPORT_DATA_DIR = "/tmp/shared_fao_data")
  expect_equal(fao_data_dir(), "/tmp/shared_fao_data")
  withr::local_options(faofishingreport.data_dir = "/tmp/session_folder")
  expect_equal(fao_data_dir(), "/tmp/session_folder")
  withr::local_options(faofishingreport.data_dir = NULL)
  withr::local_envvar(FAOFISHINGREPORT_DATA_DIR = "")
  expect_equal(fao_data_dir(), tools::R_user_dir("faofishingreport", which = "data"))
})

test_that("the species groups have FAO codes and scientific names", {
  species <- fao_group_table()$species
  expect_true(all(c("alpha3", "scientific_name") %in% names(species)))
  expect_equal(species$scientific_name[species$name_en == "European pilchard(=Sardine)"], "Sardina pilchardus")
})
