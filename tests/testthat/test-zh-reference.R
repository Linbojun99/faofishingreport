# The Chinese function reference of the website (pkgdown/zh-reference/, built into
# vignettes/articles/zh/reference/ by data-raw/make_zh_reference.R) covers every help page.
# Runs from the source package only: the folders are not in the built package.

test_that("every help page has a complete and current Chinese translation", {
  root <- test_path("..", "..")
  skip_if_not(dir.exists(file.path(root, "pkgdown", "zh-reference")) && dir.exists(file.path(root, "man")))
  rd <- list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE)
  rd <- rd[!vapply(rd, function(f) any(grepl("\\\\keyword\\{internal\\}", readLines(f, warn = FALSE))), logical(1))]
  for (file in rd) {
    topic <- sub("[.]Rd$", "", basename(file))
    json <- file.path(root, "pkgdown", "zh-reference", paste0(topic, ".json"))
    expect_true(file.exists(json), info = topic)
    if (!file.exists(json)) next
    tr <- jsonlite::read_json(json)
    expect_identical(tr$rd_md5, unname(tools::md5sum(file)),
                     info = paste(topic, "changed: run data-raw/make_zh_reference.R extract and update the translation"))
    expect_false(isTRUE(tr$stale), info = topic)
    expect_true(nzchar(tr$zh$title), info = topic)
    for (field in c("description", "details", "value")) {
      if (nzchar(tr$en[[field]])) expect_true(nzchar(tr$zh[[field]]), info = paste(topic, field))
    }
    for (arg in names(tr$en$arguments)) expect_true(nzchar(tr$zh$arguments[[arg]]), info = paste(topic, arg))
    page <- file.path(root, "vignettes", "articles", "zh", "reference", paste0(topic, ".Rmd"))
    expect_true(file.exists(page), info = paste(topic, ": run data-raw/make_zh_reference.R build"))
  }
})
