test_that("report numbers are Chinese numerals in Chinese reports", {
  expect_identical(fao_report_number(1, "zh"), "一")
  expect_identical(fao_report_number(12, "zh"), "十二")
  expect_identical(fao_report_number(20, "zh"), "二十")
  expect_identical(fao_report_number(3, "en"), "3")
})

test_that("Chinese text has no spaces next to Chinese characters", {
  expect_identical(fao_report_zh_spacing("2024 年 509.9 万 t"), "2024年509.9万t")
  expect_identical(fao_report_zh_spacing("1.1 数据", numbered = TRUE), "1.1 数据")
  expect_identical(fao_report_zh_spacing("图1-2 渔获量", numbered = TRUE), "图1-2 渔获量")
  expect_identical(fao_report_zh_spacing("Mann-Kendall test"), "Mann-Kendall test")
})

test_that("references follow GB/T 7714, APA and FAO styles", {
  ref <- list(type = "J", author = "Kleisner, K.; Zeller, D.; Froese, R.; Pauly, D.", year = "2013",
              title = "Using global catch data", container = "Fish and Fisheries", volume = "14",
              issue = "3", pages = "293-311")
  expect_identical(fao_ref_entry(ref, "numeric"),
                   "Kleisner K, Zeller D, Froese R, et al. Using global catch data[J]. Fish and Fisheries, 2013, 14(3): 293-311.")
  expect_match(fao_ref_entry(ref, "apa"), "^Kleisner, K., Zeller, D., Froese, R., & Pauly, D. \\(2013\\)")
  expect_match(fao_ref_entry(ref, "fao"), "^Kleisner, K., Zeller, D., Froese, R. & Pauly, D. 2013.")
  expect_identical(fao_ref_short("Bai, J.; Perron, P.", "apa"), "Bai & Perron")
  expect_identical(fao_ref_short(ref$author, "apa"), "Kleisner et al.")
})

test_that("report text templates exist in both languages", {
  texts <- fao_extdata_csv("report_text.csv")
  texts <- texts[texts$key != "abstract_results", ]   # "Results show that" has no English sentence
  expect_false(any(is.na(texts$en) | texts$en == ""))
  expect_false(any(is.na(texts$zh) | texts$zh == ""))
  expect_match(fao_report_text("report_no", "en", list(no = 2)), "Report 2")
})

test_that("proceedings and databases with editors are formatted", {
  jags <- list(type = "C", author = "Plummer, M.", year = "2003", title = "JAGS",
               container = "Proceedings of DSC 2003", place = "Vienna", pages = "1-10")
  expect_identical(fao_ref_entry(jags, "numeric"), "Plummer M. JAGS[C]//Proceedings of DSC 2003. Vienna, 2003: 1-10.")
  expect_match(fao_ref_entry(jags, "apa"), "In \\*Proceedings of DSC 2003\\* \\(pp. 1\u201310\\)")
  fishbase <- list(type = "DB", author = "Froese, R.; Pauly, D.", editors = TRUE, year = "2026",
                   title = "FishBase", version = "version 06/2026", accessed = "2026-10-10",
                   url = "https://www.fishbase.se")
  expect_match(fao_ref_entry(fishbase, "apa"), "^Froese, R., & Pauly, D. \\(Eds.\\). \\(2026\\). \\*FishBase\\* \\(Version 06/2026\\)")
  expect_match(fao_ref_entry(fishbase, "fao"), "^Froese, R. & Pauly, D., eds. 2026. FishBase.")
})

test_that("catch ranges are written in one unit", {
  expect_identical(fao_report_catch_range(775500, 1076000, "en"), "0.78\u20131.08 million t")
  expect_identical(fao_report_catch_range(48200, 82400, "en"), "48.2\u201382.4 thousand t")
  expect_identical(fao_report_catch_range(5618000, 9915000, "zh"), "561.8\u2013991.5 万 t")
})

test_that("settings of the analyses are passed by section prefix", {
  sections <- fao_report_sections()
  args <- fao_report_args(list(species_h_value = 0.2, status_smooth = TRUE, cmsy_top = 5), sections)
  expect_identical(args$species$h_value, 0.2)
  expect_true(args$status$smooth)
  expect_identical(args$cmsy$top, 5)
  expect_error(fao_report_args(list(species_hvalue = 0.2), sections), "not an argument of fishing_area_species")
  expect_error(fao_report_args(list(spcies_h_value = 0.2), sections), "Unknown argument")
  expect_error(fao_report_args(list(0.2), sections), "must be named")
})

test_that("new sections take their settings and extra settings", {
  sections <- fao_report_sections()
  expect_true(all(c("shocks", "collapse", "synchrony", "pelagic", "mtc", "distant") %in% names(sections)))
  args <- fao_report_args(list(collapse_method = "worm2006", collapse_seed = 2, distant_countries = 5,
                               mtc_version = "builtin", pelagic_definition = "grainger_garcia"), sections)
  expect_identical(args$collapse$method, "worm2006")
  expect_identical(args$collapse$seed, 2)
  expect_identical(args$distant$countries, 5)
  expect_error(fao_report_args(list(shocks_seed = 1), sections), "not an argument of catch_shocks")
  # every method of a section has its paragraph
  methods <- unique(unlist(lapply(sections, `[[`, "methods")))
  expect_true(all(methods %in% names(fao_report_methods)))
  texts <- fao_extdata_csv("report_text.csv")
  expect_true(all(fao_report_methods %in% texts$key))
  expect_true(all(paste0("topic_", names(sections)) %in% texts$key))
  expect_true(all(paste0("method_", methods) %in% texts$key))
})

test_that("author-date citations after the authors' names give only the year", {
  state <- list(citation = "apa", references = list(
    caddy1993 = list(author = "Caddy, J. F.", year = "1993"),
    deleiva2000 = list(author = "de Leiva Moreno, J. I.; Agostini, V. N.; Caddy, J. F.", year = "2000"),
    loreau2008 = list(author = "Loreau, M.; de Mazancourt, C.", year = "2008")))
  expect_true(fao_report_narrative(state, "caddy1993", "Following Caddy "))
  expect_true(fao_report_narrative(state, "deleiva2000", "and de Leiva Moreno et al. "))
  expect_true(fao_report_narrative(state, "loreau2008", "the index of Loreau and de Mazancourt "))
  expect_false(fao_report_narrative(state, "caddy1993", "the ratio was used "))
  expect_false(fao_report_narrative(state, c("caddy1993", "loreau2008"), "Caddy "))
  state$citation <- "numeric"
  expect_false(fao_report_narrative(state, "caddy1993", "Following Caddy "))
})

test_that("the report keeps the analyses by division (an extension) out", {
  expect_false("by" %in% fao_report_allowed_args("pelagic_demersal"))
  expect_false("by" %in% fao_report_allowed_args("mean_temperature_catch"))
  expect_true("by" %in% fao_report_allowed_args("catch_shocks"))
  expect_true("by" %in% fao_report_allowed_args("catch_synchrony"))
})

test_that("LaTeX output escapes text and keeps links, italics and tables", {
  expect_identical(fao_latex_escape("a_b & 5% #1 {x}"), "a\\_b \\& 5\\% \\#1 \\{x\\}")
  zh_link <- paste0("\u89c1https://doi.org/10.1/x.y\u4e0b\u8f7d\u3002")
  expect_identical(fao_latex_text(zh_link), paste0("\u89c1\\url{https://doi.org/10.1/x.y}\u4e0b\u8f7d\u3002"))
  expect_identical(fao_latex_text("see https://x.org/a#b."), "see \\url{https://x.org/a\\#b}.")

  file <- withr::local_tempfile(fileext = ".tex")
  x <- fao_latex_new(fao_report_state("zh", "Times New Roman", "FangSong", "numeric", 1, list()), file)
  x <- fao_report_par(x, "Text with *Sardina pilchardus* and H~2~O.")
  x <- fao_report_end_cover(x)
  x <- fao_report_table(x, "t1", data.frame(name = c("*Sardina* spp.", "b"), n = 1:2), "Caption")
  out <- fao_latex_save(x, list(title = "T", author = "A"), pdf = FALSE)
  expect_identical(names(out), "latex")
  tex <- readLines(out[["latex"]], encoding = "UTF-8")
  expect_true(any(grepl("{ctexart}", tex, fixed = TRUE)))
  expect_true(any(grepl("\\textit{Sardina pilchardus}", tex, fixed = TRUE)))
  expect_true(any(grepl("H\\textsubscript{2}O", tex, fixed = TRUE)))
  expect_true(any(grepl("\\begin{longtable}", tex, fixed = TRUE)))
  expect_true(any(grepl("\\textit{Sardina} spp.", tex, fixed = TRUE)))
  expect_identical(tail(tex, 1), "\\end{document}")
})

test_that("stock status shares follow the order of the text and sum over-exploited and collapsed", {
  labels <- c("Developing", "Exploited", "Over-exploited", "Rebuilding", "Collapsed")
  yearly <- data.frame(Year = 2020, status_label = factor(labels, levels = labels),
                       stocks_pct = c(10, 20, 30, 15, 25), catch_pct = c(5, 50, 20, 10, 15))
  ctx <- list(results = list(status = list(value = list(yearly = yearly, stocks = data.frame(x = 1:20)))),
              area_data = data.frame(Year = 2020), lang = "en",
              args = list(status = list(taxon_levels = "species", min_years = 10, min_consecutive = 5,
                                        min_catch = 1000, smooth = FALSE)))
  v <- fao_section_status_values(ctx)
  # developing, exploited, over-exploited, collapsed and rebuilding
  expect_equal(v$stock_pcts, fao_report_list(paste0(fao_report_pct(c(10, 20, 30, 25, 15)), "%"), "en"))
  expect_equal(v$oc_pct, fao_report_pct(55))
  expect_equal(v$oc_catch_pct, fao_report_pct(35))
})

test_that("English lists use semicolons when the items have commas", {
  expect_equal(fao_report_list(c("Cods, hakes, haddocks", "Herrings, sardines, anchovies", "Salmons"), "en"),
               "Cods, hakes, haddocks; Herrings, sardines, anchovies; and Salmons")
  expect_equal(fao_report_list(c("Peru", "Chile", "China"), "en"), "Peru, Chile and China")
})

test_that("CMSY++ counts agree with their verbs", {
  text <- function(n) fao_report_text("cmsy_summary", "en", list(n_overfished = n, n_ok = 5, overfished_list = "", overfished_verb = if (n == 1) "was" else "were"))
  expect_match(text(1), "1 of the 5 main species was overfished")
  expect_match(text(2), "2 of the 5 main species were overfished")
})

test_that("the Word report names the author, not the computer user, as last modified by", {
  skip_if_not_installed("officer")
  info <- list(download_date = "2026-01-01", release_date = "2026-03-01", release = "2026.1",
               years = c(1950, 2024), url = "https://www.fao.org/fishery/en/collection/capture")
  x <- fao_report_new("en", "Times New Roman", "SimSun", "author-date", NULL, fao_report_references(info, "2026-06-01"))
  x <- fao_report_par(x, "Text.")
  file <- tempfile(fileext = ".docx")
  user <- Sys.getenv("USER")
  fao_report_save(x, file, list(title = "Report", author = "Test Author"))
  expect_equal(Sys.getenv("USER"), user)
  props <- officer::doc_properties(officer::read_docx(file))
  expect_equal(props$value[props$tag == "creator"], "Test Author")
  expect_equal(props$value[props$tag == "lastModifiedBy"], "Test Author")
})

test_that("the P/D text names the most recent period once", {
  ctx_for <- function(pd_mean) {
    yearly <- data.frame(area_en = "A", year = 2024, pd_ratio = 0.5, pelagic_tonnes = 1, demersal_tonnes = 2, total_tonnes = 3)
    decades <- data.frame(area_en = "A", n_years = c(10, 10, 5), first_year = c(2000, 2010, 2020),
                          last_year = c(2009, 2019, 2024), pd_mean = pd_mean)
    list(lang = "en", area = list(name = "A"), args = list(),
         results = list(pelagic = list(value = list(yearly = yearly, decades = decades))))
  }
  low_now <- fao_section_pelagic_values(ctx_for(c(0.9, 0.5, 0.1)))
  expect_equal(low_now$pd_decades, "By decade, P/D was highest in the 2000s (0.90) and lowest in the most recent period, 2020\u20132024 (0.10).")
  expect_equal(low_now$pd_concl, "The P/D ratio was highest in the 2000s (0.90) and was 0.10 in 2020\u20132024.")
  high_now <- fao_section_pelagic_values(ctx_for(c(0.1, 0.5, 0.9)))
  expect_match(high_now$pd_decades, "lowest in the 2000s \\(0\\.10\\) and highest in the most recent period, 2020\u20132024 \\(0\\.90\\)")
  expect_equal(high_now$pd_concl, "The P/D ratio was highest in the most recent period, 2020\u20132024 (0.90).")
  middle <- fao_section_pelagic_values(ctx_for(c(0.9, 0.1, 0.5)))
  expect_match(middle$pd_decades, "lowest in the 2010s \\(0\\.10\\), and was 0\\.50 in 2020\u20132024")
})

test_that("the diversity text says once that species and countries are equally concentrated", {
  diversity <- data.frame(Year = c(1950, 2024), shannon_species = c(2.3, 2.0), hhi_species = c(1000, 3000),
                          hhi_countries = c(5000, 9000), top5_species_pct = c(60, 75))
  ctx <- list(lang = "en", results = list(diversity = list(value = diversity)))
  expect_equal(fao_section_diversity_values(ctx)$conc_text, "the catch was highly concentrated both by species and by country")
  diversity$hhi_species[2] <- 1000
  ctx$results$diversity$value <- diversity
  expect_equal(fao_section_diversity_values(ctx)$conc_text,
               "the catch was unconcentrated by species and highly concentrated by country")
})
