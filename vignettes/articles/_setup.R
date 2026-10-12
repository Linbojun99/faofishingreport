# Shared setup of the website articles.
# The analyses run in a temporary folder; a chunk with `figures = TRUE` shows the charts it
# saved in figures/png/ right below its code (`figures = "pattern"` shows only matching files).

knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE)

library(faofishingreport)
fao_settings(reset = TRUE)
# the articles show results with tables of their own, not with the summaries of the functions
fao_settings(dpi = 100, show_data = FALSE)

site_output <- tempfile("faofishingreport_site_")
dir.create(site_output)
knitr::opts_knit$set(root.dir = site_output)

site_figures <- new.env()

knitr::opts_hooks$set(figures = function(options) {
  options$results <- "hide"
  options
})

knitr::knit_hooks$set(figures = function(before, options) {
  if (before) {
    site_figures$start <- Sys.time()
    return(invisible(NULL))
  }
  files <- list.files(file.path(site_output, "figures", "png"), pattern = "[.]png$",
                      recursive = TRUE, full.names = TRUE)
  files <- files[file.mtime(files) >= site_figures$start]
  if (is.character(options$figures)) files <- files[grepl(options$figures, files)]
  files <- files[order(file.mtime(files))]
  # the charts only, without the path of the file below them
  html <- vapply(files, function(file) {
    sprintf('<figure class="fao-figure"><img src="%s" alt="%s" loading="lazy"></figure>',
            knitr::image_uri(file), basename(file))
  }, character(1))
  paste0("\n", paste(html, collapse = "\n"), "\n")
})

# Shows a table of the first rows of a data frame (thousands separators, except in years)
show_table <- function(x, n = 10, digits = 2) {
  x <- utils::head(as.data.frame(x), n)
  years <- grepl("year", names(x), ignore.case = TRUE) & vapply(x, is.numeric, logical(1))
  x[years] <- lapply(x[years], function(year) ifelse(is.na(year), NA, format(year, trim = TRUE)))
  knitr::kable(x, digits = digits, format.args = list(big.mark = ","))
}

# Data source shown at the top of each article: the FAO release used, with its download link
data_source <- function(lang = "en") {
  info <- fao_data_info()
  years <- paste0(info$years[1], "\u2013", info$years[2])
  file <- sprintf('<a href="%s">%s</a>', info$url, basename(info$url))
  page <- "https://www.fao.org/fishery/collection/capture/en"
  licence <- '<a href="https://creativecommons.org/licenses/by/4.0/">CC BY 4.0</a>'
  html <- if (lang == "zh") {
    sprintf(paste0('\U0001F4E6 <strong>\u6570\u636e\u6765\u6e90</strong>\uff1a',
                   '<a href="%s">FAO\u5168\u7403\u6355\u635e\u4ea7\u91cf</a>\uff08Global Capture Production\uff09',
                   '<strong>%s</strong>\u7248\uff08FAO\u4e8e%s\u53d1\u5e03\uff0c%s\u5e74\uff09\uff0c',
                   '\u4e0b\u8f7d\u5730\u5740%s\uff0c\u8bb8\u53ef\u8bc1%s\u3002',
                   '\u7528<code>fao_data_info()</code>\u67e5\u770b\u4f60\u4f7f\u7528\u7684\u7248\u672c\u548c\u5f15\u7528\u683c\u5f0f\u3002'),
            page, info$release, info$release_date, years, file, licence)
  } else {
    sprintf(paste0('\U0001F4E6 <strong>Data</strong>: <a href="%s">FAO Global Capture Production</a>, ',
                   'release <strong>%s</strong> (published by FAO on %s; catches %s), downloaded from %s. ',
                   'Licence %s. <code>fao_data_info()</code> shows the release you use and how to cite it.'),
            page, info$release, info$release_date, years, file, licence)
  }
  cat(sprintf('<div class="fao-data-source">%s</div>\n', html))
}
