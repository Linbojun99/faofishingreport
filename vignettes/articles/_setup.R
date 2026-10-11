# Shared setup of the website articles.
# The analyses run in a temporary folder; a chunk with `figures = TRUE` shows the charts it
# saved in figures/png/ right below its code (`figures = "pattern"` shows only matching files).

knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE)

library(faofishingreport)
fao_settings(reset = TRUE)
fao_settings(dpi = 100)

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
  html <- vapply(files, function(file) {
    shown <- substring(file, nchar(site_output) + 2)
    sprintf('<figure class="fao-figure"><img src="%s" alt="%s" loading="lazy"><figcaption><code>%s</code></figcaption></figure>',
            knitr::image_uri(file), basename(file), shown)
  }, character(1))
  paste0("\n", paste(html, collapse = "\n"), "\n")
})

# Shows a table of the first rows of a data frame
show_table <- function(x, n = 10, digits = 2) {
  knitr::kable(utils::head(as.data.frame(x), n), digits = digits, format.args = list(big.mark = ","))
}
