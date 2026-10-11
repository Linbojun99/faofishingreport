# References of fao_report(): the list of references and their formats (GB/T 7714, APA and the
# style of FAO publications).

# References of the report: the FAO data used, R and the package (made from the data and the
# session), and the references in inst/extdata/report_references.csv
fao_report_references <- function(info, date, fishbase = NULL) {
  accessed <- format(as.Date(info$download_date), "%Y-%m-%d")
  today <- format(as.Date(date), "%Y-%m-%d")
  release_year <- substr(info$release_date, 1, 4)
  refs <- list(
    fao_capture = list(type = "DB", author = "FAO", year = release_year,
                       title = paste0("FishStat: Global capture production ", info$years[1], "\u2013", info$years[2]),
                       version = paste("release", info$release), place = "Rome", publisher = "FAO", accessed = accessed,
                       url = info$url, fao_url = "https://www.fao.org/fishery/statistics/software/fishstatj/en"),
    r_core = list(type = "CP", author = "R Core Team", year = R.version$year,
                  title = "R: A language and environment for statistical computing",
                  place = "Vienna", publisher = "R Foundation for Statistical Computing",
                  accessed = today, url = "https://www.R-project.org/"),
    faofishingreport = list(type = "CP", author = "Lin, H.", year = substr(today, 1, 4),
                            title = paste0("faofishingreport: Analyse and report FAO Global Capture Production statistics. R package version ",
                                           utils::packageVersion("faofishingreport")),
                            accessed = today, url = "https://linbojun99.github.io/faofishingreport/")
  )
  if (!is.null(fishbase)) {
    version <- if (length(fishbase) == 1 && grepl("^[0-9]{2}[.][0-9]{2}$", fishbase)) {
      paste0(substr(fishbase, 4, 5), "/20", substr(fishbase, 1, 2))
    } else {
      fishbase[1]
    }
    refs$fishbase <- list(type = "DB", author = "Froese, R.; Pauly, D.", editors = TRUE,
                          year = if (grepl("/", version)) sub(".*/", "", version) else substr(today, 1, 4),
                          title = "FishBase", version = paste("version", version), accessed = today,
                          url = "https://www.fishbase.se")
  }
  table <- fao_extdata_csv("report_references.csv")
  for (i in seq_len(nrow(table))) {
    ref <- as.list(table[i, setdiff(names(table), "key")])
    refs[[table$key[i]]] <- ref[!is.na(ref) & ref != ""]
  }
  refs
}

# Authors of a reference: "Family, I. I." for persons separated by ";", or an organisation
fao_ref_authors <- function(author, style) {
  a <- trimws(strsplit(author, ";")[[1]])
  person <- grepl(",", a)
  family <- ifelse(person, sub(",.*", "", a), a)
  initials <- ifelse(person, trimws(sub("^[^,]*,", "", a)), "")
  n <- length(a)
  if (style == "numeric") {
    x <- ifelse(person, paste(family, trimws(gsub("[.]", " ", initials))), a)
    x <- gsub("\\s+", " ", x)
    if (n > 3) paste0(paste(x[1:3], collapse = ", "), ", et al") else paste(x, collapse = ", ")
  } else if (style == "apa") {
    x <- ifelse(person, paste0(family, ", ", initials), a)
    if (n == 1) x else if (n == 2) paste0(x[1], ", & ", x[2]) else paste0(paste(x[-n], collapse = ", "), ", & ", x[n])
  } else {
    x <- ifelse(person, paste0(family, ", ", gsub(" ", "", initials)), a)
    if (n == 1) x else paste0(paste(x[-n], collapse = ", "), " & ", x[n])
  }
}

# Short author names for author-date citations
fao_ref_short <- function(author, style) {
  a <- trimws(strsplit(author, ";")[[1]])
  family <- sub(",.*", "", a)
  if (length(a) == 1) family else if (length(a) == 2) {
    paste(family[1], if (style == "apa") "&" else "and", family[2])
  } else {
    paste(family[1], "et al.")
  }
}

fao_ref_date <- function(date) {
  date <- as.Date(date)
  paste(as.integer(format(date, "%d")), month.name[as.integer(format(date, "%m"))], format(date, "%Y"))
}

# A reference in the list of references, with *...* for italics
fao_ref_entry <- function(ref, style, suffix = "") {
  dot <- function(x) if (grepl("[.]$", x)) x else paste0(x, ".")
  pages <- if (!is.null(ref$pages)) gsub("-", "\u2013", ref$pages) else NULL
  doi <- if (!is.null(ref$doi)) paste0("https://doi.org/", ref$doi) else NULL
  edition <- if (!is.null(ref$edition)) {
    n <- as.integer(ref$edition)
    paste0(n, c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
  } else {
    NULL
  }
  year <- paste0(ref$year, suffix)
  if (style == "numeric") {
    authors <- fao_ref_authors(ref$author, style)
    if (isTRUE(ref$editors)) authors <- paste0(authors, ", ed")
    switch(ref$type,
      J = paste0(authors, ". ", ref$title, "[J]. ", ref$container, ", ", ref$year, ", ", ref$volume,
                 if (!is.null(ref$issue)) paste0("(", ref$issue, ")"), ": ", ref$pages, ".",
                 if (!is.null(ref$doi)) paste0(" DOI: ", ref$doi, ".")),
      M = paste0(authors, ". ", ref$title, "[M]. ", if (!is.null(edition)) paste0(edition, " ed. "),
                 ref$place, ": ", ref$publisher, ", ", ref$year, ".",
                 if (!is.null(ref$doi)) paste0(" DOI: ", ref$doi, ".")),
      R = paste0(authors, ". ", ref$title, if (!is.null(ref$series)) paste0(": ", ref$series), "[R]. ",
                 ref$place, ": ", ref$publisher, ", ", ref$year, ".", if (!is.null(ref$url)) paste0(" ", ref$url, ".")),
      C = paste0(authors, ". ", ref$title, ": ", ref$container, "[C]. ", ref$place, ", ", ref$year,
                 if (!is.null(ref$pages)) paste0(", ", ref$pages), "."),
      DB = paste0(authors, ". ", ref$title, if (!is.null(ref$version)) paste0(" (", ref$version, ")"), "[DB/OL]. ",
                  if (!is.null(ref$place)) paste0(ref$place, ": ", ref$publisher, ", "), ref$year,
                  "[", ref$accessed, "]. ", ref$url, "."),
      CP = paste0(authors, ". ", ref$title, "[CP/OL]. ",
                  if (!is.null(ref$place)) paste0(ref$place, ": ", ref$publisher, ", "), ref$year,
                  "[", ref$accessed, "]. ", ref$url, ".")
    )
  } else if (style == "apa") {
    authors <- dot(fao_ref_authors(ref$author, style))
    if (isTRUE(ref$editors)) authors <- paste0(authors, " (Eds.).")
    version <- if (!is.null(ref$version)) paste0(" (", toupper(substr(ref$version, 1, 1)), substring(ref$version, 2), ")") else ""
    switch(ref$type,
      J = paste0(authors, " (", year, "). ", ref$title, ". *", ref$container, "*, *", ref$volume, "*",
                 if (!is.null(ref$issue)) paste0("(", ref$issue, ")"), ", ", pages, ".",
                 if (!is.null(doi)) paste0(" ", doi)),
      M = paste0(authors, " (", year, "). *", ref$title, "*", if (!is.null(edition)) paste0(" (", edition, " ed.)"), ".",
                 if (!identical(ref$publisher, ref$author)) paste0(" ", ref$publisher, "."),
                 if (!is.null(doi)) paste0(" ", doi)),
      R = paste0(authors, " (", year, "). *", ref$title, "*", if (!is.null(ref$series)) paste0(" (", ref$series, ")"), ".",
                 if (!identical(ref$publisher, ref$author)) paste0(" ", ref$publisher, "."), if (!is.null(ref$url)) paste0(" ", ref$url)),
      C = paste0(authors, " (", year, "). ", ref$title, ". In *", ref$container, "*",
                 if (!is.null(pages)) paste0(" (pp. ", pages, ")"), ".", if (!is.null(ref$url)) paste0(" ", ref$url)),
      DB = paste0(authors, " (", year, "). *", ref$title, "*", version, " [Data set]. ", ref$url),
      CP = paste0(authors, " (", year, "). *", ref$title, "* [Computer software].",
                  if (!is.null(ref$publisher)) paste0(" ", ref$publisher, "."), " ", ref$url)
    )
  } else {
    authors <- dot(fao_ref_authors(ref$author, style))
    switch(ref$type,
      J = paste0(authors, " ", year, ". ", ref$title, ". *", ref$container, "*, ", ref$volume,
                 if (!is.null(ref$issue)) paste0("(", ref$issue, ")"), ": ", pages, ".",
                 if (!is.null(doi)) paste0(" ", doi)),
      M = paste0(authors, " ", year, ". *", ref$title, "*. ",
                 if (!is.null(edition)) paste0(edition, " edition. "),
                 if (!is.null(ref$series)) paste0(ref$series, ". "), ref$place,
                 if (!identical(ref$publisher, ref$author)) paste0(", ", ref$publisher), ".",
                 if (!is.null(doi)) paste0(" ", doi)),
      R = paste0(authors, " ", year, ". *", ref$title, "*. ", if (!is.null(ref$series)) paste0(ref$series, ". "),
                 ref$place, ", ", ref$publisher, ".", if (!is.null(ref$url)) paste0(" ", ref$url)),
      C = paste0(authors, " ", year, ". ", ref$title, ". In: *", ref$container, "*",
                 if (!is.null(pages)) paste0(", pp. ", pages), ". ", ref$place, ".",
                 if (!is.null(ref$url)) paste0(" ", ref$url)),
      DB = if (!is.null(ref$fao_url)) {
        paste0(authors, " ", year, ". ", ref$title, " (", ref$version, "). [Accessed on ",
               fao_ref_date(ref$accessed), "]. In: FishStatJ. Available at: ", ref$fao_url,
               ". Licence: CC-BY-4.0.")
      } else {
        paste0(authors, if (isTRUE(ref$editors)) ", eds.", " ", year, ". ", ref$title,
               ". World Wide Web electronic publication, ", ref$version, ". ", ref$url)
      },
      CP = paste0(authors, " ", year, ". *", ref$title, "*. ",
                  if (!is.null(ref$place)) paste0(ref$place, ", ", ref$publisher, ". "), ref$url)
    )
  }
}
