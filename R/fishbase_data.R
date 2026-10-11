# FishBase and SeaLifeBase data: the static snapshots of the database tables that the rfishbase
# project (https://github.com/ropensci/rfishbase) publishes as parquet files on Source Cooperative.
# Tables are downloaded to fao_data_dir()/fishbase/<fb|slb>/<version>/<table>.parquet and always
# read from there. When the computer is online, the newest release is looked up first (and the
# tables it needs are downloaded); when it is offline, the newest downloaded release is used.

#' Check for the Latest FishBase and SeaLifeBase Releases
#'
#' This function lists the FishBase and SeaLifeBase releases published online (the static
#' snapshots used by the rfishbase package) and compares the newest with the releases downloaded
#' on this computer.
#'
#' @param server "fishbase", "sealifebase" or both (default).
#' @param data_dir The directory where downloaded data is stored. Default is `fao_data_dir()`.
#' @param quiet A logical value. If TRUE, no message is printed. Default is FALSE.
#' @return Invisibly, a data frame with one row per server: newest online release (NA when
#'   offline), newest downloaded release, number of tables online and downloaded, and whether an
#'   update is available.
#' @references
#' Froese, R. and Pauly, D. (Eds.) FishBase. World Wide Web electronic publication.
#' \url{https://www.fishbase.se}
#'
#' Palomares, M.L.D. and Pauly, D. (Eds.) SeaLifeBase. World Wide Web electronic publication.
#' \url{https://www.sealifebase.se}
#'
#' Boettiger, C., Lang, D.T. and Wainwright, P.C. (2012). rfishbase: exploring, manipulating and
#' visualizing FishBase data from R. Journal of Fish Biology 81: 2030-2039.
#' \doi{10.1111/j.1095-8649.2012.03464.x}
#' @export
#' @examples
#' \dontrun{
#' check_fishbase_update()
#' }
check_fishbase_update <- function(server = c("fishbase", "sealifebase"), data_dir = fao_data_dir(),
                                  quiet = FALSE) {
  server <- match.arg(server, several.ok = TRUE)
  result <- do.call(rbind, lapply(server, function(sv) {
    releases <- tryCatch(fb_list_releases(sv), error = function(e) {
      if (!quiet) message(fb_server_name(sv), ": could not connect to the data server (", conditionMessage(e), ")")
      character(0)
    })
    latest <- if (length(releases) > 0) releases[length(releases)] else NA_character_
    local_versions <- fb_local_versions(sv, data_dir)
    local <- if (length(local_versions) > 0) local_versions[length(local_versions)] else NA_character_
    tables_online <- if (is.na(latest)) NA_integer_ else nrow(fb_list_files(sv, latest))
    tables_local <- if (is.na(local)) 0L else length(fb_local_tables(sv, local, data_dir))
    update <- !is.na(latest) && (is.na(local) || numeric_version(latest) > numeric_version(local) ||
                                   (latest == local && tables_local < tables_online))
    data.frame(server = sv, latest_online = latest, newest_local = local,
               tables_online = tables_online, tables_local = tables_local,
               update_available = update, stringsAsFactors = FALSE)
  }))

  if (!quiet) {
    for (i in seq_len(nrow(result))) {
      r <- result[i, ]
      message(fb_server_name(r$server))
      message("  Latest online release: ", if (is.na(r$latest_online)) "unknown (offline)" else
        paste0(r$latest_online, " (", r$tables_online, " tables)"))
      message("  Downloaded locally:    ", if (is.na(r$newest_local)) "none" else
        paste0(r$newest_local, " (", r$tables_local, " tables, ", fb_version_dir(r$server, r$newest_local, data_dir), ")"))
      if (r$update_available) {
        message("  Run download_fishbase_data(\"", r$server, "\") to download the latest release ",
                "(or the tables of it that are missing).")
      } else if (!is.na(r$latest_online)) {
        message("  Your copy is up to date.")
      }
    }
  }
  invisible(result)
}


#' Download FishBase and SeaLifeBase Data
#'
#' This function downloads the tables of a FishBase and/or SeaLifeBase release (the static
#' snapshots of the database tables used by the rfishbase package, as parquet files) to
#' `data_dir`, so they can be used offline. By default it downloads the tables the package uses
#' (species, estimate, families, synonyms, stocks and faoareas: about 15 MB for each database);
#' a complete release takes about 140 MB for FishBase and 110 MB for SeaLifeBase.
#'
#' Downloading beforehand is optional: functions that use FishBase data, such as
#' `fishbase_match()`, look online for the newest release first and download the tables they
#' need. When the computer is offline they use the newest release downloaded here, and when
#' nothing has been downloaded, `fishbase_match()` uses the table built into the package. Tables
#' already downloaded are skipped, so an interrupted download can be resumed by running the
#' function again.
#'
#' Reading the tables needs the nanoparquet package (`install.packages("nanoparquet")`).
#' FishBase and SeaLifeBase data are licensed CC BY-NC 4.0 by the FishBase team: they can be
#' used with attribution, but not commercially.
#'
#' @param server "fishbase", "sealifebase" or both (default).
#' @param version The release to download, e.g. "26.06". Default is "latest".
#' @param tables "needed" (default) for the tables the package uses, "all" for every table, or
#'   table names, e.g. `c("species", "estimate")`.
#' @param data_dir The directory where the data is saved. Default is `fao_data_dir()`.
#' @param overwrite A logical value. If TRUE, tables already downloaded are downloaded again.
#'   Default is FALSE.
#' @param quiet A logical value. If TRUE, progress messages are not printed. Default is FALSE.
#' @return Invisibly, a data frame with one row per table: server, version, table, size and
#'   status ("downloaded", "already downloaded" or "failed").
#' @references
#' Froese, R. and Pauly, D. (Eds.) FishBase. World Wide Web electronic publication. \url{https://www.fishbase.se}
#'
#' Palomares, M.L.D. and Pauly, D. (Eds.) SeaLifeBase. World Wide Web electronic publication.
#' \url{https://www.sealifebase.se}
#'
#' Boettiger, C., Lang, D.T. and Wainwright, P.C. (2012). rfishbase: exploring, manipulating and
#' visualizing FishBase data from R. Journal of Fish Biology 81: 2030-2039.
#' \doi{10.1111/j.1095-8649.2012.03464.x}
#' @export
#' @examples
#' \dontrun{
#' check_fishbase_update()
#' download_fishbase_data()                          # the tables the package uses
#' download_fishbase_data(tables = "all")            # every table of FishBase and SeaLifeBase
#' download_fishbase_data("fishbase", tables = c("species", "estimate"))
#' }
download_fishbase_data <- function(server = c("fishbase", "sealifebase"), version = "latest", tables = "needed",
                                   data_dir = fao_data_dir(), overwrite = FALSE, quiet = FALSE) {
  server <- match.arg(server, several.ok = TRUE)
  if (identical(tables, "all")) tables <- NULL else if (identical(tables, "needed")) tables <- fb_needed_tables
  versions <- vapply(server, function(sv) {
    releases <- fb_list_releases(sv)
    v <- if (identical(version, "latest")) releases[length(releases)] else version
    if (!v %in% releases) {
      stop(fb_server_name(sv), " release ", v, " is not available online. Available releases: ",
           paste(releases, collapse = ", "), call. = FALSE)
    }
    v
  }, character(1), USE.NAMES = FALSE)
  if (!quiet) fb_download_notice(server, versions, tables, data_dir)
  result <- do.call(rbind, lapply(seq_along(server), function(k) {
    fb_download_tables(server[k], versions[k], tables, data_dir, overwrite = overwrite, quiet = quiet)
  }))
  invisible(result)
}


#' Read a FishBase or SeaLifeBase Table
#'
#' This function returns one table of FishBase or SeaLifeBase, such as "species" (names, habitat,
#' maximum length, ...) or "estimate" (trophic level, temperature preference, ...). It is read
#' from the copy downloaded on this computer:
#' \itemize{
#'   \item when the computer is online, the newest release is looked up first, and the table is
#'         downloaded if it is not on this computer yet;
#'   \item when the computer is offline (or with `fao_settings(online = FALSE)`), the newest
#'         downloaded release that has the table is used, with a message.
#' }
#' Releases are fixed snapshots, so a downloaded table is identical to the one online.
#'
#' @param table The table name, e.g. "species", "estimate", "ecology", "synonyms" or "families".
#'   `check_fishbase_update()` shows how many tables a release has; see the rfishbase
#'   documentation for what they contain.
#' @param server "fishbase" (default) or "sealifebase".
#' @param version The release, e.g. "26.06". Default is "latest".
#' @param data_dir The directory where downloaded data is stored. Default is `fao_data_dir()`.
#' @return A data frame, with the attributes `fishbase_server`, `fishbase_version` and `source_url`.
#' @references
#' Froese, R. and Pauly, D. (Eds.) FishBase. World Wide Web electronic publication.
#' \url{https://www.fishbase.se}
#'
#' Palomares, M.L.D. and Pauly, D. (Eds.) SeaLifeBase. World Wide Web electronic publication.
#' \url{https://www.sealifebase.se}
#'
#' Boettiger, C., Lang, D.T. and Wainwright, P.C. (2012). rfishbase: exploring, manipulating and
#' visualizing FishBase data from R. Journal of Fish Biology 81: 2030-2039.
#' \doi{10.1111/j.1095-8649.2012.03464.x}
#' @export
#' @examples
#' \dontrun{
#' estimate <- fishbase_table("estimate")
#' attr(estimate, "fishbase_version")
#' }
fishbase_table <- function(table, server = c("fishbase", "sealifebase"), version = "latest",
                           data_dir = fao_data_dir()) {
  server <- match.arg(server)
  version <- fb_resolve_version(server, version, data_dir, table)
  file <- fb_table_file(server, version, table, data_dir)
  if (!file.exists(file)) {
    if (is.null(fb_online_releases(server))) {
      stop("Table \"", table, "\" of ", fb_server_name(server), " ", version, " has not been downloaded ",
           "and the data server cannot be reached. Run download_fishbase_data() when online.", call. = FALSE)
    }
    fb_note(paste(server, "download"), version,
            paste0("Downloading tables of ", fb_server_name(server), " ", version, " as they are needed, from ",
                   "https://data.source.coop/cboettig/fishbase/ (licence CC BY-NC 4.0, FishBase.org team: cite, ",
                   "no commercial use; see fishbase_sources()). download_fishbase_data() downloads them all at once."))
    message("Downloading ", fb_server_name(server), " ", version, " table \"", table, "\" to ",
            fb_version_dir(server, version, data_dir))
    status <- fb_download_tables(server, version, table, data_dir, quiet = TRUE)
    if (any(status$status == "failed")) {
      stop("Could not download table \"", table, "\" of ", fb_server_name(server), " ", version, ".", call. = FALSE)
    }
  }
  key <- paste(file, file.mtime(file))
  if (is.null(.fb_cache$tables[[key]])) {
    fb_check_nanoparquet()
    data <- as.data.frame(nanoparquet::read_parquet(file))
    attr(data, "fishbase_server") <- server
    attr(data, "fishbase_version") <- version
    attr(data, "source_url") <- fb_file_url(server, version, table)
    .fb_cache$tables[[key]] <- data
  }
  .fb_cache$tables[[key]]
}


.fb_cache <- new.env(parent = emptyenv())
.fb_cache$tables <- list()
.fb_cache$releases <- list()
.fb_cache$noted <- character(0)

# Tables used by the package: fishbase_match() (species, estimate, families, synonyms), the
# CMSY++ priors (stocks, estimate) and species occurrence by FAO area (faoareas)
fb_needed_tables <- c("species", "estimate", "families", "synonyms", "stocks", "faoareas")

fb_bucket_url <- "https://s3.us-west-2.amazonaws.com/us-west-2.opendata.source.coop"
fb_prefix <- "cboettig/fishbase"

fb_server_code <- function(server) c(fishbase = "fb", sealifebase = "slb")[[server]]

fb_server_name <- function(server) unname(c(fishbase = "FishBase", sealifebase = "SeaLifeBase")[server])

fb_version_dir <- function(server, version, data_dir) {
  file.path(data_dir, "fishbase", fb_server_code(server), version)
}

fb_table_file <- function(server, version, table, data_dir) {
  file.path(fb_version_dir(server, version, data_dir), paste0(table, ".parquet"))
}

fb_file_url <- function(server, version, table) {
  paste0("https://data.source.coop/", fb_prefix, "/", fb_server_code(server), "/v", version,
         "/parquet/", table, ".parquet")
}

# Releases downloaded on this computer (those that have `table`, if given), oldest first
fb_local_versions <- function(server, data_dir, table = NULL) {
  dirs <- list.files(file.path(data_dir, "fishbase", fb_server_code(server)), pattern = "^[0-9]+\\.[0-9]+$")
  if (!is.null(table)) dirs <- dirs[file.exists(fb_table_file(server, dirs, table, data_dir))]
  dirs[order(numeric_version(dirs))]
}

fb_local_tables <- function(server, version, data_dir) {
  sub("\\.parquet$", "", list.files(fb_version_dir(server, version, data_dir), pattern = "\\.parquet$"))
}

# Reads an S3 listing of the data bucket (one page; `token` continues a truncated listing)
fb_s3_list <- function(prefix, delimiter = NULL, token = NULL) {
  url <- paste0(fb_bucket_url, "?list-type=2&prefix=", utils::URLencode(prefix, reserved = TRUE))
  if (!is.null(delimiter)) url <- paste0(url, "&delimiter=", utils::URLencode(delimiter, reserved = TRUE))
  if (!is.null(token)) url <- paste0(url, "&continuation-token=", utils::URLencode(token, reserved = TRUE))
  old <- options(timeout = 15)
  on.exit(options(old))
  # The connection error is reported by the caller; the warning would repeat it
  paste(suppressWarnings(readLines(url, warn = FALSE, encoding = "UTF-8")), collapse = "")
}

fb_xml_values <- function(xml, tag) {
  values <- regmatches(xml, gregexpr(paste0("<", tag, ">[^<]*</", tag, ">"), xml))[[1]]
  gsub(paste0("</?", tag, ">"), "", values)
}

# Releases published online, oldest first, e.g. "24.07", "25.04", "26.06"
fb_list_releases <- function(server) {
  xml <- fb_s3_list(paste0(fb_prefix, "/", fb_server_code(server), "/"), delimiter = "/")
  versions <- sub(".*/v([0-9]+\\.[0-9]+)/$", "\\1", fb_xml_values(xml, "Prefix"))
  versions <- versions[grepl("^[0-9]+\\.[0-9]+$", versions)]
  if (length(versions) == 0) stop("no releases found", call. = FALSE)
  versions[order(numeric_version(versions))]
}

# Tables of a release published online: table name, size in bytes and URL
fb_list_files <- function(server, version) {
  prefix <- paste0(fb_prefix, "/", fb_server_code(server), "/v", version, "/parquet/")
  keys <- character(0)
  sizes <- numeric(0)
  token <- NULL
  repeat {
    xml <- fb_s3_list(prefix, token = token)
    contents <- regmatches(xml, gregexpr("<Contents>.*?</Contents>", xml, perl = TRUE))[[1]]
    keys <- c(keys, vapply(contents, fb_xml_values, character(1), tag = "Key", USE.NAMES = FALSE))
    sizes <- c(sizes, as.numeric(vapply(contents, fb_xml_values, character(1), tag = "Size", USE.NAMES = FALSE)))
    if (!identical(fb_xml_values(xml, "IsTruncated"), "true")) break
    token <- fb_xml_values(xml, "NextContinuationToken")
  }
  keep <- grepl("/[^/]+\\.parquet$", keys)
  data.frame(table = sub("\\.parquet$", "", basename(keys[keep])), size = sizes[keep],
             url = paste0("https://data.source.coop/", keys[keep]), stringsAsFactors = FALSE)
}

# Online releases, or NULL when offline or when fao_settings(online = FALSE). The result is kept
# for an hour, so an offline computer waits for the connection timeout only once.
fb_online_releases <- function(server) {
  if (!isTRUE(fao_setting("online"))) return(NULL)
  cached <- .fb_cache$releases[[server]]
  if (!is.null(cached) && difftime(Sys.time(), cached$time, units = "mins") < 60) return(cached$releases)
  releases <- tryCatch(fb_list_releases(server), error = function(e) {
    message("Could not connect to the ", fb_server_name(server), " data server (", conditionMessage(e), ").")
    NULL
  })
  .fb_cache$releases[[server]] <- list(time = Sys.time(), releases = releases)
  releases
}

# Release to use: the newest online release when online, else the newest downloaded one
fb_resolve_version <- function(server, version, data_dir, table = NULL) {
  online <- fb_online_releases(server)
  local <- fb_local_versions(server, data_dir, table)
  if (identical(version, "latest")) {
    if (!is.null(online)) {
      version <- online[length(online)]
      fb_note(server, version, paste0("Using ", fb_server_name(server), " release ", version,
                                      " (the latest online release)."))
    } else if (length(local) > 0) {
      version <- local[length(local)]
      fb_note(server, version, paste0("Offline: using the downloaded ", fb_server_name(server),
                                      " release ", version, " (", fb_version_dir(server, version, data_dir), ")."))
    } else {
      stop("The ", fb_server_name(server), " data server cannot be reached and no release has been ",
           "downloaded", if (!is.null(table)) paste0(" with table \"", table, "\""),
           ". Run download_fishbase_data() when online.", call. = FALSE)
    }
  } else if (!version %in% local && !is.null(online) && !version %in% online) {
    stop(fb_server_name(server), " release ", version, " is not available. Online releases: ",
         paste(online, collapse = ", "), call. = FALSE)
  }
  version
}

# Prints a message once per session for each server and release
fb_note <- function(server, version, text) {
  key <- paste(server, version)
  if (!key %in% .fb_cache$noted) {
    message(text)
    .fb_cache$noted <- c(.fb_cache$noted, key)
  }
}

# Downloads tables of a release (all if `tables` is NULL); returns the status of each table
fb_download_tables <- function(server, version, tables = NULL, data_dir, overwrite = FALSE, quiet = FALSE) {
  files <- fb_list_files(server, version)
  if (!is.null(tables)) {
    unknown <- setdiff(tables, files$table)
    if (length(unknown) > 0) {
      stop("Unknown ", fb_server_name(server), " table: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    files <- files[files$table %in% tables, ]
  }
  dir <- fb_version_dir(server, version, data_dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dir, paste0(files$table, ".parquet"))
  todo <- overwrite | !file.exists(dest) | file.size(dest) != files$size
  status <- ifelse(todo, "failed", "already downloaded")

  if (!quiet) {
    message(fb_server_name(server), " ", version, ": ", sum(todo), " of ", nrow(files), " tables to download (",
            format(round(sum(files$size[todo]) / 1e6, 1), nsmall = 1), " MB) to ", dir)
  }
  old <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  bar <- if (!quiet && sum(todo) > 1) utils::txtProgressBar(max = sum(todo), style = 3) else NULL
  for (i in seq_along(which(todo))) {
    j <- which(todo)[i]
    part <- paste0(dest[j], ".part")
    ok <- tryCatch({
      utils::download.file(files$url[j], part, mode = "wb", quiet = TRUE)
      file.size(part) == files$size[j]
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok) {
      file.rename(part, dest[j])
      status[j] <- "downloaded"
    } else {
      unlink(part)
    }
    if (!is.null(bar)) utils::setTxtProgressBar(bar, i)
  }
  if (!is.null(bar)) close(bar)

  result <- data.frame(server = server, version = version, table = files$table, size = files$size,
                       status = status, stringsAsFactors = FALSE)
  fb_update_manifest(result[result$status == "downloaded", ], files, dir)
  if (any(status == "failed")) {
    warning(sum(status == "failed"), " table(s) could not be downloaded: ",
            paste(files$table[status == "failed"], collapse = ", "), ". Run the function again to retry.",
            call. = FALSE)
  }
  if (!quiet) message("Done: ", sum(status == "downloaded"), " downloaded, ",
                      sum(status == "already downloaded"), " already downloaded.")
  result
}

# files.csv in each release folder records where and when each table was downloaded
fb_update_manifest <- function(downloaded, files, dir) {
  if (nrow(downloaded) == 0) return(invisible())
  manifest_file <- file.path(dir, "files.csv")
  new <- data.frame(table = downloaded$table, size = downloaded$size,
                    url = files$url[match(downloaded$table, files$table)],
                    downloaded = as.character(Sys.Date()), stringsAsFactors = FALSE)
  if (file.exists(manifest_file)) {
    old <- utils::read.csv(manifest_file, colClasses = "character")
    new <- rbind(old[!old$table %in% new$table, names(new)], new)
  }
  utils::write.csv(new[order(new$table), ], manifest_file, row.names = FALSE)
}

# Reading parquet files needs the nanoparquet package (in Suggests)
fb_check_nanoparquet <- function() {
  if (!requireNamespace("nanoparquet", quietly = TRUE)) {
    stop("Reading FishBase data needs the nanoparquet package: install.packages(\"nanoparquet\")",
         call. = FALSE)
  }
  invisible(TRUE)
}
