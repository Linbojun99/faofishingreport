# Which JAGS goes with which R version on each system, and the download of the matching
# installer (and, for JAGS 5, of the matching rjags package). The rules come from the README
# files of the JAGS and rjags downloads on SourceForge and from the runs of
# .github/workflows/cmsy-cross-platform.yaml.

cmsy_sf_readme <- c(
  windows4 = "https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Windows/",
  macos4 = "https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Mac%20OS%20X/",
  source4 = "https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Source/",
  windows5 = "https://sourceforge.net/projects/mcmc-jags/files/JAGS/5.x/Windows/",
  macos5 = "https://sourceforge.net/projects/mcmc-jags/files/JAGS/5.x/macOS/",
  source5 = "https://sourceforge.net/projects/mcmc-jags/files/JAGS/5.x/Source/"
)

# One row per system, JAGS version and range of R versions. r_to NA: no upper limit known.
cmsy_jags_compat <- data.frame(
  os = c("windows", "windows", "macos", "macos", "linux", "windows", "macos", "linux"),
  jags = c("4.3.2", "4.3.1", "4.3.2", "4.3.1", "4.3.2", "5.0.0", "5.0.0", "5.0.0"),
  r_from = c("4.5.0", "4.2.0", "4.3.0", "4.0.0", "4.2.0", "4.5.0", "4.3.0", "4.2.0"),
  r_to = c("4.6.99", "4.4.99", NA, "4.2.99", NA, "4.6.99", NA, NA),
  installer = c("JAGS-4.3.2.exe", "JAGS-4.3.1.exe", "JAGS-4.3.2.pkg", "JAGS-4.3.1.pkg", "JAGS-4.3.2.tar.gz",
                "JAGS-5.0.0.exe", "JAGS-5.0.0-universal.pkg", "JAGS-5.0.0.tar.gz"),
  rjags = c("4-17 (CRAN)", "4-17 (CRAN)", "4-17 (CRAN)", "4-15 or later (CRAN; its binary for R 4.2 is 4-15)",
            "4-17 (CRAN, built from source)",
            "5-3 (SourceForge, Windows binary)", "5-3 (SourceForge: binary for R 4.6, else source)",
            "5-3 (SourceForge, source)"),
  default = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  evidence = c(
    "JAGS README: the Windows binary of 4.3.2 \"was built with Rtools45 and is suitable for interfacing with R 4.5.x\"; Rtools45 builds serve R 4.5.0 to 4.6.x (JAGS 5 README); run on R 4.6 by the package's CI",
    "JAGS README: earlier releases \"are built with earlier versions of the Rtools tool chain\"; 4.3.1 (April 2022) is the build for R before 4.5; run on R 4.2, 4.3 and 4.4 by the package's CI",
    "JAGS README: \"JAGS-4.3.2.pkg for compatibility with R version 4.3.0 and later\"; run on R 4.6 (Apple silicon) by the package's CI",
    "JAGS README: \"JAGS-4.3.1.pkg for compatibility with R versions 4.0.x to 4.2.x\"; run on R 4.2 (Apple silicon, with rjags 4-15) by the package's CI",
    "Install JAGS with the package manager (e.g. Debian/Ubuntu: sudo apt-get install jags) or from source; rjags is then built for the installed JAGS and any R version; run on Ubuntu by the package's CI",
    "JAGS 5 README: \"For compatibility with R 4.5.0 to R 4.6.x, use binaries in the Rtools45 subfolder\"; needs rjags 5-x, not yet on CRAN; run on R 4.6 by the package's CI",
    "JAGS 5 README: universal installer built for R for macOS 4.3.0 and later (it also contains JAGS 4.3.2); needs rjags 5-x: SourceForge has binaries for R 4.6, other R versions build it from source; run on R 4.6 (Apple silicon) by the package's CI",
    "JAGS 5 source code; needs rjags 5-x from source"),
  readme = unname(cmsy_sf_readme[c("windows4", "windows4", "macos4", "macos4", "source4", "windows5", "macos5", "source5")]),
  stringsAsFactors = FALSE
)

#' Which JAGS Version Goes with Which R Version
#'
#' CMSY++ runs its MCMC in JAGS through the rjags package, and the JAGS installer must match the
#' operating system and the R version: on Windows and macOS each JAGS binary is built for a range
#' of R versions, and rjags from CRAN (4-17) works only with JAGS 4. This function lists, for each
#' system, the R versions, the JAGS installer, the rjags version it needs, whether it is the
#' default, and the evidence (the README of the JAGS downloads, and the runs on Windows, macOS
#' and Linux of the package's GitHub Actions workflow), and marks the row for this computer.
#'
#' In short (JAGS 4, the default):
#' \tabular{lll}{
#'   \bold{System} \tab \bold{R version} \tab \bold{JAGS installer} \cr
#'   Windows \tab 4.5 or 4.6 \tab JAGS-4.3.2.exe \cr
#'   Windows \tab 4.2 to 4.4 \tab JAGS-4.3.1.exe \cr
#'   macOS \tab 4.3 or later \tab JAGS-4.3.2.pkg \cr
#'   macOS \tab 4.2 \tab JAGS-4.3.1.pkg \cr
#'   Linux \tab any \tab package manager (e.g. `apt-get install jags`) or JAGS-4.3.2.tar.gz \cr
#' }
#' All of them work with rjags 4 from CRAN (4-17; the CRAN binary for R 4.2 on macOS is 4-15). JAGS 5.0.0 (`version = "latest"` in
#' [cmsy_download_jags()] and `jags = "latest"` in [cmsy_setup()]) needs rjags 5-3, which is not
#' yet on CRAN and is downloaded from SourceForge.
#'
#' [cmsy_download_jags()] downloads the matching installer automatically, and [cmsy_setup()]
#' downloads and opens it.
#'
#' @param os "all" (default), or one system: "auto" (this computer), "windows", "macos" or "linux".
#' @param r_version The R version marked as `this_computer`. Default is the running R.
#' @param silent A logical value. If FALSE (default), the match for this computer is printed.
#' @return A data frame with one row per system, JAGS version and range of R versions.
#' @references
#' Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical models using Gibbs
#' sampling. Proceedings of DSC 2003, Vienna. \url{https://mcmc-jags.sourceforge.io/}
#' @export
#' @examples
#' # The whole table, with the row for this computer marked
#' cmsy_jags_versions()
#'
#' # One system, e.g. to tell a colleague on Windows with R 4.4 which installer to use
#' cmsy_jags_versions("windows", r_version = "4.4.1")
cmsy_jags_versions <- function(os = "all", r_version = getRversion(), silent = fao_setting("silent")) {
  table <- cmsy_jags_compat
  if (!identical(os, "all")) table <- table[table$os == cmsy_os(os), ]
  r_text <- ifelse(is.na(table$r_to), paste0(">= ", sub("\\.0$", "", table$r_from)),
                   paste0(sub("\\.0$", "", table$r_from), " to ", sub("\\.99$", ".x", table$r_to)))
  here <- cmsy_detect_os()
  this <- table$os == here & table$default & cmsy_r_in_range(r_version, table$r_from, table$r_to)
  out <- data.frame(system = c(windows = "Windows", macos = "macOS", linux = "Linux")[table$os], r_versions = r_text,
                    jags = table$jags, installer = table$installer, rjags = table$rjags,
                    default = table$default, this_computer = this, evidence = table$evidence, readme = table$readme,
                    stringsAsFactors = FALSE, row.names = NULL)
  if (!silent) {
    hit <- table[table$os == here & table$default & cmsy_r_in_range(r_version, table$r_from, table$r_to), ]
    if (nrow(hit) > 0) {
      message("This computer (", c(windows = "Windows", macos = "macOS", linux = "Linux")[[here]], ", R ", r_version,
              "): JAGS ", hit$jags[1], " (", hit$installer[1], ") with rjags ", hit$rjags[1], ".")
    } else {
      message("No JAGS build is listed for R ", r_version, " on this system; see ", cmsy_sf_page, "JAGS/")
    }
  }
  out
}

#' Download the JAGS Installer that Matches This Computer
#'
#' This function picks the JAGS installer that matches the operating system, the R version and
#' the processor (see [cmsy_jags_versions()]), downloads it from its pinned link, and checks its
#' size and checksums (MD5, and SHA-256 when R can compute it). For JAGS 5 it also downloads the
#' matching rjags 5-3 package. It can also download the installers of other systems or R
#' versions, e.g. to install JAGS on computers without internet or for colleagues.
#'
#' The files are kept in the download folder of the package (`fao_data_dir()/cmsy/downloads`),
#' so they are downloaded only once, and copied to `dest` when given. Install JAGS by opening the
#' installer ([cmsy_install_jags()] or [cmsy_setup()] do this) and restart R.
#'
#' @param version "recommended" (default: JAGS 4 for this R version, which works with rjags from
#'        CRAN), "latest" (JAGS 5.0.0 with rjags 5-3) or a JAGS version such as "4.3.1".
#' @param os "auto" (default, this computer), "windows", "macos", "linux" or "all" (the
#'        installers of all three systems for `r_version`).
#' @param r_version The R version the installer is for. Default is the running R.
#' @param arch The processor, for the rjags 5 binary on macOS: "auto" (default), "arm64" (Apple
#'        silicon) or "x86_64" (Intel).
#' @param dest Optional folder to copy the files to.
#' @param rjags A logical value. If TRUE (default), the rjags package needed by JAGS 5 is also
#'        downloaded (rjags 4-17 for JAGS 4 is installed from CRAN by [cmsy_setup()]).
#' @param data_dir The directory where downloads are kept. Default is `fao_data_dir()`.
#' @param silent A logical value. If FALSE (default), the matching and the download are reported.
#' @return Invisibly, a data frame with one row per file: system, R version, component, version,
#'         file, local path, link, SHA-256 and whether the checksums matched.
#' @references
#' Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical models using Gibbs
#' sampling. Proceedings of the 3rd International Workshop on Distributed Statistical Computing
#' (DSC 2003), Vienna. \url{https://mcmc-jags.sourceforge.io/}
#' @export
#' @examples
#' \dontrun{
#' # The installer for this computer (JAGS 4 for this R version)
#' cmsy_download_jags()
#'
#' # JAGS 5.0.0 and the matching rjags 5-3
#' cmsy_download_jags(version = "latest")
#'
#' # All three systems for R 4.4, copied to a folder to share in a lab
#' cmsy_download_jags(os = "all", r_version = "4.4.1", dest = "jags_installers")
#' }
cmsy_download_jags <- function(version = "recommended", os = "auto", r_version = getRversion(), arch = "auto",
                               dest = NULL, rjags = TRUE, data_dir = fao_data_dir(),
                               silent = fao_setting("silent")) {
  systems <- if (identical(os, "all")) c("windows", "macos", "linux") else cmsy_os(os)
  if (identical(arch, "auto")) arch <- if (grepl("aarch64|arm64", R.version$arch)) "arm64" else "x86_64"
  rows <- list()
  for (system in systems) {
    entry <- cmsy_jags_choice(version, system, r_version)
    files <- list(entry)
    if (rjags && cmsy_major(entry$version) >= 5) files[[2]] <- cmsy_rjags_choice(entry$version, system, arch, r_version)
    for (f in files) {
      if (!nzchar(f$file) || is.na(f$size)) next          # rjags 4-17 comes from CRAN
      path <- cmsy_fetch(f, data_dir, quiet = silent)
      sha_ok <- cmsy_sha256_ok(path, f$sha256)
      if (isFALSE(sha_ok)) {
        unlink(path)
        stop("The SHA-256 checksum of ", f$file, " does not match the one recorded; the file was deleted.", call. = FALSE)
      }
      if (!is.null(dest)) {
        dir.create(dest, recursive = TRUE, showWarnings = FALSE)
        file.copy(path, file.path(dest, f$file), overwrite = TRUE)
        path <- normalizePath(file.path(dest, f$file))
      }
      rows[[length(rows) + 1]] <- data.frame(system = system, r_version = as.character(r_version),
                                             component = f$component, version = f$version, file = f$file,
                                             path = path, url = f$url, sha256 = f$sha256,
                                             verified = if (is.na(sha_ok)) "size and MD5" else "size, MD5 and SHA-256",
                                             stringsAsFactors = FALSE)
    }
    if (!silent) {
      message(c(windows = "Windows", macos = "macOS", linux = "Linux")[[system]], ", R ", r_version, ": ",
              entry$file, " (JAGS ", entry$version, ", ", entry$requires, ")",
              if (system == "linux") " - or install JAGS with the package manager, e.g. sudo apt-get install jags")
    }
  }
  invisible(do.call(rbind, rows))
}

# TRUE or FALSE when R can compute SHA-256 (R >= 4.5), NA otherwise
cmsy_sha256_ok <- function(path, sha256) {
  if (is.na(sha256) || !exists("sha256sum", envir = asNamespace("tools"), inherits = FALSE)) return(NA)
  identical(unname(get("sha256sum", envir = asNamespace("tools"))(path)), sha256)
}

cmsy_r_in_range <- function(r_version, from, to) {
  r <- numeric_version(as.character(r_version))
  r >= numeric_version(from) & (is.na(to) | r <= numeric_version(ifelse(is.na(to), "0", to)))
}

# JAGS installer (registry row) for a version, system and R version
cmsy_jags_choice <- function(version = "recommended", os = cmsy_os(), r_version = getRversion()) {
  compat <- cmsy_jags_compat[cmsy_jags_compat$os == os, ]
  fits <- cmsy_r_in_range(r_version, compat$r_from, compat$r_to)
  ranges <- function(rows) paste0("JAGS ", rows$jags, " for R ", rows$r_from,
                                  ifelse(is.na(rows$r_to), " and later", paste0(" to ", sub("\\.99$", ".x", rows$r_to))),
                                  collapse = "; ")
  pick <- if (identical(version, "recommended")) {
    compat[compat$default & fits, ]
  } else if (identical(version, "latest")) {
    newest <- as.character(max(numeric_version(compat$jags)))
    compat[compat$jags == newest & fits, ]
  } else {
    if (!version %in% compat$jags) {
      stop("JAGS ", version, " is not available for ", os, ". Versions: ", paste(unique(compat$jags), collapse = ", "),
           call. = FALSE)
    }
    compat[compat$jags == version & fits, ]
  }
  if (nrow(pick) == 0) {
    stop("No ", if (identical(version, "recommended")) "" else paste0("JAGS ", version, " "), "build for R ", r_version,
         " on ", os, " is known: ", ranges(compat), ". See cmsy_jags_versions() and ", cmsy_sf_page, "JAGS/",
         call. = FALSE)
  }
  registry <- cmsy_registry()
  registry[registry$component == "JAGS" & registry$file == pick$installer[1], ]
}

# rjags package matching a JAGS version: CRAN for JAGS 4, SourceForge files for JAGS 5
cmsy_rjags_choice <- function(jags_version, os, arch, r_version = getRversion()) {
  rjags <- cmsy_registry()
  rjags <- rjags[rjags$component == "rjags", ]
  if (cmsy_major(jags_version) < 5) return(rjags[rjags$version == "4-17", ])
  r <- numeric_version(as.character(r_version))
  platform <- if (os == "windows") {
    "windows"
  } else if (os == "macos" && r >= "4.6.0" && r < "4.7.0") {
    paste0("macos-", if (arch == "arm64") "arm64" else "x86_64")
  } else {
    "source"
  }
  rjags[rjags$version == "5-3" & rjags$platform == platform, ]
}
