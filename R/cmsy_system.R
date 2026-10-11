# Checking the computer for CMSY++ (operating system, R, JAGS, R packages), choosing the settings
# for it, and installing what is missing.

#' Check the Computer for CMSY++ and Choose the Settings for It
#'
#' This function finds the operating system, R version and processor of the computer, checks
#' whether JAGS and the R packages that CMSY++ needs are installed, and shows the settings that
#' [cmsy_run()] will use on this system.
#'
#' The CMSY++ code (`CMSY++16.R`) was written on Windows. Its user guide (step 12) tells Mac users
#' to change every `windows()` call to `quartz()` and `antialias = "cleartype"` to
#' `antialias = "default"`, and to comment out `windows()` on other systems. The code tests for
#' Windows with `grepl("win", sysname)`, which is also true for macOS ("Darwin"), so on a Mac it
#' calls `windows()` and stops. [cmsy_run()] makes these changes automatically for the system
#' found here, in a copy of the code; the bundled original is never changed. The other changes,
#' made on every system, are listed in the `changes` element.
#'
#' The system is detected automatically. To use the settings of another system (e.g. when the
#' detection is wrong), set `os`, or set it for the session with
#' `options(faofishingreport.cmsy_os = "macos")`.
#'
#' @param os The operating system whose settings are used: "auto" (default, detect it),
#'        "windows", "macos" or "linux".
#' @param device Where CMSY++ draws its charts before saving them as JPEG files: "file"
#'        (default), an off-screen device that works on every system and in batch runs, or
#'        "screen", the system's own window (`windows()`, `quartz()` or `x11()`).
#' @param jags The JAGS version to recommend: "recommended" (default; the newest JAGS 4 release
#'        built for this R version, which works with rjags from CRAN), "latest" (JAGS 5.0.0, which
#'        needs rjags 5-x from SourceForge) or a version number such as "4.3.2".
#' @param silent A logical value. If TRUE, no summary is printed. Default is FALSE, or the value
#'        set with `fao_settings(silent = )`.
#' @return Invisibly, a list with the system (`os`, `os_detected`, `arch`, `r_version`), the
#'         installed `jags` and `rjags` versions, the `packages` table, the recommended `jags_installer`
#'         and `rjags_source`, the chart `device` and `antialias` settings, and the `changes`
#'         made to the CMSY++ code.
#' @seealso [cmsy_setup()] to install what is missing, [cmsy_sources()] for versions and links.
#' @export
#' @examples
#' \dontrun{
#' cmsy_system()
#' cmsy_system(os = "windows")   # settings that would be used on Windows
#' }
cmsy_system <- function(os = "auto", device = c("file", "screen"), jags = "recommended",
                        silent = fao_setting("silent")) {
  device <- match.arg(device)
  detected <- cmsy_detect_os()
  os <- cmsy_os(os)
  arch <- if (grepl("aarch64|arm64", R.version$arch)) "arm64" else R.version$arch
  r_version <- getRversion()

  packages <- cmsy_packages()
  jags_found <- cmsy_jags_installed()
  installer <- tryCatch(cmsy_jags_choice(jags, os, r_version), error = function(e) NULL)
  rjags_source <- if (!is.null(installer)) cmsy_rjags_choice(installer$version, os, arch) else NULL

  result <- list(
    os = os, os_detected = detected, arch = arch, r_version = as.character(r_version),
    rscript = cmsy_rscript(os),
    jags = jags_found$jags, rjags = jags_found$rjags, jags_message = jags_found$message,
    packages = packages, jags_installer = installer, rjags_source = rjags_source,
    device = device, device_call = cmsy_device_call(os, device),
    antialias = if (os == "windows") "cleartype" else "default",
    changes = cmsy_changes(os, device)
  )
  if (!silent) cmsy_print_system(result)
  invisible(result)
}

#' Install JAGS and the R Packages Needed by CMSY++
#'
#' This function prepares the computer for [cmsy_run()]: it installs the missing R packages from
#' CRAN, downloads the JAGS installer for this system from its pinned link (checking its size
#' and checksum, see [cmsy_sources()]) and opens it, and checks the bundled CMSY++ files. It asks
#' before installing anything when R is used interactively.
#'
#' JAGS is a separate program, so it cannot be installed from R. On Windows and macOS the
#' installer is opened and you click through it; on Linux the commands to install it are shown.
#' Restart R after installing JAGS and run `cmsy_setup()` again to check.
#'
#' JAGS 4.3.2 (or 4.3.1 for older R versions) is used by default because rjags on CRAN (4-17)
#' needs JAGS 4. JAGS 5.0.0 (`jags = "latest"`) also installs rjags 5-3 from SourceForge.
#'
#' @inheritParams cmsy_system
#' @param ask A logical value. If TRUE (default), ask before installing anything. In a
#'        non-interactive session nothing is installed when `ask` is TRUE.
#' @param data_dir The directory where downloads are kept. Default is `fao_data_dir()`.
#' @return Invisibly, the result of [cmsy_system()] after the installation. With `silent = TRUE`
#'         no summary or download messages are printed (questions are still asked when `ask`
#'         is TRUE).
#' @export
#' @examples
#' \dontrun{
#' cmsy_setup()
#' cmsy_setup(jags = "latest")   # JAGS 5.0.0 with rjags 5-3
#' }
cmsy_setup <- function(jags = "recommended", os = "auto", ask = TRUE, data_dir = fao_data_dir(),
                       silent = fao_setting("silent")) {
  sys <- cmsy_system(os = os, jags = jags, silent = TRUE)
  can_ask <- interactive()
  agree <- function(question) {
    if (!isTRUE(ask)) return(TRUE)
    if (!can_ask) {
      message(question, " Skipped: run cmsy_setup(ask = FALSE) to install without asking.")
      return(FALSE)
    }
    isTRUE(utils::askYesNo(question))
  }

  # 1. R packages from CRAN (rjags 5-x for JAGS 5 comes from SourceForge, see below)
  missing <- sys$packages$package[!sys$packages$ok]
  if (!is.null(sys$jags_installer) && cmsy_major(sys$jags_installer$version) >= 5) {
    missing <- setdiff(missing, "rjags")
  }
  if (length(missing) > 0 && agree(paste0("Install the R packages ", paste(missing, collapse = ", "),
                                          " from CRAN?"))) {
    utils::install.packages(missing)
  }

  # 2. JAGS
  target <- sys$jags_installer
  if (is.null(target)) stop("No JAGS installer is available for this system and R version.", call. = FALSE)
  need_jags <- is.na(sys$jags) || cmsy_major(sys$jags) != cmsy_major(target$version)
  if (need_jags) {
    if (agree(paste0("JAGS ", target$version, " is needed. Download ", target$file, " (",
                     round(target$size / 1e6, 1), " MB) and open the installer?"))) {
      cmsy_install_jags(target$version, os = sys$os, data_dir = data_dir, silent = silent)
      message("After JAGS is installed, restart R and run cmsy_setup() again to check.")
    }
  }

  # 3. rjags 5-x for JAGS 5
  if (!need_jags && cmsy_major(target$version) >= 5 && !is.null(sys$rjags_source) &&
      (is.na(sys$rjags) || cmsy_major(sys$rjags) < 5)) {
    entry <- sys$rjags_source
    if (agree(paste0("JAGS 5 needs rjags ", entry$version, ". Install it from ", entry$url, "?"))) {
      path <- cmsy_fetch(entry, data_dir, quiet = silent)
      type <- if (entry$platform == "source") "source" else "binary"
      utils::install.packages(path, repos = NULL, type = type)
    }
  }

  # 4. The bundled CMSY++ files
  bundled <- cmsy_registry()
  bundled <- bundled[bundled$bundled, ]
  for (i in seq_len(nrow(bundled))) {
    path <- system.file("cmsy", bundled$file[i], package = "faofishingreport")
    if (!cmsy_file_ok(path, bundled[i, ])) {
      warning("The bundled file ", bundled$file[i], " is missing or changed; reinstall faofishingreport.",
              call. = FALSE)
    }
  }

  cmsy_system(os = os, jags = jags, silent = silent)
}

#' Download and Open the JAGS Installer
#'
#' This function downloads the JAGS installer for the operating system from its pinned link (see
#' [cmsy_sources()]), checks its size and checksum, and opens it: the `.exe` installer on
#' Windows and the `.pkg` installer on macOS. On Linux it downloads the source code and shows
#' how to install JAGS with the package manager or from source. An installer placed in the
#' package folder `inst/jags/` (installed as `jags/`) is used instead of downloading.
#'
#' @inheritParams cmsy_setup
#' @param version The JAGS version: "recommended" (default), "latest" or a version number.
#' @param open A logical value. If TRUE (default), the installer is opened.
#' @return Invisibly, the path of the downloaded installer.
#' @export
#' @examples
#' \dontrun{
#' cmsy_install_jags()
#' }
cmsy_install_jags <- function(version = "recommended", os = "auto", open = TRUE,
                              data_dir = fao_data_dir(), silent = fao_setting("silent")) {
  os <- cmsy_os(os)
  entry <- cmsy_jags_choice(version, os, getRversion())
  path <- cmsy_fetch(entry, data_dir, quiet = silent)
  if (!silent) {
    message("JAGS ", entry$version, " (", entry$file, ", ", entry$license, "): ", path,
            "\nSource: ", entry$page, "\nRequires: ", entry$requires)
  }
  if (!open) return(invisible(path))
  if (os == "windows") {
    get("shell.exec", envir = baseenv())(path)
    message("The JAGS installer has been opened. Follow its steps, then restart R.")
  } else if (os == "macos") {
    system2("open", shQuote(path))
    message("The JAGS installer has been opened. Follow its steps, then restart R.")
  } else {
    message("Install JAGS with your package manager, e.g.\n",
            "  sudo apt-get install jags        (Debian, Ubuntu)\n",
            "or from the downloaded source code:\n",
            "  tar xzf ", shQuote(path), "\n  cd JAGS-", entry$version,
            " && ./configure && make && sudo make install\n",
            "Then restart R. JAGS ", entry$version, " needs rjags ",
            if (cmsy_major(entry$version) >= 5) "5-x" else "4-x", ".")
  }
  invisible(path)
}

# Operating system from the computer, the `os` argument or the faofishingreport.cmsy_os option
cmsy_os <- function(os = "auto") {
  if (is.null(os) || identical(os, "auto")) os <- getOption("faofishingreport.cmsy_os", "auto")
  if (identical(os, "auto")) return(cmsy_detect_os())
  os <- tolower(os)
  if (os %in% c("mac", "darwin", "osx")) os <- "macos"
  if (os %in% c("win", "win32", "win64")) os <- "windows"
  if (!os %in% c("windows", "macos", "linux")) {
    stop("`os` must be \"auto\", \"windows\", \"macos\" or \"linux\".", call. = FALSE)
  }
  os
}

cmsy_detect_os <- function() {
  switch(Sys.info()[["sysname"]], Windows = "windows", Darwin = "macos", "linux")
}

cmsy_rscript <- function(os = cmsy_detect_os()) {
  file.path(R.home("bin"), if (os == "windows") "Rscript.exe" else "Rscript")
}

cmsy_major <- function(version) as.integer(sub("^([0-9]+).*", "\\1", version))

# R packages loaded by CMSY++16.R, with the installed versions
cmsy_packages <- function() {
  needed <- c("R2jags", "rjags", "coda", "gplots", "mvtnorm", "neuralnet", "conicfit")
  installed <- vapply(needed, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
  }, character(1))
  data.frame(package = needed, installed = unname(installed), ok = !is.na(installed),
             stringsAsFactors = FALSE)
}

# Installed JAGS and rjags versions. rjags is loaded in a separate R process, so a missing or
# incompatible JAGS cannot affect this session.
cmsy_jags_installed <- function() {
  if (!requireNamespace("rjags", quietly = TRUE)) {
    return(list(jags = NA_character_, rjags = NA_character_, message = "rjags is not installed"))
  }
  rjags <- as.character(utils::packageVersion("rjags"))
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c("v <- tryCatch({suppressMessages(loadNamespace('rjags'));",
               "  paste0('JAGS:', as.character(rjags::jags.version()))},",
               "  error = function(e) paste0('ERROR:', conditionMessage(e)))",
               "cat(v, '\\n')"), script)
  old_libs <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = cmsy_lib_paths())
  on.exit(if (is.na(old_libs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old_libs), add = TRUE)
  out <- suppressWarnings(system2(cmsy_rscript(), shQuote(script), stdout = TRUE, stderr = TRUE))
  line <- grep("^(JAGS|ERROR):", out, value = TRUE)
  if (length(line) > 0 && startsWith(line[1], "JAGS:")) {
    list(jags = trimws(sub("^JAGS:", "", line[1])), rjags = rjags, message = "")
  } else {
    list(jags = NA_character_, rjags = rjags,
         message = trimws(paste(sub("^ERROR:", "", c(line, out)[1]), collapse = " ")))
  }
}

# Package libraries of this session, for R child processes (R_LIBS)
cmsy_lib_paths <- function() {
  paste(.libPaths(), collapse = .Platform$path.sep)
}

# JAGS installer in the registry for the version, system and R version
cmsy_jags_choice <- function(version = "recommended", os = cmsy_os(), r_version = getRversion()) {
  jags <- cmsy_registry()
  jags <- jags[jags$component == "JAGS", ]
  platform <- if (os == "linux") "source" else os
  r_version <- numeric_version(r_version)
  if (identical(version, "recommended")) {
    version <- if (os == "windows" && r_version < "4.5.0") "4.3.1" else
      if (os == "macos" && r_version < "4.3.0") "4.3.1" else "4.3.2"
  } else if (identical(version, "latest")) {
    version <- max(numeric_version(jags$version))
    version <- as.character(version)
  }
  entry <- jags[jags$version == version & jags$platform == platform, ]
  if (nrow(entry) == 0) {
    stop("JAGS ", version, " is not available for ", os, ". Versions: ",
         paste(unique(jags$version[jags$platform == platform]), collapse = ", "), call. = FALSE)
  }
  if (version == "5.0.0" && os == "windows" && (r_version < "4.5.0" || r_version >= "4.7.0")) {
    stop("The Windows build of JAGS 5.0.0 is for R 4.5 and 4.6; use jags = \"recommended\".", call. = FALSE)
  }
  entry
}

# rjags package matching a JAGS version: CRAN for JAGS 4, SourceForge files for JAGS 5
cmsy_rjags_choice <- function(jags_version, os, arch) {
  rjags <- cmsy_registry()
  rjags <- rjags[rjags$component == "rjags", ]
  if (cmsy_major(jags_version) < 5) return(rjags[rjags$version == "4-17", ])
  platform <- switch(os, windows = "windows", macos = paste0("macos-", if (arch == "arm64") "arm64" else "x86_64"),
                     "source")
  rjags[rjags$version == "5-3" & rjags$platform == platform, ]
}

# How CMSY++ opens a chart window on each system
cmsy_device_call <- function(os, device) {
  if (device == "file") return("grDevices::pdf(NULL, width, height) (off-screen)")
  switch(os, windows = "windows(width, height)", macos = "quartz(width = width, height = height)",
         linux = "x11(width = width, height = height)")
}

# Changes made to a copy of CMSY++16.R before it is run (see cmsy_patch_code())
cmsy_changes <- function(os, device) {
  device_text <- if (device == "screen") {
    switch(os, windows = "kept as windows()", macos = "changed to quartz() (user guide, step 12)",
           linux = "changed to x11()")
  } else {
    "changed to an off-screen device, so charts are drawn and saved without opening windows"
  }
  data.frame(
    change = c("Package installation", "snpar", "Working directory", "Settings",
               "Chart windows", "JPEG antialias", "Results for charts"),
    applies_to = c("all", "all", "all", "all", os, os, "all"),
    what = c(
      "Lines that install missing packages on every run are removed (cmsy_setup() installs them).",
      "library(snpar) is removed and snpar::runs.test() is replaced by the same approximate runs test (snpar was archived on CRAN on 2022-05-10).",
      "setwd(rstudioapi::getActiveDocumentContext()$path) is removed, so RStudio is not needed; each stock runs in its own folder.",
      "File names, the stock and the general settings are set by cmsy_run() after the original settings section.",
      paste0("The test grepl(\"win\", sysname), which is also true on macOS (\"Darwin\"), is replaced; windows(): ", device_text, "."),
      if (os == "windows") "antialias = \"cleartype\" is kept (Windows)." else
        "antialias = \"cleartype\" (Windows only) is changed to \"default\" (user guide, step 12).",
      paste("After the main analysis of a stock, its priors, the posterior draws of r, k and B/k, the yearly",
            "estimates with 95% limits and the last-year Kobe draws are saved to cmsy_extra.rds (retrospective",
            "runs to cmsy_retro.rds) for cmsy_plot(); no calculation is changed.")
    ),
    stringsAsFactors = FALSE
  )
}

cmsy_print_system <- function(x) {
  os_names <- c(windows = "Windows", macos = "macOS", linux = "Linux")
  message("CMSY++ on this computer")
  message("  System:   ", os_names[[x$os]], if (x$os != x$os_detected)
    paste0(" (set by the user; detected: ", os_names[[x$os_detected]], ")") else " (detected)",
    ", ", x$arch, ", R ", x$r_version)
  message("  JAGS:     ", if (is.na(x$jags)) paste0("not found (", x$jags_message, ")") else x$jags,
          if (!is.na(x$rjags)) paste0(", rjags ", x$rjags) else "")
  if (!is.null(x$jags_installer)) {
    message("  Installer for this system: JAGS ", x$jags_installer$version, " (", x$jags_installer$file,
            ") ", x$jags_installer$page)
  }
  missing <- x$packages$package[!x$packages$ok]
  message("  R packages: ", if (length(missing) == 0) "all installed" else
    paste0("missing ", paste(missing, collapse = ", ")))
  message("  Charts:   ", x$device_call, "; JPEG antialias = \"", x$antialias, "\"")
  ready <- !is.na(x$jags) && length(missing) == 0
  message(if (ready) "  Ready to run cmsy_run()." else "  Run cmsy_setup() to install what is missing.")
}
