# CMSY++ and the software it needs: versions, download links, checksums, licences and citations.
# The CMSY++ files are bundled in inst/cmsy/; JAGS and rjags are downloaded from the pinned links
# when needed (or taken from inst/jags/ if installers are placed there). Every run records the
# rows it used, so results can be traced back to their exact sources.

#' Sources of CMSY++ and the Software It Needs
#'
#' This function lists every component used by the CMSY++ functions of the package, with its
#' version, download link, file size, checksums (MD5 and SHA-256), licence and the reference to
#' cite: the CMSY++ code, neural network, user guide and training data (bundled with the
#' package), the JAGS installers for each operating system, the rjags package, the other R
#' packages CMSY++ uses, and the FishBase and SeaLifeBase releases behind the built-in table of
#' priors (see [cmsy_priors()]).
#'
#' The CMSY++ files bundled with the package are the unmodified March 2021 release from GEOMAR's
#' repository OceanRep (record 52147), published under the CC BY 4.0 licence; the same code is on
#' GitHub at <https://github.com/SISTA16/cmsyPlusPlus>. [cmsy_run()] applies a few documented
#' changes to a copy of the code before running it (see [cmsy_system()]); they do not change the
#' method. faofishingreport is not affiliated with or endorsed by the authors of CMSY++.
#'
#' @param component Optional text to keep only matching components, e.g. "JAGS" or "CMSY".
#' @return A data frame with one row per file or package.
#' @references
#' Froese, R., Winker, H., Coro, G., Palomares, M.L.D., Tsikliras, A.C., Dimarchopoulou, D.,
#' Touloumis, K., Demirel, N., Vianna, G.M.S., Scarcella, G., Schijns, R., Liang, C. and Pauly,
#' D. (2023). New developments in the analysis of catch time series as the basis for fish stock
#' assessments: the CMSY++ method. Acta Ichthyologica et Piscatoria 53: 173-189.
#' \doi{10.3897/aiep.53.105910}
#'
#' Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for CMSY++. GEOMAR,
#' Germany, 17 pp. \url{https://oceanrep.geomar.de/id/eprint/52147/}
#'
#' Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical models using Gibbs
#' sampling. Proceedings of the 3rd International Workshop on Distributed Statistical Computing
#' (DSC 2003), Vienna. \url{https://mcmc-jags.sourceforge.io/}
#' @export
#' @examples
#' # Every component, with version, file, licence and link
#' src <- cmsy_sources()
#' src[, c("component", "version", "platform", "file", "license")]
#'
#' # The JAGS installers for each system, with their checksums
#' cmsy_sources("JAGS")[, c("component", "version", "platform", "url", "sha256")]
#'
#' # The references to cite
#' unique(src$citation[nzchar(src$citation)])
cmsy_sources <- function(component = NULL) {
  sources <- cmsy_registry()
  if (!is.null(component)) {
    sources <- sources[grepl(component, sources$component, ignore.case = TRUE), ]
  }
  rownames(sources) <- NULL
  sources
}

cmsy_oceanrep_zip <- "https://oceanrep.geomar.de/id/eprint/52147/4/UserGuideCodeDataMarch2021.zip"
cmsy_oceanrep_page <- "https://oceanrep.geomar.de/id/eprint/52147/"
cmsy_sf <- "https://downloads.sourceforge.net/project/mcmc-jags/"
cmsy_sf_page <- "https://sourceforge.net/projects/mcmc-jags/files/"

cmsy_cite_paper <- paste(
  "Froese R, Winker H, Coro G, Palomares MLD, Tsikliras AC, Dimarchopoulou D, Touloumis K,",
  "Demirel N, Vianna GMS, Scarcella G, Schijns R, Liang C, Pauly D (2023). New developments in",
  "the analysis of catch time series as the basis for fish stock assessments: the CMSY++ method.",
  "Acta Ichthyologica et Piscatoria 53: 173-189. https://doi.org/10.3897/aiep.53.105910")
cmsy_cite_guide <- paste(
  "Froese R, Demirel N, Coro G, Winker H (2021). User Guide for CMSY++. GEOMAR, Germany, 17 pp.",
  "https://oceanrep.geomar.de/id/eprint/52147/")
cmsy_cite_jags <- paste(
  "Plummer M (2003). JAGS: A program for analysis of Bayesian graphical models using Gibbs",
  "sampling. Proceedings of DSC 2003, Vienna. https://mcmc-jags.sourceforge.io/")
cmsy_cite_fishbase <- "Froese R, Pauly D (Eds.) (2026). FishBase. World Wide Web electronic publication, version (06/2026). https://www.fishbase.se"
cmsy_cite_sealifebase <- "Palomares MLD, Pauly D (Eds.) (2026). SeaLifeBase. World Wide Web electronic publication, version (06/2026). https://www.sealifebase.se"

cmsy_registry <- function() {
  row <- function(component, version, platform, file, url, page, size = NA, md5 = NA, sha256 = NA,
                  license, released, requires = "", bundled = FALSE, citation = "") {
    data.frame(component = component, version = version, platform = platform, file = file,
               bundled = bundled, url = url, page = page, size = size, md5 = md5, sha256 = sha256,
               license = license, released = released, requires = requires, citation = citation,
               stringsAsFactors = FALSE)
  }
  cran <- function(pkg, version, license, requires = "") {
    row(pkg, version, "all", pkg, paste0("https://cran.r-project.org/package=", pkg),
        paste0("https://cran.r-project.org/package=", pkg), license = license,
        released = "CRAN (version current in October 2026)", requires = requires,
        citation = paste0("citation(\"", pkg, "\")"))
  }
  rbind(
    # CMSY++ files, bundled in inst/cmsy/ (OceanRep 52147, CC BY 4.0)
    row("CMSY++ code", "16 (January 2021)", "all", "CMSY++16.R", cmsy_oceanrep_zip, cmsy_oceanrep_page,
        132726, "9ed00c053d05a10024170be1a48f8d85",
        "38a66b2baa62774b8facbbd611bbf67bfabdedff1e36c37d93f7154181639387",
        "CC BY 4.0", "2021-03", "JAGS 4.x and the R packages below", TRUE, cmsy_cite_paper),
    row("CMSY++ neural network", "16 (January 2021)", "all", "ffnn.bin", cmsy_oceanrep_zip, cmsy_oceanrep_page,
        1635526, "367498b9060d29649ec4b06cc6fe16bd",
        "bf5ca292b902fafea59fe6c25c850a3ea9274590b32a9db024783c7497a1be0d",
        "CC BY 4.0", "2021-03", "neuralnet", TRUE, cmsy_cite_paper),
    row("CMSY++ user guide", "March 2021", "all", "CMSYUserGuideMarch2021.pdf", cmsy_oceanrep_zip,
        cmsy_oceanrep_page, 1674707, "acba624286bd29ce43469888291d0b1e",
        "c5d594e9303ed2316bfa1eb567fc20bb3dc348d920064be8950bcb75353e750f",
        "CC BY 4.0", "2021-03", "", TRUE, cmsy_cite_guide),
    row("CMSY++ training data (catch)", "9e", "all", "Train_Catch_9e.csv", cmsy_oceanrep_zip, cmsy_oceanrep_page,
        903162, "936b35fbf9483d7693a6cd1f5af89b0a",
        "db119dc6812b36bd1394073b58aae5667ebc44231be91ebbf2d673b11b18a4a9",
        "CC BY 4.0", "2021-03", "", TRUE, cmsy_cite_paper),
    row("CMSY++ training data (stocks)", "9e", "all", "Train_ID_9e.csv", cmsy_oceanrep_zip, cmsy_oceanrep_page,
        178460, "4b497654e8df98a41ad9821b47ae13b1",
        "7fde3a15b8e900e136de0cfbbe98b15df724881c65c677087e545badba72e9fe",
        "CC BY 4.0", "2021-03", "", TRUE, cmsy_cite_paper),
    # JAGS installers (GPL-2), pinned to the files on SourceForge
    row("JAGS", "4.3.2", "windows", "JAGS-4.3.2.exe", paste0(cmsy_sf, "JAGS/4.x/Windows/JAGS-4.3.2.exe"),
        paste0(cmsy_sf_page, "JAGS/4.x/Windows/"), 29014264, "1ca06a15d4b03b1248129ed84037207f",
        "197f3b32f35462fdbbb566035f459bade6c61207fb2bada265be1504c0fa414f",
        "GPL-2", "2025-12-15", "R >= 4.5 (built with Rtools45); rjags 4-x", FALSE, cmsy_cite_jags),
    row("JAGS", "4.3.1", "windows", "JAGS-4.3.1.exe", paste0(cmsy_sf, "JAGS/4.x/Windows/JAGS-4.3.1.exe"),
        paste0(cmsy_sf_page, "JAGS/4.x/Windows/"), 25997787, "419e3b7df94bb8a2218ccdd6ce1a2919",
        "e7f1ea777749b17c0d64f07d80f9c3a93a5f95fa6b0b009f1568858b0106a2b3",
        "GPL-2", "2022-04-12", "R 4.2 to 4.4; rjags 4-x", FALSE, cmsy_cite_jags),
    row("JAGS", "4.3.2", "macos", "JAGS-4.3.2.pkg", paste0(cmsy_sf, "JAGS/4.x/Mac%20OS%20X/JAGS-4.3.2.pkg"),
        paste0(cmsy_sf_page, "JAGS/4.x/Mac%20OS%20X/"), 2045588, "520ccc0cfeece63c605aedb948fb998f",
        "14d171621a582d29a7e6ce11eccf853170367c17d41554e07b88abed76a2fb7e",
        "GPL-2", "2023-04-19", "R >= 4.3, macOS >= 11; rjags 4-x", FALSE, cmsy_cite_jags),
    row("JAGS", "4.3.1", "macos", "JAGS-4.3.1.pkg", paste0(cmsy_sf, "JAGS/4.x/Mac%20OS%20X/JAGS-4.3.1.pkg"),
        paste0(cmsy_sf_page, "JAGS/4.x/Mac%20OS%20X/"), 2035290, "d1ccb5f712c1004066191bba6318c061",
        "6f8f220f975556aec82275c93a4556184476f4d32ee828ee8f839590fe7c94bb",
        "GPL-2", "2022-05-03", "R 4.0 to 4.2; rjags 4-x", FALSE, cmsy_cite_jags),
    row("JAGS", "4.3.2", "source", "JAGS-4.3.2.tar.gz", paste0(cmsy_sf, "JAGS/4.x/Source/JAGS-4.3.2.tar.gz"),
        paste0(cmsy_sf_page, "JAGS/4.x/Source/"), 2032834, "7d628ee7fffc29adbee20cabe299fe02",
        "871f556af403a7c2ce6a0f02f15cf85a572763e093d26658ebac55c4ab472fc8",
        "GPL-2", "2023-03-04", "C++ compiler, BLAS and LAPACK; rjags 4-x", FALSE, cmsy_cite_jags),
    row("JAGS", "5.0.0", "windows", "JAGS-5.0.0.exe", paste0(cmsy_sf, "JAGS/5.x/Windows/Rtools45/JAGS-5.0.0.exe"),
        paste0(cmsy_sf_page, "JAGS/5.x/Windows/"), 27897120, "20b45e6351b47b32b9efa6911d7a0d9a",
        "f79f4af3da180f9e9a7c1c1ff036e806063d883ee485b5e7a458c50b0315acd4",
        "GPL-2", "2026-08-01", "R 4.5 to 4.6 (Rtools45); rjags 5-x (not on CRAN)", FALSE, cmsy_cite_jags),
    row("JAGS", "5.0.0", "macos", "JAGS-5.0.0-universal.pkg", paste0(cmsy_sf, "JAGS/5.x/macOS/JAGS-5.0.0-universal.pkg"),
        paste0(cmsy_sf_page, "JAGS/5.x/macOS/"), 14157787, "2e5f37566341653dc426a08665090be2",
        "49753af665f4ebe309c4b1b7b7d53fd10009b5b19ed766e040e5581842772316",
        "GPL-2", "2026-09-25", "R >= 4.3; rjags 5-x (not on CRAN)", FALSE, cmsy_cite_jags),
    row("JAGS", "5.0.0", "source", "JAGS-5.0.0.tar.gz", paste0(cmsy_sf, "JAGS/5.x/Source/JAGS-5.0.0.tar.gz"),
        paste0(cmsy_sf_page, "JAGS/5.x/Source/"), 2158256, "bc72c483f35b10795cc732a9d03884f4",
        "64fcd4883b8a8ee907722f49366cc9f277477a0647ada61356f17568f84ffff8",
        "GPL-2", "2026-08-01", "C++ compiler, BLAS and LAPACK; rjags 5-x", FALSE, cmsy_cite_jags),
    # rjags: 4-x from CRAN works with JAGS 4; 5-x (JAGS 5) is only on SourceForge so far
    row("rjags", "4-17", "all", "rjags", "https://cran.r-project.org/package=rjags",
        "https://cran.r-project.org/package=rjags", license = "GPL-2", released = "2025-03-24",
        requires = "JAGS 4.x", citation = "citation(\"rjags\")"),
    row("rjags", "5-3", "windows", "rjags_5-3.zip", paste0(cmsy_sf, "rjags/5/Windows/Rtools45/rjags_5-3.zip"),
        paste0(cmsy_sf_page, "rjags/5/"), 515095, "6c55023bf5a8145720436814393e39ba",
        "2e063131bd7200de18268d7579c7f927d22b2745c178b809b160e9213970abee",
        "GPL-2", "2026-08-16", "JAGS 5.x; R 4.5 to 4.6", FALSE, "citation(\"rjags\")"),
    row("rjags", "5-3", "macos-arm64", "rjags_5-3-R4.6.1-arm64.tgz",
        paste0(cmsy_sf, "rjags/5/macOS/rjags_5-3-R4.6.1-arm64.tgz"), paste0(cmsy_sf_page, "rjags/5/"),
        177358, "eea530d47a48a0423b5b5524ba17dbc3",
        "1fe67195947f9cb0df085b8d434e348a6bf4c69108bf4fa78ceee0cc5d322d5c",
        "GPL-2", "2026-09-25", "JAGS 5.x; R 4.6 (Apple silicon)", FALSE, "citation(\"rjags\")"),
    row("rjags", "5-3", "macos-x86_64", "rjags_5-3-R4.6.1-x86_64.tgz",
        paste0(cmsy_sf, "rjags/5/macOS/rjags_5-3-R4.6.1-x86_64.tgz"), paste0(cmsy_sf_page, "rjags/5/"),
        176491, "b1b87ca190afea3f832f1849785a6fdc",
        "c39a3527e2580271113c76c4ea504b6553d2eed7f17dd271c8ff966223b82d72",
        "GPL-2", "2026-09-25", "JAGS 5.x; R 4.6 (Intel)", FALSE, "citation(\"rjags\")"),
    row("rjags", "5-3", "source", "rjags_5-3.tar.gz", paste0(cmsy_sf, "rjags/5/Source/rjags_5-3.tar.gz"),
        paste0(cmsy_sf_page, "rjags/5/"), 76892, "0585c57e2d343873d41d0d7c3d560fb1",
        "dac0e7f2da817a15d8a796025ce2f36f0aa3154921bd520be4f98df2753f617a",
        "GPL-2", "2026-08-14", "JAGS 5.x; a C++ compiler", FALSE, "citation(\"rjags\")"),
    # Other R packages loaded by CMSY++16.R (snpar, archived on CRAN, is replaced; see cmsy_system())
    cran("R2jags", "0.8-9", "GPL (> 2)", "rjags"),
    cran("coda", "0.19-4.1", "GPL (>= 2)"),
    cran("gplots", "3.3.0", "GPL-2"),
    cran("mvtnorm", "1.4-2", "GPL-2"),
    cran("neuralnet", "1.44.2", "GPL (>= 2)"),
    cran("conicfit", "1.0.4", "GPL (>= 3)", "pracma, geigen"),
    # FishBase and SeaLifeBase releases behind inst/extdata/cmsy_priors.csv
    row("FishBase", "26.06", "all", "species, synonyms, stocks, estimate",
        "https://data.source.coop/cboettig/fishbase/fb/v26.06/parquet/", "https://www.fishbase.se",
        license = "CC BY-NC 4.0 (FishBase.org team, as stated for the snapshots)", released = "2026-06",
        requires = "", citation = cmsy_cite_fishbase),
    row("SeaLifeBase", "26.06", "all", "species, synonyms, stocks, estimate",
        "https://data.source.coop/cboettig/fishbase/slb/v26.06/parquet/", "https://www.sealifebase.se",
        license = "CC BY-NC 4.0 (FishBase.org team, as stated for the snapshots)", released = "2026-06",
        requires = "", citation = cmsy_cite_sealifebase)
  )
}

# Path of a bundled CMSY++ file
cmsy_file <- function(file) {
  path <- system.file("cmsy", file, package = "faofishingreport")
  if (!nzchar(path)) stop("The bundled CMSY++ file ", file, " is missing; reinstall faofishingreport.", call. = FALSE)
  path
}

# Folder for downloaded installers and packages
cmsy_download_dir <- function(data_dir = fao_data_dir()) {
  file.path(data_dir, "cmsy", "downloads")
}

# TRUE if the file has the size and MD5 checksum recorded in the registry
cmsy_file_ok <- function(path, entry) {
  file.exists(path) && (is.na(entry$size) || file.size(path) == entry$size) &&
    (is.na(entry$md5) || identical(unname(tools::md5sum(path)), entry$md5))
}

# Local copy of a registry file: an installer placed in inst/jags/, an earlier download, or a new
# download from the pinned link. The checksum is always checked.
cmsy_fetch <- function(entry, data_dir = fao_data_dir(), quiet = FALSE) {
  bundled <- system.file("jags", entry$file, package = "faofishingreport")
  if (nzchar(bundled) && cmsy_file_ok(bundled, entry)) return(bundled)
  dest <- file.path(cmsy_download_dir(data_dir), entry$file)
  if (cmsy_file_ok(dest, entry)) {
    if (!quiet) message("Using ", entry$file, " downloaded earlier: ", dest)
    return(dest)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  old <- options(timeout = max(1200, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  if (!quiet) message("Downloading ", entry$file, " (", format(structure(entry$size, class = "object_size"),
                                                         units = "auto"), ") from ", entry$url)
  utils::download.file(entry$url, dest, mode = "wb", quiet = quiet)
  if (!cmsy_file_ok(dest, entry)) {
    unlink(dest)
    stop("The downloaded ", entry$file, " does not match the size and checksum recorded for version ",
         entry$version, ". Download it by hand from ", entry$page, " and check it.", call. = FALSE)
  }
  dest
}
