# The general settings of CMSY++16.R (its section "General settings for the analysis" and the
# MCMC settings of its JAGS call), with their defaults, and the checks of settings changed by
# the user in cmsy_run(settings = ...).

cmsy_default_settings <- list(
  CV.C = 0.15, CV.cpue = 0.2, sigmaR = 0.1, cor.log.rk = -0.76, rk.cor.beta = c(2.52, 3.37), nbk = 3,
  bt4pr = FALSE, ct_MSY.lim = 1.21, q.biomass.pr = c(0.9, 1.1), n = 5000, ni = 3, nab = 3, bw = 3,
  e.creep.line = TRUE, select.yr = NA, n.chains = 2, n.iter = 60000, n.burnin = 30000, n.thin = 10
)

cmsy_settings_info <- data.frame(
  setting = names(cmsy_default_settings),
  applies_to = c("all", "BSM", "not used", "charts", "all", "BSM", "BSM", "all", "BSM", "charts", "not used", "BSM", "all",
                 "charts", "output", "all", "all", "all", "all"),
  en = c("Coefficient of variation of the catch (observation error)",
         "Minimum realistic coefficient of variation of CPUE",
         "Not used by CMSY++16.R: the code sets it (sigR <- sigmaR) but never uses it; the model estimates its process error with the prior itau2 ~ dgamma(4, 0.01). It cannot be changed",
         "Correlation of log r and log k, used only in charts",
         "Beta prior of the r-k correlation plus 1",
         "Number of B/k priors used by BSM: 1 (first year), 2 (and intermediate year), 3 (and last year)",
         "Use the abundance data to set the B/k priors",
         "Ratio of catch to the MSY prior above which the B/k prior is kept constant",
         "Prior range of q when the abundance data are biomass",
         "Number of points of the r-k cloud in the analysis chart",
         "Not used by CMSY++16.R: the code sets it but never uses it. It cannot be changed",
         "Minimum number of years with abundance data to run BSM",
         "Bandwidth of ksmooth() for smoothing the catch",
         "Show the CPUE not corrected for effort creep in the biomass chart",
         "Year for which B, F, B/Bmsy and F/Fmsy are also reported (NA: none)",
         "Number of MCMC chains (JAGS)",
         "Number of MCMC iterations per chain (JAGS)",
         "Number of burn-in iterations (JAGS)",
         "Thinning of the MCMC chains (JAGS)"),
  zh = c("\u6e14\u83b7\u91cf\u89c2\u6d4b\u8bef\u5dee\u7684\u53d8\u5f02\u7cfb\u6570",
         "CPUE \u7684\u6700\u5c0f\u53d8\u5f02\u7cfb\u6570",
         "CMSY++16.R\u4e0d\u4f7f\u7528\uff1a\u4ee3\u7801\u4e2d\u6709\u8d4b\u503c\uff08sigR <- sigmaR\uff09\uff0c\u4f46\u4ece\u672a\u4f7f\u7528\uff1b\u6a21\u578b\u7684\u8fc7\u7a0b\u8bef\u5dee\u7531\u5148\u9a8c itau2 ~ dgamma(4, 0.01) \u4f30\u8ba1\u3002\u4e0d\u80fd\u4fee\u6539",
         "log r \u4e0e log k \u7684\u76f8\u5173\u7cfb\u6570\uff0c\u4ec5\u7528\u4e8e\u4f5c\u56fe",
         "r-k \u76f8\u5173\u7cfb\u6570\u52a0 1 \u7684 beta \u5148\u9a8c",
         "BSM \u4f7f\u7528\u7684 B/k \u5148\u9a8c\u4e2a\u6570\uff1a1\uff08\u9996\u5e74\uff09\u30012\uff08\u52a0\u4e2d\u95f4\u5e74\uff09\u30013\uff08\u518d\u52a0\u672b\u5e74\uff09",
         "\u7528\u4e30\u5ea6\u6570\u636e\u8bbe\u5b9a B/k \u5148\u9a8c",
         "\u6e14\u83b7\u91cf\u4e0e MSY \u5148\u9a8c\u4e4b\u6bd4\u8d85\u8fc7\u6b64\u503c\u65f6\uff0cB/k \u5148\u9a8c\u4fdd\u6301\u4e0d\u53d8",
         "\u4e30\u5ea6\u6570\u636e\u4e3a\u751f\u7269\u91cf\u65f6 q \u7684\u5148\u9a8c\u8303\u56f4",
         "\u5206\u6790\u56fe\u4e2d r-k \u70b9\u4e91\u7684\u70b9\u6570",
         "CMSY++16.R\u4e0d\u4f7f\u7528\uff1a\u4ee3\u7801\u4e2d\u6709\u8d4b\u503c\uff0c\u4f46\u4ece\u672a\u4f7f\u7528\u3002\u4e0d\u80fd\u4fee\u6539",
         "\u8fd0\u884c BSM \u6240\u9700\u4e30\u5ea6\u6570\u636e\u7684\u6700\u5c11\u5e74\u6570",
         "\u6e14\u83b7\u91cf\u5e73\u6ed1\uff08ksmooth()\uff09\u7684\u5e26\u5bbd",
         "\u5728\u751f\u7269\u91cf\u56fe\u4e2d\u663e\u793a\u672a\u6309\u6e14\u6548\u589e\u957f\uff08effort creep\uff09\u6821\u6b63\u7684 CPUE",
         "\u53e6\u5916\u62a5\u544a\u8be5\u5e74 B\u3001F\u3001B/Bmsy \u548c F/Fmsy \u7684\u5e74\u4efd\uff08NA \u4e3a\u4e0d\u62a5\u544a\uff09",
         "MCMC \u94fe\u6570\uff08JAGS\uff09",
         "\u6bcf\u6761\u94fe\u7684 MCMC \u8fed\u4ee3\u6b21\u6570\uff08JAGS\uff09",
         "\u9884\u70e7\uff08burn-in\uff09\u8fed\u4ee3\u6b21\u6570\uff08JAGS\uff09",
         "MCMC \u62bd\u7a00\u95f4\u9694\uff08JAGS\uff09"),
  stringsAsFactors = FALSE
)

#' Settings of CMSY++ and Their Defaults
#'
#' This function lists the general settings of the CMSY++ code (the section "General settings
#' for the analysis" of `CMSY++16.R` and the MCMC settings of its JAGS call), their default
#' values in CMSY++, the values [cmsy_run()] uses (the defaults, unless changed with its
#' `settings` argument), what each one does and which part of CMSY++ it affects.
#'
#' Two settings of the code, `sigmaR` and `ni`, are set in `CMSY++16.R` but used nowhere in it: the
#' process error of the model is estimated in JAGS with the prior `itau2 ~ dgamma(4, 0.01)`, so
#' changing `sigmaR` would not change any result. They are listed as "not used" and cannot be
#' changed, so that a run is not taken for a sensitivity analysis of the process error.
#'
#' Settings marked "BSM" only matter when abundance data (CPUE or biomass) are given; FAO data
#' have none, so with FAO data alone only the settings marked "all" change the results. The chart
#' and output switches of CMSY++ (`mgraphs`, `kobe.plot`, `pp.plot`, `retros`, `save.plots`,
#' `write.output`, ...) are set by the `plots` argument of [cmsy_run()], and the file names by
#' the run itself.
#'
#' @param settings Optional named list of changed settings, e.g. `list(CV.C = 0.2)`, to show
#'        them next to the defaults and check them.
#' @param lang The language of the `description` column: "en", "zh" or "both". Default is NULL,
#'        the language set with `fao_settings()`.
#' @return A data frame with one row per setting: `setting`, `cmsy_default`, `used`, `changed`,
#'         `applies_to` and `description`.
#' @references
#' Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for CMSY++. GEOMAR,
#' Germany, 17 pp. \url{https://oceanrep.geomar.de/id/eprint/52147/}
#' @export
#' @examples
#' # All settings with their CMSY++ defaults
#' cmsy_defaults()
#'
#' # Check changed settings before a run: here a larger catch error and longer chains
#' cmsy_defaults(list(CV.C = 0.2, n.iter = 100000, n.burnin = 50000), lang = "zh")
cmsy_defaults <- function(settings = list(), lang = NULL) {
  lang <- fao_lang(lang)
  used <- cmsy_check_settings(settings)
  show <- function(x) vapply(x, function(v) paste(format(v, scientific = FALSE, trim = TRUE), collapse = ", "), character(1))
  description <- switch(lang, en = cmsy_settings_info$en, zh = cmsy_settings_info$zh,
                        both = paste0(cmsy_settings_info$zh, " (", cmsy_settings_info$en, ")"))
  out <- data.frame(setting = names(cmsy_default_settings), cmsy_default = show(cmsy_default_settings),
                    used = show(used), changed = !mapply(function(a, b) isTRUE(all.equal(a, b)), cmsy_default_settings, used),
                    applies_to = cmsy_settings_info$applies_to, description = description,
                    stringsAsFactors = FALSE, row.names = NULL)
  out
}

# Full list of settings: the CMSY++ defaults with the user's changes, after checking them
cmsy_check_settings <- function(settings = list()) {
  if (is.null(settings)) settings <- list()
  if (!is.list(settings) || (length(settings) > 0 && (is.null(names(settings)) || any(names(settings) == "")))) {
    stop("`settings` must be a named list, e.g. list(CV.C = 0.2).", call. = FALSE)
  }
  managed <- c("mgraphs", "kobe.plot", "pp.plot", "retros", "BSMfits.plot", "rk.diags", "save.plots", "close.plots",
               "write.output", "write.pdf", "write.rdata", "auto.start", "catch_file", "id_file", "nn_file", "outfile",
               "stocks")
  bad <- intersect(names(settings), managed)
  if (length(bad) > 0) {
    stop("Set ", paste(bad, collapse = ", "), " with the arguments of cmsy_run() (plots, auto_start, stocks), ",
         "not in `settings`.", call. = FALSE)
  }
  unused <- intersect(names(settings), c("sigmaR", "ni"))
  if (length(unused) > 0) {
    stop("CMSY++16.R sets ", paste(unused, collapse = " and "), " but does not use ", if (length(unused) > 1) "them" else "it",
         ": changing ", if (length(unused) > 1) "them" else "it", " would not change the results",
         if ("sigmaR" %in% unused) " (the process error of the model is estimated with the prior itau2 ~ dgamma(4, 0.01))",
         ". See cmsy_defaults().", call. = FALSE)
  }
  unknown <- setdiff(names(settings), names(cmsy_default_settings))
  if (length(unknown) > 0) {
    stop("Unknown CMSY++ setting: ", paste(unknown, collapse = ", "), ". See cmsy_defaults() for the settings.",
         call. = FALSE)
  }
  out <- cmsy_default_settings
  for (name in names(settings)) {
    value <- settings[[name]]
    default <- cmsy_default_settings[[name]]
    ok <- if (is.logical(default) && !is.na(default)) {
      is.logical(value) && length(value) == 1 && !is.na(value)
    } else if (name == "select.yr") {
      length(value) == 1 && (is.na(value) || (is.numeric(value) && value == round(value)))
    } else {
      is.numeric(value) && length(value) == length(default) && all(is.finite(value))
    }
    if (!ok) {
      stop("Setting ", name, " must be ", if (is.logical(default) && !is.na(default)) "TRUE or FALSE" else
        paste0(length(default), " number", if (length(default) > 1) "s"), " (default: ",
        paste(default, collapse = ", "), ").", call. = FALSE)
    }
    out[[name]] <- value
  }
  positive <- c("CV.C", "CV.cpue", "ct_MSY.lim", "n", "nab", "bw", "n.chains", "n.iter", "n.thin")
  for (name in positive) {
    if (any(out[[name]] <= 0)) stop("Setting ", name, " must be positive.", call. = FALSE)
  }
  whole <- c("n", "nab", "n.chains", "n.iter", "n.burnin", "n.thin")
  for (name in whole) {
    if (out[[name]] != round(out[[name]])) stop("Setting ", name, " must be a whole number.", call. = FALSE)
  }
  if (!out$nbk %in% 1:3) stop("Setting nbk must be 1, 2 or 3.", call. = FALSE)
  if (out$cor.log.rk < -1 || out$cor.log.rk > 1) stop("Setting cor.log.rk must be between -1 and 1.", call. = FALSE)
  if (out$n.burnin < 0 || out$n.burnin >= out$n.iter) {
    stop("Setting n.burnin must be at least 0 and smaller than n.iter.", call. = FALSE)
  }
  if ((out$n.iter - out$n.burnin) / out$n.thin < 100) {
    stop("Too few MCMC draws per chain: (n.iter - n.burnin) / n.thin must be at least 100.", call. = FALSE)
  }
  out
}

# Lines of the settings file that set the CMSY++ settings
cmsy_settings_lines <- function(settings) {
  value <- function(x) {
    if (length(x) == 1 && is.na(x)) return("NA")
    if (is.logical(x)) return(if (x) "TRUE" else "FALSE")
    text <- format(x, scientific = FALSE, trim = TRUE, digits = 15)
    if (length(x) > 1) paste0("c(", paste(text, collapse = ", "), ")") else text
  }
  names_map <- c(n.iter = "cmsy_n_iter", n.burnin = "cmsy_n_burnin", n.thin = "cmsy_n_thin")
  vapply(names(settings), function(name) {
    target <- if (name %in% names(names_map)) names_map[[name]] else name
    paste0(formatC(target, width = -13), "<- ", value(settings[[name]]))
  }, character(1), USE.NAMES = FALSE)
}
