# MCMC settings and convergence diagnostics of CMSY++ runs.

cmsy_mcmc_parameters <- c("r", "k", "msy", "bk_first", "bk_last", "b_bmsy_last", "f_fmsy_last", "deviance")

#' MCMC Settings and Convergence of CMSY++ Runs
#'
#' This function shows how the Markov chain Monte Carlo (MCMC) of each CMSY++ run was set up and
#' whether its chains converged. CMSY++ fits its Bayesian state-space Schaefer model with JAGS
#' (Plummer 2003), here with the settings `n.chains`, `n.iter`, `n.burnin` and `n.thin` (see
#' [cmsy_defaults()]; CMSY++16.R uses 2 chains of 60,000 iterations, the first 30,000 discarded
#' as burn-in and every 10th kept). [cmsy_run()] keeps the draws of each chain of the main
#' parameters; nothing of the model is changed.
#'
#' For r, k, MSY (r k / 4, as CMSY++16.R computes it), B/k in the first and last years, B/Bmsy
#' and F/Fmsy in the last year and the deviance, the function gives the posterior mean, standard
#' deviation and 95% interval, the potential scale reduction factor Rhat of Gelman and Rubin
#' (1992), computed with `coda::gelman.diag()`, and the effective sample size of all chains
#' together, computed with `coda::effectiveSize()` (Plummer et al. 2006). Rhat compares the
#' variance between chains with the variance within chains; values near 1 mean the chains have
#' mixed: a parameter is taken as converged when Rhat is at most `rhat_max` (1.1, Gelman et al.
#' 2013). The effective sample size is the number of independent draws the autocorrelated draws
#' are worth; below `n_eff_min` (400, Vehtari et al. 2021) the 95% limits are imprecise. With the
#' settings of CMSY++16.R the draws of r and k are strongly autocorrelated, and their effective
#' sample size is often a few hundred; longer and more chains raise it, e.g.
#' `cmsy_run(settings = list(n.chains = 4, n.iter = 300000, n.burnin = 150000, n.thin = 50))`,
#' which takes about five times as long. Converged chains do not mean that the priors or the model
#' fit the stock.
#'
#' With `plot = TRUE`, a chart of each stock shows the draws of each chain in the order they were
#' kept (trace) and their densities, for r, k, MSY and the last-year B/k, saved as
#' `figures/png/cmsy/<stock>/mcmc.png`; it is also the "mcmc" chart of [cmsy_plot()].
#'
#' @param result The result of [cmsy_run()] (or [cmsy_area()]).
#' @param stocks Optional stocks: `Stock` identifiers or species, by English or Chinese FAO name,
#'        scientific name or FAO 3-alpha code. Default is NULL, all stocks that ran.
#' @param rhat_max Largest Rhat taken as converged. Default is 1.1.
#' @param n_eff_min Smallest effective sample size (all chains together) taken as enough. Default
#'        is 400.
#' @param plot A logical value. If TRUE (default), the trace and density chart of each stock is
#'        saved in `figures/png/cmsy/<stock>/`.
#' @param table A logical value. If TRUE (default), the two tables are saved in `tables/cmsy/`
#'        (`mcmc_settings.csv` and `mcmc_diagnostics.csv`).
#' @param colors Colours of the chains in the chart: NULL (default) for the colour-blind safe
#'        Okabe-Ito colours (blue, vermilion, green, ...), the name of a palette (any palette of
#'        `fao_settings(palette = )`, e.g. "npg", "Set1", "Tableau 10" or "viridis"), or a vector
#'        of colours, one per chain (e.g. `c("black", "#D7191C")`); with fewer colours than
#'        chains, colours are interpolated.
#' @param lang The language of the chart text and parameter names: "en" (English), "zh"
#'        (Chinese) or "both". Default is NULL, the language set with `fao_settings()`.
#' @param pptx A logical value. If TRUE, the charts are also saved as editable PowerPoint files.
#'        Default is the value set with `fao_settings()` (FALSE unless changed).
#' @param silent A logical value. If FALSE (default), the stocks and parameters that did not
#'        converge or have too few effective draws are reported. Default can be changed with
#'        `fao_settings(silent = TRUE)`.
#' @return Invisibly, a list with `settings` (one row per stock: `n_chains`, `n_iter`,
#'         `n_burnin`, `n_thin`, `n_kept` per chain and `n_draws` in all), `diagnostics` (one row
#'         per stock and parameter: `mean`, `sd`, `lcl`, `median`, `ucl`, `rhat`, `n_eff`,
#'         `converged` (Rhat at most `rhat_max`) and `enough_draws` (effective sample size at least
#'         `n_eff_min`)) and `plots` (the ggplot objects).
#' @references
#' Gelman, A. and Rubin, D.B. (1992). Inference from iterative simulation using multiple
#' sequences. *Statistical Science* 7(4): 457-472. \doi{10.1214/ss/1177011136}
#'
#' Gelman, A., Carlin, J.B., Stern, H.S., Dunson, D.B., Vehtari, A. and Rubin, D.B. (2013).
#' *Bayesian Data Analysis*, 3rd edition. Boca Raton: CRC Press. \doi{10.1201/b16018}
#'
#' Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical models using Gibbs
#' sampling. Proceedings of the 3rd International Workshop on Distributed Statistical Computing
#' (DSC 2003), Vienna, Austria.
#'
#' Plummer, M., Best, N., Cowles, K. and Vines, K. (2006). CODA: convergence diagnosis and output
#' analysis for MCMC. *R News* 6(1): 7-11. <https://journal.r-project.org/articles/RN-2006-002/>
#'
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B. and Buerkner, P.-C. (2021).
#' Rank-normalization, folding, and localization: an improved Rhat for assessing convergence of
#' MCMC. *Bayesian Analysis* 16(2): 667-718. \doi{10.1214/20-BA1221}
#' @seealso [cmsy_defaults()] for the MCMC settings, [cmsy_run()] to change them.
#' @export
#' @examples
#' \dontrun{
#' result <- cmsy_area("Atlantic, Northeast", top = 10)
#' mcmc <- cmsy_mcmc(result, lang = "zh")
#'
#' # Chains in other colours: a palette, or one colour per chain
#' cmsy_mcmc(result, colors = "Set1")
#' cmsy_mcmc(result, colors = c("grey30", "#D7191C"))
#' mcmc$settings
#' mcmc$diagnostics[!mcmc$diagnostics$converged, ]
#'
#' # Longer chains for a stock that did not converge
#' result <- cmsy_run(input, stocks = "PRA_27", seed = 1,
#'                    settings = list(n.iter = 200000, n.burnin = 100000, n.thin = 20))
#' cmsy_mcmc(result)
#' }
cmsy_mcmc <- function(result, stocks = NULL, rhat_max = 1.1, n_eff_min = 400, plot = TRUE, table = TRUE,
                      colors = NULL, lang = NULL, pptx = fao_setting("pptx"), silent = fao_setting("silent")) {
  if (!inherits(result, "cmsy_result")) stop("`result` must be the result of cmsy_run().", call. = FALSE)
  cmsy_chain_colors(colors, 2)
  lang <- fao_lang(lang)
  ok <- result$summary[cmsy_has_results(result$summary$run_status), ]
  if (!is.null(stocks)) ok <- ok[ok$Stock %in% cmsy_quietly(cmsy_match_stocks(stocks, ok), silent), ]
  if (nrow(ok) == 0) stop("No stock has CMSY++ results.", call. = FALSE)
  mcmc <- lapply(ok$Stock, function(stock) result$details[[stock]]$mcmc)
  has <- !vapply(mcmc, is.null, logical(1))
  if (!any(has)) {
    stop("These results have no MCMC draws: they were run with an earlier version of faofishingreport. ",
         "Run CMSY++ again (cmsy_run() or cmsy_area()) to keep them.", call. = FALSE)
  }
  if (!all(has) && !silent) {
    message("No MCMC draws for ", paste(ok$Stock[!has], collapse = ", "), " (run with an earlier version).")
  }
  ok <- ok[has, ]
  mcmc <- mcmc[has]
  species <- fao_translate(ok$species_en, "species", lang)

  settings <- do.call(rbind, lapply(seq_along(mcmc), function(i) {
    s <- mcmc[[i]]$settings
    d <- dim(mcmc[[i]]$draws)
    data.frame(Stock = ok$Stock[i], species_name = species[i], n_chains = d[2], n_iter = unname(s["n.iter"]),
               n_burnin = unname(s["n.burnin"]), n_thin = unname(s["n.thin"]), n_kept = d[1], n_draws = d[1] * d[2],
               stringsAsFactors = FALSE)
  }))
  diagnostics <- do.call(rbind, lapply(seq_along(mcmc), function(i) {
    out <- cmsy_mcmc_stats(mcmc[[i]]$draws)
    cbind(Stock = ok$Stock[i], species_name = species[i], out, stringsAsFactors = FALSE)
  }))
  diagnostics$label <- vapply(diagnostics$parameter, function(p) cmsy_label(paste0("mcmc_", p), lang), character(1))
  diagnostics$converged <- !is.na(diagnostics$rhat) & diagnostics$rhat <= rhat_max
  diagnostics$enough_draws <- !is.na(diagnostics$n_eff) & diagnostics$n_eff >= n_eff_min
  diagnostics <- diagnostics[, c("Stock", "species_name", "parameter", "label", "mean", "sd", "lcl", "median", "ucl",
                                 "rhat", "n_eff", "converged", "enough_draws")]
  rownames(diagnostics) <- NULL

  if (!silent) {
    main <- diagnostics[diagnostics$parameter != "deviance", ]
    by_stock <- function(rows, value) {
      parts <- tapply(paste0(rows$parameter, " ", value(rows)), rows$Stock, paste, collapse = ", ")
      paste0(names(parts), " (", parts, ")", collapse = "; ")
    }
    not_converged <- main[!main$converged, ]
    few <- main[main$converged & !main$enough_draws, ]
    if (nrow(not_converged) == 0) {
      message("MCMC: the chains of all ", nrow(ok), " stocks converged (Rhat <= ", rhat_max, ").")
    } else {
      message("MCMC not converged (Rhat > ", rhat_max, "): ",
              by_stock(not_converged, function(x) sprintf("%.2f", x$rhat)),
              ". Run these stocks again with longer chains before using their results.")
    }
    if (nrow(few) > 0) {
      message("Effective sample size below ", n_eff_min, ": ", by_stock(few, function(x) round(x$n_eff)),
              ". Their 95% limits are imprecise; longer and more chains help, e.g. settings = list(n.chains = 4, ",
              "n.iter = 300000, n.burnin = 150000, n.thin = 50) in cmsy_run().")
    }
  }

  plots <- list()
  if (plot) {
    for (i in seq_len(nrow(ok))) {
      p <- cmsy_chart_mcmc(list(mcmc = mcmc[[i]]), lang, cmsy_annotations(FALSE), diagnostics[diagnostics$Stock == ok$Stock[i], ],
                           colors = colors)
      if (is.null(p)) next
      fao_save_plot(p, file.path("cmsy", cmsy_safe_name(ok$Stock[i]), "mcmc"), width = 12, height = 9, pptx = pptx)
      plots[[ok$Stock[i]]] <- p
    }
  }
  if (table) {
    fao_ensure_dirs(file.path("tables", "cmsy"))
    fao_write_csv(settings, file.path("tables", "cmsy", "mcmc_settings.csv"))
    fao_write_csv(diagnostics, file.path("tables", "cmsy", "mcmc_diagnostics.csv"))
  }
  invisible(list(settings = settings, diagnostics = diagnostics, plots = plots))
}

# Posterior summary, Rhat (Gelman and Rubin 1992, coda::gelman.diag) and effective sample size
# (coda::effectiveSize) of each parameter of an array of draws (kept draws x chains x parameters)
cmsy_mcmc_stats <- function(draws) {
  parameters <- intersect(cmsy_mcmc_parameters, dimnames(draws)[[3]])
  rows <- lapply(parameters, function(p) {
    x <- matrix(draws[, , p], nrow = dim(draws)[1])
    chains <- coda::mcmc.list(lapply(seq_len(ncol(x)), function(j) coda::mcmc(x[, j])))
    rhat <- if (ncol(x) > 1 && stats::var(as.vector(x)) > 0) {
      tryCatch(coda::gelman.diag(chains, autoburnin = FALSE, multivariate = FALSE)$psrf[1, 1],
               error = function(e) NA_real_)
    } else NA_real_
    n_eff <- if (stats::var(as.vector(x)) > 0) sum(coda::effectiveSize(chains)) else NA_real_
    q <- stats::quantile(x, c(0.025, 0.5, 0.975), names = FALSE)
    data.frame(parameter = p, mean = mean(x), sd = stats::sd(as.vector(x)), lcl = q[1], median = q[2], ucl = q[3],
               rhat = unname(rhat), n_eff = unname(n_eff), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Trace and density of each chain for r, k, MSY and the last-year B/k, with Rhat and the effective
# sample size in the panel titles; without patchwork, the traces only
cmsy_chart_mcmc <- function(d, lang, ann, diagnostics = NULL, colors = NULL) {
  m <- d$mcmc
  if (is.null(m)) return(NULL)
  shown <- intersect(c("r", "k", "msy", "bk_last"), dimnames(m$draws)[[3]])
  if (is.null(diagnostics)) diagnostics <- cmsy_mcmc_stats(m$draws)
  # k and MSY are drawn in thousand tonnes
  names_shown <- c(r = "mcmc_short_r", k = "mcmc_short_k", msy = "mcmc_short_msy", bk_last = "mcmc_short_bk_last")
  title <- vapply(shown, function(p) {
    row <- diagnostics[diagnostics$parameter == p, ]
    paste0(cmsy_label(names_shown[[p]], lang), "\nRhat ", sprintf("%.3f", row$rhat), ", n_eff ", round(row$n_eff))
  }, character(1))
  long <- do.call(rbind, lapply(shown, function(p) {
    x <- matrix(m$draws[, , p], nrow = dim(m$draws)[1])
    scale <- if (p %in% c("k", "msy")) 1000 else 1
    data.frame(parameter = factor(title[[p]], levels = title), draw = rep(seq_len(nrow(x)), ncol(x)),
               chain = factor(rep(seq_len(ncol(x)), each = nrow(x))), value = as.vector(x) / scale)
  }))
  colors <- cmsy_chain_colors(colors, dim(m$draws)[2])
  chain_label <- cmsy_label("mcmc_chain", lang)
  trace <- ggplot2::ggplot(long, ggplot2::aes(x = draw, y = value, colour = chain)) +
    ggplot2::geom_line(linewidth = 0.2, alpha = 0.7) +
    ggplot2::facet_wrap(~parameter, ncol = 1, scales = "free_y", strip.position = "left") +
    ggplot2::scale_colour_manual(values = colors, name = chain_label) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linewidth = 1.2, alpha = 1))) +
    ggplot2::labs(x = cmsy_label("mcmc_draw", lang), y = NULL) +
    fao_theme() + cmsy_strip_theme()
  if (!requireNamespace("patchwork", quietly = TRUE)) return(trace)
  density <- ggplot2::ggplot(long, ggplot2::aes(x = value, colour = chain)) +
    ggplot2::geom_density(linewidth = 0.5, show.legend = FALSE) +
    ggplot2::facet_wrap(~parameter, ncol = 1, scales = "free") +
    ggplot2::scale_colour_manual(values = colors, name = chain_label) +
    ggplot2::labs(x = cmsy_label("mcmc_value", lang), y = cmsy_label("mcmc_density", lang)) +
    fao_theme() + ggplot2::theme(strip.text = ggplot2::element_blank())
  # combined with wrap_plots() and the theme of plot_annotation(), not with `&`, which has no method
  # for ggplot2 objects in R 4.3
  patchwork::wrap_plots(trace, density, widths = c(2, 1), guides = "collect") +
    patchwork::plot_annotation(theme = ggplot2::theme(legend.position = "bottom"))
}

# Colours of the MCMC chains: the Okabe-Ito colours by default, a palette name or colours given
cmsy_chain_colors <- function(colors, n) {
  if (is.null(colors)) colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442", "#000000")
  if (length(colors) > 1) {
    bad <- colors[vapply(colors, function(x) inherits(tryCatch(grDevices::col2rgb(x), error = function(e) e), "error"),
                         logical(1))]
    if (length(bad) > 0) stop("Unknown colour: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  fao_palette(n, palette = colors)
}

utils::globalVariables(c("draw", "value", "chain"))
