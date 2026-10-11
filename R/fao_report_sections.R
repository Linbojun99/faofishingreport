# The sections of fao_report(). Each section is one analysis of the package, switched on or off
# with the argument of fao_report() of the same name; settings of its function are given with
# the section name as prefix (e.g. species_h_value = 0.2 for fishing_area_species()).
#
# A section has:
#   fun         the function of the analysis (its arguments are the allowed settings)
#   part        "results" (a subsection of 2 Results and conclusions) or "assessment" (its own
#               part after the results, as CMSY++)
#   methods     the methods it uses: their paragraphs in 1.2 Methods and names in the abstract
#   run         function(ctx, args) running the analysis; returns list(value, files)
#   values      function(ctx) the values of its text
#   write       function(x, ctx) writing its text, figures and tables
#   abstract, conclusion   keys of its sentence in the abstract and in the conclusions
fao_report_sections <- function() {
  list(
    total = list(fun = "fishing_area_data", part = "results", methods = c("breakpoints", "trend"),
                 run = fao_section_total_run, values = fao_section_total_values, write = fao_section_total_write,
                 abstract = "abstract_total", conclusion = "concl_total"),
    species = list(fun = "fishing_area_species", part = "results", methods = c("breakpoints", "trend"),
                   run = fao_section_species_run, values = fao_section_species_values, write = fao_section_species_write,
                   abstract = "abstract_species", conclusion = "concl_species"),
    countries = list(fun = "fishing_area_country", part = "results", methods = c("breakpoints", "trend"),
                     run = fao_section_countries_run, values = fao_section_countries_values,
                     write = fao_section_countries_write, abstract = "abstract_countries", conclusion = "concl_countries"),
    groups = list(fun = "species_group_composition", part = "results", methods = character(0),
                  run = fao_section_simple_run("species_group_composition"), values = fao_section_groups_values,
                  write = fao_section_groups_write, abstract = NULL, conclusion = "concl_groups"),
    status = list(fun = "stock_status", part = "results", methods = "status",
                  run = fao_section_simple_run("stock_status"), values = fao_section_status_values,
                  write = fao_section_status_write, abstract = "abstract_status", conclusion = "concl_status"),
    quality = list(fun = "data_quality", part = "results", methods = character(0),
                   run = fao_section_simple_run("data_quality"), values = fao_section_quality_values,
                   write = fao_section_quality_write, abstract = NULL, conclusion = "concl_quality"),
    diversity = list(fun = "diversity_concentration", part = "results", methods = "diversity",
                     run = fao_section_simple_run("diversity_concentration"), values = fao_section_diversity_values,
                     write = fao_section_diversity_write, abstract = NULL, conclusion = "concl_diversity"),
    fleets = list(fun = "coastal_distant", part = "results", methods = character(0),
                  run = fao_section_simple_run("coastal_distant"), values = fao_section_fleets_values,
                  write = fao_section_fleets_write, abstract = NULL, conclusion = "concl_fleets"),
    cmsy = list(fun = "cmsy_area", part = "assessment", methods = "cmsy",
                run = fao_section_cmsy_run, values = fao_section_cmsy_values, write = fao_section_cmsy_write,
                abstract = "abstract_cmsy", conclusion = NULL)
  )
}

# Paragraphs of the methods and their names in the abstract
fao_report_methods <- c(breakpoints = "methods_bp", trend = "methods_trend", status = "methods_status",
                        diversity = "methods_diversity", cmsy = "methods_cmsy")

# Settings of the analyses: arguments of fao_report() starting with a section name and "_" go to
# the function of that section, e.g. species_h_value = 0.2 -> fishing_area_species(h_value = 0.2)
fao_report_args <- function(dots, sections) {
  if (length(dots) == 0) return(list())
  given <- names(dots)
  if (is.null(given) || any(given == "")) {
    stop("Settings of the analyses must be named, with the section as prefix, e.g. species_h_value = 0.2.",
         call. = FALSE)
  }
  out <- list()
  for (name in given) {
    prefix <- names(sections)[startsWith(name, paste0(names(sections), "_"))]
    if (length(prefix) == 0) {
      stop("Unknown argument of fao_report(): ", name, ". Settings of an analysis start with its section: ",
           paste0(names(sections), "_", collapse = ", "), call. = FALSE)
    }
    prefix <- prefix[which.max(nchar(prefix))]
    arg <- substring(name, nchar(prefix) + 2)
    allowed <- fao_report_allowed_args(sections[[prefix]]$fun)
    if (!arg %in% allowed) {
      stop("fao_report(): ", name, " would set ", arg, ", which is not an argument of ", sections[[prefix]]$fun,
           "(). Its arguments: ", paste(allowed, collapse = ", "), call. = FALSE)
    }
    out[[prefix]][[arg]] <- dots[[name]]
  }
  out
}

fao_report_allowed_args <- function(fun) {
  args <- names(formals(get(fun, envir = asNamespace("faofishingreport"))))
  if (fun == "cmsy_area") args <- c(args, names(formals(cmsy_input)))   # cmsy_area() passes ... to cmsy_input()
  setdiff(unique(args), c("...", "area", "area_data", "area_name", "lang", "pptx", "table"))
}

# Runs the analyses of the switched-on sections in a temporary folder (charts at 300 dpi, no
# PowerPoint), keeping the result and the charts of each
fao_report_run <- function(ctx) {
  work <- tempfile("fao_report_")
  dir.create(work)
  old_wd <- setwd(work)
  on.exit(setwd(old_wd), add = TRUE)
  old_options <- options(faofishingreport.dpi = 300, faofishingreport.pptx = FALSE)
  on.exit(options(old_options), add = TRUE)
  charts <- function() {
    files <- list.files(file.path(work, "figures", "png"), pattern = "[.]png$", recursive = TRUE, full.names = TRUE)
    stats::setNames(file.mtime(files), files)
  }
  # each step keeps the charts it saved: new files, or files written again
  ctx$step <- function(step, expr) {
    message("  ", step)
    before <- charts()
    value <- NULL
    utils::capture.output(value <- suppressWarnings(suppressMessages(expr)))
    after <- charts()
    saved <- after[is.na(before[names(after)]) | after > before[names(after)]]
    list(value = value, files = names(sort(saved)))
  }
  # the area data (and its total catch charts, also needed for the map of the countries)
  ctx$results$total <- fao_section_total_run(ctx, ctx$args$total)
  ctx$area_data <- ctx$results$total$value$area_data
  for (name in setdiff(ctx$on, "total")) {
    ctx$results[[name]] <- ctx$sections[[name]]$run(ctx, ctx$args[[name]])
  }
  invisible(ctx)
}

# Calls the function of a section with the report settings and the user's settings (which win)
fao_section_call <- function(ctx, fun, args, defaults) {
  args <- utils::modifyList(defaults, if (is.null(args)) list() else args)
  do.call(get(fun, envir = asNamespace("faofishingreport")), args)
}

fao_section_simple_run <- function(fun) {
  force(fun)
  function(ctx, args) {
    ctx$step(fun, fao_section_call(ctx, fun, args, list(area_data = ctx$area_data, table = FALSE, lang = ctx$lang)))
  }
}

# The chart of a step whose path matches `pattern` (or the n-th chart)
fao_section_chart <- function(ctx, section, pattern = NULL, n = 1) {
  files <- ctx$results[[section]]$files
  if (!is.null(pattern)) files <- files[grepl(pattern, files)]
  if (length(files) >= n) files[n] else NA_character_
}

# Values shared by all sections
fao_report_base_values <- function(ctx) {
  data <- ctx$area_data
  last_year <- max(data$Year)
  list(area = ctx$area$label, first_year = min(data$Year), last_year = last_year,
       y5 = last_year - 4, y10 = last_year - 9, top_n = ctx$top_n,
       n_countries = length(unique(data$Country..Name.[data$Production > 0])),
       n_species = length(unique(data$ASFIS.species..Name.[data$Production > 0])))
}

# ---- Total catch ---------------------------------------------------------------------------
fao_section_total_run <- function(ctx, args) {
  plot <- any(c("total", "countries") %in% ctx$on)
  out <- ctx$step("total catch", fao_section_call(ctx, "fishing_area_data", args, list(
    area_name = ctx$area$name, lang = ctx$lang, data_version = ctx$data_version, plot = plot)))
  out$trend <- suppressMessages(peak_trend(out$value$area_data, by = "species", top_n = 0, plot = FALSE, table = FALSE, lang = "en"))
  out$world <- suppressMessages(load_fao_data(ctx$data_version))
  out
}

fao_section_total_values <- function(ctx) {
  lang <- ctx$lang
  data <- ctx$area_data
  last_year <- max(data$Year)
  first_year <- min(data$Year)
  catch <- function(x) fao_report_catch(x, lang)
  list_text <- function(x) fao_report_list(x, lang)
  total <- tapply(data$Production, data$Year, sum)
  years <- as.integer(names(total))
  test <- suppressWarnings(stats::cor.test(years, as.numeric(total), method = "kendall", exact = FALSE))
  overall <- if (test$p.value < 0.05) ifelse(test$estimate > 0, "overall_up", "overall_down") else "overall_none"
  recent <- ctx$results$total$trend[1, ]
  coefs <- ctx$results$total$value$coefs_data
  stages <- if (!is.null(coefs) && nrow(coefs) > 1) {
    data.frame(from = coefs$start_year, to = c(coefs$start_year[-1] - 1, last_year), mean = coefs$intercept)
  }
  # share of the world catch of marine areas (or inland waters for an inland area)
  areas <- fao_group_table()$area
  same_water <- areas$name_en[areas$inland_marine_en == areas$inland_marine_en[match(ctx$area$name, areas$name_en)]]
  world <- ctx$results$total$world
  world <- world[world$FAO.major.fishing.area..Name. %in% same_water, ]
  share <- 100 * as.numeric(total) / as.numeric(tapply(world$Production, world$Year, sum)[names(total)])
  list(
    total_first = catch(total[1]), total_last = catch(total[length(total)]),
    peak_year = years[which.max(total)], peak_catch = catch(max(total)),
    pct_peak = fao_report_pct(100 * total[length(total)] / max(total)),
    overall = fao_report_text(overall, lang),
    growth10 = sprintf("%.1f", recent$growth_pct_per_year),
    trend10 = fao_report_text(paste0("trend10_", recent$trend), lang, list(p = fao_report_p(recent$p_value, lang))),
    n_stages = if (is.null(stages)) 1 else nrow(stages),
    stage_years = if (is.null(stages)) paste0(first_year, "–", last_year) else
      list_text(paste0(stages$from, "–", stages$to, if (lang == "zh") " 年")),
    break_years = if (is.null(stages)) "" else list_text(stages$from[-1]),
    stages = if (is.null(stages)) "" else paste(vapply(seq_len(nrow(stages)), function(i) {
      fao_report_text("stage", lang, list(from = stages$from[i], to = stages$to[i], mean = catch(stages$mean[i])))
    }, character(1)), collapse = if (lang == "zh") "；" else "; "),
    has_stages = !is.null(stages),
    share_last = fao_report_pct(share[length(share)]), share_mean = fao_report_pct(mean(share, na.rm = TRUE))
  )
}

fao_section_total_write <- function(x, ctx) {
  text <- ctx$text
  x <- fao_report_par(x, text("total_p1", fig_total = fao_report_ref(x, "figures", "total")))
  x <- fao_report_figure(x, "total", fao_section_chart(ctx, "total", "fishing_trend"), text("fig_total"))
  x <- fao_report_par(x, text(if (isTRUE(ctx$values$has_stages)) "total_p2" else "total_p2_none"))
  x <- fao_report_par(x, text("total_p3", fig_share = fao_report_ref(x, "figures", "share")))
  fao_report_figure(x, "share", fao_section_chart(ctx, "total", "fishing_percentage"), text("fig_share"))
}

# ---- Main species and countries of the last five years --------------------------------------
fao_section_species_run <- function(ctx, args) {
  out <- ctx$step("main species", fao_section_call(ctx, "fishing_area_species", args, list(
    area_data = ctx$area_data, rank_range = c(1, ctx$top_n), table = FALSE, lang = ctx$lang)))
  out$trend <- suppressMessages(peak_trend(ctx$area_data, by = "species", top_n = Inf, plot = FALSE, table = FALSE, lang = "en"))
  out
}

fao_section_countries_run <- function(ctx, args) {
  out <- ctx$step("main countries", fao_section_call(ctx, "fishing_area_country", args, list(
    area_data = ctx$area_data, rank_range = c(1, ctx$top_n), table = FALSE, lang = ctx$lang)))
  out$trend <- suppressMessages(peak_trend(ctx$area_data, by = "country", top_n = Inf, plot = FALSE, table = FALSE, lang = "en"))
  out
}

# The main items of the last five years, with their peak and 10-year trend
fao_section_top <- function(ctx, column, trend_table) {
  data <- ctx$area_data
  window <- data[data$Year >= max(data$Year) - 4, ]
  sums <- sort(tapply(window$Production, window[[column]], sum), decreasing = TRUE)
  top <- utils::head(sums[sums > 0], ctx$top_n)
  trend <- trend_table[match(names(top), trend_table$name_en), ]
  data.frame(name_en = names(top), mean = as.numeric(top) / 5, share = 100 * as.numeric(top) / sum(window$Production),
             peak_year = trend$peak_year, pct_of_peak = trend$pct_of_peak, trend = trend$trend, stringsAsFactors = FALSE)
}

fao_section_trend_parts <- function(kind, trend, names, lang) {
  up <- names[trend %in% "up"]
  down <- names[trend %in% "down"]
  part <- function(key, items) {
    if (length(items) > 0) {
      fao_report_text(key, lang, list(n = length(items), list = fao_report_list(items, lang)))
    } else {
      fao_report_text(paste0(key, "_none"), lang)
    }
  }
  list(up_part = part(paste0(kind, "_up"), up), down_part = part(paste0(kind, "_down"), down))
}

fao_section_species_values <- function(ctx) {
  lang <- ctx$lang
  species <- fao_section_top(ctx, "ASFIS.species..Name.", ctx$results$species$trend)
  names <- fao_report_species_names(species$name_en, lang, fao_group_table()$species)
  trends <- fao_section_trend_parts("species", species$trend, names$plain, lang)
  down <- names$plain[species$trend %in% "down"]
  list(
    species_table = species, species_names = names,
    top_species = fao_report_list(names$full[seq_len(min(5, nrow(species)))], lang),
    top5_species_share = fao_report_pct(sum(utils::head(species$share, 5))),
    top1_species_plain = names$plain[1], top1_species_mean = fao_report_catch(species$mean[1], lang),
    top1_species_share = fao_report_pct(species$share[1]),
    species_up_part = trends$up_part, species_down_part = trends$down_part,
    species_note = if (length(down) > 0) {
      fao_report_text("concl_species_down", lang, list(list = fao_report_list(utils::head(down, 3), lang)))
    } else {
      fao_report_text("concl_species_none", lang)
    }
  )
}

fao_section_countries_values <- function(ctx) {
  lang <- ctx$lang
  countries <- fao_section_top(ctx, "Country..Name.", ctx$results$countries$trend)
  names <- if (lang == "zh") fao_translate(countries$name_en, "country", "zh") else countries$name_en
  trends <- fao_section_trend_parts("countries", countries$trend, names, lang)
  last <- ctx$area_data[ctx$area_data$Year == max(ctx$area_data$Year), ]
  list(
    country_table = countries, country_names = names,
    n_countries_last = sum(tapply(last$Production, last$Country..Name., sum) > 0),
    top_countries = fao_report_list(utils::head(names, 5), lang),
    top5_countries_share = fao_report_pct(sum(utils::head(countries$share, 5))),
    top1_country = names[1], top1_country_mean = fao_report_catch(countries$mean[1], lang),
    top1_country_share = fao_report_pct(countries$share[1]),
    countries_up_part = trends$up_part, countries_down_part = trends$down_part
  )
}

fao_section_species_write <- function(x, ctx) {
  text <- ctx$text
  values <- ctx$values
  x <- fao_report_par(x, text("species_p1", tab_species = fao_report_ref(x, "tables", "species"),
                              fig_species = fao_report_ref(x, "figures", "species")))
  x <- fao_report_table(x, "species", fao_report_top_table(values, "species", ctx$lang), text("tab_species"))
  x <- fao_report_figure(x, "species", fao_section_chart(ctx, "species", "area_species[.]png$"), text("fig_species"))
  x <- fao_report_par(x, text("species_p2", up_part = values$species_up_part, down_part = values$species_down_part,
                              fig_species_wrap = fao_report_ref(x, "figures", "species_wrap")))
  fao_report_figure(x, "species_wrap", fao_section_chart(ctx, "species", "area_species_wrap"), text("fig_species_wrap"))
}

fao_section_countries_write <- function(x, ctx) {
  text <- ctx$text
  values <- ctx$values
  x <- fao_report_par(x, text("countries_p1", tab_countries = fao_report_ref(x, "tables", "countries"),
                              fig_countries = fao_report_ref(x, "figures", "countries"),
                              fig_map = fao_report_ref(x, "figures", "map")))
  x <- fao_report_table(x, "countries", fao_report_top_table(values, "countries", ctx$lang), text("tab_countries"))
  x <- fao_report_figure(x, "countries", fao_section_chart(ctx, "countries", "area_country[.]png$"), text("fig_countries"))
  x <- fao_report_figure(x, "map", fao_section_chart(ctx, "total", "area_country_map"), text("fig_map"))
  x <- fao_report_par(x, text("countries_p2", up_part = values$countries_up_part, down_part = values$countries_down_part,
                              fig_countries_wrap = fao_report_ref(x, "figures", "countries_wrap")))
  fao_report_figure(x, "countries_wrap", fao_section_chart(ctx, "countries", "area_country_wrap"), text("fig_countries_wrap"))
}

# ---- ISSCAAP groups of the last year ---------------------------------------------------------
fao_section_groups_values <- function(ctx) {
  lang <- ctx$lang
  data <- ctx$area_data
  last <- data[data$Year == max(data$Year), ]
  groups <- fao_group_table()$species
  group_of <- groups[[paste0("isscaap_", if (lang == "zh") "zh" else "en")]][match(last$ASFIS.species..Name., groups$name_en)]
  sums <- sort(tapply(last$Production, group_of, sum), decreasing = TRUE)
  share <- 100 * sums / sum(last$Production)
  value <- function(i, what) {
    if (length(sums) < i) return("-")
    if (what == "name") names(sums)[i] else fao_report_pct(share[i])
  }
  list(g1 = value(1, "name"), g1_share = value(1, "share"), g2 = value(2, "name"), g2_share = value(2, "share"),
       g3 = value(3, "name"), g3_share = value(3, "share"))
}

fao_section_groups_write <- function(x, ctx) {
  x <- fao_report_par(x, ctx$text("groups_p1", fig_groups = fao_report_ref(x, "figures", "groups")))
  fao_report_figure(x, "groups", fao_section_chart(ctx, "groups", "_share[.]png$"), ctx$text("fig_groups"))
}

# ---- Stock status plots ----------------------------------------------------------------------
fao_section_status_values <- function(ctx) {
  status <- ctx$results$status$value
  last <- status$yearly[status$yearly$Year == max(ctx$area_data$Year), ]
  last <- last[order(as.integer(last$status_label)), ]
  oc <- as.integer(last$status_label) %in% c(3, 4)
  pcts <- function(column) fao_report_list(paste0(fao_report_pct(last[[column]]), "%"), ctx$lang)
  list(n_stocks = nrow(status$stocks), stock_pcts = pcts("stocks_pct"), catch_pcts = pcts("catch_pct"),
       oc_pct = fao_report_pct(sum(last$stocks_pct[oc])), oc_catch_pct = fao_report_pct(sum(last$catch_pct[oc])))
}

fao_section_status_write <- function(x, ctx) {
  x <- fao_report_par(x, ctx$text("status_p1", fig_status = fao_report_ref(x, "figures", c("status", "status_catch"))))
  x <- fao_report_figure(x, "status", fao_section_chart(ctx, "status", n = 1), ctx$text("fig_status"))
  x <- fao_report_figure(x, "status_catch", fao_section_chart(ctx, "status", n = 2), ctx$text("fig_status_catch"))
  fao_report_par(x, ctx$text("status_p2"))
}

# ---- Data quality of the last year -----------------------------------------------------------
fao_section_quality_values <- function(ctx) {
  data <- ctx$area_data
  last <- data[data$Year == max(data$Year), ]
  groups <- fao_group_table()$species
  level <- groups$taxon_level[match(last$ASFIS.species..Name., groups$name_en)]
  pct <- function(keep) fao_report_pct(100 * sum(last$Production[keep], na.rm = TRUE) / sum(last$Production))
  list(official_pct = pct(last$Production.Type == "actual"), est_pct = pct(last$Production.Type == "estimate"),
       species_pct = pct(level %in% "species"), nonspecies_pct = pct(!level %in% "species"))
}

fao_section_quality_write <- function(x, ctx) {
  x <- fao_report_par(x, ctx$text("quality_p1", fig_quality = fao_report_ref(x, "figures", c("quality", "quality_taxon"))))
  x <- fao_report_figure(x, "quality", fao_section_chart(ctx, "quality", n = 1), ctx$text("fig_quality"))
  x <- fao_report_figure(x, "quality_taxon", fao_section_chart(ctx, "quality", n = 2), ctx$text("fig_quality_taxon"))
  fao_report_par(x, ctx$text("quality_p2"))
}

# ---- Diversity and concentration -------------------------------------------------------------
fao_section_diversity_values <- function(ctx) {
  diversity <- ctx$results$diversity$value
  first <- diversity[diversity$Year == min(diversity$Year), ]
  last <- diversity[diversity$Year == max(diversity$Year), ]
  concentration <- function(hhi) {
    fao_report_text(if (hhi < 1500) "conc_low" else if (hhi < 2500) "conc_moderate" else "conc_high", ctx$lang)
  }
  list(h_first = sprintf("%.2f", first$shannon_species), h_last = sprintf("%.2f", last$shannon_species),
       hhi_species = round(last$hhi_species), hhi_countries = round(last$hhi_countries),
       conc_species = concentration(last$hhi_species), conc_countries = concentration(last$hhi_countries),
       top5_last = fao_report_pct(last$top5_species_pct))
}

fao_section_diversity_write <- function(x, ctx) {
  x <- fao_report_par(x, ctx$text("diversity_p1", fig_diversity = fao_report_ref(x, "figures", "diversity")))
  fao_report_figure(x, "diversity", fao_section_chart(ctx, "diversity"), ctx$text("fig_diversity"))
}

# ---- Coastal countries and distant-water fleets ----------------------------------------------
fao_section_fleets_values <- function(ctx) {
  lang <- ctx$lang
  fleets <- ctx$results$fleets$value
  levels <- levels(fleets$yearly$category)
  by_year <- function(category) {
    keep <- fleets$yearly$category == category
    tapply(fleets$yearly$Tonnes[keep], fleets$yearly$Year[keep], sum)
  }
  total <- tapply(fleets$yearly$Tonnes, fleets$yearly$Year, sum)
  coastal <- 100 * by_year(levels[1]) / total
  distant <- 100 * by_year(levels[2]) / total
  peak <- which.max(distant)
  recent <- fleets$distant[fleets$distant$Year > max(ctx$area_data$Year) - 5 &
                             as.character(fleets$distant$category) != fao_label("other_distant", lang), ]
  top <- utils::head(sort(tapply(recent$Tonnes, as.character(recent$category), sum), decreasing = TRUE), 3)
  list(coastal_pct = fao_report_pct(coastal[length(coastal)]), distant_pct = fao_report_pct(distant[length(distant)]),
       distant_peak_year = names(distant)[peak], distant_peak_pct = fao_report_pct(distant[peak]),
       distant_change = fao_report_text(if (distant[length(distant)] < 0.8 * distant[peak]) "distant_down" else "distant_same", lang),
       top_distant = fao_report_list(names(top), lang))
}

fao_section_fleets_write <- function(x, ctx) {
  x <- fao_report_par(x, ctx$text("fleets_p1", fig_fleets = fao_report_ref(x, "figures", "fleets")))
  fao_report_figure(x, "fleets", fao_section_chart(ctx, "fleets", n = 1), ctx$text("fig_fleets"))
}

# ---- CMSY++ ------------------------------------------------------------------------------------
fao_section_cmsy_run <- function(ctx, args) {
  if (!is.null(ctx$cmsy_result)) return(list(value = ctx$cmsy_result, files = character(0)))
  args <- utils::modifyList(list(top = ctx$top_n), if (is.null(args)) list() else args)
  list(value = fao_report_cmsy_run(ctx$area_data, args, ctx$lang, ctx$cmsy_dir), files = character(0))
}

fao_section_cmsy_values <- function(ctx) fao_report_cmsy_values(ctx$results$cmsy$value, ctx$lang)

fao_section_cmsy_write <- function(x, ctx) fao_report_cmsy_section(x, ctx$values, ctx$text, ctx$part_number)

# ---- The report --------------------------------------------------------------------------------
# Abstract, data and methods, the results of every section, conclusions, the assessment part,
# references and appendix
fao_report_body <- function(x, meta, ctx) {
  state <- attr(x, "fao_report")
  lang <- ctx$lang
  text <- ctx$text
  sections <- ctx$sections[ctx$on]
  word <- function(key) fao_report_text(key, lang)
  par <- function(x, key, style = "FAOBody") fao_report_par(x, text(key), style)
  heading <- function(x, title, level = 1) fao_report_par(x, title, paste0("FAOHeading", level))
  blank <- function(x, style = "FAOAuthor") officer::body_add_par(x, "", style = fao_report_style_name(style))

  # Cover, on its own page without page number
  x <- blank(x, "FAOCoverText")
  x <- blank(x, "FAOCoverText")
  x <- fao_report_par(x, meta$project, "FAOCoverProject")
  x <- blank(x, "FAOCoverText")
  x <- blank(x, "FAOCoverText")
  x <- fao_report_par(x, meta$title, "FAOCoverTitle")
  for (i in 1:6) x <- blank(x, "FAOCoverText")
  x <- fao_report_par(x, text("cover_no", value = meta$report_no), "FAOCoverText")
  x <- fao_report_par(x, text("cover_author", value = meta$author), "FAOCoverText")
  if (!is.null(meta$affiliation)) x <- fao_report_par(x, text("cover_affiliation", value = meta$affiliation), "FAOCoverText")
  x <- fao_report_par(x, text("cover_date", value = meta$date), "FAOCoverText")
  x <- officer::body_end_block_section(x, officer::block_section(fao_report_section(state, footer = FALSE)))

  # Title and abstract: the topics and methods of the switched-on sections, one sentence of
  # results per section, and a recommendation
  x <- fao_report_par(x, meta$report_no, "FAOTitle")
  x <- fao_report_par(x, meta$title, "FAOTitle")
  x <- fao_report_par(x, meta$author, "FAOAuthor")
  x <- blank(x)
  x <- par(x, "abstract_heading", "FAOAbstractHeading")
  x <- blank(x)
  methods <- unique(unlist(lapply(sections, `[[`, "methods")))
  topics <- vapply(names(sections), function(n) word(paste0("topic_", n)), character(1))
  intro <- text("abstract_intro", topics = fao_report_list(topics, lang),
                methods_part = if (length(methods) > 0) {
                  text("abstract_methods", methods = fao_report_list(vapply(methods, function(m) word(paste0("method_", m)), character(1)), lang))
                } else "")
  results <- unlist(lapply(sections, function(s) if (!is.null(s$abstract)) text(s$abstract)))
  separator <- if (lang == "zh") "" else " "
  x <- fao_report_par(x, paste(c(intro, paste0(word("abstract_results"), paste(results, collapse = separator)),
                                 word("abstract_end")), collapse = separator))

  # 1 Data and methods
  x <- heading(x, word("h_data_methods"))
  x <- heading(x, word("h_data"), 2)
  x <- par(x, "data_p1")
  x <- par(x, "data_p2")
  x <- par(x, "data_note", "FAONote")
  x <- heading(x, word("h_methods"), 2)
  for (key in c(fao_report_methods[intersect(names(fao_report_methods), methods)], "methods_software")) x <- par(x, key)

  # 2 Results and conclusions: one subsection per section, then the conclusions
  results_sections <- names(sections)[vapply(sections, function(s) s$part == "results", logical(1))]
  x <- heading(x, word("h_results"))
  number <- 0
  for (name in results_sections) {
    number <- number + 1
    x <- heading(x, paste0("2.", number, " ", word(paste0("h_", name))), 2)
    x <- sections[[name]]$write(x, ctx)
  }
  x <- heading(x, paste0("2.", number + 1, " ", word("h_conclusions")), 2)
  conclusions <- unlist(lapply(sections[results_sections], function(s) if (!is.null(s$conclusion)) text(s$conclusion)))
  for (i in seq_along(conclusions)) {
    x <- fao_report_par(x, paste0(fao_report_text("concl_number", lang, list(n = i)), conclusions[i]))
  }
  x <- par(x, "concl_note", "FAONote")

  # 3 and following: assessments (CMSY++), each in its own part
  part <- 2
  for (name in names(sections)[vapply(sections, function(s) s$part == "assessment", logical(1))]) {
    part <- part + 1
    ctx$part_number <- part
    x <- sections[[name]]$write(x, ctx)
  }

  # References, in the order of citation or alphabetical
  x <- heading(x, word("h_references"))
  entries <- vapply(state$cited, function(k) fao_ref_entry(state$references[[k]], state$citation, state$suffix[[k]]), character(1))
  if (state$citation == "numeric") {
    entries <- paste0("[", seq_along(entries), "] ", entries)
  } else {
    entries <- entries[order(tolower(gsub("[*]", "", entries)))]
  }
  for (entry in entries) x <- fao_report_par(x, entry, "FAOReference")
  x <- par(x, paste0("ref_note_", state$citation), "FAONote")

  # Appendix: catch by decade of the main species and countries, CMSY++ tables
  x <- heading(x, word("h_appendix"))
  if ("species" %in% ctx$on) {
    x <- fao_report_table(x, "app_species", fao_report_decade_table(ctx$area_data, ctx$values, "species", lang),
                          text("app_species"), appendix = TRUE)
  }
  if ("countries" %in% ctx$on) {
    x <- fao_report_table(x, "app_countries", fao_report_decade_table(ctx$area_data, ctx$values, "countries", lang),
                          text("app_countries"), appendix = TRUE)
  }
  if ("cmsy" %in% ctx$on) {
    x <- fao_report_table(x, "app_cmsy_points", ctx$values$cmsy_appendix$points, text("app_cmsy_points"), appendix = TRUE)
    x <- fao_report_table(x, "app_cmsy_priors", ctx$values$cmsy_appendix$priors, text("app_cmsy_priors"), appendix = TRUE)
    x <- fao_report_table(x, "app_cmsy_sources", ctx$values$cmsy_appendix$sources, text("app_cmsy_sources"), appendix = TRUE)
    x <- fao_report_par(x, text("app_cmsy_note"), "FAONote")
  }
  officer::body_set_default_section(x, fao_report_section(state, footer = TRUE))
}
