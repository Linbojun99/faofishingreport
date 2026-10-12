#' Fishing Area Species Analysis
#'
#' This function analyzes the fishing area data and generates plots for specified species based on their ranking in terms of total tonnes.
#'
#' @param area_data A data frame containing the fishing area data.
#' @param rank_range The ranks of the species to show, ranked by their catch (see `rank_years`): the ranks
#'   listed, e.g. `1:10` (default) for the first ten, `c(5, 6)` for the fifth and sixth, or
#'   `c(1, 10)` for the first and the tenth. Ranks beyond the number of species are left out.
#'   Until version 0.9.0 two numbers gave the first and the last rank (`c(1, 10)` was the
#'   first ten).
#' @param rank_years The number of recent years whose catch ranks the species items, e.g. 5 for the
#'        last five years (as in the tables of [fao_report()]). Default is NULL: the catch of all years.
#'        The charts show the whole series either way.
#' @param plot A logical value indicating whether to generate and save plots.
#' @param table A logical value indicating whether to generate and save tables
#' @param timeseries_analysis A logical value indicating whether to analysis timeseries.
#' @param line_width A numeric value specifying the line width. Default is 1.5.
#' @param point_size A numeric value specifying the point size. Default is 1.8.
#' @param point_alpha A numeric value specifying the point alpha. Default is 0.7.
#' @param point_shape A numeric or character value specifying the shape of the points. Default is 1.
#' @param line_alpha A numeric value specifying the line alpha. Default is 0.7.
#' @param axis_text_size A numeric value specifying the size of the axis text. Default is 10.
#' @param axis_title_size A numeric value specifying the size of the axis title. Default is 10.
#' @param strip_text_size A numeric value specifying the size of the strip text. Default is 10.
#' @param axis_text_face The font face of the axis text. Default is "plain".
#' @param axis_title_face The font face of the axis title. Default is "bold".
#' @param strip_text_face The font face of the strip text. Default is "bold".
#' @param smooth_method A character string specifying the smoothing method to be used. Default is "loess".
#' @param smooth_linetype A character string specifying the line type for the smoothing curve. Default is "dashed".
#' @param line_linewidth A numeric value specifying the line width for the line geom. Default is 1.5.
#' @param text_hjust A numeric value specifying the horizontal justification of the text geom. Default is 0.9.
#' @param text_vjust A numeric value specifying the vertical justification of the text geom. Default is -4.
#' @param text_size A numeric value specifying the size of the text geom. Default is 4.5.
#' @param text_color A character string specifying the color of the text geom. Default is "black".
#' @param facet_ncol A numeric value specifying the number of columns to be used in facet_wrap. Default is 2.
#' @param h_value The minimum segment size passed to `strucchange::breakpoints()`: a fraction of
#'        the series length (default 0.15) or a number of years.
#' @param ZH A logical value. If TRUE, chart text and names are in Chinese. Default is FALSE. `ZH = TRUE` is the same as `lang = "zh"`.
#' @param lang The language of the chart text and names: "en" (English), "zh" (Chinese) or "both"
#'        (Chinese and English). Default is NULL, which uses "zh" if `ZH = TRUE`, and otherwise the language
#'        set with `fao_settings()` ("en" unless changed).
#' @param pptx A logical value. Charts are always saved as PNG in `figures/png/`; if TRUE they are
#'        also saved as editable PowerPoint files in `figures/pptx/`. Default is FALSE, which can be
#'        changed for all functions with `fao_settings(pptx = TRUE)`.
#' @return A list with `area_species` (yearly catch of every species item in the area),
#'         `area_species_ranked` (yearly catch of the ranked species, with the others grouped),
#'         `area_species_ranked_total` (total catch and rank of the selected species) and
#'         `coefs_data` (start year, end year and mean catch of each period between breakpoints).
#'         Charts and tables are saved if `plot` and `table` are TRUE.
#' @family fishing area analyses
#' @inheritParams summary_params
#' @export
#'
#' @examples
#' \dontrun{
#' FAO27 <- fishing_area_data("Atlantic, Northeast")
#' fishing_area_species(FAO27$area_data, rank_range = 1:10)    # the ten main species
#' fishing_area_species(FAO27$area_data, rank_range = 11:20)   # the next ten
#' }
fishing_area_species <- function(area_data, rank_range = 1:10, rank_years = NULL, plot = TRUE, table=TRUE,timeseries_analysis=TRUE,
                                 line_width = 1.5,
                                 point_size = 1.8,
                                 point_alpha = 0.7,
                                 point_shape=1,
                                 line_alpha = 0.7,
                                 axis_text_size = 10,
                                 axis_title_size = 10,
                                 strip_text_size = 10,
                                 axis_text_face = "plain",
                                 axis_title_face = "bold",
                                 strip_text_face = "bold", smooth_method = "loess",
                                 smooth_linetype = "dashed",
                                 line_linewidth = 1.5,
                                 text_hjust = 0.9,
                                 text_vjust = -4,
                                 text_size = 4.5,
                                 text_color = "black",
                                 facet_ncol = 2,
                                 h_value = 0.15,
                                 ZH = FALSE, lang = NULL,
                                 pptx = getOption("faofishingreport.pptx", FALSE), plot_show = fao_setting("plot_show")) {
  run <- fao_run_begin(plot_show)
  on.exit(fao_run_end(run), add = TRUE)
  ranks <- fao_rank_select(rank_range)

  lang <- fao_lang(lang, ZH)
  area_data$ASFIS.species..Name. <- fao_translate(area_data$ASFIS.species..Name., "species", lang)
  last_year <- max(area_data$Year, na.rm = TRUE)
  year_breaks <- fao_year_breaks(area_data$Year)

  #area_species
  area_species <- area_data %>%
    dplyr::group_by(Year,ASFIS.species..Name.) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE))

  area_species$ASFIS.species..Name.<-as.factor(area_species$ASFIS.species..Name.)

  unique_species_count <- area_species %>%
    dplyr::group_by(ASFIS.species..Name.) %>%
    dplyr::summarise(total_tonnes = sum(Tonnes, na.rm = TRUE)) %>%
    dplyr::filter(total_tonnes > 0) %>%
    nrow()

  fao_verbose_message(paste("The total number of species with non-zero catch over the years is:", unique_species_count))

  area_species_table <- area_species %>%
    dplyr::mutate(Decade = fao_decade(Year))

  # Mean yearly catch of each decade: its catch divided by the years of the data in that decade, the
  # same for every item (years without catch count as zero, and rows of several areas are added)
  decade_years <- table(fao_decade(unique(area_data$Year)))
  decade_avg_production_species <- area_species_table %>%
    dplyr::group_by(Decade, ASFIS.species..Name.) %>%
    dplyr::summarize(avg_production = sum(Tonnes, na.rm = TRUE) / decade_years[[Decade[1]]])

  # 计算最近5年的产量
  recent_production_species <- area_species_table %>%
    dplyr::filter(Year > last_year - 5) %>%
    dplyr::group_by(Year, ASFIS.species..Name.) %>%
    dplyr::summarize(yearly_production = sum(Tonnes, na.rm = TRUE))

  # 计算最后一年每个品种的产量
  total_production_2021_species <- area_species_table %>%
    dplyr::filter(Year == last_year) %>%
    dplyr::group_by(ASFIS.species..Name.) %>%
    dplyr::summarize(total_2021_production = sum(Tonnes, na.rm = TRUE))

  # 计算最后一年的总产量
  total_production_2021 <- sum(total_production_2021_species$total_2021_production, na.rm = TRUE)

  # 计算每个品种在最后一年的产量占总产量的百分比
  percentage_of_total_species <- total_production_2021_species %>%
    dplyr::mutate(percentage_of_total = (total_2021_production / total_production_2021) * 100)


  # 输出结果
  fao_verbose_print(decade_avg_production_species)
  fao_verbose_print(recent_production_species)
  fao_verbose_print(percentage_of_total_species)
  # 检查并创建 'tables' 目录
  if (table && !dir.exists("tables")) {
    dir.create("tables")
  }

  # ...

  #area_species_Total
  # Steps 1 and 2: Aggregate data by year and species name, then calculate total Tonnes for each species
  area_species_Total <- area_data %>%
    dplyr::group_by(Year, ASFIS.species..Name.) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(ASFIS.species..Name.) %>%
    dplyr::summarise(Total_Tonnes = sum(Tonnes, na.rm = TRUE), .groups = "drop")

  total_tonnes <- sum(area_species_Total$Total_Tonnes)

  area_species_Total <- area_species_Total %>%
    dplyr::mutate(Percentage = (Total_Tonnes / total_tonnes) * 100)

  # The ranking: the catch of all years, or of the last `rank_years` years
  rank_data <- if (is.null(rank_years)) area_data else area_data[area_data$Year > last_year - rank_years, ]
  rank_tonnes <- tapply(rank_data$Production, rank_data$ASFIS.species..Name., sum, na.rm = TRUE)
  area_species_Total$Rank_Tonnes <- as.numeric(rank_tonnes[as.character(area_species_Total$ASFIS.species..Name.)])
  area_species_Total$Rank_Tonnes[is.na(area_species_Total$Rank_Tonnes)] <- 0

  # Adjust these lines to filter species based on the specified rank range
  ranked_species <- area_species_Total %>%
    dplyr::arrange(desc(Rank_Tonnes), desc(Total_Tonnes))

  selected_species <- ranked_species %>%
    fao_rank_slice(ranks, "species")

  plot_data <- area_data %>%
    dplyr::filter(ASFIS.species..Name. %in% selected_species$ASFIS.species..Name.) %>%
    dplyr::group_by(Year, ASFIS.species..Name.) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE), .groups = "drop")

  plot_data <- plot_data %>%
    dplyr::left_join(selected_species, by = "ASFIS.species..Name.") %>%
    dplyr::mutate(ASFIS.species..Name. = forcats::fct_reorder(ASFIS.species..Name., -Rank_Tonnes))

  # 根据排名范围筛选数据
  decade_avg_production_species_ranked <- decade_avg_production_species %>%
    dplyr::inner_join(selected_species, by = "ASFIS.species..Name.") %>%
    dplyr::select(-Total_Tonnes, -Percentage, -Rank_Tonnes)  # 删除不需要的列

  recent_production_species_ranked <- recent_production_species %>%
    dplyr::inner_join(selected_species, by = "ASFIS.species..Name.") %>%
    dplyr::select(-Total_Tonnes, -Percentage, -Rank_Tonnes)  # 删除不需要的列

  percentage_of_total_species_ranked <- percentage_of_total_species %>%
    dplyr::inner_join(selected_species, by = "ASFIS.species..Name.") %>%
    dplyr::select(-Total_Tonnes, -Percentage, -Rank_Tonnes)  # 删除不需要的列

  # 输出结果
  fao_verbose_print(decade_avg_production_species_ranked)
  fao_verbose_print(recent_production_species_ranked)
  fao_verbose_print(percentage_of_total_species_ranked)



  # 将 decade_avg_production_species_ranked 数据框重塑为宽格式
  decade_avg_wide <- decade_avg_production_species_ranked %>%
    tidyr::spread(key = Decade, value = avg_production)

  # 将 recent_production_species_ranked 数据框重塑为宽格式
  recent_production_wide <- recent_production_species_ranked %>%
    tidyr::spread(key = Year, value = yearly_production)

  # 重命名 percentage_of_total_species_ranked 中的列以避免连接时的名称冲突
  percentage_of_total_species_ranked <- percentage_of_total_species_ranked %>%
    dplyr::rename(!!paste0("total_", last_year, "_production") := total_2021_production,
                  !!paste0("percentage_of_total_", last_year) := percentage_of_total)

  # 将所有数据框连接到一起
  final_data <- decade_avg_wide %>%
    dplyr::left_join(recent_production_wide, by = "ASFIS.species..Name.") %>%
    dplyr::left_join(percentage_of_total_species_ranked, by = "ASFIS.species..Name.")




  get_bp_coefs <- function(species_name, data, h_value ) {
    country_data <- data[data$ASFIS.species..Name. == species_name, ]
    country_data <- country_data[order(country_data$Year), ]  # Ensure data is sorted by year

    # Find the longest continuous sequence of years
    years <- country_data$Year
    diff_years <- diff(years)
    breaks <- which(diff_years != 1)

    if (length(breaks) == 0) {
      seq_start <- 1
      seq_end <- length(years)
    } else {
      longest_seq_end <- which.max(c(breaks, length(years) + 1) - c(0, breaks))
      seq_start <- ifelse(longest_seq_end == 1, 1, breaks[longest_seq_end - 1] + 1)
      seq_end <- ifelse(longest_seq_end == length(breaks) + 1, length(years), breaks[longest_seq_end])
    }

    if (is.na(seq_end) || is.nan(seq_end)) {
      seq_end <- length(years)
    }

    # Use the continuous segment of data
    country_data_continuous <- country_data[seq_start:seq_end, ]
    years_continuous <- country_data_continuous$Year

    # Create the time series object with continuous data only
    ts_data <- ts(country_data_continuous$Tonnes, start = min(years_continuous))

    # Ensure there's enough data for breakpoint analysis
    if (length(ts_data) > h_value) {
      bp <- breakpoints(ts_data ~ 1, h = h_value)
      coefs <- coef(bp)

      periods <- fao_bp_periods(breakpoints(bp)$breakpoints, years_continuous)
      start_years <- periods$start_year
      end_years <- periods$end_year
      bps <- c(start_years, max(years_continuous))

      if (length(coefs) == length(start_years)) {
        coefs_data <- data.frame(
          ASFIS.species..Name. = species_name,
          start_year = start_years,
          end_year = end_years,
          intercept = as.numeric(coefs)
        )
      } else {
        coefs_data <- NULL
      }
    } else {
      coefs_data <- NULL
    }

    return(list(coefs_data = coefs_data, bps = bps))
  }
  # 获取所有独特的物种名
  unique_species <- unique(plot_data$ASFIS.species..Name.)

  # 应用函数到每一个物种
  results <- fao_breakpoints_by(unique_species, get_bp_coefs, data = plot_data, h_value)


  # 提取和合并结果为一个数据框
  coefs_data_list <- lapply(results, `[[`, "coefs_data")
  coefs_data <- do.call(rbind, coefs_data_list)
  # 现在，你可以使用coefs_data来创建分面图
  # 现在，你可以使用coefs_data来创建分面图

  # 输出结果
  fao_verbose_print(final_data)
  if (table) {
    fao_write_csv(final_data, "tables/species_ranked_table.csv")
    fao_write_csv(coefs_data, "tables/species_coefs_data.csv")

  }
  if (plot) {
    # Replace plot_data_Top10 with plot_data in your plotting code
    p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.)) +
      ggplot2::geom_point(alpha=point_alpha, shape=point_shape, size=point_size) +
      ggplot2::geom_line(linewidth=line_width, alpha=line_alpha) +
      fao_base_theme() +
      fao_scale_discrete("colour")+
      ggplot2::labs( x = fao_label("year", lang), y = fao_label("million_tonnes", lang), color = fao_label("species", lang)) +
      ggplot2::scale_x_continuous(breaks = year_breaks,expand = c(0,0))+
      ggplot2::scale_y_continuous(expand = c(0,0))+
      ggplot2::theme(
        text = ggplot2::element_text(family = fao_font()),
        panel.grid.minor.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
        axis.title.x = ggplot2::element_text(size=axis_title_size, face = axis_title_face),
        axis.title.y = ggplot2::element_text(size=axis_title_size, face = axis_title_face),
        axis.text.x = ggplot2::element_text(size=axis_text_size,face = axis_text_face),
        axis.text.y = ggplot2::element_text(size=axis_text_size,face = axis_text_face),
        strip.text = ggplot2::element_text(face = strip_text_face, size=strip_text_size),
        legend.position = "bottom",
        legend.box = "horizontal")+
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2))


    fao_save_plot(p1, "area/area_species", width = 12, height = 6, pptx = pptx)

    if(timeseries_analysis){
    p2 <- ggplot2::ggplot() +
      ggplot2:: geom_smooth(plot_data, mapping=ggplot2::aes(x=Year,y = Tonnes/1e6, color = ASFIS.species..Name.), method = "lm", se = FALSE, linetype = "dashed",linewidth=1) +
      ggplot2::geom_line(plot_data, mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.),linewidth=1.5) +
      ggplot2::geom_text(data = area_species_Total %>%
                           dplyr::filter(ASFIS.species..Name. %in% selected_species$ASFIS.species..Name.) %>%
                           dplyr::ungroup() %>%
                           dplyr::mutate(ASFIS.species..Name. = factor(ASFIS.species..Name., levels = unique(ASFIS.species..Name.[order(-Rank_Tonnes)]))),
                         ggplot2::aes(x = 1957, y = 0, label = paste0(round(Percentage, 2), "%")),
                         hjust = text_hjust, vjust = text_vjust, size = text_size, color = text_color) +
      ggplot2::geom_segment(data = coefs_data,
                            ggplot2::aes(x = start_year, xend = end_year, y = intercept/1e6, yend = intercept/1e6),
                            color = "black",linetype="solid",linewidth = 1.5,alpha=0.5) +
      ggplot2::geom_vline(data = coefs_data, ggplot2::aes(xintercept = start_year, group = ASFIS.species..Name.), linetype = "dashed", color = "black",linewidth=1) +
      fao_base_theme() +
      fao_scale_discrete("colour")+
      ggplot2::facet_wrap(~ASFIS.species..Name.,ncol=2,scales = "free_y")+
      ggplot2::labs(x = fao_label("year", lang), y = fao_label("million_tonnes", lang), color = fao_label("species", lang)) +
      ggplot2::scale_x_continuous(breaks = year_breaks,expand = c(0,0))+
      ggplot2::scale_y_continuous()+
      ggplot2::theme(
        text = ggplot2::element_text(family = fao_font()),
        #panel.grid.major.x = element_blank()
        panel.grid.minor.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
        strip.text = ggplot2::element_text(face = 'bold',size=10),
        axis.title.x = ggplot2::element_text(size=10,face = 'bold'),
        axis.title.y = ggplot2::element_text(size=10,face = 'bold'),
        legend.position = "bottom",
        legend.box = "horizontal",
        axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))+
      guides(color = ggplot2::guide_legend(nrow = 2))
    fao_save_plot(p2, "area/area_species_wrap", width = 12, height = 8, pptx = pptx)
    }
else{
  p3 <- ggplot2::ggplot() +
    ggplot2::geom_smooth(plot_data, mapping=ggplot2::aes(x=Year,y = Tonnes/1e6, color = ASFIS.species..Name.), method = smooth_method, se = FALSE, linetype = smooth_linetype) +
    ggplot2::geom_line(plot_data, mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.), linewidth=line_linewidth, alpha=line_alpha) +
    ggplot2::geom_point(plot_data, mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.), alpha=point_alpha, shape=point_shape, size=point_size) +
    ggplot2::geom_text(data = area_species_Total %>%
                         dplyr::filter(ASFIS.species..Name. %in% selected_species$ASFIS.species..Name.) %>%
                         dplyr::ungroup() %>%
                         dplyr::mutate(ASFIS.species..Name. = factor(ASFIS.species..Name., levels = unique(ASFIS.species..Name.[order(-Rank_Tonnes)]))),
                       ggplot2::aes(x = 1957, y = 0, label = paste0(round(Percentage, 2), "%")),
                       hjust = text_hjust, vjust = text_vjust, size = text_size, color = text_color) +
    fao_base_theme() +
    fao_scale_discrete("colour")+
    ggplot2::facet_wrap(~ASFIS.species..Name., ncol=facet_ncol, scales = "free_y") +
    ggplot2::labs(x = fao_label("year", lang), y = fao_label("million_tonnes", lang), color = fao_label("species", lang)) +
    ggplot2::scale_x_continuous(breaks = year_breaks,expand = c(0,0))+
    ggplot2::scale_y_continuous()+
    ggplot2::theme(
      text = ggplot2::element_text(family = fao_font()),
      panel.grid.minor.x = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
      strip.text = ggplot2::element_text(face = strip_text_face,size=strip_text_size),
      axis.title.x = ggplot2::element_text(size=axis_title_size,face = axis_title_face),
      axis.title.y = ggplot2::element_text(size=axis_title_size,face = axis_title_face),
      axis.text.y  = ggplot2::element_text(size=axis_text_size,face=axis_text_face),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = ggplot2::element_text(size=12),
      axis.text.x = ggplot2::element_text(size=12,angle = 60, hjust = 1))+
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2))

  fao_save_plot(p3, "area/area_species_wrap", width = 12, height = 8, pptx = pptx)

}
      }

  rank_cols <- c(catch_t = "Total_Tonnes", share_pct = "Percentage")
  if (!is.null(rank_years)) rank_cols[[paste0("catch_last_", rank_years, "_years_t")]] <- "Rank_Tonnes"
  fao_run_result(list(area_species=area_species,area_species_ranked=plot_data,area_species_ranked_total=selected_species,coefs_data=coefs_data), run,
                 fao_run_title("fishing_area_species", area_data),
                 main = fao_rank_table(selected_species, ranks, "species", "ASFIS.species..Name.", rank_cols),
                 note = paste0("Ranks ", fao_ranks_text(ranks), " of ", nrow(ranked_species), " species items, by catch ",
                               if (is.null(rank_years)) "over all years" else paste("in the last", rank_years, "years"),
                               " (catch_t and share_pct: all years):"))
}
