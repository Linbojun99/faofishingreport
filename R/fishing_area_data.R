#' Extract Data for a Specified Fishing Area and Optionally Generate Plots
#'
#' This function extracts and returns data for a specified FAO major fishing area.
#' If the plot parameter is TRUE, it will also generate and save plots to the figures directory.
#' The percentage chart shows the share of the area in the world catch of all marine areas (or,
#' for an inland area, of all inland waters).
#'
#' @param area_name The name of the FAO major fishing area.
#' @param plot A logical indicating whether to generate and save plots. Default is TRUE.
#' @param timeseries_analysis A logical value. If TRUE (default), the total catch chart shows the
#'        periods between breakpoints in the mean catch (Bai-Perron test, `strucchange::breakpoints()`).
#' @param area_color1 The fill colour of the total catch chart. Default is "grey80".
#' @param area_color2 The outline colour of the total catch chart. Default is "black".
#' @param area_alpha The alpha transparency for the area plot. Default is 0.7.
#' @param line_color The color to use for line plots. Default is "black".
#' @param line_alpha The alpha transparency for line plots. Default is 0.8.
#' @param point_size The size of points in the percentage plot. Default is 2.
#' @param hline_color The color of the horizontal line in the percentage plot. Default is "red".
#' @param hline_alpha The alpha transparency of the horizontal line in the percentage plot. Default is 0.7.
#' @param hline_linewidth Line width for the horizontal line. Default is 1.5.
#' @param line_linewidth Line width for the line geom. Default is 1.5.
#' @param axis_text_face The font face of the axis text of the total catch chart. Default is "plain".
#' @param axis_title_face The font face of the axis titles of the charts and the map. Default is "bold".
#' @param strip_text_face Not used (the charts have no panels). Default is "bold".
#' @param axis_text_size The size of the axis text of the percentage chart. Default is 10.
#' @param axis_title_size The size of the axis titles of the total catch and percentage charts. Default is 12.
#' @param strip_text_size Not used (the charts have no panels). Default is 12.
#' @param polygon_color The color of the borders of the polygons in the map plot. Default is "grey90".
#' @param fill_scale The colour palette of the map. Default is NULL, which uses `map_palette` of
#'        `fao_settings()` ("deep-orange" unless changed); see `?fao_settings` for the choices.
#' @param axis_title_size_map The size of the axis title text in the map plot. Default is 14.
#' @param axis_text_size_map The size of the axis text in the map plot. Default is 12.
#' @param legend_text_size_map The size of the legend text in the map plot. Default is 12.
#' @param ZH A logical value. If TRUE, chart text and names are in Chinese. Default is FALSE. `ZH = TRUE` is the same as `lang = "zh"`.
#' @param lang The language of the chart text: "en" (English), "zh" (Chinese) or "both"
#'        (Chinese and English). Default is NULL, which uses "zh" if `ZH = TRUE`, and otherwise the language
#'        set with `fao_settings()` ("en" unless changed).
#' @param pptx A logical value. Charts are always saved as PNG in `figures/png/`; if TRUE they are
#'        also saved as editable PowerPoint files in `figures/pptx/`. Default is FALSE, which can be
#'        changed for all functions with `fao_settings(pptx = TRUE)`.
#' @param data_version The FAO data release to use: "latest" (default) uses the newest of the
#'        releases downloaded with `download_fao_data()` and the data bundled with the package;
#'        "bundled" uses the data bundled with the package; or a release such as "2026.1.0".
#' @return A list with `area_data` (the FAO data of the area), `coefs_data` (start year, end year
#'         and mean catch of each period between breakpoints) and `act_world_map` (the map data).
#' @family fishing area analyses
#' @export
#' @examples
#' \dontrun{
#' # Firstly, data preparing and loading.
#' FAO34 <- fishing_area_data(area_name="Atlantic, Eastern Central", plot = TRUE, timeseries_analysis=TRUE)
#' }
fishing_area_data <- function(area_name, plot = TRUE,timeseries_analysis=TRUE,
                              area_color1 = "grey80", area_alpha = 0.7,
                              area_color2 = "black",
                              line_color = "black", line_alpha = 0.8,
                              point_size = 2,
                              hline_color = "red", hline_alpha = 0.7,
                              hline_linewidth = 1.5,
                              line_linewidth = 1.5,
                              axis_text_face = "plain",
                              axis_title_face = "bold",
                              strip_text_face = "bold",
                              axis_text_size = 10, axis_title_size = 12,
                              strip_text_size = 12,
                              polygon_color = "grey90",
                              fill_scale = NULL,
                              axis_title_size_map = 14,
                              axis_text_size_map = 12,
                              legend_text_size_map = 12,
                              ZH=FALSE, lang = NULL, data_version = "latest",
                              pptx = getOption("faofishingreport.pptx", FALSE)) {
  lang <- fao_lang(lang, ZH)
  data <- load_fao_data(data_version)
  area_data <- subset(data, data$FAO.major.fishing.area..Name. == area_name)
  area_data <- na.omit(area_data)

  area_production <- dplyr::group_by(area_data, Year) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE))
  get_bp_coefs <- function(data) {
    years <- unique(data$Year)
    start_year1 <- min(years)
    end_year1 <- max(years)

    ts_data1 <- ts(data[, "Tonnes"], start = start_year1)

    if (length(ts_data1) > 0) {
      bp <- breakpoints(ts_data1 ~ 1)
      coefs <- coef(bp)

      periods <- fao_bp_periods(breakpoints(bp)$breakpoints, data$Year)
      start_years <- periods$start_year
      end_years <- periods$end_year
      bps <- c(start_years, max(data$Year))

      if(length(coefs) == length(start_years)) {
        coefs_data <- data.frame(
          start_year = start_years,
          end_year = end_years,
          intercept = as.numeric(coefs)
        )
      } else {
        coefs_data <- NULL
      }
    } else {
      coefs_data <- NULL
      bps <- NULL
    }

    list(coefs_data = coefs_data)
  }

  coefs_data<-get_bp_coefs(area_production)

  coefs_data <- do.call(rbind, coefs_data)


  #draw set
  # The share is of the world catch in waters of the same kind: marine areas or inland waters
  area_types <- fao_group_table()$area
  water <- area_types$inland_marine_en[match(area_name, area_types$name_en)]
  same_water <- area_types$name_en[area_types$inland_marine_en %in% water]
  global_data <- if (length(same_water) > 0) data[data$FAO.major.fishing.area..Name. %in% same_water, ] else data

  ylab <- fao_label("million_tonnes", lang)
  xlab2 <- fao_label(if (identical(water, "Inland waters")) "share_world_inland" else "share_world_marine", lang)
  xlab <- fao_label("year", lang)
  year_breaks <- fao_year_breaks(area_data$Year)

  act_world_map <- NULL
  if (plot) {


    if(timeseries_analysis){
      p1 <- ggplot2::ggplot() +
        ggplot2::geom_area(data=area_production,
                           mapping=ggplot2::aes(y=Tonnes/1e6,x=Year),
                           fill=area_color1,
                           alpha=area_alpha,
                           color=area_color2) +
        ggplot2::geom_segment(data = coefs_data,
                              ggplot2::aes(x = start_year, xend = end_year, y = intercept/1e6, yend = intercept/1e6),
                     color = "black",linetype="solid",linewidth = 1.5,alpha=0.5) +
        ggplot2::geom_vline(data = coefs_data, ggplot2::aes(xintercept = start_year), linetype = "dashed", color = "black",linewidth=1) +
        fao_base_theme() +
        ggplot2::scale_x_continuous(breaks = year_breaks,expand = c(0,0)) +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::theme(
          text = ggplot2::element_text(family = fao_font()),
          panel.grid.minor.x = ggplot2::element_blank(),

          axis.text = ggplot2::element_text(face = axis_text_face),
          axis.title.x = ggplot2::element_text(size=axis_title_size,face = axis_title_face),
          axis.title.y = ggplot2::element_text(size=axis_title_size,face = axis_title_face),
          legend.position = "bottom") +
        ggplot2::labs(x=xlab, y=ylab)
      fao_save_plot(p1, "fishing_trend", width = 12, height = 6, pptx = pptx)
            coefs_data<-tibble::as_tibble(coefs_data)
            fao_verbose_message(paste("Mutation point detection:",coefs_data))
  }
    else{
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_area(data=area_production,
                         mapping=ggplot2::aes(y=Tonnes/1e6,x=Year),
                         fill=area_color1,
                         alpha=area_alpha,
                         color=area_color2) +
      fao_base_theme() +
      ggplot2::scale_x_continuous(breaks = year_breaks,expand = c(0,0)) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::theme(
        text = ggplot2::element_text(family = fao_font()),
        panel.grid.minor.x = ggplot2::element_blank(),

        axis.text = ggplot2::element_text(face = axis_text_face),
          axis.title.x = ggplot2::element_text(size=axis_title_size,face = axis_title_face),
        axis.title.y = ggplot2::element_text(size=axis_title_size,face = axis_title_face),
        legend.position = "bottom") +
      ggplot2::labs(x=xlab, y=ylab)

    fao_save_plot(p1, "fishing_trend", width = 12, height = 6, pptx = pptx)
}
    # Percentage calculations
    Global_production_total <- dplyr::group_by(global_data, Year) %>%
      dplyr::summarise(Global_Tonnes = sum(Production, na.rm = TRUE))

    merged_data <- dplyr::left_join(area_production, Global_production_total, by = "Year")
    merged_data <- merged_data %>% dplyr::mutate(Percentage_area = (Tonnes / Global_Tonnes) * 100)

    total_area = sum(area_production$Tonnes, na.rm = TRUE)
    total_Global = sum(Global_production_total$Global_Tonnes, na.rm = TRUE)
    total_Percentage_area = (total_area / total_Global) * 100

    merged_data = merged_data %>%
      dplyr::mutate(mean_Percentage_area = mean(Percentage_area, na.rm=TRUE))

    fao_verbose_print(merged_data)
    fao_verbose_message(paste("Total percentage of area:", total_Percentage_area))

    # Plotting percentage
    percentage_plot <- ggplot2::ggplot() +
      ggplot2::geom_hline(data=merged_data, mapping=ggplot2::aes(yintercept = mean_Percentage_area),
                          linewidth=hline_linewidth, color=hline_color, alpha=hline_alpha) +
      ggplot2::geom_line(data=merged_data, mapping=ggplot2::aes(x = Year, y = Percentage_area),
                         linewidth=line_linewidth, alpha=line_alpha, color=line_color) +
      ggplot2::geom_point(data=merged_data, mapping=ggplot2::aes(x = Year, y = Percentage_area),
                          size=point_size) +
      fao_base_theme() +
      ggplot2::labs(y = xlab2, x = xlab) +
      ggplot2::theme(text = ggplot2::element_text(family = fao_font()), axis.text.x = ggplot2::element_text(size=axis_text_size),
                     legend.position = "none") +
      ggplot2::scale_x_continuous(breaks = year_breaks, expand = c(0,0)) +
      ggplot2::scale_y_continuous() +
      ggplot2::theme(text = ggplot2::element_text(family = fao_font()), panel.grid.minor.x = ggplot2::element_blank(),
                     strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
                     strip.text = ggplot2::element_text(face = strip_text_face, size=strip_text_size),
                     axis.text.x =  ggplot2::element_text(size=axis_text_size),
                     axis.text.y =  ggplot2::element_text(size=axis_text_size),
                     axis.title.x = ggplot2::element_text(size=axis_title_size, face = axis_title_face),
                     axis.title.y = ggplot2::element_text(size=axis_title_size, face = axis_title_face),
                     legend.box = "horizontal")

    fao_save_plot(percentage_plot, "fishing_percentage", width = 12, height = 6, pptx = pptx)

    # 地图：各国在该渔区的总产量，国家按 ISO3 代码匹配
    area_country_Total <- area_data %>%
      dplyr::group_by(Country..Name.) %>%
      dplyr::summarise(Total_Tonnes = sum(Production, na.rm = TRUE), .groups = "drop")
    area_country_Total$Percentage <- 100 * area_country_Total$Total_Tonnes / sum(area_country_Total$Total_Tonnes)

    country_map <- fao_country_value_map(area_country_Total$Country..Name., area_country_Total$Total_Tonnes / 1e6,
                                         ylab, polygon_color, fill_scale)
    act_world_map <- country_map$data
    map_plot <- country_map$plot +
      ggplot2::theme(
        text = ggplot2::element_text(family = fao_font()),
        axis.title.x = ggplot2::element_text(size=axis_title_size_map, face = axis_title_face),
        axis.title.y = ggplot2::element_text(size=axis_title_size_map, face = axis_title_face),
        axis.text.y = ggplot2::element_text(size=axis_text_size_map),
        axis.text.x = ggplot2::element_text(size=axis_text_size_map),
        legend.text = ggplot2::element_text(size=legend_text_size_map),
        legend.position = "bottom",
        legend.box = "horizontal")

    fao_save_plot(map_plot, "area_country_map", width = 12, height = 6, pptx = pptx)

     }

  return(list(area_data=area_data,coefs_data=coefs_data,act_world_map=act_world_map))
}
