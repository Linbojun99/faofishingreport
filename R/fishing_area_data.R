#' Extract Data for a Specified Fishing Area and Optionally Generate Plots
#'
#' This function extracts and returns data for a specified FAO major fishing area.
#' If the plot parameter is TRUE, it will also generate and save plots to the figures directory.
#'
#' @param area_name The name of the FAO major fishing area.
#' @param plot A logical indicating whether to generate and save plots. Default is TRUE.
#' @param timeseries_analysis A logical value indicating whether to analysis timeseries.
#' @param area_color1 The color to use for the area plot. Default is "grey80".
#' @param area_color2 The color to use for the area plot. Default is "black".
#' @param area_alpha The alpha transparency for the area plot. Default is 0.7.
#' @param line_color The color to use for line plots. Default is "black".
#' @param line_alpha The alpha transparency for line plots. Default is 0.8.
#' @param point_size The size of points in the percentage plot. Default is 2.
#' @param hline_color The color of the horizontal line in the percentage plot. Default is "red".
#' @param hline_alpha The alpha transparency of the horizontal line in the percentage plot. Default is 0.7.
#' @param hline_linewidth Line width for the horizontal line. Default is 1.5.
#' @param line_linewidth Line width for the line geom. Default is 1.5.
#' @param axis_text_face The font face of the axis text. Default is "plain".
#' @param axis_title_face The font face of the axis title. Default is "bold".
#' @param strip_text_face The font face of the strip text. Default is "bold".
#' @param axis_text_size The size of the axis text. Default is 10.
#' @param axis_title_size The size of the axis title text. Default is 12.
#' @param strip_text_size The size of the strip text. Default is 12.
#' @param polygon_color The color of the borders of the polygons in the map plot. Default is "grey90".
#' @param fill_scale The color scale for filling polygons in the map plot. Default is "deep-orange".
#' @param axis_title_size_map The size of the axis title text in the map plot. Default is 14.
#' @param axis_text_size_map The size of the axis text in the map plot. Default is 12.
#' @param legend_text_size_map The size of the legend text in the map plot. Default is 12.
#' @return A data frame containing data for the specified fishing area.
#' @export
#' @examples
#' \dontrun{
#' FAO34 <- fishing_area_data(area_name="Atlantic, Eastern Central", plot = TRUE,timeseries_analysis=TRUE)
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
                              fill_scale = "deep-orange",
                              axis_title_size_map = 14,
                              axis_text_size_map = 12,
                              legend_text_size_map = 12) {
  data_path <- system.file("extdata", "Global_Production_long.rda", package="faofishingreport")
  load(data_path, envir = .GlobalEnv)
  data <- Global_Production_long
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

      bps <- c(start_year1, breakpoints(bp)$breakpoints + start_year1, end_year1)

      start_years <- bps[-length(bps)]
      end_years <- bps[-1] - 1

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




  if (plot) {
    # Create figures directory if it doesn't exist
    if (!dir.exists("figures")) {
      dir.create("figures")
    }



    if(timeseries_analysis){
      p1 <- ggplot2::ggplot() +
        ggplot2::geom_area(data=area_production,
                           mapping=ggplot2::aes(y=Tonnes/1e6,x=Year),
                           fill=area_color1,
                           alpha=area_alpha,
                           color=area_color2) +
        ggplot2::geom_segment(data = coefs_data,
                              ggplot2::aes(x = start_year, xend = end_year, y = intercept/1e6, yend = intercept/1e6),
                     color = "black",linetype="solid",size=1.5,alpha=0.5) +
        ggplot2::geom_vline(data = coefs_data, ggplot2::aes(xintercept = start_year), linetype = "dashed", color = "black",linewidth=1) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous(breaks = seq(1950,2021,5),expand = c(0,0)) +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::theme(
          panel.grid.minor.x = ggplot2::element_blank(),

          axis.title.x = ggplot2::element_text(size=axis_text_size,face = axis_text_face),
          axis.title.y = ggplot2::element_text(size=axis_text_size,face = axis_text_face),
          legend.position = "bottom") +
        ggplot2::labs(y="Million Tonne")
      ggplot2::ggsave(filename = "figures/fishing_trend.png", plot = p1, dpi=600,width = 12, height = 6)
      eoffice::topptx(figure=p1,filename = "figures/fishing_trend.pptx",devsize = TRUE)
            coefs_data<-tibble::as_tibble(coefs_data)
            print(paste("Mutation point detection:",coefs_data))
  }
    else{
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_area(data=area_production,
                         mapping=ggplot2::aes(y=Tonnes/1e6,x=Year),
                         fill=area_color1,
                         alpha=area_alpha,
                         color=area_color2) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = seq(1950,2021,5),expand = c(0,0)) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),

        axis.title.x = ggplot2::element_text(size=axis_text_size,face = axis_text_face),
        axis.title.y = ggplot2::element_text(size=axis_text_size,face = axis_text_face),
        legend.position = "bottom") +
      ggplot2::labs(y="Million Tonne")

    ggplot2::ggsave(filename = "figures/fishing_trend.png", plot = p1, dpi=600,width = 12, height = 6)
    eoffice::topptx(figure=p1,filename = "figures/fishing_trend.pptx",devsize = TRUE)
}
    # Percentage calculations
    Global_production_total <- dplyr::group_by(Global_Production_long, Year) %>%
      dplyr::summarise(Global_Tonnes = sum(Production, na.rm = TRUE))

    merged_data <- dplyr::left_join(area_production, Global_production_total, by = "Year")
    merged_data <- merged_data %>% dplyr::mutate(Percentage_area = (Tonnes / Global_Tonnes) * 100)

    total_area = sum(area_production$Tonnes, na.rm = TRUE)
    total_Global = sum(Global_production_total$Global_Tonnes, na.rm = TRUE)
    total_Percentage_area = (total_area / total_Global) * 100

    merged_data = merged_data %>%
      dplyr::mutate(mean_Percentage_area = mean(Percentage_area, na.rm=TRUE))

    print(merged_data)
    print(paste("Total percentage of area:", total_Percentage_area))

    # Plotting percentage
    percentage_plot <- ggplot2::ggplot() +
      ggplot2::geom_hline(data=merged_data, mapping=ggplot2::aes(yintercept = mean_Percentage_area),
                          linewidth=hline_linewidth, color=hline_color, alpha=hline_alpha) +
      ggplot2::geom_line(data=merged_data, mapping=ggplot2::aes(x = Year, y = Percentage_area),
                         linewidth=line_linewidth, alpha=line_alpha, color=line_color) +
      ggplot2::geom_point(data=merged_data, mapping=ggplot2::aes(x = Year, y = Percentage_area),
                          size=point_size) +
      ggplot2::theme_bw() +
      ggplot2::labs(y = "Percentage of Total Production", x = "Year") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size=axis_text_size),
                     legend.position = "none") +
      ggplot2::scale_x_continuous(breaks = seq(1950,2021,5), expand = c(0,0)) +
      ggplot2::scale_y_continuous() +
      ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
                     strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
                     strip.text = ggplot2::element_text(face = strip_text_face, size=strip_text_size),
                     axis.text.x =  ggplot2::element_text(size=axis_text_size),
                     axis.text.y =  ggplot2::element_text(size=axis_text_size),
                     axis.title.x = ggplot2::element_text(size=axis_title_size, face = axis_title_face),
                     axis.title.y = ggplot2::element_text(size=axis_title_size, face = axis_title_face),
                     legend.box = "horizontal")

    ggplot2::ggsave(filename = "figures/fishing_percentage.png", plot = percentage_plot, dpi=600, width = 12, height = 6)

    eoffice::topptx(figure=percentage_plot,filename = "figures/fishing_percentage.pptx", devsize = TRUE)

    # 地图绘制代码
    # 创建一个命名的替换规则向量
    replace_rules <- c(
      "Cape Verde" = "Cabo Verde",
      "Curacao" = "Cura?ao",
      "Netherlands" = "Netherlands (Kingdom of the)",
      "Democratic Republic of the Congo" = "Congo, Dem. Rep. of the",
      "Republic of Congo" = "Congo",
      "North Korea" = "Korea, Dem. People's Rep",
      "South Korea" = "Korea, Republic of",
      "Russia" = "Russian Federation",
      "Vietnam" = "Viet Nam",
      "Laos" = "Lao People's Democratic Republic",
      "Moldova" = "Republic of Moldova",
      "Czech Republic" = "Czechia",
      "Ivory Coast" = "C?te d'Ivoire",
      "UK" = "United Kingdom",
      "Syria" = "Syrian Arab Republic",
      "Guam" = "Guam (USA)",
      "New Caledonia" = "New Caledonia (France)",
      "Martinique" = "Martinique (France)",
      "Mayotte" = "Mayotte (France)",
      "Saint Pierre and Miquelon" = "Saint Pierre & Miquelon (France)",
      "Bermuda" = "Bermuda (UK)",
      "Saint Helena" = "Saint Helena (UK)",
      "Turks and Caicos Islands" = "Turks & Caicos Isl. (UK)",
      "Virgin Islands" = "British Virgin Isl. (UK)",
      "Northern Mariana Islands" = "North Marianas (USA)",
      "Niue" = "Niue (New Zealand)",
      "Taiwan" = "Taiwan Province of China",
      "USA" = "United States of America",
      "Saint Vincent" = "Saint Vincent/Grenadines",
      "Saint Kitts" = "Saint Kitts and Nevis"
    )

    # 应用替换规则

    worldMap <- ggplot2::map_data("world") %>%
      dplyr::filter(region != "Antarctica") %>%
      dplyr::rename(country = region) %>%
      dplyr::mutate(country = dplyr::recode(country, !!!replace_rules))

    area_country_Total <- area_data %>%
      dplyr::group_by(Year, Country..Name.) %>%
      dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(Country..Name.) %>%
      dplyr::summarise(Total_Tonnes = sum(Tonnes, na.rm = TRUE), .groups = "drop")

    total_tonnes_country <- sum(area_country_Total$Total_Tonnes)

    area_country_Total <- area_country_Total %>%
      dplyr::mutate(Percentage = (Total_Tonnes / total_tonnes_country) * 100)

    worldMap %>%
      dplyr::left_join(area_country_Total, by = c("country" = "Country..Name.")) -> act_world_map

    map_plot <- ggplot2::ggplot() +
      ggplot2::geom_polygon(act_world_map,
                            mapping=ggplot2::aes(x = long, y = lat, group = group,
                                                 fill= Total_Tonnes/1e6),
                            colour = polygon_color) +
      ggplot2::coord_sf() +
      ggsci::scale_fill_material(fill_scale, na.value = "grey90") +
      ggplot2::labs(fill="Million Tonnes") +
      metR::scale_x_longitude() +
      metR::scale_y_latitude() +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
        strip.text = ggplot2::element_text(face = strip_text_face, size=strip_text_size),
        axis.title.x = ggplot2::element_text(size=axis_title_size_map, face = axis_title_face),
        axis.title.y = ggplot2::element_text(size=axis_title_size_map, face = axis_title_face),
        axis.text.y = ggplot2::element_text(size=axis_text_size_map),
        axis.text.x = ggplot2::element_text(size=axis_text_size_map),
        legend.text = ggplot2::element_text(size=legend_text_size_map),
        legend.position = "bottom",
        legend.box = "horizontal")

    ggplot2::ggsave(filename = "figures/area_country_map.png", plot = map_plot, dpi=600, width = 12, height = 6)
    eoffice::topptx(figure = map_plot,filename = "figures/area_country_map.pptx", width=12, height=6)

     }

  return(area_data)
}
