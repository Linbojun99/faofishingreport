#' Fishing Area Species Analysis
#'
#' This function analyzes the fishing area data and generates plots for specified species based on their ranking in terms of total tonnes.
#'
#' @param area_data A data frame containing the fishing area data.
#' @param rank_range A numeric vector of length 2 specifying the rank range of species to be analyzed.
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
#' @param text_hjust A numeric value specifying the horizontal justification of the text geom. Default is 0.8.
#' @param text_vjust A numeric value specifying the vertical justification of the text geom. Default is -8.
#' @param text_size A numeric value specifying the size of the text geom. Default is 4.5.
#' @param text_color A character string specifying the color of the text geom. Default is "black".
#' @param facet_ncol A numeric value specifying the number of columns to be used in facet_wrap. Default is 2.
#' @param h_value The minimum segment size, which must be greater than the
#'        number of regressors. For the model `ts_data ~ 1`, this means `h_value`
#'        should be greater than 1. The default value is 0.15.
#' @return NULL. The function saves the generated plots to the specified directory if plot = TRUE.
#' @export
#'
#' @examples
#' \dontrun{
#' fishing_area_species(FAO34[["area_data"]], c(1, 10), plot=TRUE,table=TRUE,h_value = 0.15) #area_data=Fao34
#' }
fishing_area_species <- function(area_data, rank_range = c(1, 10), plot = TRUE, table=TRUE,timeseries_analysis=TRUE,
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
                                 h_value = 0.15) {

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

  print(paste("The total number of species with non-zero catch over the years is:", unique_species_count))

  area_species_table <- area_species %>%
    dplyr::mutate(Decade = dplyr::case_when(
      Year >= 1950 & Year < 1960 ~ "1950s",
      Year >= 1960 & Year < 1970 ~ "1960s",
      Year >= 1970 & Year < 1980 ~ "1970s",
      Year >= 1980 & Year < 1990 ~ "1980s",
      Year >= 1990 & Year < 2000 ~ "1990s",
      Year >= 2000 & Year < 2010 ~ "2000s",
      Year >= 2010 & Year < 2020 ~ "2010s",
      TRUE ~ "2020s"
    ))

  # 计算每个年代的平均产量
  decade_avg_production_species <- area_species_table %>%
    dplyr::group_by(Decade, ASFIS.species..Name.) %>%
    dplyr::summarize(avg_production = mean(Tonnes, na.rm = TRUE))

  # 计算2017到2021年的产量
  recent_production_species <- area_species_table %>%
    dplyr::filter(Year >= 2017 & Year <= 2021) %>%
    dplyr::group_by(Year, ASFIS.species..Name.) %>%
    dplyr::summarize(yearly_production = sum(Tonnes, na.rm = TRUE))

  # 计算2021年每个品种的产量
  total_production_2021_species <- area_species_table %>%
    dplyr::filter(Year == 2021) %>%
    dplyr::group_by(ASFIS.species..Name.) %>%
    dplyr::summarize(total_2021_production = sum(Tonnes, na.rm = TRUE))

  # 计算2021年的总产量
  total_production_2021 <- sum(total_production_2021_species$total_2021_production, na.rm = TRUE)

  # 计算每个品种在2021年的产量占总产量的百分比
  percentage_of_total_species <- total_production_2021_species %>%
    dplyr::mutate(percentage_of_total = (total_2021_production / total_production_2021) * 100)


  # 输出结果
  print(decade_avg_production_species)
  print(recent_production_species)
  print(percentage_of_total_species)
  # 检查并创建 'tables' 目录
  if (!dir.exists("tables")) {
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

  # Adjust these lines to filter species based on the specified rank range
  ranked_species <- area_species_Total %>%
    dplyr::arrange(desc(Total_Tonnes))

  selected_species <- ranked_species %>%
    dplyr::slice(rank_range[1]:rank_range[2])

  plot_data <- area_data %>%
    dplyr::filter(ASFIS.species..Name. %in% selected_species$ASFIS.species..Name.) %>%
    dplyr::group_by(Year, ASFIS.species..Name.) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE), .groups = "drop")

  plot_data <- plot_data %>%
    dplyr::left_join(selected_species, by = "ASFIS.species..Name.") %>%
    dplyr::mutate(ASFIS.species..Name. = forcats::fct_reorder(ASFIS.species..Name., -Total_Tonnes))

  # 根据排名范围筛选数据
  decade_avg_production_species_ranked <- decade_avg_production_species %>%
    dplyr::inner_join(selected_species, by = "ASFIS.species..Name.") %>%
    dplyr::select(-Total_Tonnes, -Percentage)  # 删除不需要的列

  recent_production_species_ranked <- recent_production_species %>%
    dplyr::inner_join(selected_species, by = "ASFIS.species..Name.") %>%
    dplyr::select(-Total_Tonnes, -Percentage)  # 删除不需要的列

  percentage_of_total_species_ranked <- percentage_of_total_species %>%
    dplyr::inner_join(selected_species, by = "ASFIS.species..Name.") %>%
    dplyr::select(-Total_Tonnes, -Percentage)  # 删除不需要的列

  # 输出结果
  print(decade_avg_production_species_ranked)
  print(recent_production_species_ranked)
  print(percentage_of_total_species_ranked)



  # 将 decade_avg_production_species_ranked 数据框重塑为宽格式
  decade_avg_wide <- decade_avg_production_species_ranked %>%
    tidyr::spread(key = Decade, value = avg_production)

  # 将 recent_production_species_ranked 数据框重塑为宽格式
  recent_production_wide <- recent_production_species_ranked %>%
    tidyr::spread(key = Year, value = yearly_production)

  # 重命名 percentage_of_total_species_ranked 中的列以避免连接时的名称冲突
  percentage_of_total_species_ranked <- percentage_of_total_species_ranked %>%
    dplyr::rename(percentage_of_total_2021 = percentage_of_total)

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

      bps <- c(min(years_continuous), breakpoints(bp)$breakpoints + min(years_continuous) - 1, max(years_continuous))

      start_years <- bps[-length(bps)]
      end_years <- bps[-1] - 1

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
  results <- lapply(unique_species, get_bp_coefs, data = plot_data,h_value)


  # 提取和合并结果为一个数据框
  coefs_data_list <- lapply(results, `[[`, "coefs_data")
  coefs_data <- do.call(rbind, coefs_data_list)
  # 现在，你可以使用coefs_data来创建分面图
  # 现在，你可以使用coefs_data来创建分面图

  # 输出结果
  print(final_data)
  if (table) {
    write.csv(final_data, file = "tables/species_ranked_table.csv", row.names = FALSE)
    write.csv(coefs_data, file = "tables/species_coefs_data.csv", row.names = FALSE)

  }
  if (plot) {
    # Replace plot_data_Top10 with plot_data in your plotting code
    p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.)) +
      ggplot2::geom_point(alpha=point_alpha, shape=point_shape, size=point_size) +
      ggplot2::geom_line(linewidth=line_width, alpha=line_alpha) +
      ggplot2::theme_bw() +
      ggsci::scale_color_npg()+
      ggplot2::labs( x = "Year", y = "Million Tonnes") +
      ggplot2::scale_x_continuous(breaks = seq(1950,2021,5),expand = c(0,0))+
      ggplot2::scale_y_continuous(expand = c(0,0))+
      ggplot2::theme(
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


    #ggplot2::ggsave(filename = "figures/area_species.png", plot = p1, dpi = 600)
    eoffice::topptx(figure=p1,filename = "figures/area_species.pptx", width = 12, height = 6)

    if(timeseries_analysis){
    p2 <- ggplot2::ggplot() +
      ggplot2:: geom_smooth(plot_data, mapping=ggplot2::aes(x=Year,y = Tonnes/1e6, color = ASFIS.species..Name.), method = "lm", se = FALSE, linetype = "dashed",linewidth=1) +
      ggplot2::geom_line(plot_data, mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.),linewidth=1.5) +
      ggplot2::geom_text(data = area_species_Total %>%
                           dplyr::filter(ASFIS.species..Name. %in% selected_species$ASFIS.species..Name.) %>%
                           dplyr::ungroup() %>%
                           dplyr::mutate(ASFIS.species..Name. = factor(ASFIS.species..Name., levels = unique(ASFIS.species..Name.[order(-Total_Tonnes)]))),
                         ggplot2::aes(x = 1957, y = 0, label = paste0(round(Percentage, 2), "%")),
                         hjust = text_hjust, vjust = text_vjust, size = text_size, color = text_color) +
      ggplot2::geom_segment(data = coefs_data,
                            ggplot2::aes(x = start_year, xend = end_year, y = intercept/1e6, yend = intercept/1e6),
                            color = "black",linetype="solid",size=1.5,alpha=0.5) +
      ggplot2::geom_vline(data = coefs_data, ggplot2::aes(xintercept = start_year, group = ASFIS.species..Name.), linetype = "dashed", color = "black",linewidth=1) +
      ggplot2::theme_bw() +
      ggsci::scale_color_npg()+
      ggplot2::facet_wrap(~ASFIS.species..Name.,ncol=2,scales = "free_y")+
      ggplot2::labs(x = "Year", y = "Million Tonnes") +
      ggplot2::scale_x_continuous(breaks = seq(1950,2021,5),expand = c(0,0))+
      ggplot2::scale_y_continuous()+
      ggplot2::theme(
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
    #ggplot2::ggsave(filename = "figures/area_species_wrap.png", plot = p2, dpi = 600)
    eoffice::topptx(figure=p2,filename = "figures/area_species_wrap.pptx", width = 12, height = 8)
    }
else{
  p3 <- ggplot2::ggplot() +
    ggplot2::geom_smooth(plot_data, mapping=ggplot2::aes(x=Year,y = Tonnes/1e6, color = ASFIS.species..Name.), method = smooth_method, se = FALSE, linetype = smooth_linetype) +
    ggplot2::geom_line(plot_data, mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.), linewidth=line_linewidth, alpha=line_alpha) +
    ggplot2::geom_point(plot_data, mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.), alpha=point_alpha, shape=point_shape, size=point_size) +
    ggplot2::geom_text(data = area_species_Total %>%
                         dplyr::filter(ASFIS.species..Name. %in% selected_species$ASFIS.species..Name.) %>%
                         dplyr::ungroup() %>%
                         dplyr::mutate(ASFIS.species..Name. = factor(ASFIS.species..Name., levels = unique(ASFIS.species..Name.[order(-Total_Tonnes)]))),
                       ggplot2::aes(x = 1957, y = 0, label = paste0(round(Percentage, 2), "%")),
                       hjust = text_hjust, vjust = text_vjust, size = text_size, color = text_color) +
    ggplot2::theme_bw() +
    ggsci::scale_color_npg()+
    ggplot2::facet_wrap(~ASFIS.species..Name., ncol=facet_ncol, scales = "free_y") +
    ggplot2::labs(x = "Year", y = "Million Tonnes") +
    ggplot2::scale_x_continuous(breaks = seq(1950,2021,5),expand = c(0,0))+
    ggplot2::scale_y_continuous()+
    ggplot2::theme(
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

  #ggplot2::ggsave(filename = "figures/area_species_wrap.png", plot = p3, dpi = 600)
  eoffice::topptx(figure=p3,filename = "figures/area_species_wrap.pptx", width = 12, height = 8)

}
      }

  return(list(area_species=area_species,area_species_ranked=plot_data,area_species_ranked_total=selected_species,coefs_data=coefs_data))
}
