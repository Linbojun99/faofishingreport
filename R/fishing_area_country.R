#' Fishing Area country Analysis
#'
#' This function analyzes the fishing area data and generates plots for specified country based on their ranking in terms of total tonnes.
#'
#' @param area_data A data frame containing the fishing area data.
#' @param rank_range A numeric vector of length 2 specifying the rank range of country to be analyzed.
#' @param plot A logical value indicating whether to generate and save plots.
#' @param table A logical value indicating whether to generate and save tables
#'
#' @return NULL. The function saves the generated plots to the specified directory if plot = TRUE.
#' @export
#'
#' @examples
#' \dontrun{
#' fishing_area_country(FAO34, c(1, 10), plot=TRUE,table=TRUE) #area_data=Fao34
#' }
fishing_area_country <- function(area_data, rank_range = c(1, 10), plot = TRUE, table=TRUE) {

  #area_country
  area_country <- area_data %>%
    dplyr::group_by(Year,Country..Name.) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE))

  area_country$Country..Name.<-as.factor(area_country$Country..Name.)

  unique_country_count <- area_country %>%
    dplyr::group_by(Country..Name.) %>%
    dplyr::summarise(total_tonnes = sum(Tonnes, na.rm = TRUE)) %>%
    dplyr::filter(total_tonnes > 0) %>%
    nrow()

  print(paste("The total number of country with non-zero catch over the years is:", unique_country_count))

  area_country_table <- area_country %>%
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
  decade_avg_production_country <- area_country_table %>%
    dplyr::group_by(Decade, Country..Name.) %>%
    dplyr::summarize(avg_production = mean(Tonnes, na.rm = TRUE))

  # 计算2017到2021年的产量
  recent_production_country <- area_country_table %>%
    dplyr::filter(Year >= 2017 & Year <= 2021) %>%
    dplyr::group_by(Year, Country..Name.) %>%
    dplyr::summarize(yearly_production = sum(Tonnes, na.rm = TRUE))

  # 计算2021年每个品种的产量
  total_production_2021_country <- area_country_table %>%
    dplyr::filter(Year == 2021) %>%
    dplyr::group_by(Country..Name.) %>%
    dplyr::summarize(total_2021_production = sum(Tonnes, na.rm = TRUE))

  # 计算2021年的总产量
  total_production_2021 <- sum(total_production_2021_country$total_2021_production, na.rm = TRUE)

  # 计算每个品种在2021年的产量占总产量的百分比
  percentage_of_total_country <- total_production_2021_country %>%
    dplyr::mutate(percentage_of_total = (total_2021_production / total_production_2021) * 100)


  # 输出结果
  print(decade_avg_production_country)
  print(recent_production_country)
  print(percentage_of_total_country)
  # 检查并创建 'tables' 目录
  if (!dir.exists("tables")) {
    dir.create("tables")
  }

  # ...

  #area_country_Total
  # Steps 1 and 2: Aggregate data by year and country name, then calculate total Tonnes for each country
  area_country_Total <- area_data %>%
    dplyr::group_by(Year, Country..Name.) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(Country..Name.) %>%
    dplyr::summarise(Total_Tonnes = sum(Tonnes, na.rm = TRUE), .groups = "drop")

  total_tonnes <- sum(area_country_Total$Total_Tonnes)

  area_country_Total <- area_country_Total %>%
    dplyr::mutate(Percentage = (Total_Tonnes / total_tonnes) * 100)

  # Adjust these lines to filter country based on the specified rank range
  ranked_country <- area_country_Total %>%
    dplyr::arrange(desc(Total_Tonnes))

  selected_country <- ranked_country %>%
    dplyr::slice(rank_range[1]:rank_range[2])

  plot_data <- area_data %>%
    dplyr::filter(Country..Name. %in% selected_country$Country..Name.) %>%
    dplyr::group_by(Year, Country..Name.) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE), .groups = "drop")


  plot_data <- plot_data %>%
    dplyr::left_join(selected_country, by = "Country..Name.") %>%
    dplyr::mutate(Country..Name. = forcats::fct_reorder(Country..Name., -Total_Tonnes))

  # 根据排名范围筛选数据
  decade_avg_production_country_ranked <- decade_avg_production_country %>%
    dplyr::inner_join(selected_country, by = "Country..Name.") %>%
    dplyr::select(-Total_Tonnes, -Percentage)  # 删除不需要的列

  recent_production_country_ranked <- recent_production_country %>%
    dplyr::inner_join(selected_country, by = "Country..Name.") %>%
    dplyr::select(-Total_Tonnes, -Percentage)  # 删除不需要的列

  percentage_of_total_country_ranked <- percentage_of_total_country %>%
    dplyr::inner_join(selected_country, by = "Country..Name.") %>%
    dplyr::select(-Total_Tonnes, -Percentage)  # 删除不需要的列

  # 输出结果
  print(decade_avg_production_country_ranked)
  print(recent_production_country_ranked)
  print(percentage_of_total_country_ranked)


  # 将 decade_avg_production_country_ranked 数据框重塑为宽格式
  decade_avg_wide <- decade_avg_production_country_ranked %>%
    tidyr::spread(key = Decade, value = avg_production)

  # 将 recent_production_country_ranked 数据框重塑为宽格式
  recent_production_wide <- recent_production_country_ranked %>%
    tidyr::spread(key = Year, value = yearly_production)

  # 重命名 percentage_of_total_country_ranked 中的列以避免连接时的名称冲突
  percentage_of_total_country_ranked <- percentage_of_total_country_ranked %>%
    dplyr::rename(percentage_of_total_2021 = percentage_of_total)

  # 将所有数据框连接到一起
  final_data <- decade_avg_wide %>%
    dplyr::left_join(recent_production_wide, by = "Country..Name.") %>%
    dplyr::left_join(percentage_of_total_country_ranked, by = "Country..Name.")

  # 输出结果
  print(final_data)
  if (table) {
    write.csv(final_data, file = "tables/country_ranked_table.csv", row.names = FALSE)
  }



  if (plot) {
    # Replace plot_data_Top10 with plot_data in your plotting code
    p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = Tonnes/1e6, color = Country..Name., group = Country..Name.)) +
      ggplot2::geom_point(alpha=0.7,shape=1,size=1.8)+
      ggplot2::geom_line(linewidth=1.5,alpha=0.7) +
      ggplot2::theme_bw() +
      ggsci::scale_color_npg()+
      ggplot2::labs( x = "Year", y = "Million Tonnes") +
      ggplot2::scale_x_continuous(breaks = seq(1950,2021,5),expand = c(0,0))+
      ggplot2::scale_y_continuous(expand = c(0,0))+
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
        strip.text = ggplot2::element_text(face = 'bold',size=10),
        axis.title.x = ggplot2::element_text(size=10,face = 'bold'),
        axis.title.y = ggplot2::element_text(size=10,face = 'bold'),
        legend.position = "bottom",
        legend.box = "horizontal")+
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2))


    ggplot2::ggsave(filename = "figures/area_country.png", plot = p1, dpi = 600)
    eoffice::topptx(figure=p1,filename = "figures/area_country.pptx", width = 12, height = 6)

    p2 <- ggplot2::ggplot() +
      ggplot2::geom_smooth(plot_data, mapping=ggplot2::aes(x=Year,y = Tonnes/1e6, color = Country..Name.), method = "loess", se = FALSE, linetype = "dashed") +
      ggplot2::geom_line(plot_data, mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = Country..Name., group = Country..Name.),linewidth=1.5,alpha=0.7) +
      ggplot2::geom_point(plot_data, mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = Country..Name., group = Country..Name.),alpha=0.7,shape=1,size=2)+
      ggplot2::geom_text(data = area_country_Total %>%
                           dplyr::filter(Country..Name. %in% selected_country$Country..Name.) %>%
                           dplyr::ungroup() %>%
                           dplyr::mutate(Country..Name. = factor(Country..Name., levels = unique(Country..Name.[order(-Total_Tonnes)]))),
                         ggplot2::aes(x = 1957, y = 0, label = paste0(round(Percentage, 2), "%")),
                         hjust = 0.8, vjust = -8, size = 4.5, color = "black") +
      ggplot2::theme_bw() +
      ggsci::scale_color_npg()+
      ggplot2::facet_wrap(~Country..Name.,ncol=2,scales = "free_y")+
      ggplot2::labs(x = "Year", y = "Million Tonnes") +
      ggplot2::scale_x_continuous(breaks = seq(1950,2021,5),expand = c(0,0))+
      ggplot2::scale_y_continuous()+
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
        strip.text = ggplot2::element_text(face = 'bold',size=14),
        axis.title.x = ggplot2::element_text(size=14,face = 'bold'),
        axis.title.y = ggplot2::element_text(size=14,face = 'bold'),
        axis.text.y  = ggplot2::element_text(size=12),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = ggplot2::element_text(size=12),
        axis.text.x = ggplot2::element_text(size=12,angle = 60, hjust = 1))+
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2))

    ggplot2::ggsave(filename = "figures/area_country_wrap.png", plot = p2, dpi = 600)
    eoffice::topptx(figure=p2,filename = "figures/area_country_wrap.pptx", width = 12, height = 6)
  }

  return(NULL)
}
