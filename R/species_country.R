#' Analyze and Plot Specified Species Data Ranked by Country Production
#'
#' This function takes a data frame and a specified species name,
#' analyzes the data for the specified species, and identifies the top countries
#' based on production within the specified rank range.
#'
#' @param area_data A data frame containing the fishing area data.
#' @param species_name A string specifying the species to be analyzed.
#' @param rank_range A numeric vector of length 2 specifying the rank range of countries to be analyzed.
#' @param plot A logical value indicating whether to generate and save plots.
#' @param table A logical value indicating whether to generate and save tables
#' @param timeseries_analysis A logical value indicating whether to analysis timeseries.
#' @param h_value The minimum segment size, which must be greater than the
#'        number of regressors. For the model `ts_data ~ 1`, this means `h_value`
#'        should be greater than 1. The default value is 0.15.

#' @export
#'
#' @examples
#' \dontrun{
#' species_country(FAO34[["area_data"]], species_name = "European pilchard(=Sardine)",c(1, 10), plot=TRUE,table=TRUE,h_value = 0.15) #area_data=Fao34
#' }
species_country <- function(area_data, species_name, rank_range = c(1, 10),plot=TRUE,table=TRUE,timeseries_analysis=TRUE,h_value = 0.15) {

  options(scipen=999,digits=2)



  species_data <- area_data %>%
    dplyr::filter(ASFIS.species..Name. == species_name)

  species_data <- species_data %>%
    dplyr::group_by(Year, Country..Name.) %>%
    dplyr::summarise(Production = sum(Production,na.rm=T))

  species_country_total <- species_data %>%
    dplyr::group_by(Country..Name.) %>%
    dplyr::summarise(country_production=sum(Production,na.rm = T))

  species_country_total$Country..Name. <- as.factor(species_country_total$Country..Name.)

  total_species_country <- sum(species_country_total$country_production)
  species_country_total <- species_country_total %>%
    dplyr::mutate(percentage_country=(country_production/total_species_country)*100)


  # Adjusted code for ranked selection based on specified rank range
  species_country_ranked_total <- species_country_total %>%
    dplyr::arrange(desc(country_production)) %>%
    dplyr::slice(rank_range[1]:rank_range[2]) %>%
    dplyr::mutate(Country..Name. = forcats::fct_reorder(Country..Name., -country_production))


  max_value <- max(species_country_ranked_total$country_production)/1e6*1.1

  str(species_country_ranked_total)
  head(species_country_ranked_total)


  species_country_ranked <- species_data %>%
    dplyr::filter(Country..Name. %in% species_country_ranked_total$Country..Name.) %>%
    dplyr::group_by(Year, Country..Name.) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE), .groups = "drop")


  species_country_ranked <- species_country_ranked %>%
    dplyr::group_by(Country..Name.) %>%
    dplyr::mutate(Total_Tonnes = sum(Tonnes, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Country..Name. = factor(Country..Name., levels = unique(Country..Name.[order(-Total_Tonnes)])))

  # 计算每年每个国家的总量和占比
  species_pie <- species_data %>%
    dplyr::group_by(Year, `Country..Name.`) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(total_tonnes = sum(Tonnes),
                  percentage = (Tonnes / total_tonnes) * 100) %>%
    dplyr::arrange(Year, desc(Tonnes)) %>%
    dplyr::mutate(`Country..Name.` = dplyr::if_else(percentage > 5, `Country..Name.`, "Other")) %>%
    dplyr::group_by(Year, `Country..Name.`) %>%
    dplyr::summarise(Tonnes = sum(Tonnes), percentage = sum(percentage), .groups = 'drop')


  # 计算每年每个物种的总量和占比，并识别每年的前10名物种
  species_data <- species_data %>%
    dplyr::mutate(Decade = dplyr::case_when(
      Year %in% 1950:1959 ~ "1950s",
      Year %in% 1960:1969 ~ "1960s",
      Year %in% 1970:1979 ~ "1970s",
      Year %in% 1980:1989 ~ "1980s",
      Year %in% 1990:1999 ~ "1990s",
      Year %in% 2000:2009 ~ "2000s",
      Year %in% 2010:2019 ~ "2010s",
      TRUE               ~ as.character(Year)
    ))

  species_pie <- species_data %>%
    dplyr::group_by(Decade, `Country..Name.`) %>%
    dplyr::summarise(Tonnes = mean(sum(Production, na.rm = TRUE)), .groups = 'drop') %>%
    dplyr:: group_by(Decade) %>%
    dplyr::mutate(total_tonnes = sum(Tonnes),
                  percentage = (Tonnes / total_tonnes) * 100) %>%
    dplyr:: arrange(Decade, desc(Tonnes)) %>%
    dplyr:: mutate(`Country..Name.` = dplyr::if_else(percentage > 5, `Country..Name.`, "Other")) %>%
    dplyr::group_by(Decade, `Country..Name.`) %>%
    dplyr::summarise(Tonnes = sum(Tonnes), percentage = sum(percentage), .groups = 'drop')

  # 筛选特定的年份，你可以根据需要更改
  selected_decades <- c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s",
                        "2017", "2018", "2019", "2020", "2021")

  species_pie_selected <- species_pie %>%
    dplyr::filter(Decade %in% selected_decades)


  species_pie_selected<-species_pie_selected%>%
    dplyr::mutate(Country..Name.=forcats::fct_reorder(Country..Name.,-Tonnes))


  color_palette <- RColorBrewer::brewer.pal(10, "Set3") # 这将生成一个包含10种不同颜色的调色板
  color_palette <- c(color_palette, "grey") # 添加第11种颜色为灰色，表示"Other"





  get_bp_coefs <- function(country_name, data,h_value) {

    country_data <- data[data$Country..Name. == country_name, ]

    # 检查数据连续性
    years <- country_data$Year
    if (length(years) > 1 && any(diff(years) != 1)) {
      warning(paste("Data for", country_name, "is not continuous. Breakpoint analysis may be inaccurate."))
    }

    ts_data <- ts(country_data$Tonnes, start = min(years))

    if (length(ts_data) > 1) {
      bp <- breakpoints(ts_data ~ 1,h_value)

      coefs <- coef(bp)

      bps <- c(min(years), breakpoints(bp)$breakpoints + min(years), max(years))

      start_years <- bps[-length(bps)]
      end_years <- bps[-1] - 1

      if (length(coefs) == length(start_years)) {
        coefs_data <- data.frame(
          Country..Name. = country_name,
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

    list(coefs_data = coefs_data, bps = bps)
  }


  # 获取所有独特的国家名
  unique_country <- unique(species_country_ranked[species_country_ranked$Tonnes>0,]$Country..Name.)

  # 应用函数到每一个国家
  results <- lapply(unique_country, get_bp_coefs, data = species_country_ranked[species_country_ranked$Tonnes>0,],h_value)


  # 提取和合并结果为一个数据框
  coefs_data_list <- lapply(results, `[[`, "coefs_data")
  coefs_data <- do.call(rbind, coefs_data_list)
  # 现在，你可以使用coefs_data来创建分面图


  if(plot){

  p1<-ggplot2::ggplot() +
    ggplot2::geom_bar(data = species_country_ranked_total,mapping=ggplot2::aes(y=forcats::fct_reorder(Country..Name., country_production), x=country_production/1e6, fill=Country..Name.), stat="identity") +
    ggplot2::geom_text(data=species_country_ranked_total,
              mapping=ggplot2::aes(y=forcats::fct_reorder(Country..Name.,country_production),
                          x=country_production/1e6,
                          label=sprintf("%.2f%%",percentage_country)),
              hjust=-0.1,size=4.5) +
    ggplot2::theme_bw()+
    ggplot2::labs(x="Million tonnes",y="Counrty")+
    ggsci::scale_fill_npg()+
    ggplot2:: scale_x_continuous(expand = c(0,0), limits = c(0, max_value))+
    ggplot2:: theme(
      panel.grid.minor.x = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
      strip.text = ggplot2::element_text(face = 'bold',size=14),
      axis.title.x = ggplot2::element_text(size=14,face = 'bold'),
      axis.title.y = ggplot2::element_text(size=14,face = 'bold'),
      axis.text.x = ggplot2::element_text(size=12),
      axis.text.y = ggplot2::element_text(size=12),
      legend.text= ggplot2::element_text(size=12),
      legend.position = "none",
      legend.box = "horizontal")
    # 更改文件名以匹配排名数据
  filename <- paste0("figures/", gsub("[[:punct:][:space:]]", "_", species_name), "_country_ranked_total.pptx")
  eoffice::topptx(figure=p1, filename = filename, width=12, height=8)





    p2<-ggplot2::ggplot(species_country_ranked[species_country_ranked$Tonnes>0,], ggplot2::aes(x = Year, y = Tonnes/1e6, color = Country..Name., group = Country..Name.)) +
      ggplot2::geom_point(alpha=0.7,shape=1,size=1.8)+
      ggplot2::geom_line(linewidth=1.5,alpha=0.7) +
      ggplot2::theme_bw() +
      ggsci::scale_color_npg()+
      ggplot2::labs( x = "Year", y = "Million Tonnes") +
      ggplot2::scale_x_continuous(breaks = seq(1950,2021,5),expand = c(0,0))+
      ggplot2::scale_y_continuous(expand = c(0,0))+
      ggplot2::theme(
        panel.grid.minor.x =ggplot2:: element_blank(),
        strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
        strip.text = ggplot2::element_text(face = 'bold',size=10),
        axis.title.x = ggplot2::element_text(size=14,face = 'bold'),
        axis.title.y = ggplot2::element_text(size=14,face = 'bold'),
        axis.text.x = ggplot2::element_text(size=12),
        axis.text.y = ggplot2::element_text(size=12),
        legend.text=ggplot2::element_text(size=12),
        legend.position = "bottom",
        legend.box = "horizontal")+
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2))

    filename <- paste0("figures/", gsub("[[:punct:][:space:]]", "_", species_name), "_country_ranked.pptx")
    eoffice::topptx(figure=p2, filename = filename, width=12, height=8)




    if(timeseries_analysis){
      p3<-ggplot2::ggplot() +
        ggplot2:: geom_smooth(species_country_ranked[species_country_ranked$Tonnes>0,], mapping=ggplot2::aes(x=Year,y = Tonnes/1e6, color = Country..Name.), method = "lm", se = FALSE, linetype = "dashed",linewidth=1) +
        ggplot2::geom_line(species_country_ranked[species_country_ranked$Tonnes>0,], mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = Country..Name., group = Country..Name.),linewidth=1.5) +
        ggplot2::geom_text(data = species_country_ranked_total %>%
                             dplyr::filter(Country..Name. %in% species_country_ranked_total$Country..Name.) %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(Country..Name. = factor(Country..Name., levels = unique(Country..Name.[order(-country_production)]))),
                           ggplot2::aes(x = 1957, y = 0, label = paste0(round(percentage_country, 2), "%")),
                           hjust = 0.9, vjust = -4, size = 4.5, color = "black") +
        ggplot2::geom_segment(data = coefs_data,
                              ggplot2::aes(x = start_year, xend = end_year, y = intercept/1e6, yend = intercept/1e6),
                              color = "black",linetype="solid",size=1.5,alpha=0.5) +
        ggplot2::geom_vline(data = coefs_data, ggplot2::aes(xintercept = start_year, group = Country..Name.), linetype = "dashed", color = "black",linewidth=1) +
        ggplot2::theme_bw() +
        ggsci::scale_color_npg()+
        ggplot2::facet_wrap(~Country..Name.,ncol=2,scales = "free_y")+
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
      #ggplot2::ggsave(filename = "figures/area_country_wrap.png", plot = p2, dpi = 600)
      filename <- paste0("figures/", gsub("[[:punct:][:space:]]", "_", species_name), "_country_ranked_wrap.pptx")
      eoffice::topptx(figure=p3,filename = filename, width = 12, height = 8)

    }
    else{
    p3<-ggplot2::ggplot() +
      ggplot2::geom_smooth(species_country_ranked[species_country_ranked$Tonnes>0,], mapping=ggplot2::aes(x=Year,y = Tonnes/1e6, color = Country..Name.), method = "loess", se = FALSE, linetype = "dashed") +
      ggplot2::geom_line(species_country_ranked[species_country_ranked$Tonnes>0,], mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = Country..Name., group = Country..Name.),linewidth=1.5,alpha=0.7) +
      ggplot2::geom_point(species_country_ranked[species_country_ranked$Tonnes>0,], mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = Country..Name., group = Country..Name.),alpha=0.7,shape=1,size=2)+
      ggplot2::geom_text(data =species_country_ranked_total %>% dplyr::filter(Country..Name. %in% species_country_ranked$Country..Name.) %>% dplyr::ungroup() %>%
                           dplyr::mutate(Country..Name. = factor(Country..Name., levels = unique(Country..Name.[order(-country_production)]))),
                  ggplot2::aes(x = 1957, y = 0, label = paste0( round(percentage_country, 2), "%")),
                hjust = 0.9, vjust = -4, size = 4.5, color = "black") +
      ggplot2::theme_bw() +
      ggsci::scale_color_npg()+
      ggplot2::labs( x = "Year", y = "Million Tonnes") +
      ggplot2::scale_x_continuous(breaks = seq(1950,2021,5),expand = c(0,0))+
      ggplot2::scale_y_continuous()+
      ggplot2::facet_wrap(~Country..Name.,ncol=2,scale="free_y")+
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
        strip.text = ggplot2::element_text(face = 'bold',size=14),
        axis.title.x = ggplot2::element_text(size=14,face = 'bold'),
        axis.title.y = ggplot2::element_text(size=14,face = 'bold'),
        axis.text.y = ggplot2::element_text(size=12),
        legend.position = "bottom",
        legend.box = "horizontal",
        axis.text.x = ggplot2::element_text(size=12,angle = 60, hjust = 1))+
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2))
    filename <- paste0("figures/", gsub("[[:punct:][:space:]]", "_", species_name), "_country_ranked_wrap.pptx")

    eoffice::topptx(figure=p3, filename = filename, width=12, height=8)

}

    # 筛选特定的年份，你可以根据需要更改
      p4<-ggplot2::ggplot(species_pie_selected[species_pie_selected$Tonnes>0,],ggplot2:: aes(x = "", y = percentage, fill = `Country..Name.`)) +
      ggplot2::geom_bar(stat="identity", width=1,color="black") +
      ggplot2::coord_polar("y") +
      ggplot2::geom_text(ggplot2::aes(label=sprintf("%.2f%%",percentage)),position = ggplot2::position_stack(vjust = 0.5))+
      #scale_fill_manual(values = color_palette) + # 使用自定义调色板
      ggplot2::facet_wrap(~Decade) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x  = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(face = 'bold',size=14),
            axis.title.x = ggplot2::element_text(size=14,face = 'bold'),
            axis.title.y = ggplot2::element_text(size=14,face = 'bold'),
            legend.text=ggplot2::element_text(size=12)) +
      ggplot2::labs(
        y = "Percentage of Total Production",
        x = NULL
      )
    filename <- paste0("figures/", gsub("[[:punct:][:space:]]", "_", species_name), "_country_pie_selected.pptx")
    eoffice::topptx(figure=p4, filename = filename, width=12, height=8)
}


    unique_country_count <- species_data %>%
      dplyr::group_by(Country..Name.) %>%
      dplyr::summarise(total_tonnes = sum(Production, na.rm = TRUE)) %>%
      dplyr::filter(total_tonnes > 0) %>%
      nrow()

    print(paste("The total number of country with non-zero catch over the years is:", unique_country_count))

    species_country_table <- species_data %>%
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
    decade_avg_production_country <- species_country_table %>%
      dplyr::group_by(Decade, Country..Name.) %>%
      dplyr::summarize(avg_production = mean(Production, na.rm = TRUE))

    # 计算2017到2021年的产量
    recent_production_country <- species_country_table %>%
      dplyr::filter(Year >= 2017 & Year <= 2021) %>%
      dplyr::group_by(Year, Country..Name.) %>%
      dplyr::summarize(yearly_production = sum(Production, na.rm = TRUE))

    # 计算2021年每个品种的产量
    total_production_2021_country <- species_country_table %>%
      dplyr::filter(Year == 2021) %>%
      dplyr::group_by(Country..Name.) %>%
      dplyr::summarize(total_2021_production = sum(Production, na.rm = TRUE))

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

    # 根据排名范围筛选数据
    decade_avg_production_country_ranked <- decade_avg_production_country %>%
      dplyr::inner_join(species_country_ranked_total, by = "Country..Name.") %>%
      dplyr::select(-country_production,-percentage_country)  # 删除不需要的列

    recent_production_country_ranked <- recent_production_country %>%
      dplyr::inner_join(species_country_ranked_total, by = "Country..Name.") %>%
      dplyr::select(-country_production,-percentage_country)  # 删除不需要的列

    percentage_of_total_country_ranked <- percentage_of_total_country %>%
      dplyr::inner_join(species_country_ranked_total, by = "Country..Name.") %>%
      dplyr::select(-country_production,-percentage_country)  # 删除不需要的列

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
      filename <- paste0(gsub("[[:punct:][:space:]]", "_", species_name), "_country_ranked_table.csv")
      write.csv(final_data, file = paste0("tables/", filename), row.names = FALSE)
    }



  return(species_country_ranked)
}

