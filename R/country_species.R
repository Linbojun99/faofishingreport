#' Analyze and Plot the Main Species Caught by a Country in a Fishing Area
#'
#' This function takes the fishing area data and a country name,
#' analyzes the catch of that country, and identifies the top species
#' based on production within the specified rank range.
#'
#' @param area_data A data frame containing the fishing area data.
#' @param country_name A string specifying the country to be analyzed.
#' @param plot A logical value indicating whether to generate and save plots.
#' @param table A logical value indicating whether to generate and save tables
#' @param timeseries_analysis A logical value indicating whether to analysis timeseries.
#' @param time_analysis_text_displacement The number of years after the first year of data at which
#'        the percentage labels are placed in the faceted time series plot. Default is 10.
#' @param rank_range A numeric vector of length 2 specifying the rank range of species to be analyzed.
#' @param h_value The minimum segment size, which must be greater than the
#'        number of regressors. For the model `ts_data ~ 1`, this means `h_value`
#'        should be greater than 1. The default value is 0.15.
#' @param ZH A logical value to Chinese. Default is "FALSE". `ZH = TRUE` is the same as `lang = "zh"`.
#' @param lang The language of the chart text and names: "en" (English), "zh" (Chinese) or "both"
#'        (Chinese and English). Default is NULL, which uses "zh" if `ZH = TRUE`, and otherwise the language
#'        set with `fao_settings()` ("en" unless changed).
#' @param pptx A logical value. Charts are always saved as PNG in `figures/png/`; if TRUE they are
#'        also saved as editable PowerPoint files in `figures/pptx/`. Default is FALSE, which can be
#'        changed for all functions with `fao_settings(pptx = TRUE)`.
#' @export
#'
#' @examples
#' \dontrun{
#' country_species(FAO34[["area_data"]], country_name = "Morocco",c(1, 10), plot=TRUE,table=TRUE) #area_data=Fao34
#' }
country_species <- function(area_data, country_name, rank_range = c(1, 10),plot=TRUE,table=TRUE,timeseries_analysis=TRUE,time_analysis_text_displacement=10,h_value=0.15,ZH = FALSE, lang = NULL,
                            pptx = getOption("faofishingreport.pptx", FALSE)) {

  options(scipen=999,digits=2)


  #draw set
  lang <- fao_lang(lang, ZH)
  MT <- fao_label("million_tonnes", lang)
  PoTP <- fao_label("share", lang)
  nian <- fao_label("year", lang)
  legendtext <- fao_label("species", lang)
  Other <- fao_label("other_species", lang)
  last_year <- max(area_data$Year, na.rm = TRUE)
  year_breaks <- fao_year_breaks(area_data$Year)

  country_data <- area_data %>%
    dplyr::filter(Country..Name. == country_name)

  country_data <- country_data %>%
    dplyr::group_by(Year, ASFIS.species..Name.) %>%
    dplyr::summarise(Production = sum(Production,na.rm=T))

  country_data$ASFIS.species..Name. <- fao_translate(country_data$ASFIS.species..Name., "species", lang)
  country_species_total <- country_data %>%
    dplyr::group_by(ASFIS.species..Name.) %>%
    dplyr::summarise(species_production=sum(Production,na.rm = T))

  country_species_total$ASFIS.species..Name. <- as.factor(country_species_total$ASFIS.species..Name.)

  total_country_species <- sum(country_species_total$species_production)
  country_species_total <- country_species_total %>%
    dplyr::mutate(percentage_country=(species_production/total_country_species)*100)


  # Adjusted code for ranked selection based on specified rank range
  # Adjusted code for ranked selection based on specified rank range
  country_species_ranked_total <- country_species_total %>%
    dplyr::arrange(desc(species_production)) %>%
    dplyr::slice(rank_range[1]:rank_range[2]) %>%
    dplyr::mutate(ASFIS.species..Name. = forcats::fct_reorder(ASFIS.species..Name., -species_production))


  max_value <- max(country_species_ranked_total$species_production)/1e6*1.1

  str(country_species_ranked_total)
  head(country_species_ranked_total)

  country_species_ranked <- country_data %>%
    dplyr::filter(ASFIS.species..Name. %in% country_species_ranked_total$ASFIS.species..Name.) %>%
    dplyr::group_by(Year, ASFIS.species..Name.) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE), .groups = "drop")


  country_species_ranked <- country_species_ranked %>%
    dplyr::group_by(ASFIS.species..Name.) %>%
    dplyr::mutate(Total_Tonnes = sum(Tonnes, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ASFIS.species..Name. = factor(ASFIS.species..Name., levels = unique(ASFIS.species..Name.[order(-Total_Tonnes)])))

  # 计算每年每个物种的总量和占比
  country_pie <- country_data %>%
    dplyr::group_by(Year, `ASFIS.species..Name.`) %>%
    dplyr::summarise(Tonnes = sum(Production, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(total_tonnes = sum(Tonnes),
                  percentage = (Tonnes / total_tonnes) * 100) %>%
    dplyr::arrange(Year, desc(Tonnes)) %>%
    dplyr::mutate(`ASFIS.species..Name.` = dplyr::if_else(percentage > 5, `ASFIS.species..Name.`, "Other")) %>%
    dplyr::group_by(Year, `ASFIS.species..Name.`) %>%
    dplyr::summarise(Tonnes = sum(Tonnes), percentage = sum(percentage), .groups = 'drop')


  # 计算每年每个物种的总量和占比，并识别每年的前10名物种
  country_data <- country_data %>%
    dplyr::mutate(Decade = fao_pie_period(Year, last_year))

  country_pie <- country_data %>%
    dplyr::group_by(Decade, `ASFIS.species..Name.`) %>%
    dplyr::summarise(Tonnes = mean(sum(Production, na.rm = TRUE)), .groups = 'drop') %>%
    dplyr:: group_by(Decade) %>%
    dplyr::mutate(total_tonnes = sum(Tonnes),
                  percentage = (Tonnes / total_tonnes) * 100) %>%
    dplyr:: arrange(Decade, desc(Tonnes)) %>%
    dplyr:: mutate(`ASFIS.species..Name.` = dplyr::if_else(percentage > 5, `ASFIS.species..Name.`, Other)) %>%
    dplyr::group_by(Decade, `ASFIS.species..Name.`) %>%
    dplyr::summarise(Tonnes = sum(Tonnes), percentage = sum(percentage), .groups = 'drop')

  country_pie <- country_pie %>%
    group_by(Decade) %>%
    mutate(Rank = rank(-Tonnes, ties.method = "first")) %>%
    ungroup() %>%
    mutate(`ASFIS.species..Name.` = if_else(Rank <= 4, as.character(`ASFIS.species..Name.`), Other)) %>%
    group_by(Decade, `ASFIS.species..Name.`) %>%
    summarise(Tonnes = sum(Tonnes), .groups = 'drop') %>%
    mutate(percentage = (Tonnes / sum(Tonnes)) * 100) %>%
    arrange(Decade, desc(Tonnes))

  # 重新计算百分比，因为合并为 "Other" 后总吨位可能会改变
  country_pie <- country_pie %>%
    group_by(Decade) %>%
    mutate(percentage = (Tonnes / sum(Tonnes)) * 100) %>%
    ungroup()

  # 筛选特定的年份，你可以根据需要更改
  selected_decades <- unique(country_pie$Decade)

  country_pie_selected <- country_pie %>%
    dplyr::filter(Decade %in% selected_decades)


  country_pie_selected<-country_pie_selected%>%
    dplyr::mutate(ASFIS.species..Name.=forcats::fct_reorder(ASFIS.species..Name.,-Tonnes))


  color_palette <- RColorBrewer::brewer.pal(10, "Set3") # 这将生成一个包含10种不同颜色的调色板
  color_palette <- c(color_palette, "grey") # 添加第11种颜色为灰色，表示"Other"




  get_bp_coefs <- function(species_name, data, h_value) {
    country_data <- data[data$ASFIS.species..Name. == species_name, ]

    ts_data <- ts(country_data$Tonnes, start = min(country_data$Year))

    if (length(ts_data) > 1) {
      bp <- breakpoints(ts_data ~ 1, h = h_value)
      coefs <- coef(bp)

      bps <- c(min(country_data$Year), breakpoints(bp)$breakpoints + min(country_data$Year), max(country_data$Year))


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
  unique_species <- unique(country_species_ranked[country_species_ranked$Tonnes>0,]$ASFIS.species..Name.)

  # 应用函数到每一个物种
  results <- fao_breakpoints_by(unique_species, get_bp_coefs, data = country_species_ranked[country_species_ranked$Tonnes>0,], h_value)

  # 检查是否所有物种都有结果
  all_species <- unique(country_species_ranked$ASFIS.species..Name.)
  missing_species <- setdiff(all_species, sapply(results, function(x) x$coefs_data$ASFIS.species..Name.))


    # 提取和合并结果为一个数据框

  coefs_data_list <- lapply(results, `[[`, "coefs_data")
  coefs_data <- do.call(rbind, coefs_data_list)

  # 现在，你可以使用coefs_data来创建分面图


  if(plot){


    p1<-ggplot2::ggplot() +
      ggplot2::geom_bar(data = country_species_ranked_total,mapping=ggplot2::aes(y=forcats::fct_reorder(ASFIS.species..Name., species_production), x=species_production/1e6, fill=ASFIS.species..Name.), stat="identity") +
      ggplot2::geom_text(data=country_species_ranked_total,
                         mapping=ggplot2::aes(y=forcats::fct_reorder(ASFIS.species..Name.,species_production),
                                              x=species_production/1e6,
                                              label=sprintf("%.2f%%",percentage_country)),
                         hjust=-0.1,size=4.5) +
      fao_base_theme()+
      ggplot2::labs(x=MT,y=legendtext,fill=legendtext)+
      fao_scale_discrete("fill")+
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
        legend.box = "horizontal",
        text = ggplot2::element_text(family = fao_font()))
    # 更改文件名以匹配排名数据
    fao_save_plot(p1, paste0("country/", fao_file_name(country_name), "_species_ranked_total"), width = 12, height = 8, pptx = pptx)





    p2<-ggplot2::ggplot(country_species_ranked[country_species_ranked$Tonnes>0,], ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.)) +
      ggplot2::geom_point(alpha=0.7,shape=1,size=1.8)+
      ggplot2::geom_line(linewidth=1.5,alpha=0.7) +
      fao_base_theme() +
      fao_scale_discrete("colour")+
      ggplot2::labs( x = nian, y = MT,color=legendtext) +
      ggplot2::scale_x_continuous(breaks = year_breaks,expand = c(0,0))+
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
        legend.box = "horizontal",
        text = ggplot2::element_text(family = fao_font()))+
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2))

    fao_save_plot(p2, paste0("country/", fao_file_name(country_name), "_species_ranked"), width = 12, height = 8, pptx = pptx)




    text_min_year <- country_species_ranked %>%
      filter(Tonnes > 0) %>%
      summarise(text_min_year = min(Year)) %>%
      pull(text_min_year)

    if(timeseries_analysis){

      p3<-ggplot2::ggplot() +
        ggplot2:: geom_smooth(country_species_ranked[country_species_ranked$Tonnes>0,], mapping=ggplot2::aes(x=Year,y = Tonnes/1e6, color = ASFIS.species..Name.), method = "lm", se = FALSE, linetype = "dashed",linewidth=1) +
        ggplot2::geom_line(country_species_ranked[country_species_ranked$Tonnes>0,], mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.),linewidth=1.5) +
        ggplot2::geom_text(data = country_species_ranked_total %>%
                             dplyr::filter(ASFIS.species..Name. %in% country_species_ranked_total$ASFIS.species..Name.) %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(ASFIS.species..Name. = factor(ASFIS.species..Name., levels = unique(ASFIS.species..Name.[order(-species_production)]))),
                           ggplot2::aes(x = text_min_year+time_analysis_text_displacement, y = 0, label = paste0(round(percentage_country, 2), "%")),
                           hjust = 0.9, vjust = -4, size = 4.5, color = "black") +
        ggplot2::geom_segment(data = coefs_data,
                              ggplot2::aes(x = start_year, xend = end_year, y = intercept/1e6, yend = intercept/1e6),
                              color = "black",linetype="solid",size=1.5,alpha=0.5) +
        ggplot2::geom_vline(data = coefs_data, ggplot2::aes(xintercept = start_year, group = ASFIS.species..Name.), linetype = "dashed", color = "black",linewidth=1) +
        fao_base_theme() +
        fao_scale_discrete("colour")+
        ggplot2::facet_wrap(~ASFIS.species..Name.,ncol=2,scales = "free_y")+
        ggplot2::labs(x = nian, y = MT,color=legendtext) +
        ggplot2::scale_x_continuous(breaks = year_breaks,expand = c(0,0))+
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
          axis.text.x = ggplot2::element_text(angle = 60, hjust = 1),
          text = ggplot2::element_text(family = fao_font()))+
        guides(color = ggplot2::guide_legend(nrow = 2))
      fao_save_plot(p3, paste0("country/", fao_file_name(country_name), "_species_ranked_wrap"), width = 12, height = 8, pptx = pptx)

    }
    else{
      p3<-ggplot2::ggplot() +
        ggplot2::geom_smooth(country_species_ranked[country_species_ranked$Tonnes>0,], mapping=ggplot2::aes(x=Year,y = Tonnes/1e6, color = ASFIS.species..Name.), method = "loess", se = FALSE, linetype = "dashed") +
        ggplot2::geom_line(country_species_ranked[country_species_ranked$Tonnes>0,], mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.),linewidth=1.5,alpha=0.7) +
        ggplot2::geom_point(country_species_ranked[country_species_ranked$Tonnes>0,], mapping=ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.),alpha=0.7,shape=1,size=2)+
        ggplot2::geom_text(data =country_species_ranked_total %>% dplyr::filter(ASFIS.species..Name. %in% country_species_ranked$ASFIS.species..Name.) %>% dplyr::ungroup() %>%
                             dplyr::mutate(ASFIS.species..Name. = factor(ASFIS.species..Name., levels = unique(ASFIS.species..Name.[order(-species_production)]))),
                           ggplot2::aes(x = text_min_year+time_analysis_text_displacement, y = 0, label = paste0( round(percentage_country, 2), "%")),
                           hjust = 0.9, vjust = -4, size = 4.5, color = "black") +
        fao_base_theme() +
        fao_scale_discrete("colour")+
        ggplot2::labs( x = nian, y = MT,color=legendtext) +
        ggplot2::scale_x_continuous(breaks = year_breaks,expand = c(0,0))+
        ggplot2::scale_y_continuous()+
        ggplot2::facet_wrap(~ASFIS.species..Name.,ncol=2,scale="free_y")+
        ggplot2::theme(
          panel.grid.minor.x = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
          strip.text = ggplot2::element_text(face = 'bold',size=14),
          axis.title.x = ggplot2::element_text(size=14,face = 'bold'),
          axis.title.y = ggplot2::element_text(size=14,face = 'bold'),
          axis.text.y = ggplot2::element_text(size=12),
          legend.position = "bottom",
          legend.box = "horizontal",
          axis.text.x = ggplot2::element_text(size=12,angle = 60, hjust = 1),
          text = ggplot2::element_text(family = fao_font()))+
        ggplot2::guides(color = ggplot2::guide_legend(nrow = 2))
      fao_save_plot(p3, paste0("country/", fao_file_name(country_name), "_species_ranked_wrap"), width = 12, height = 8, pptx = pptx)
    }

    # 筛选特定的年份，你可以根据需要更改
    p4<-ggplot2::ggplot(country_pie_selected[country_pie_selected$Tonnes>0,],ggplot2:: aes(x = "", y = percentage, fill = `ASFIS.species..Name.`)) +
      ggplot2::geom_bar(stat="identity", width=1,color="black") +
      ggplot2::coord_polar("y") +
      fao_scale_discrete("fill") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f%%", percentage)), position = position_stack(vjust = 0.5), size=3, color = "white") + # 使用 position_stack 调整文本位置

      #scale_fill_manual(values = color_palette) + # 使用自定义调色板
      ggplot2::facet_wrap(~Decade) +
      fao_minimal_theme() +
      ggplot2::theme(axis.text.x  = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(face = 'bold',size=14),
                     axis.title.x = ggplot2::element_text(size=14,face = 'bold'),
                     axis.title.y = ggplot2::element_text(size=14,face = 'bold'),
                     legend.text=ggplot2::element_text(size=12),
                     text = ggplot2::element_text(family = fao_font())) +
      ggplot2::labs(
        y = PoTP,
        x = NULL,
        fill=legendtext
      )
    fao_save_plot(p4, paste0("country/", fao_file_name(country_name), "_species_pie_selected"), width = 12, height = 8, pptx = pptx)
  }


  unique_country_count <- country_data %>%
    dplyr::group_by(ASFIS.species..Name.) %>%
    dplyr::summarise(total_tonnes = sum(Production, na.rm = TRUE)) %>%
    dplyr::filter(total_tonnes > 0) %>%
    nrow()

  print(paste("The total number of country with non-zero catch over the years is:", unique_country_count))

  country_species_table <- country_data %>%
    dplyr::mutate(Decade = fao_decade(Year))

  # 计算每个年代的平均产量
  decade_avg_production_country <- country_species_table %>%
    dplyr::group_by(Decade, ASFIS.species..Name.) %>%
    dplyr::summarize(avg_production = mean(Production, na.rm = TRUE))

  # 计算最近5年的产量
  recent_production_country <- country_species_table %>%
    dplyr::filter(Year > last_year - 5) %>%
    dplyr::group_by(Year, ASFIS.species..Name.) %>%
    dplyr::summarize(yearly_production = sum(Production, na.rm = TRUE))

  # 计算最后一年每个品种的产量
  total_production_2021_country <- country_species_table %>%
    dplyr::filter(Year == last_year) %>%
    dplyr::group_by(ASFIS.species..Name.) %>%
    dplyr::summarize(total_2021_production = sum(Production, na.rm = TRUE))

  # 计算最后一年的总产量
  total_production_2021 <- sum(total_production_2021_country$total_2021_production, na.rm = TRUE)

  # 计算每个品种在最后一年的产量占总产量的百分比
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
    dplyr::inner_join(country_species_ranked_total, by = "ASFIS.species..Name.") %>%
    dplyr::select(-species_production,-percentage_country)  # 删除不需要的列

  recent_production_country_ranked <- recent_production_country %>%
    dplyr::inner_join(country_species_ranked_total, by = "ASFIS.species..Name.") %>%
    dplyr::select(-species_production,-percentage_country)  # 删除不需要的列

  percentage_of_total_country_ranked <- percentage_of_total_country %>%
    dplyr::inner_join(country_species_ranked_total, by = "ASFIS.species..Name.") %>%
    dplyr::select(-species_production,-percentage_country)  # 删除不需要的列

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
    dplyr::rename(!!paste0("total_", last_year, "_production") := total_2021_production,
                  !!paste0("percentage_of_total_", last_year) := percentage_of_total)

  # 将所有数据框连接到一起
  final_data <- decade_avg_wide %>%
    dplyr::left_join(recent_production_wide, by = "ASFIS.species..Name.") %>%
    dplyr::left_join(percentage_of_total_country_ranked, by = "ASFIS.species..Name.")

  # 输出结果
  print(final_data)
  if (table) {
    filename <- paste0(gsub("[[:punct:][:space:]]", "_", country_name), "_country_ranked_table.csv")
    fao_write_csv(final_data, paste0("tables/", filename))
  }



  return(country_species_ranked)
}

