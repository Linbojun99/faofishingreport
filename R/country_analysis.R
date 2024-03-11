#' Analyze and Plot Specified Species Data Ranked by Country Production Global
#'
#' This function takes a data frame and a specified species name,
#' analyzes the data for the specified species, and identifies the top countries
#' based on production within the specified rank range.
#'
#' @param country_name A string specifying the species to be analyzed.
#' @param rank_range A numeric vector of length 2 specifying the rank range of countries to be analyzed.
#' @param plot A logical value indicating whether to generate and save plots.
#' @param table A logical value indicating whether to generate and save tables
#' @param timeseries_analysis A logical value for time series analysis of text positional offsets.
#' @param rank_range A numeric vector of length 2 specifying the rank range of countries to be analyzed.
#' @param h_value The minimum segment size, which must be greater than the
#'        number of regressors. For the model `ts_data ~ 1`, this means `h_value`
#'        should be greater than 1. The default value is 0.15.
#' @param ZH A logical value to Chinese. Default is "FALSE"
#' @param fill_scale The color scale for filling polygons in the map plot. Default is "deep-orange".
#' @export
#'
#' @examples
#' \dontrun{
#' country_analysis(country_name = "Morocco",c(1, 10), plot=TRUE,table=TRUE)
#' }
country_analysis <- function(country_name, rank_range = c(1, 10),plot=TRUE,table=TRUE,timeseries_analysis=TRUE,time_analysis_text_displacement=10,h_value=0.15,ZH = FALSE,fill_scale="deep-orange") {

  options(scipen=999,digits=2)


  #draw set
  MT="Million Tonnes"
  PoTP="Percentage of Total Production"
  Ct="Country"
  nian="Year"
  legendtext="ASFIS.species..Name."
  Other="Other"
  FA="Fishing Area"
  if(ZH){
    MT="百万吨"
    Ct="国家"
    nian="年"
    PoTP="捕捞产量占比"
    legendtext="ASFIS物种名"
    Other="其余物种"
    FA="渔区"
  }
  data_path <- system.file("extdata", "Global_Production_long.rda", package="faofishingreport")
  load(data_path, envir = .GlobalEnv)
  global_data <- Global_Production_long

  country_data <- global_data %>%
    dplyr::filter(Country..Name. == country_name)


  country_data <- country_data %>%
    dplyr::group_by(Year, ASFIS.species..Name.) %>%
    dplyr::summarise(Production = sum(Production,na.rm=T))

  data_path <- system.file("extdata", "ASFIS_sp.rda", package="faofishingreport")
  load(data_path, envir = .GlobalEnv)

  colnames(ASFIS_sp)[5]<-"ASFIS.species..Name."


  if(ZH)
  {

    country_data <- left_join(country_data, ASFIS_sp, by = "ASFIS.species..Name.")
    country_data <- country_data %>%
      mutate(Chinese_name = ifelse(is.na(Chinese_name), ASFIS.species..Name., Chinese_name))
    country_data<-country_data[,-2]
    colnames(country_data)[10]<-"ASFIS.species..Name."
  }
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
  selected_decades <- c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s",
                        "2017", "2018", "2019", "2020", "2021")

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
  results <- lapply(unique_species, get_bp_coefs, data = country_species_ranked[country_species_ranked$Tonnes>0,],h_value)

  # 检查是否所有物种都有结果
  all_species <- unique(country_species_ranked$ASFIS.species..Name.)
  missing_species <- setdiff(all_species, sapply(results, function(x) x$coefs_data$ASFIS.species..Name.))


  # 提取和合并结果为一个数据框

  coefs_data_list <- lapply(results, `[[`, "coefs_data")
  coefs_data <- do.call(rbind, coefs_data_list)

  # 现在，你可以使用coefs_data来创建分面图


  if(plot){

    if (!dir.exists("figures")) {
      dir.create("figures")
    }

    if (!dir.exists("figures/png")) {
      dir.create("figures/png")
    }

    if (!dir.exists("figures/pptx")) {
      dir.create("figures/pptx")
    }

    if (!dir.exists("figures/png/country")) {
      dir.create("figures/png/country")
    }

    if (!dir.exists("figures/pptx/country")) {
      dir.create("figures/pptx/country")
    }

    p1<-ggplot2::ggplot() +
      ggplot2::geom_bar(data = country_species_ranked_total,mapping=ggplot2::aes(y=forcats::fct_reorder(ASFIS.species..Name., species_production), x=species_production/1e6, fill=ASFIS.species..Name.), stat="identity") +
      ggplot2::geom_text(data=country_species_ranked_total,
                         mapping=ggplot2::aes(y=forcats::fct_reorder(ASFIS.species..Name.,species_production),
                                              x=species_production/1e6,
                                              label=sprintf("%.2f%%",percentage_country)),
                         hjust=-0.1,size=4.5) +
      ggplot2::theme_bw()+
      ggplot2::labs(x=MT,y=Ct,fill=legendtext)+
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
        legend.box = "horizontal",
        text = ggplot2::element_text(family = "Songti SC"))
    # 更改文件名以匹配排名数据
    filename <- paste0("figures/pptx/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_species_ranked_total.pptx")

    eoffice::topptx(figure=p1, filename = filename, width=12, height=8)

    filename <- paste0("figures/png/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_species_ranked_total.png")
    ggsave(plot=p1,filename = filename,dpi=600, width=12, height=8)





    p2<-ggplot2::ggplot(country_species_ranked[country_species_ranked$Tonnes>0,], ggplot2::aes(x = Year, y = Tonnes/1e6, color = ASFIS.species..Name., group = ASFIS.species..Name.)) +
      ggplot2::geom_point(alpha=0.7,shape=1,size=1.8)+
      ggplot2::geom_line(linewidth=1.5,alpha=0.7) +
      ggplot2::theme_bw() +
      ggsci::scale_color_npg()+
      ggplot2::labs( x = nian, y = MT,color=legendtext) +
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
        legend.box = "horizontal",
        text = ggplot2::element_text(family = "Songti SC"))+
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2))

    filename <- paste0("figures/pptx/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_species_ranked.pptx")

    eoffice::topptx(figure=p2, filename = filename, width=12, height=8)

    filename <- paste0("figures/png/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_species_ranked.png")
    ggsave(plot=p2,filename = filename,dpi=600, width=12, height=8)




    if(timeseries_analysis){

      text_min_year <- country_species_ranked %>%
        filter(Tonnes > 0) %>%
        summarise(text_min_year = min(Year)) %>%
        pull(text_min_year)

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
        ggplot2::theme_bw() +
        ggsci::scale_color_npg()+
        ggplot2::facet_wrap(~ASFIS.species..Name.,ncol=2,scales = "free_y")+
        ggplot2::labs(x = nian, y = MT,color=legendtext) +
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
          axis.text.x = ggplot2::element_text(angle = 60, hjust = 1),
          text = ggplot2::element_text(family = "Songti SC"))+
        guides(color = ggplot2::guide_legend(nrow = 2))
      #ggplot2::ggsave(filename = "figures/area_country_wrap.png", plot = p2, dpi = 600)
      filename <- paste0("figures/pptx/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_species_ranked_wrap.pptx")
      eoffice::topptx(figure=p3,filename = filename, width = 12, height = 8)

      filename <- paste0("figures/png/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_species_ranked_wrap.png")
      ggsave(plot=p3,filename = filename,dpi=600, width=12, height=8)

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
        ggplot2::theme_bw() +
        ggsci::scale_color_npg()+
        ggplot2::labs( x = nian, y = MT,color=legendtext) +
        ggplot2::scale_x_continuous(breaks = seq(1950,2021,5),expand = c(0,0))+
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
          text = ggplot2::element_text(family = "Songti SC"))+
        ggplot2::guides(color = ggplot2::guide_legend(nrow = 2))
      filename <- paste0("figures/pptx/country/", gsub("[[:punct:][:space:]]", "_", country_name), "_species_ranked_wrap.pptx")

      eoffice::topptx(figure=p3, filename = filename, width=12, height=8)

      filename <- paste0("figures/png/country/", gsub("[[:punct:][:space:]]", "_", country_name), "_species_ranked_wrap.png")
      ggsave(plot=p3,filename = filename,dpi=600, width=12, height=8)
    }

    # 筛选特定的年份，你可以根据需要更改
    p4<-ggplot2::ggplot(country_pie_selected[country_pie_selected$Tonnes>0,],ggplot2:: aes(x = "", y = percentage, fill = `ASFIS.species..Name.`)) +
      ggplot2::geom_bar(stat="identity", width=1,color="black") +
      ggplot2::coord_polar("y") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f%%", percentage)), position = position_stack(vjust = 0.5), size=3, color = "white") + # 使用 position_stack 调整文本位置

      #scale_fill_manual(values = color_palette) + # 使用自定义调色板
      ggplot2::facet_wrap(~Decade) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x  = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(face = 'bold',size=14),
                     axis.title.x = ggplot2::element_text(size=14,face = 'bold'),
                     axis.title.y = ggplot2::element_text(size=14,face = 'bold'),
                     legend.text=ggplot2::element_text(size=12),
                     text = ggplot2::element_text(family = "Songti SC")) +
      ggplot2::labs(
        y = PoTP,
        x = NULL,
        fill=legendtext
      )
    filename <- paste0("figures/pptx/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_species_pie_selected.pptx")

    eoffice::topptx(figure=p4, filename = filename, width=12, height=8)

    filename <- paste0("figures/png/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_species_pie_selected.png")
    ggsave(plot=p4,filename = filename,dpi=600, width=12, height=8)
  }


  unique_country_count <- country_data %>%
    dplyr::group_by(ASFIS.species..Name.) %>%
    dplyr::summarise(total_tonnes = sum(Production, na.rm = TRUE)) %>%
    dplyr::filter(total_tonnes > 0) %>%
    nrow()

  print(paste("The total number of country with non-zero catch over the years is:", unique_country_count))

  country_species_table <- country_data %>%
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
  decade_avg_production_country <- country_species_table %>%
    dplyr::group_by(Decade, ASFIS.species..Name.) %>%
    dplyr::summarize(avg_production = mean(Production, na.rm = TRUE))

  # 计算2017到2021年的产量
  recent_production_country <- country_species_table %>%
    dplyr::filter(Year >= 2017 & Year <= 2021) %>%
    dplyr::group_by(Year, ASFIS.species..Name.) %>%
    dplyr::summarize(yearly_production = sum(Production, na.rm = TRUE))

  # 计算2021年每个品种的产量
  total_production_2021_country <- country_species_table %>%
    dplyr::filter(Year == 2021) %>%
    dplyr::group_by(ASFIS.species..Name.) %>%
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
    dplyr::rename(percentage_of_total_2021 = percentage_of_total)

  # 将所有数据框连接到一起
  final_data <- decade_avg_wide %>%
    dplyr::left_join(recent_production_wide, by = "ASFIS.species..Name.") %>%
    dplyr::left_join(percentage_of_total_country_ranked, by = "ASFIS.species..Name.")

  # 输出结果
  print(final_data)
  if (table) {
    filename <- paste0(gsub("[[:punct:][:space:]]", "_", country_name), "analysis_country_ranked_table.csv")
    write.csv(final_data, file = paste0("tables/", filename), row.names = FALSE)
  }





  #######by area


    #######map
    country_data <- global_data %>%
      dplyr::filter(Country..Name. == country_name)

    #
    country_species_total <- country_data %>%
      dplyr::group_by(ASFIS.species..Name.) %>%
      dplyr::summarise(species_production=sum(Production,na.rm = T))

    country_species_total$ASFIS.species..Name. <- as.factor(country_species_total$ASFIS.species..Name.)

    total_country_species <- sum(country_species_total$species_production)
    country_species_total <- country_species_total %>%
      dplyr::mutate(percentage_country=(species_production/total_country_species)*100)

    #
    country_species_ranked_total_range <- country_species_total %>%
      arrange(desc(species_production)) %>%
      slice(rank_range[1]:rank_range[2]) %>%
      mutate(ASFIS.species..Name. = fct_reorder(ASFIS.species..Name., -species_production))

    #
    top_species <- as.factor(country_species_ranked_total_range$ASFIS.species..Name.)
    #
    country_species_by_area <- country_data %>%
      filter(ASFIS.species..Name. %in% top_species) %>%
      group_by(ASFIS.species..Name., FAO.major.fishing.area..Name.) %>%
      summarise(species_area_production = sum(Production, na.rm = TRUE)) %>%
      mutate(percentage_area = (species_area_production / total_country_species) * 100)

    ########
    #
    country_species_area_total <- country_data %>%
      group_by(ASFIS.species..Name., FAO.major.fishing.area..Name.) %>%
      summarise(area_species_production = sum(Production, na.rm = TRUE)) %>%
      ungroup()

    #
    country_species_total <- country_species_area_total %>%
      group_by(ASFIS.species..Name.) %>%
      summarise(total_species_production = sum(area_species_production, na.rm = TRUE)) %>%
      ungroup()

    #
    country_species_area_total <- country_species_area_total %>%
      left_join(country_species_total, by = "ASFIS.species..Name.")

    #
    country_species_area_total <- country_species_area_total %>%
      mutate(percentage_of_total = (area_species_production / total_species_production) * 100)

    country_species_area_total <- country_species_area_total %>%
      filter(ASFIS.species..Name. %in% top_species)

    #
    country_species_area_total <- country_species_area_total %>%
      arrange(desc(total_species_production), ASFIS.species..Name.)

    data_path <- system.file("extdata", "FAO_AREAS_CWP.shp", package="faofishingreport")
    fao_areas <- st_read(shapefile_path)

    fao_areas_filtered <- fao_areas%>%
      filter(as.integer(F_CODE) == F_CODE)

    tryCatch({
      # 可能产生警告的代码
      fao_areas_filtered <- fao_areas %>%
        filter(!is.na(as.numeric(F_CODE)) & F_CODE == as.character(floor(as.numeric(F_CODE))))
    }, warning = function(w) {
      # 打印原始警告信息
      print(w)
      # 向用户提供额外的信息
      message("上述警告信息可以忽略。")
    })


    world <- ne_countries(scale = "medium", returnclass = "sf")


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
    world$admin <- ifelse(world$admin %in% names(replace_rules), replace_rules[world$admin], world$admin)



    country_map <- world %>%
      filter(admin == country_name)

    # 假设'country_species_area_total'数据框中的'area_species_production'列已经按百万吨进行了缩放
    act_world_map <- fao_areas_filtered %>%
      left_join(country_species_area_total, by = c("NAME_EN" = "FAO.major.fishing.area..Name."))
    # 使用ggplot绘制地图
    if(plot){
      pmap<-ggplot() +
        geom_sf(data = act_world_map, aes(fill = area_species_production/1e6)) +  # 根据产量映射颜色填充
        geom_sf(data = world, fill = "gray80", color = "gray80") +  # 绘制底图
        geom_sf(data = fao_areas_filtered, fill = NA, color = "gray80") +  # 绘制FAO渔区的边框
        geom_sf(data = country_map, fill = "red", color = "black", size = 0.5) +  # 突出显示摩洛哥
        scale_fill_material(fill_scale, na.value = "White")+
        theme_minimal() +  # 无主题，不显示坐标轴和背景
        coord_sf(crs = st_crs(4326)) +
        ggplot2::theme(
          panel.grid.minor.x = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
          strip.text = ggplot2::element_text(face = 12, size=12),
          axis.title.x = ggplot2::element_text(size=12),
          axis.title.y = ggplot2::element_text(size=12),
          axis.text.y = ggplot2::element_text(size=12),
          axis.text.x = ggplot2::element_text(size=12),
          legend.text = ggplot2::element_text(size=12),
          legend.position = "bottom",
          legend.box = "horizontal",
          text =  ggplot2::element_text(family = "Songti SC"))+
        labs(fill=MT)

      filename <- paste0("figures/pptx/country/", gsub("[[:punct:][:space:]]", "_", country_name), "fishing_area_map.pptx")

      eoffice::topptx(figure=pmap, filename = filename, width=12, height=8)

      filename <- paste0("figures/png/country/", gsub("[[:punct:][:space:]]", "_", country_name), "fishing_area_map.png")
      ggsave(plot=pmap,filename = filename,dpi=600, width=12, height=8)

    }



  country_data <- global_data %>%
    dplyr::filter(Country..Name. == country_name)


  fao_area_names_en_to_cn <- c(
    "Africa - Inland waters" = "非洲内陆水域",
    "America, North - Inland waters" = "北美洲内陆水域",
    "America, South - Inland waters" = "南美洲内陆水域",
    "Asia -Inland waters" = "亚洲内陆水域",
    "Europe - Inland waters" = "欧洲内陆水域",
    "Oceania - Inland waters" = "大洋洲内陆水域",
    "Antarctica - Inland waters" = "南极洲内陆水域",

    "Arctic Sea" = "北冰洋",

    "Atlantic, Northwest" = "西北大西洋",
    "Atlantic, Northeast" = "东北大西洋",
    "Atlantic, Western Central" = "中西大西洋",
    "Atlantic, Eastern Central" = "中东大西洋",
    "Mediterranean and Black Sea" = "地中海和黑海",
    "Atlantic, Southwest" = "西南大西洋",
    "Atlantic, Southeast" = "东南大西洋",

    "Indian Ocean, Western" = "西印度洋",
    "Indian Ocean, Eastern" = "东印度洋",

    "Pacific, Northwest" = "西北太平洋",
    "Pacific, Northeast" = "东北太平洋",
    "Pacific, Western Central" = "中西太平洋",
    "Pacific, Eastern Central" = "中东太平洋",
    "Pacific, Southwest" = "西南太平洋",
    "Pacific, Southeast" = "东南太平洋",

    "Atlantic, Antarctic" = "南大西洋",
    "Indian Ocean, Antarctic" = "南印度洋",
    "Pacific, Antarctic" = "南太平洋"
  )


  if(ZH)
  {

    country_data <- left_join(country_data, ASFIS_sp, by = "ASFIS.species..Name.")
    country_data <- country_data %>%
      mutate(Chinese_name = ifelse(is.na(Chinese_name), ASFIS.species..Name., Chinese_name))
    country_data<-country_data[,-2]
    colnames(country_data)[15]<-"ASFIS.species..Name."

    country_data <- country_data %>%
      mutate(FAO.major.fishing.area..Name. = fao_area_names_en_to_cn[FAO.major.fishing.area..Name.])

  }

  country_species_total <- country_data %>%
    dplyr::group_by(ASFIS.species..Name.) %>%
    dplyr::summarise(species_production=sum(Production,na.rm = T))

  country_species_total$ASFIS.species..Name. <- as.factor(country_species_total$ASFIS.species..Name.)

  total_country_species <- sum(country_species_total$species_production)
  country_species_total <- country_species_total %>%
    dplyr::mutate(percentage_country=(species_production/total_country_species)*100)



  # 排序并筛选出总产量排名靠前的物种
  country_species_ranked_total_range <- country_species_total %>%
    arrange(desc(species_production)) %>%
    slice(rank_range[1]:rank_range[2]) %>%
    mutate(ASFIS.species..Name. = fct_reorder(ASFIS.species..Name., -species_production))

  # 获取前10排名的物种的名称
  top_species <- as.factor(country_species_ranked_total_range$ASFIS.species..Name.)
  # 重新计算这些物种在不同区域的捕捞总量和占比
  country_species_by_area <- country_data %>%
    filter(ASFIS.species..Name. %in% top_species) %>%
    group_by(ASFIS.species..Name., FAO.major.fishing.area..Name.) %>%
    summarise(species_area_production = sum(Production, na.rm = TRUE)) %>%
    mutate(percentage_area = (species_area_production / total_country_species) * 100)

  ########
  # 首先按物种和区域分组，然后计算每组的总产量
  country_species_area_total <- country_data %>%
    group_by(ASFIS.species..Name., FAO.major.fishing.area..Name.) %>%
    summarise(area_species_production = sum(Production, na.rm = TRUE)) %>%
    ungroup()

  # 然后按物种分组计算每个物种的全球总产量
  country_species_total <- country_species_area_total %>%
    group_by(ASFIS.species..Name.) %>%
    summarise(total_species_production = sum(area_species_production, na.rm = TRUE)) %>%
    ungroup()

  # 将全球总产量合并回按区域分组的数据中
  country_species_area_total <- country_species_area_total %>%
    left_join(country_species_total, by = "ASFIS.species..Name.")

  # 计算每个物种在每个区域的捕捞量占该物种全球总捕捞量的百分比
  country_species_area_total <- country_species_area_total %>%
    mutate(percentage_of_total = (area_species_production / total_species_production) * 100)

  country_species_area_total <- country_species_area_total %>%
    filter(ASFIS.species..Name. %in% top_species)

  # 首先，确保数据是按照物种的总产量排序的
  country_species_area_total <- country_species_area_total %>%
    arrange(desc(total_species_production), ASFIS.species..Name.)

  options(scipen=999,digits=3)

  if(plot)
    {
  p5<- ggplot2::ggplot(country_species_area_total,  ggplot2::aes(x = ASFIS.species..Name., y = area_species_production/1e6, fill = FAO.major.fishing.area..Name.)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") + # 保持position为"stack"以反映总量
    ggplot2::coord_flip() + # 使柱状图横向显示
    ggplot2::labs(x = legendtext, y = MT, fill = FA) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(percentage_of_total > 0.01, paste0(round(percentage_of_total, 2), "%"), "")),
              position = position_stack(vjust = 0.5),
              check_overlap = TRUE, # 避免文本重叠
              size = 3.5, # 文本大小
              hjust = -0.3) +
    ggplot2::theme_bw()+
    ggplot2::scale_y_continuous(expand=c(0,0))+
    ggplot2::theme(
      panel.grid.minor.y =  ggplot2::element_blank(),
      strip.background =  ggplot2::element_rect(fill = "white", colour = "black"),
      strip.text =  ggplot2::element_text(face = 'bold', size=14),
      axis.title.y =  ggplot2::element_text(size=14, face = 'bold'),
      axis.title.x =  ggplot2::element_text(size=14, face = 'bold'),
      axis.text.x =  ggplot2::element_text(size=12),
      legend.box = "horizontal",
      axis.text.y =  ggplot2::element_text(size=12, hjust = 1), # 由于已经调整为横向，可能需要重新考虑角度和对齐方式
      text =  ggplot2::element_text(family = "Songti SC")
    )

  filename <- paste0("figures/pptx/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_species_by_area.pptx")

  eoffice::topptx(figure=p5, filename = filename, width=12, height=8)

  filename <- paste0("figures/png/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_species_by_area.png")
  ggsave(plot=p5,filename = filename,dpi=600, width=12, height=8)
  }



  ###############################area
  # 按渔区分组计算总产量
  area_total_production <- country_data %>%
    group_by(FAO.major.fishing.area..Name.) %>%
    summarise(area_total_production = sum(Production, na.rm = TRUE)) %>%
    ungroup() # 确保后续操作不受分组影响

  # 计算全球所有渔区的总产量
  global_total_production <- sum(area_total_production$area_total_production)

  # 计算每个渔区产量占全球总产量的百分比
  area_total_production <- area_total_production %>%
    mutate(percentage_of_global = (area_total_production / global_total_production) * 100)


  if(plot)
  {
    p6<-
  ggplot(area_total_production[area_total_production$percentage_of_global>0,], ggplot2::aes(x = reorder(FAO.major.fishing.area..Name., -area_total_production), y = area_total_production/1e6, fill = FAO.major.fishing.area..Name.)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() + # 使柱状图横向显示，便于阅读
    ggplot2::labs(x = FA, y = MT, fill = FA) +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(percentage_of_global > 0, paste0(round(percentage_of_global, 2), "%"), "")),
        position = position_stack(vjust = 0.5),
        hjust = -0.1
      )+
      ggplot2::theme_bw()+
    ggplot2::scale_y_continuous(expand=c(0,0))+
    ggplot2::theme(
      panel.grid.minor.y =  ggplot2::element_blank(),
      strip.background =  ggplot2::element_rect(fill = "white", colour = "black"),
      strip.text =  ggplot2::element_text(face = 'bold', size=14),
      axis.title.y =  ggplot2::element_text(size=14, face = 'bold'),
      axis.title.x =  ggplot2::element_text(size=14, face = 'bold'),
      axis.text.x =  ggplot2::element_text(size=12),
      legend.box = "horizontal",
      legend.position="none",
      axis.text.y =  ggplot2::element_text(size=12, hjust = 1), # 由于已经调整为横向，可能需要重新考虑角度和对齐方式
      text =  ggplot2::element_text(family = "Songti SC")
    )

    filename <- paste0("figures/pptx/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_area_total_production.pptx")

    eoffice::topptx(figure=p6, filename = filename, width=12, height=8)

    filename <- paste0("figures/png/country/", gsub("[[:punct:][:space:]]", "_", country_name), "analysis_area_total_production.png")
    ggsave(plot=p6,filename = filename,dpi=600, width=12, height=8)
  }


  # 假设country_data已经包含了'Year'、'FAO.major.fishing.area..Name.'和'Production'列

  # 按年份和渔区分组计算每组的总产量
  yearly_area_production <- country_data %>%
    group_by(Year, FAO.major.fishing.area..Name.) %>%
    summarise(yearly_area_production = sum(Production, na.rm = TRUE)) %>%
    ungroup()

  # 计算每年全球总产量
  global_yearly_production <- yearly_area_production %>%
    group_by(Year) %>%
    summarise(global_yearly_total = sum(yearly_area_production, na.rm = TRUE)) %>%
    ungroup()

  # 将全球年产量合并回每年每个渔区的数据中
  yearly_area_production <- yearly_area_production %>%
    left_join(global_yearly_production, by = "Year")

  # 计算每个渔区产量在对应年份全球总产量的占比
  yearly_area_production <- yearly_area_production %>%
    mutate(percentage_of_yearly_global = (yearly_area_production / global_yearly_total) * 100)

  # 查看结果
  print(yearly_area_production)

  if(plot){
  # 绘制折线图，展示每年每个渔区的产量占比变化
  p7<-ggplot2::ggplot(yearly_area_production, aes(x = Year, y = yearly_area_production/1e6, group = FAO.major.fishing.area..Name., color = FAO.major.fishing.area..Name.)) +
    ggplot2::geom_line(linewidth=1.5) + # 绘制折线
    ggplot2::geom_point() + # 在每个数据点上绘制点
    labs(x = nian, y = MT, color = FA) +
    ggplot2::theme_bw()+
    ggplot2::scale_y_continuous(expand=c(0,0))+
    ggplot2::theme(
      panel.grid.minor.y =  ggplot2::element_blank(),
      strip.background =  ggplot2::element_rect(fill = "white", colour = "black"),
      strip.text =  ggplot2::element_text(face = 'bold', size=14),
      axis.title.y =  ggplot2::element_text(size=14, face = 'bold'),
      axis.title.x =  ggplot2::element_text(size=14, face = 'bold'),
      axis.text.x =  ggplot2::element_text(size=12),
      legend.box = "horizontal",
      legend.position = "bottom",
      axis.text.y =  ggplot2::element_text(size=12, hjust = 1), # 由于已经调整为横向，可能需要重新考虑角度和对齐方式
      text =  ggplot2::element_text(family = "Songti SC"))+
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 2))


  filename <- paste0("figures/pptx/country/", gsub("[[:punct:][:space:]]", "_", country_name), "yearly_area_production.pptx")

  eoffice::topptx(figure=p7, filename = filename, width=12, height=8)

  filename <- paste0("figures/png/country/", gsub("[[:punct:][:space:]]", "_", country_name), "yearly_area_production.png")
  ggsave(plot=p7,filename = filename,dpi=600, width=12, height=8)

 p8<- ggplot2:: ggplot(yearly_area_production[yearly_area_production$yearly_area_production>0,], ggplot2::aes(x = Year, y = yearly_area_production/1e6, group = FAO.major.fishing.area..Name., color = FAO.major.fishing.area..Name.)) +
    ggplot2::geom_line(linewidth=1.5) + # 绘制折线
    ggplot2::geom_point() + # 在每个数据点上绘制点
    ggplot2::labs(x = nian, y = MT, color = FA) +
    ggplot2::theme_bw()+
    ggplot2::scale_y_continuous(expand=c(0,0))+
    ggplot2::theme(
      panel.grid.minor.y =  ggplot2::element_blank(),
      strip.background =  ggplot2::element_rect(fill = "white", colour = "black"),
      strip.text =  ggplot2::element_text(face = 'bold', size=14),
      axis.title.y =  ggplot2::element_text(size=14, face = 'bold'),
      axis.title.x =  ggplot2::element_text(size=14, face = 'bold'),
      axis.text.x =  ggplot2::element_text(size=12),
      legend.box = "horizontal",
      legend.position = "none",
      axis.text.y =  ggplot2::element_text(size=12, hjust = 1), # 由于已经调整为横向，可能需要重新考虑角度和对齐方式
      text =  ggplot2::element_text(family = "Songti SC")
    )+
    ggplot2::facet_wrap(~FAO.major.fishing.area..Name.,scales="free")



 filename <- paste0("figures/pptx/country/", gsub("[[:punct:][:space:]]", "_", country_name), "yearly_area_production_wrap.pptx")

 eoffice::topptx(figure=p8, filename = filename, width=16, height=9)

 filename <- paste0("figures/png/country/", gsub("[[:punct:][:space:]]", "_", country_name), "yearly_area_production_wrap.png")
 ggsave(plot=p8,filename = filename,dpi=600, width=16, height=9)

  }





  return(list(country_species_ranked=country_species_ranked,country_species_ranked_coefs_data=coefs_data,table=final_data,country_species_area_total=country_species_area_total,country_yearly_area_production=yearly_area_production))
}

