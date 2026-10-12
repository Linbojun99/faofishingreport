#Example:FAO Fishing Area 67  "Pacific, Northeast"
#东北太平洋（67渔区）完整分析的代码集合，对应网站教程「东北太平洋（67区）完整分析」（tutorial-67）
library(faofishingreport)   #加载时会显示识别到的系统和图表字体（Windows 默认宋体 SimSun）

#环境目录设置：图表输出到工作目录下的 figures/，表格输出到 tables/，报告输出到 reports/
#dir.create("~/fao67", showWarnings = FALSE)
#setwd("~/fao67")

#图表设置：在这里改一次即可（fao_settings() 查看当前设置，fao_settings(reset = TRUE) 恢复默认）
#lang："en" 英文，"zh" 中文，"both" 中英双语；dpi 默认 600，屏幕浏览可用 300；pptx = TRUE 另存可编辑的PPT图
fao_settings(lang = "zh", theme = "classic", pptx = FALSE)
#fao_settings(dpi = 300)
#fao_settings(map_projection = "robinson", map_center = 180)   #太平洋居中的地图



###============================================================================###
###1. 数据
#检查并下载FAO最新数据（第一次使用、或FAO发布新版本时运行一次即可）
check_fao_update()
download_fao_data()

#全部数据：每行一个国家、物种、渔区和年份；fao_data_info() 查看所用数据的版本
fao <- load_fao_data()
dim(fao)
fao_data_info()
fao_data_dir()   #数据保存的位置

#全球渔区与67渔区地图（67.1-67.3 及其分区；装了 ggOceanMaps 时带海底地形）
areas <- fao_areas()
NEP_map <- fishing_area_map(67)



###============================================================================###
###2. 67渔区的总产量、物种和国家
NEP <- fishing_area_data("Pacific, Northeast")
area67 <- NEP$area_data

#主要物种（前10）和主要国家（前8）
sp <- fishing_area_species(area67, rank_range = c(1, 10))
co <- fishing_area_country(area67, rank_range = c(1, 8))

#物种和国家的中文名：fao_translate("Pacific cod") 查询；查不到对应中文名的保留英文名
#unique(area67$ASFIS.species..Name.)   #列出全部物种名

#主要物种的捕捞国家
pollock <- species_country(area67, "Alaska pollock(=Walleye poll.)", rank_range = c(1, 5))
hake    <- species_country(area67, "North Pacific hake", rank_range = c(1, 5))
cod     <- species_country(area67, "Pacific cod", rank_range = c(1, 5))
pink    <- species_country(area67, "Pink(=Humpback) salmon", rank_range = c(1, 5))
sockeye <- species_country(area67, "Sockeye(=Red) salmon", rank_range = c(1, 5))

#主要国家的捕捞物种；country_analysis() 是该国在全球所有渔区的捕捞
usa_67  <- country_species(area67, "United States of America", rank_range = c(1, 10))
usa_all <- country_analysis("United States of America", rank_range = c(1, 10))
canada_67 <- country_species(area67, "Canada", rank_range = c(1, 10))
#突变点检测默认 h_value=0.15；如果没出图或突变点太多，就把h_value调大（如3、5）
#"... is not continuous"（数据年份不连续）和 "Error in processing ... minimum segment size"（年数太少，
#不做该国的突变点检测）只是提示，有图片出来就可以忽略



###============================================================================###
###3. 汇总分析：图在 figures/png/summary/，表格在 tables/
groups <- species_group_composition(area67)   #物种类群结构
status <- stock_status(area67)                #资源开发状态（仅基于产量，结果仅供参考）
pt <- peak_trend(area67)                      #峰值与近10年趋势；by = "country" 按国家
shifts <- regime_shift(area67)                #突变点同步性
overview <- area_overview()                   #对比全部海洋渔区
quality <- data_quality(area67)               #数据质量：官方数据与估算值、鉴定到种的比例
div <- diversity_concentration(area67)        #集中度与多样性
income <- country_group_summary(area67)       #按国家收入水平分组；group = "continent" 或 "region"
fleets <- coastal_distant(area67)             #沿岸国与远洋捕捞国



###============================================================================###
###4. 文献方法
shocks_country <- catch_shocks(area67)                                          #产量骤降（按国家）
shocks_species <- catch_shocks(area67, by = "species", label = "area67_species") #产量骤降（按物种）
set.seed(1)                                   #与随机序列的比较用到随机数
collapses <- collapse_analysis(area67)        #渔业崩溃
sync <- catch_synchrony(area67)               #同步性与组合效应
pd <- pelagic_demersal(area67)                #中上层鱼类与底层鱼类的比值（P/D）
pd$decades
japan_dw <- distant_water_profile("Japan")    #远洋捕捞国：日本在相邻渔区和远洋渔区的产量



###============================================================================###
###5. 物种特征与温度
#FishBase/SeaLifeBase 物种特征（营养级、偏好温度等）；version = "builtin" 用包内置的精简表，不联网
traits <- fishbase_match(area67, version = "builtin")
head(traits[, c("species_en", "catch_pct", "match_type", "trophic_level", "temp_pref_mean")], 8)

#海表温度（SST，默认用内置的 NOAA ERSST v5）和渔获平均温度（MTC）
sst67 <- fao_area_sst("Pacific, Northeast")
mtc <- mean_temperature_catch(area67, version = "builtin")
mtc$trend
#偏好温度是自动初配的结果，需要逐条核对：打开 tables/ 下 *_mtc_species.csv，核对后用 match = 重新计算
#mtc <- mean_temperature_catch(area67, match = "tables/Pacific__Northeast_mtc_species.csv")



###============================================================================###
###6. CMSY++ 资源评估（可选）：需要 JAGS，第一次运行 cmsy_setup() 安装
#CMSY++ 由 Froese et al. (2021, 2023) 开发，本包原样调用其代码（https://github.com/SISTA16/cmsyPlusPlus）
#cmsy_setup()                                            #只需一次：JAGS 和 R 包
#nep_cmsy <- cmsy_area("Pacific, Northeast", top = 10)   #十个主要物种
#nep_cmsy$summary                                        #B/Bmsy、F/Fmsy、MSY 和资源状态
#cmsy_mcmc(nep_cmsy)                                     #MCMC 设置、Rhat、有效样本量、各条链的轨迹与密度图
#默认设置的有效样本量常低于 400；用于报告的结果建议加长、加多链（约慢五倍）：
#nep_cmsy <- cmsy_area("Pacific, Northeast", top = 10,
#                      settings = list(n.chains = 4, n.iter = 300000, n.burnin = 150000, n.thin = 50))



###============================================================================###
###7. 一键生成评估报告（Word；PDF 需要 TinyTeX：install.packages("tinytex"); tinytex::install_tinytex()）
#报告图表默认 300 dpi，印刷用 dpi = 600
report <- fao_report(67, shocks = TRUE, mtc = TRUE)
report
#fao_report(67, lang = "zh", format = c("docx", "latex", "pdf"), shocks = TRUE, mtc = TRUE,
#           dpi = 600, cmsy = nep_cmsy)



###============================================================================###
###8. 输出的文件，以及保存结果下次继续
head(list.files("figures/png", recursive = TRUE), 20)
head(list.files("tables"), 10)
saveRDS(list(NEP = NEP, status = status, mtc = mtc), "area67_results.rds")
#results <- readRDS("area67_results.rds")
#FAO发布新版本时：运行 download_fao_data()，再运行同样的代码，所有图表和报告都会更新
