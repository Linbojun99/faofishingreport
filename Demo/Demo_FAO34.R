#Example:FAO Fishing Area 34  "Atlantic, Eastern Central"
#Data preparing and loading
library(faofishingreport)   #加载时会显示识别到的系统和图表字体（Windows 默认宋体 SimSun）

#环境目录设置：图表输出到工作目录下的 figures/，表格输出到 tables/
#setwd("你的工作目录")

#检查并下载FAO最新数据（第一次使用、或FAO发布新版本时运行一次即可）
#之后所有函数默认使用最新的数据；data_version = "bundled" 可改用包自带的数据
check_fao_update()
download_fao_data()

#图表语言：在这里改一次即可，"en" 英文，"zh" 中文，"both" 中英双语
my_lang <- "zh"

#绘图风格（可选，对之后所有图表生效；fao_settings() 查看当前设置，fao_settings(reset = TRUE) 恢复默认）
#fao_settings(theme = "classic", palette = "lancet")                                  #主题、分类色板
#fao_settings(map_projection = "robinson", map_center = 180, map_palette = "YlOrRd")  #地图投影、太平洋居中、地图色板

#默认只输出PNG图；需要可编辑的PPT图时，取消下面一行的注释（对所有函数生效），或在单个函数里加 pptx=TRUE
#fao_settings(pptx = TRUE)

#加载数据 FAO34渔区  Atlantic, Eastern Central
FAO34 <- fishing_area_data(area_name="Atlantic, Eastern Central", plot = TRUE,timeseries_analysis=TRUE,lang=my_lang)



###analyse species and country in Fishing area

###FAO34渔区的捕捞物种 fishing_area_species
FAO34_species<-fishing_area_species(FAO34[["area_data"]], c(1, 10), plot=TRUE,table=TRUE,timeseries_analysis=TRUE,lang=my_lang) #area_data=Fao34


###FAO34渔区的捕捞国家 fishing_area_country
FAO34_country<-fishing_area_country(FAO34[["area_data"]], c(1, 10), plot=TRUE,table=TRUE,timeseries_analysis=TRUE,lang=my_lang) #area_data=Fao34

###主要捕捞物种的捕捞国家 species_country
#注意：0.2.0 版起 species_country 的 h_value 参数真正生效（之前一直固定为 0.15），突变点结果会和旧图不同
#European pilchard(=Sardine)
sardine=species_country(FAO34[["area_data"]], species_name = "European pilchard(=Sardine)",c(1, 10), plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao34

#Jack and horse mackerels nei
Jack=species_country(FAO34[["area_data"]], species_name = "Jack and horse mackerels nei",c(1, 10), plot=TRUE,table=TRUE,h_value=5,lang=my_lang) #area_data=Fao34

#Round sardinella
Rsardine=species_country(FAO34[["area_data"]], species_name = "Round sardinella",c(1, 10), plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao34

#Atlantic chub mackerel
achubmackerel=species_country(FAO34[["area_data"]], species_name = "Atlantic chub mackerel",c(1, 10), plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao34


#Bonga shad
bongashad=species_country(FAO34[["area_data"]], species_name = "Bonga shad",c(1, 10), plot=TRUE,table=TRUE,h_value=5,lang=my_lang) #area_data=Fao34

#Skipjack tuna
skipjacktuna=species_country(FAO34[["area_data"]], species_name = "Skipjack tuna",c(1, 10), plot=TRUE,table=TRUE,h_value=5,lang=my_lang) #area_data=Fao34

#European anchovy
Europeananchovy=species_country(FAO34[["area_data"]], species_name = "European anchovy",c(1, 10), plot=TRUE,table=TRUE,h_value=5,lang=my_lang) #area_data=Fao34

#Yellowfin tuna
yellowfintuna=species_country(FAO34[["area_data"]], species_name = "Yellowfin tuna",c(1, 10), plot=TRUE,table=TRUE,h_value=3,lang=my_lang) #area_data=Fao34

#Bigeye tuna
Bigeyetuna=species_country(FAO34[["area_data"]], species_name = "Bigeye tuna",c(1, 10), plot=TRUE,table=TRUE,h_value=3,lang=my_lang) #area_data=Fao34

#Madeiran sardinella
Madeiransardinella=species_country(FAO34[["area_data"]], species_name = "Madeiran sardinella",c(1, 10), plot=TRUE,table=TRUE,h_value=3,lang=my_lang) #area_data=Fao34

###============================================================================###
###如果生成报错信息，但有图片出来 就忽略错误，如果没出图，就改h_value数值
#Warning messages:
 # 1: In FUN(X[[i]], ...) :
  #Data for Korea, Republic of is not continuous. Breakpoint analysis may be inaccurate.
#2: In FUN(X[[i]], ...) :
 # Data for Panama is not continuous. Breakpoint analysis may be inaccurate.
#3: In FUN(X[[i]], ...) :
 # Data for Other nei is not continuous. Breakpoint analysis may be inaccurate.
###============================================================================###


###主要捕捞国家的捕捞物种 country_species
#一开始都默认h_value=0.15，不行就改成3、5这样往上调
Morocco<-country_species(FAO34[["area_data"]], country_name = "Morocco",c(1, 10), plot=TRUE,table=TRUE,h_value=5,lang=my_lang)

#Senegal
Senegal<-country_species(FAO34[["area_data"]], country_name = "Senegal",c(1, 10), plot=TRUE,table=TRUE,h_value=5,lang=my_lang)


#如果觉得h_value=0.15、3都不OK，可以调成5，即让突变点检测的可视化结果美观一些，这些都是通过自己设置的值，没有一个标准，主观即可
Spain<-country_species(FAO34[["area_data"]], country_name = "Spain",c(1, 10), plot=TRUE,table=TRUE,h_value=5,lang=my_lang)


#Ghana
Ghana<-country_species(FAO34[["area_data"]], country_name = "Ghana",c(1, 10), plot=TRUE,table=TRUE,lang=my_lang)

#Nigeria
Nigeria<-country_species(FAO34[["area_data"]], country_name = "Nigeria",c(1, 10), plot=TRUE,table=TRUE,lang=my_lang)


#Mauritania
Mauritania<-country_species(FAO34[["area_data"]], country_name = "Mauritania",c(1, 10), plot=TRUE,table=TRUE,lang=my_lang)


#Sierra Leone
Sierraleone<-country_species(FAO34[["area_data"]], country_name = "Sierra Leone",c(1, 10), plot=TRUE,table=TRUE,lang=my_lang)


#Russian Federation
RussianFederation<-country_species(FAO34[["area_data"]], country_name = "Russian Federation",c(1, 10), plot=TRUE,h_value=5,table=TRUE,lang=my_lang)


#Cameroon
Cameroon<-country_species(FAO34[["area_data"]], country_name = "Cameroon",c(1, 10), plot=TRUE,table=TRUE,h_value=5,lang=my_lang)


#China
China<-country_species(FAO34[["area_data"]], country_name = "China",c(1, 10), plot=TRUE,table=TRUE,h_value=3,lang=my_lang,time_analysis_text_displacement = 6)


#Korea, Republic of
korea<-country_species(FAO34[["area_data"]], country_name = "Korea, Republic of",c(1, 10), plot=TRUE,table=TRUE,h_value=3,lang=my_lang,time_analysis_text_displacement = 6)



###============================================================================###
###汇总分析：图在 figures/png/summary/，表格在 tables/
#物种类群结构（ISSCAAP 类群；group = "major" 为鱼类、甲壳类、软体动物等大类）
species_group_composition(FAO34[["area_data"]], lang=my_lang)

#资源开发状态（资源状态图）：仅基于产量，结果仅供参考
stock_status(FAO34[["area_data"]], lang=my_lang)

#峰值与近10年趋势：物种、国家；area_overview() 对比全部海洋渔区
peak_trend(FAO34[["area_data"]], by = "species", lang=my_lang)
peak_trend(FAO34[["area_data"]], by = "country", lang=my_lang)
area_overview(lang=my_lang)

#数据质量：官方数据与估算值、鉴定到种的比例
data_quality(FAO34[["area_data"]], lang=my_lang)

#集中度与多样性
diversity_concentration(FAO34[["area_data"]], lang=my_lang)

#按国家分组：收入水平（"income"）、大洲（"continent"）、地区（"region"）
country_group_summary(FAO34[["area_data"]], group = "income", lang=my_lang)

#突变点同步性：主要物种在哪些年份同时发生突变
regime_shift(FAO34[["area_data"]], lang=my_lang)

#沿岸国与远洋捕捞国
coastal_distant(FAO34[["area_data"]], lang=my_lang)

#海表温度（SST）：各渔区的年平均、渔区内范围（格点10%-90%分位）和季节范围（最冷月、最暖月）
#可选的SST产品及其分辨率、年份、数据来源、许可和引用：
fao_sst_products()[, c("product", "name", "grid", "first_year", "bundled")]
#默认产品 NOAA ERSST v5 随包附带；其他产品需先下载并计算渔区平均（只需一次，HadISST1 约 240 MB）
#download_sst_data("hadisst1")
sst34 <- fao_area_sst("34")                          #默认 ERSST v5
#sst34_had <- fao_area_sst("34", product = "hadisst1")

#渔获平均温度（MTC）：各物种在 FishBase/SeaLifeBase 中的偏好温度按渔获量加权平均，与渔区海表温度对照
#第一次运行需要联网，会自动下载用到的 FishBase 和 SeaLifeBase 表格（约 23 MB），之后可离线使用
#只用种一级的条目（学名精确匹配、官方同物异名、经 WoRMS 有效名匹配、人工更正）；属、科等类群条目不参与计算
#注意：偏好温度是程序自动初配的结果，正误需要使用者逐条核对（见下）
mtc34 <- mean_temperature_catch(FAO34[["area_data"]], lang=my_lang)              #SST 默认用 ERSST v5
#mtc34 <- mean_temperature_catch(FAO34[["area_data"]], sst = "hadisst1", lang=my_lang)   #换用其他 SST 产品
mtc34$trend   #全时段和1970年以来的斜率（°C/十年）、Mann-Kendall检验、与SST的相关（含逐年差分）
#核对：打开 tables/ 下 *_mtc_species.csv，按渔获量从大到小逐条点 url、search_url、worms_url 链接，
#核对匹配的物种和偏好温度；核对过的在 checked 列写上核对人或日期，错的改 temp_pref_mean 并在 note 写明出处，
#然后用核对后的表重新计算（图中"尚未全部核对"的提示会在全部核对后消失）
#mtc34 <- mean_temperature_catch(FAO34[["area_data"]], match = "tables/Atlantic__Eastern_Central_mtc_species.csv", lang=my_lang)
head(mtc34$species[, c("species_name", "match_type", "temp_pref_mean", "catch_pct", "used", "checked")], 20)
#与 Cheung et al. (2013) 相同的时段 1970-2006
mtc34_1970_2006 <- mean_temperature_catch(subset(FAO34[["area_data"]], Year >= 1970 & Year <= 2006),
                                          label = "FAO34_1970_2006", lang=my_lang)


####major country：国家在全球所有渔区的捕捞情况
Morocco_analysis<-country_analysis(country_name = "Morocco",c(1, 10), plot=TRUE,table=TRUE,h_value=5,lang=my_lang)

Spain_analysis<-country_analysis(country_name = "Spain",c(1, 10), plot=TRUE,table=TRUE,lang=my_lang)

Mauritania_analysis<-country_analysis(country_name = "Mauritania",c(1, 10), plot=TRUE,table=TRUE,lang=my_lang)
