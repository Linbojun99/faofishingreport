#Example:FAO Fishing Area 27  "Atlantic, Northeast"
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
#fao_settings(map_projection = "robinson", map_center = 0, map_palette = "YlOrRd")    #地图投影、大西洋居中、地图色板

#图片分辨率：默认 600 dpi；屏幕浏览或文件太大时可改为 300
#fao_settings(dpi = 300)

#默认只输出PNG图；需要可编辑的PPT图时，取消下面一行的注释（对所有函数生效），或在单个函数里加 pptx=TRUE
#fao_settings(pptx = TRUE)

#每个函数运行结束会在控制台显示结果摘要（主要结果表、保存的图表和表格）；结果需要赋值保存，如 x <- stock_status(...)
#fao_settings(show_data = FALSE)    #不显示摘要
#保存图表的同时在 RStudio 的 Plots 窗格显示：在单个函数里加 plot_show = TRUE，或对所有函数生效：
#fao_settings(plot_show = TRUE)

#rank_range 列出要显示的名次：1:10 为前十名，11:20 为其后十名，c(5, 6) 为第五和第六名



###============================================================================###
###全球渔区与27渔区地图
#全部FAO主要渔区：代码、名称、所在大洋、最近一年的产量和FAO官方地图链接，并画世界地图
areas <- fao_areas(lang=my_lang)

#27渔区（即ICES海区，27.1巴伦支海 至 27.14东格陵兰）及其分区；装了 ggOceanMaps 时带海底地形
#level = "division" 或 "subdivision" 画更细的分区；engine = "sf" 不用 ggOceanMaps
FAO27_map <- fishing_area_map(27, level = "subarea", lang=my_lang)



###============================================================================###
#加载数据 FAO27渔区  Atlantic, Northeast
FAO27 <- fishing_area_data(area_name="Atlantic, Northeast", plot = TRUE,timeseries_analysis=TRUE,lang=my_lang)



###analyse species and country in Fishing area

###FAO27渔区的捕捞物种 fishing_area_species
FAO27_species<-fishing_area_species(FAO27[["area_data"]], 1:10, plot=TRUE,table=TRUE,timeseries_analysis=TRUE,lang=my_lang) #area_data=Fao27


###FAO27渔区的捕捞国家 fishing_area_country
FAO27_country<-fishing_area_country(FAO27[["area_data"]], 1:10, plot=TRUE,table=TRUE,timeseries_analysis=TRUE,lang=my_lang) #area_data=Fao27

#物种和国家的中文名：fao_translate("Atlantic cod") 查询；查不到对应中文名的保留英文名
#unique(FAO27[["area_data"]]$ASFIS.species..Name.)   #列出全部物种名

###主要捕捞物种的捕捞国家 species_country
#Atlantic herring
herring=species_country(FAO27[["area_data"]], species_name = "Atlantic herring",1:10, plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao27

#Blue whiting(=Poutassou)
bluewhiting=species_country(FAO27[["area_data"]], species_name = "Blue whiting(=Poutassou)",1:10, plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao27

#Atlantic mackerel
mackerel=species_country(FAO27[["area_data"]], species_name = "Atlantic mackerel",1:10, plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao27

#Atlantic cod
cod=species_country(FAO27[["area_data"]], species_name = "Atlantic cod",1:10, plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao27

#European sprat
sprat=species_country(FAO27[["area_data"]], species_name = "European sprat",1:10, plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao27

#Capelin
capelin=species_country(FAO27[["area_data"]], species_name = "Capelin",1:10, plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao27

#Saithe(=Pollock)
saithe=species_country(FAO27[["area_data"]], species_name = "Saithe(=Pollock)",1:10, plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao27

#Haddock
haddock=species_country(FAO27[["area_data"]], species_name = "Haddock",1:10, plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao27

#European hake
hake=species_country(FAO27[["area_data"]], species_name = "European hake",1:10, plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao27

#Atlantic horse mackerel
horsemackerel=species_country(FAO27[["area_data"]], species_name = "Atlantic horse mackerel",1:10, plot=TRUE,table=TRUE,lang=my_lang) #area_data=Fao27

###============================================================================###
###突变点检测默认 h_value=0.15；如果没出图或突变点太多，就把h_value调大（如3、5）
###运行时出现下面两类提示但有图片出来，就可以忽略：
#Data for ... is not continuous. Breakpoint analysis may be inaccurate.
#  （该国的数据年份不连续，突变点结果可能不准）
#Error in processing ... : minimum segment size must be greater than the number of regressors
#  （该国的数据年数太少，不做突变点检测，其余结果和图照常输出）
###============================================================================###


###主要捕捞国家的捕捞物种 country_species
#Norway
Norway<-country_species(FAO27[["area_data"]], country_name = "Norway",1:10, plot=TRUE,table=TRUE,lang=my_lang)

#Iceland
Iceland<-country_species(FAO27[["area_data"]], country_name = "Iceland",1:10, plot=TRUE,table=TRUE,lang=my_lang)

#Russian Federation
RussianFederation<-country_species(FAO27[["area_data"]], country_name = "Russian Federation",1:10, plot=TRUE,table=TRUE,lang=my_lang)

#United Kingdom
UK<-country_species(FAO27[["area_data"]], country_name = "United Kingdom",1:10, plot=TRUE,table=TRUE,lang=my_lang)

#Denmark
Denmark<-country_species(FAO27[["area_data"]], country_name = "Denmark",1:10, plot=TRUE,table=TRUE,lang=my_lang)

#Faroe Islands
Faroe<-country_species(FAO27[["area_data"]], country_name = "Faroe Islands",1:10, plot=TRUE,table=TRUE,lang=my_lang)

#France
France<-country_species(FAO27[["area_data"]], country_name = "France",1:10, plot=TRUE,table=TRUE,lang=my_lang)

#Netherlands (Kingdom of the)
Netherlands<-country_species(FAO27[["area_data"]], country_name = "Netherlands (Kingdom of the)",1:10, plot=TRUE,table=TRUE,lang=my_lang)

#Spain
Spain<-country_species(FAO27[["area_data"]], country_name = "Spain",1:10, plot=TRUE,table=TRUE,lang=my_lang)

#Ireland
Ireland<-country_species(FAO27[["area_data"]], country_name = "Ireland",1:10, plot=TRUE,table=TRUE,lang=my_lang)



###============================================================================###
###汇总分析：图在 figures/png/summary/，表格在 tables/
#物种类群结构（ISSCAAP 类群；group = "major" 为鱼类、甲壳类、软体动物等大类）
species_group_composition(FAO27[["area_data"]], lang=my_lang)

#资源开发状态（资源状态图）：仅基于产量，结果仅供参考
stock_status(FAO27[["area_data"]], lang=my_lang)

#峰值与近10年趋势：物种、国家；area_overview() 对比全部海洋渔区
peak_trend(FAO27[["area_data"]], by = "species", lang=my_lang)
peak_trend(FAO27[["area_data"]], by = "country", lang=my_lang)
area_overview(lang=my_lang)

#数据质量：官方数据与估算值、鉴定到种的比例
data_quality(FAO27[["area_data"]], lang=my_lang)

#集中度与多样性
diversity_concentration(FAO27[["area_data"]], lang=my_lang)

#按国家分组：收入水平（"income"）、大洲（"continent"）、地区（"region"）
country_group_summary(FAO27[["area_data"]], group = "income", lang=my_lang)

#突变点同步性：主要物种在哪些年份同时发生突变
regime_shift(FAO27[["area_data"]], lang=my_lang)

#沿岸国与远洋捕捞国
coastal_distant(FAO27[["area_data"]], lang=my_lang)

#海表温度（SST）：各渔区的年平均、渔区内范围（格点10%-90%分位）和季节范围（最冷月、最暖月）
#包里内置的是 NOAA ERSST v5（2°网格，1950年起，仅现场观测），运行时会打印所用的SST数据
sst27 <- fao_area_sst("27")
#用自己的SST数据：给出 NetCDF 文件（或文件夹）的路径；第一次运行时计算渔区平均并保存，之后直接读取
#sst27_own <- fao_area_sst("27", sst = "D:/data/HadISST_sst.nc")
#fao_settings(sst = "D:/data/HadISST_sst.nc")       #本次会话的所有函数都用这个文件
#也可以用包下载常用产品（分辨率、年份、来源、许可和引用见 fao_sst_products()）
#fao_sst_products()[, c("product", "name", "grid", "first_year", "built_in")]
#download_sst_data("hadisst1")                       #约 240 MB，只需一次；之后 sst = "hadisst1"

#渔获平均温度（MTC）：各物种在 FishBase/SeaLifeBase 中的偏好温度按渔获量加权平均，与渔区海表温度对照
#第一次运行需要联网，会自动下载用到的 FishBase 和 SeaLifeBase 表格（约 23 MB），之后可离线使用
#只用种一级的条目（学名精确匹配、官方同物异名、经 WoRMS 有效名匹配、人工更正）；属、科等类群条目不参与计算
#注意：偏好温度是程序自动初配的结果，正误需要使用者逐条核对（见下）
mtc27 <- mean_temperature_catch(FAO27[["area_data"]], lang=my_lang)              #SST 默认用内置的 ERSST v5
#mtc27 <- mean_temperature_catch(FAO27[["area_data"]], sst = "D:/data/HadISST_sst.nc", lang=my_lang)   #换用自己的 SST 文件
mtc27$trend   #全时段和1970年以来的斜率（°C/十年）、Mann-Kendall检验、与SST的相关（含逐年差分）
#核对：打开 tables/ 下 *_mtc_species.csv，按渔获量从大到小逐条点 url、search_url、worms_url 链接，
#核对匹配的物种和偏好温度；核对过的在 checked 列写上核对人或日期，错的改 temp_pref_mean 并在 note 写明出处，
#然后用核对后的表重新计算（图中"尚未全部核对"的提示会在全部核对后消失）
#mtc27 <- mean_temperature_catch(FAO27[["area_data"]], match = "tables/Atlantic__Northeast_mtc_species.csv", lang=my_lang)
head(mtc27$species[, c("species_name", "match_type", "temp_pref_mean", "catch_pct", "used", "checked")], 20)
#与 Cheung et al. (2013) 相同的时段 1970-2006
mtc27_1970_2006 <- mean_temperature_catch(subset(FAO27[["area_data"]], Year >= 1970 & Year <= 2006),
                                          label = "FAO27_1970_2006", lang=my_lang)



####major country：国家在全球所有渔区的捕捞情况
Norway_analysis<-country_analysis(country_name = "Norway",1:10, plot=TRUE,table=TRUE,lang=my_lang)

Iceland_analysis<-country_analysis(country_name = "Iceland",1:10, plot=TRUE,table=TRUE,lang=my_lang)

RussianFederation_analysis<-country_analysis(country_name = "Russian Federation",1:10, plot=TRUE,table=TRUE,lang=my_lang)



###============================================================================###
###CMSY++ 资源评估（可选）：需要 JAGS，第一次运行 cmsy_setup() 安装
#CMSY++ 由 Froese et al. (2021, 2023) 开发，本包原样调用其代码（https://github.com/SISTA16/cmsyPlusPlus）
#cmsy_setup()
#cmsy_system()                                        #查看操作系统、JAGS 和 R 包

#27渔区的主要物种，一步完成（图表和表格在 cmsy_areas/Atlantic__Northeast/）
#FAO27_cmsy <- cmsy_area("Atlantic, Northeast", top = 10, lang=my_lang)
#FAO27_cmsy$summary                                   #B/Bmsy、F/Fmsy、MSY 和资源状态

#MCMC 设置与收敛诊断：每个种群的链数、迭代、预烧、抽稀，Rhat 和有效样本量，各条链的轨迹与密度图
#cmsy_mcmc(FAO27_cmsy)
#cmsy_mcmc(FAO27_cmsy, colors = c("grey30", "#D7191C"))   #改链的颜色
#默认设置（2 条链，60,000 次迭代）的有效样本量常低于 400；用于报告的结果建议加长、加多链（约慢五倍）：
#FAO27_cmsy <- cmsy_area("Atlantic, Northeast", top = 10, lang=my_lang,
#                        settings = list(n.chains = 4, n.iter = 300000, n.burnin = 150000, n.thin = 50))



###============================================================================###
###一键生成27渔区的评估报告（Word；format = c("docx", "pdf") 同时生成 PDF，需要 TinyTeX）
#报告图表默认 300 dpi，印刷用 dpi = 600
FAO27_report <- fao_report(27, lang = my_lang, shocks = TRUE, mtc = TRUE)
#FAO27_report <- fao_report(27, lang = my_lang, shocks = TRUE, mtc = TRUE, dpi = 600,
#                           format = c("docx", "pdf"), cmsy = FAO27_cmsy)   #加入上面的 CMSY++ 结果
FAO27_report
