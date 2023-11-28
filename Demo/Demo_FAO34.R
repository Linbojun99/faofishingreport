#Example:FAO Fishing Area 34  "Atlantic, Eastern Central"
#Data preparing and loading
library(faofishingreport)

#环境目录设置
#setwd("/Users/linbojun/Library/CloudStorage/OneDrive-共用文件庫－onedrive/Oceanic Fisheries Ecosystem Laboratory/FAO34/中期")
setwd()#工作空间

#加载数据 FAO34渔区  Atlantic, Eastern Central
FAO34 <- fishing_area_data(area_name="Atlantic, Eastern Central", plot = TRUE,timeseries_analysis=TRUE)



###analyse species and country in Fishing area

###FAO34渔区的捕捞物种 fishing_area_species
FAO34_species<-fishing_area_species(FAO34[["area_data"]], c(1, 10), plot=TRUE,table=TRUE,timeseries_analysis=TRUE) #area_data=Fao34


###FAO34渔区的捕捞国家 fishing_area_country
FAO34_country<-fishing_area_country(FAO34[["area_data"]], c(1, 10), plot=TRUE,table=TRUE,timeseries_analysis=TRUE) #area_data=Fao34

###主要捕捞物种的捕捞国家 species_country
sardine=species_country(FAO34[["area_data"]], species_name = "European pilchard(=Sardine)",c(1, 10), plot=TRUE,table=TRUE) #area_data=Fao34

#
bongashad=species_country(FAO34[["area_data"]], species_name = "Bonga shad",c(1, 10), plot=TRUE,table=TRUE,h_value=5) #area_data=Fao34

#
yellowfintuna=species_country(FAO34[["area_data"]], species_name = "Yellowfin tuna",c(1, 10), plot=TRUE,table=TRUE,h_value=3) #area_data=Fao34

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
#一开始都默认h_value=NULL. (默认0.15)，不行就改成3、5这样往上调
Morocco<-country_species(FAO34[["area_data"]], country_name = "Morocco",c(1, 10), plot=TRUE,table=TRUE,h_value=3)

#如果觉得h_value=NULL，3都不OK，可以调成5，即让突变点检测的可视化结果美观一些，这些都是通过自己设置的值，没有一个标准，主观即可
Spain<-country_species(FAO34[["area_data"]], country_name = "Spain",c(1, 10), plot=TRUE,table=TRUE,h_value=5)


#
China<-country_species(FAO34[["area_data"]], country_name = "China",c(1, 10), plot=TRUE,table=TRUE,h_value=3)
