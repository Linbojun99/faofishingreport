#Example:FAO Fishing Area 34  "Atlantic, Eastern Central"
#Data preparing and loading
library(faofishingreport)
library(rnaturalearth)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
#环境目录设置
#setwd("/Users/linbojun/Library/CloudStorage/OneDrive-共用文件庫－onedrive/Oceanic Fisheries Ecosystem Laboratory/FAO34/38")
setwd()#工作空间

#加载数据 FAO34渔区  Atlantic, Eastern Central
FAO34 <- fishing_area_data(area_name="Atlantic, Eastern Central", plot = T,timeseries_analysis=TRUE,ZH=TRUE)



###analyse species and country in Fishing area

###FAO34渔区的捕捞物种 fishing_area_species
FAO34_species<-fishing_area_species(FAO34[["area_data"]], c(1, 10), plot=TRUE,table=TRUE,timeseries_analysis=TRUE) #area_data=Fao34


###FAO34渔区的捕捞国家 fishing_area_country
FAO34_country<-fishing_area_country(FAO34[["area_data"]], c(1, 10), plot=TRUE,table=TRUE,timeseries_analysis=TRUE) #area_data=Fao34

###主要捕捞物种的捕捞国家 species_country
#European pilchard(=Sardine)
sardine=species_country(FAO34[["area_data"]], species_name = "European pilchard(=Sardine)",c(1, 10), plot=TRUE,table=TRUE,ZH=TRUE) #area_data=Fao34

#Jack and horse mackerels nei
Jack=species_country(FAO34[["area_data"]], species_name = "Jack and horse mackerels nei",c(1, 10), plot=TRUE,table=TRUE,h_value=5,ZH=TRUE) #area_data=Fao34

#Round sardinella
Rsardine=species_country(FAO34[["area_data"]], species_name = "Round sardinella",c(1, 10), plot=TRUE,table=TRUE,ZH=TRUE) #area_data=Fao34

#Atlantic chub mackerel
achubmackerel=species_country(FAO34[["area_data"]], species_name = "Atlantic chub mackerel",c(1, 10), plot=TRUE,table=TRUE,ZH=TRUE) #area_data=Fao34


#Bonga shad
bongashad=species_country(FAO34[["area_data"]], species_name = "Bonga shad",c(1, 10), plot=TRUE,table=TRUE,h_value=5,ZH=TRUE) #area_data=Fao34

#Skipjack tuna
skipjacktuna=species_country(FAO34[["area_data"]], species_name = "Skipjack tuna",c(1, 10), plot=TRUE,table=TRUE,h_value=5,ZH=TRUE) #area_data=Fao34

#European anchovy
Europeananchovy=species_country(FAO34[["area_data"]], species_name = "European anchovy",c(1, 10), plot=TRUE,table=TRUE,h_value=5,ZH=TRUE) #area_data=Fao34

#Yellowfin tuna
yellowfintuna=species_country(FAO34[["area_data"]], species_name = "Yellowfin tuna",c(1, 10), plot=TRUE,table=TRUE,h_value=3,ZH=TRUE) #area_data=Fao34

#Bigeye tuna
Bigeyetuna=species_country(FAO34[["area_data"]], species_name = "Bigeye tuna",c(1, 10), plot=TRUE,table=TRUE,h_value=3,ZH=TRUE) #area_data=Fao34

#Madeiran sardinella
Madeiransardinella=species_country(FAO34[["area_data"]], species_name = "Madeiran sardinella",c(1, 10), plot=TRUE,table=TRUE,h_value=3,ZH=TRUE) #area_data=Fao34

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
Morocco<-country_species(FAO34[["area_data"]], country_name = "Morocco",c(1, 10), plot=TRUE,table=TRUE,h_value=5,ZH=T)

#Senegal
Senegal<-country_species(FAO34[["area_data"]], country_name = "Senegal",c(1, 10), plot=TRUE,table=TRUE,h_value=5,ZH=T)


#如果觉得h_value=NULL，3都不OK，可以调成5，即让突变点检测的可视化结果美观一些，这些都是通过自己设置的值，没有一个标准，主观即可
Spain<-country_species(FAO34[["area_data"]], country_name = "Spain",c(1, 10), plot=TRUE,table=TRUE,h_value=5,ZH=T)


#Ghana
Ghana<-country_species(FAO34[["area_data"]], country_name = "Ghana",c(1, 10), plot=TRUE,table=TRUE,ZH=T)

#Nigeria
Nigeria<-country_species(FAO34[["area_data"]], country_name = "Nigeria",c(1, 10), plot=TRUE,table=TRUE,ZH=T)


#
Mauritania<-country_species(FAO34[["area_data"]], country_name = "Mauritania",c(1, 10), plot=TRUE,table=TRUE,ZH=T)


#Sierra Leone
Sierraleone<-country_species(FAO34[["area_data"]], country_name = "Sierra Leone",c(1, 10), plot=TRUE,table=TRUE,ZH=T)


#Russian Federation
RussianFederation<-country_species(FAO34[["area_data"]], country_name = "Russian Federation",c(1, 10), plot=TRUE,h_value=5,table=TRUE,ZH=T)






#Cameroon
Cameroon<-country_species(FAO34[["area_data"]], country_name = "Cameroon",c(1, 10), plot=TRUE,table=TRUE,h_value=5,ZH=T)




#
China<-country_species(FAO34[["area_data"]], country_name = "China",c(1, 10), plot=TRUE,table=TRUE,h_value=3,ZH=T,time_analysis_text_displacement = 6)



#
korea<-country_species(FAO34[["area_data"]], country_name = "Korea, Republic of",c(1, 10), plot=TRUE,table=TRUE,h_value=3,ZH=T,time_analysis_text_displacement = 6)





####major country
Morocco_analysis<-country_analysis(country_name = "Morocco",c(1, 10), plot=TRUE,table=TRUE,h_value=5,ZH=T)

Spain_analysis<-country_analysis(country_name = "Spain",c(1, 10), plot=TRUE,table=TRUE,ZH=T)

Mauritania_analysis<-country_analysis(country_name = "Mauritania",c(1, 10), plot=TRUE,table=TRUE,ZH=T)
