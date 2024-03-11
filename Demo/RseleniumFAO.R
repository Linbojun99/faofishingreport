library(RSelenium)
library(wdman)
library(rvest)

# 启动Selenium服务器和浏览器
# cd /Users/linbojun/Downloads
# java -jar selenium-server-standalone-4.0.0-alpha-2.jar
# 这里使用的是 Chrome，确保你的系统上安装了 Chrome 浏览器
# 也可以根据你的情况使用Firefox或其他浏览器
remDr <- remoteDriver(browserName = "firefox")
remDr$open()



# 导航到目标页面
remDr$navigate("https://www.fao.org/fishery/zh/countries")

# 可能需要一些时间等待页面加载
Sys.sleep(5)  # 这里的延迟根据网页的实际加载速度进行调整

# 提取页面的HTML内容
page_source_zh <- remDr$getPageSource()[[1]]

# 使用 rvest 解析HTML
page_zh <- read_html(page_source_zh)
table_zh <- html_table(html_nodes(page_zh, "table"), fill = TRUE)


# 检查提取的表格数据
print(table_zh[[1]])

table_zh<-table_zh[[1]]


remDr$navigate("https://www.fao.org/fishery/en/countries")
# 可能需要一些时间等待页面加载
Sys.sleep(5)  # 这里的延迟根据网页的实际加载速度进行调整

# 提取页面的HTML内容
page_source_en <- remDr$getPageSource()[[1]]

# 使用 rvest 解析HTML
page_en <- read_html(page_source_en)
table_en <- html_table(html_nodes(page_en, "table"), fill = TRUE)


# 检查提取的表格数据
print(table_en[[1]])

table_en<-table_en[[1]]
# 如果需要，可以将数据保存到CSV文件
# write.csv(table[[1]], "countries_data.csv", row.names = FALSE)


# 关闭Selenium服务
remDr$close()
#rD[["server"]]$stop()

table<-left_join(table_en,table_zh,by=c("ISO3","ISO2","UN","UNDP"))
colnames(table)[7]<-"Short name ZH"
colnames(table)[8]<-"Official name ZH"

name_mapping=table
#write.csv(name_mapping, "/Users/linbojun/Library/CloudStorage/OneDrive-共用文件庫－onedrive/Oceanic Fisheries Ecosystem Laboratory/FAO34/0306/countries_name_data.csv", row.names = FALSE)
