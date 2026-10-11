# 📝 一键生成评估报告（Word、LaTeX、PDF）

``` r

fao_settings(lang = "zh")   # 本文所有图表使用中文
```

📦
**数据来源**：[FAO全球捕捞产量](https://www.fao.org/fishery/collection/capture/en)（Global
Capture
Production）**2026.1.0**版（FAO于2026-03-30发布，1950–2024年），下载地址[Capture_2026.1.0.zip](https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip)，许可证[CC
BY
4.0](https://creativecommons.org/licenses/by/4.0/)。用[`fao_data_info()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_info.md)查看你使用的版本和引用格式。

[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)一次调用即可为一个FAO主要渔区生成中文或英文的Word评估报告。它对该渔区运行本包的各项分析，把结果填入文字模板，插入带编号题注的图表，并列出正文引用的参考文献。

``` r

fao_report(34)                                                  # 语言取自fao_settings()
fao_report("Atlantic, Eastern Central", lang = "zh", author = "Hongyu Lin")
fao_report(34, lang = "en", font = "Times New Roman", citation = "fao")
```

生成的报告是初稿。只有作者才能写的部分，如其他数据来源、结论的讨论和其他参考文献，以灰色“【待补充：……】”标出（英文报告中为“To
be completed”）。使用前请核对自动生成的文字。

## 🚀 第一份报告

``` r

report <- fao_report(34, file = "FAO34_report_zh.docx", lang = "zh")
report
#> FAO report: /tmp/Rtmprkl2xQ/faofishingreport_site_1bf3818daf3bc/FAO34_report_zh.docx
#> Sections: total, species, countries, groups, status, quality, diversity, fleets
#>   $main_species           main species of the last five years (10 rows)
#>   $main_countries         main fishing countries of the last five years (10 rows)
#>   $analyses               results of the analyses: total, species, countries, groups, status, quality, diversity, fleets
```

渔区可以用代码（`34`）或英文名称（`"Atlantic, Eastern Central"`）指定。运行约需半分钟，期间以消息显示进度（本页未显示）：先是各项分析，然后是写入的文件。不指定`file`时，报告保存为工作目录下的`reports/FAO34_report_zh.docx`；本页的工作目录是一个临时文件夹。

各项分析在另一个临时文件夹中运行，图表分辨率为300
dpi，不生成PowerPoint文件。图表只插入Word文件，不会在你的文件夹中留下图表文件；如需保留图表，请直接运行相应的分析函数（见[🌊
渔区分析](https://linbojun99.github.io/faofishingreport/articles/zh/fishing-areas.md)）。

[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)以不可见的方式返回报告中的表格，因此请把结果赋给一个变量。近5年的主要渔获种类：

``` r

show_table(report$main_species, digits = 1)
```

| species | species_en | scientific_name | mean_catch_t | share_pct | peak_year | pct_of_peak | trend_10_years |
|:---|:---|:---|---:|---:|:---|---:|:---|
| 沙丁鱼 | European pilchard(=Sardine) | Sardina pilchardus | 1,041,854.9 | 20.4 | 2018 | 52.6 | none |
| 科利鲭 | Atlantic chub mackerel | Scomber colias | 414,307.8 | 8.1 | 2019 | 96.6 | up |
| 筛鲱 | Bonga shad | Ethmalosa fimbriata | 348,662.7 | 6.8 | 2017 | 87.1 | none |
| 竹筴鱼属物种 | Jack and horse mackerels nei | Trachurus spp. | 263,295.6 | 5.1 | 2010 | 58.7 | none |
| 未分类的沙丁鱼 | Sardinellas nei | Sardinella spp. | 246,900.5 | 4.8 | 2018 | 32.8 | down |
| 未分类的海洋鱼类 | Marine fishes nei | Actinopterygii | 243,873.7 | 4.8 | 1995 | 60.5 | none |
| 短体小沙丁鱼 | Madeiran sardinella | Sardinella maderensis | 243,476.6 | 4.8 | 2021 | 73.4 | none |
| 鲣 | Skipjack tuna | Katsuwonus pelamis | 195,951.7 | 3.8 | 2018 | 69.2 | none |
| 金色小沙丁鱼 | Round sardinella | Sardinella aurita | 127,328.2 | 2.5 | 1991 | 34.9 | down |
| 欧洲鳀 | European anchovy | Engraulis encrasicolus | 124,334.4 | 2.4 | 1990 | 48.9 | up |

## 📑 报告包含哪些内容

报告依次包括：

1.  **封面**，单独一页，不编页码：项目名称（未指定`project`时为“课题报告”）、标题、报告编号、作者、单位（仅在指定时显示）和日期。
2.  **标题部分**：“报告一”（中文报告用中文数字，英文报告为“Report
    1”）、标题和作者，居中。
3.  **摘要**：一段文字，包括渔区、FAO数据的年份和版本、所开启章节的分析内容和方法，以及总渔获量、主要种类、主要国家、资源开发状态和CMSY++（开启这些章节时）各一句结果，最后是一句建议。
4.  **1 数据来源与分析方法**
    - **1.1
      数据来源**：所用的FAO全球捕捞产量统计版本、FAO发布该版本的日期、下载日期和文件链接；数据年份，以及该渔区的国家和物种条目数；ASFIS水产物种名录、FAO渔业和水产养殖统计年鉴，以及沿岸国的确定方法。一条灰色备注提示补充其他数据来源（区域渔业管理组织、国家渔业管理和统计机构、Sea
      Around Us等）。
    - **1.2
      分析方法**：所开启章节用到的每种方法各一段（突变点检测、Mann-Kendall趋势检验、资源开发状态图、多样性指数等），附参考文献；最后一段说明所用软件（R和faofishingreport）。
5.  **2
    结果与结论**：每个开启的章节一个编号小节（2.1、2.2……），包括文字、图和表。最后一个小节**结论**按章节逐条列出编号结论（1）、（2）……，并附一条灰色备注，提示结合资源评估结果、相关文献和管理措施加以完善。
6.  **3
    主要渔获种类资源评估（CMSY++）**，仅在`cmsy = TRUE`时出现：评估概况、每个物种一个小节和小结（见[📉
    CMSY++资源评估](#cmsy)）。
7.  **参考文献**：正文引用的文献，然后是一条说明如何补充其他文献的灰色备注。
8.  **附件**：主要渔获种类（附表1）和主要捕捞国家（地区）（附表2）各年代的年均渔获量；有CMSY++时，还有参考点、先验参数及其来源（附表3至附表5）。

灰色备注是作者需要撰写或核对的段落：其他数据来源、结论、其他参考文献、每个CMSY++评估与其他评估结果的比较，以及渔获平均温度所用物种的偏好温度。如果某张图未能生成，报告中会在相应位置显示“【图表未生成】”。

## 🔎 查看Word文件的内容

报告中的每个段落都有一个命名的Word样式（“FAO Body”、“FAO Heading
1”等）。在Word中修改某个样式，即可统一修改整份报告的格式；也可以用officer包在R中读回文件内容：

``` r

word <- officer::docx_summary(officer::read_docx(report$file))
word <- word[word$content_type == "paragraph", ]
table(word$style_name)
#> 
#> FAO Abstract Heading           FAO Author             FAO Body 
#>                    1                    3                   30 
#>          FAO Caption    FAO Cover Project       FAO Cover Text 
#>                   14                    1                   13 
#>      FAO Cover Title           FAO Figure        FAO Heading 1 
#>                    1                   14                    4 
#>        FAO Heading 2             FAO Note        FAO Reference 
#>                   11                    3                   11 
#>    FAO Table Caption       FAO Table Text            FAO Title 
#>                    4                    4                    2
```

各级标题，即样式为“FAO Heading 1”和“FAO Heading 2”的段落：

``` r

outline <- function(word) {
  headings <- word[word$style_name %in% c("FAO Heading 1", "FAO Heading 2"), ]
  cat(paste0(ifelse(headings$style_name == "FAO Heading 2", "    ", ""), headings$text), sep = "\n")
}
outline(word)
#> 1 数据来源与分析方法
#>     1.1 数据来源
#>     1.2 分析方法
#> 2 结果与结论
#>     2.1 渔获量变化与阶段划分
#>     2.2 主要渔获种类
#>     2.3 主要捕捞国家（地区）
#>     2.4 渔获类群组成
#>     2.5 资源开发状态
#>     2.6 数据质量
#>     2.7 渔获多样性与集中度
#>     2.8 沿岸国与远洋捕捞
#>     2.9 结论
#> 参考文献
#> 附件
```

摘要，即“摘　要”标题之后的第一个“FAO Body”段落：

``` r

after_heading <- seq_len(nrow(word)) > which(word$style_name == "FAO Abstract Heading")
abstract <- word$text[after_heading & word$style_name == "FAO Body"][1]
show_table(data.frame(abstract))
```

| abstract |
|:---|
| FAO 34渔区（中东大西洋）是全球重要的海洋捕捞海域之一。本报告根据联合国粮食及农业组织（FAO）全球捕捞产量统计1950–2024年数据（2026.1.0版），采用突变点检测、Mann-Kendall趋势检验、资源开发状态图和多样性与集中度指数，分析了该海域渔获量的变化、主要渔获种类、主要捕捞国家（地区）、渔获类群组成、资源开发状态、数据质量、渔获多样性和沿岸国与远洋捕捞。结果表明，2024年该海域渔获量为509.9万t，占全球海洋捕捞产量的6.3%；历史最高渔获量出现在2018年（552.0万t），2024年渔获量为峰值的92.4%。渔获量变化可划分为4个阶段（1950–1968年、1969–1987年、1988–2009年和2010–2024年）。近5年沙丁鱼是最主要的渔获种类，占总渔获量的20.4%。摩洛哥是最主要的捕捞国家（地区），占近5年渔获量的27.7%。2024年处于过度开发或崩溃状态的资源占36.1%。建议加强主要渔获种类的资源监测与评估，提高渔获统计的物种分辨率。 |

图表题注，按报告编号：

``` r

captions <- word[word$style_name %in% c("FAO Caption", "FAO Table Caption"), ]
show_table(data.frame(style = captions$style_name, caption = captions$text), n = 20)
```

| style | caption |
|:---|:---|
| FAO Caption | 图1-1 1950–2024年FAO 34渔区（中东大西洋）渔获量变化及突变点 |
| FAO Caption | 图1-2 FAO 34渔区（中东大西洋）渔获量占全球海洋捕捞产量的比例 |
| FAO Table Caption | 表1-1 近5年（2020–2024年）FAO 34渔区（中东大西洋）主要渔获种类 |
| FAO Caption | 图1-3 FAO 34渔区（中东大西洋）主要渔获种类的渔获量变化 |
| FAO Caption | 图1-4 FAO 34渔区（中东大西洋）主要渔获种类的渔获量变化及突变点 |
| FAO Table Caption | 表1-2 近5年（2020–2024年）FAO 34渔区（中东大西洋）主要捕捞国家（地区） |
| FAO Caption | 图1-5 FAO 34渔区（中东大西洋）主要捕捞国家（地区）的渔获量变化 |
| FAO Caption | 图1-6 各国家和地区在FAO 34渔区（中东大西洋）的累计渔获量 |
| FAO Caption | 图1-7 FAO 34渔区（中东大西洋）主要捕捞国家（地区）的渔获量变化及突变点 |
| FAO Caption | 图1-8 FAO 34渔区（中东大西洋）各ISSCAAP类群的渔获量占比 |
| FAO Caption | 图1-9 FAO 34渔区（中东大西洋）资源开发状态（按资源数量） |
| FAO Caption | 图1-10 FAO 34渔区（中东大西洋）资源开发状态（按渔获量） |
| FAO Caption | 图1-11 FAO 34渔区（中东大西洋）渔获量的数据来源构成 |
| FAO Caption | 图1-12 FAO 34渔区（中东大西洋）渔获量的分类阶元构成 |
| FAO Caption | 图1-13 FAO 34渔区（中东大西洋）渔获的多样性与集中度指数 |
| FAO Caption | 图1-14 FAO 34渔区（中东大西洋）沿岸国与远洋捕捞国家和地区的渔获量 |
| FAO Table Caption | 附表1 FAO 34渔区（中东大西洋）主要渔获种类各年代的年均渔获量（万t） |
| FAO Table Caption | 附表2 FAO 34渔区（中东大西洋）主要捕捞国家（地区）各年代的年均渔获量（万t） |

灰色备注：

``` r

show_table(data.frame(note = word$text[word$style_name == "FAO Note"]))
```

| note |
|:---|
| 【待补充：如使用区域渔业管理组织（如ICCAT、CECAF、WCPFC）、国家渔业管理和统计机构（如NOAA）、Sea Around Us、FishBase、Global Fishing Watch等渔业统计或产业信息网站，或远洋渔业数据、商业数据及论文报告等资料，请在此说明数据来源、时间范围和处理方法。】 |
| 【待补充：以上结论根据FAO统计数据自动生成，请结合区域渔业管理组织的资源评估结果、相关文献和渔业管理措施进一步分析和完善。】 |
| 【待补充：其他参考文献按正文中引用的先后顺序排列，格式如下。期刊：\[序号\] 作者. 题目\[J\]. 刊名，出版年份，卷号(期号)：起止页码. 专著：\[序号\] 作者. 书名\[M\]，版本（第一版不标注）. 出版地：出版社，出版年：起止页码. 论文集：\[序号\] 作者，题名：论文集名\[C\]. 出版地：出版社，出版年，起止页码. 学位论文：\[序号\] 作者. 题名\[D\]. 保存地点：保存单位，年份. 作者不超过三位时全部列出，超过三位时只列前三位，其后加“等”或“et al.”；作者姓名之间用逗号分开，姓在前、名在后。】 |

以及参考文献列表，样式为“FAO Reference”：

``` r

show_table(data.frame(reference = word$text[word$style_name == "FAO Reference"]), n = 30)
```

| reference |
|:---|
| \[1\] FAO. FishStat: Global capture production 1950–2024 (release 2026.1.0)\[DB/OL\]. Rome: FAO, 2026\[2026-10-10\]. <https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip>. |
| \[2\] FAO. Fishery and Aquaculture Statistics – Yearbook 2024\[M\]. Rome: FAO, 2026. DOI: 10.4060/ce1217en. |
| \[3\] Bai J, Perron P. Computation and analysis of multiple structural change models\[J\]. Journal of Applied Econometrics, 2003, 18(1): 1-22. DOI: 10.1002/jae.659. |
| \[4\] Zeileis A, Kleiber C, Krämer W, et al. Testing and dating of structural changes in practice\[J\]. Computational Statistics & Data Analysis, 2003, 44(1-2): 109-123. DOI: 10.1016/S0167-9473(03)00030-6. |
| \[5\] Mann H B. Nonparametric tests against trend\[J\]. Econometrica, 1945, 13(3): 245-259. DOI: 10.2307/1907187. |
| \[6\] Kendall M G. Rank correlation methods\[M\]. 4th ed. London: Charles Griffin, 1975. |
| \[7\] Kleisner K, Zeller D, Froese R, et al. Using global catch data for inferences on the world’s marine fisheries\[J\]. Fish and Fisheries, 2013, 14(3): 293-311. DOI: 10.1111/j.1467-2979.2012.00469.x. |
| \[8\] Shannon C E. A mathematical theory of communication\[J\]. Bell System Technical Journal, 1948, 27(3): 379-423. DOI: 10.1002/j.1538-7305.1948.tb01338.x. |
| \[9\] Rhoades S A. The Herfindahl-Hirschman index\[J\]. Federal Reserve Bulletin, 1993, 79(3): 188-189. |
| \[10\] R Core Team. R: A language and environment for statistical computing\[CP/OL\]. Vienna: R Foundation for Statistical Computing, 2026\[2026-10-11\]. <https://www.R-project.org/>. |
| \[11\] Lin H. faofishingreport: Analyse and report FAO Global Capture Production statistics. R package version 0.5.0\[CP/OL\]. 2026\[2026-10-11\]. <https://linbojun99.github.io/faofishingreport/>. |

## 🧾 参数

| 参数 | 默认值 | 含义 |
|----|----|----|
| `area` | （必填） | FAO主要渔区：代码（如`34`）或英文名称（如`"Atlantic, Eastern Central"`）。渔区不存在时报错，并列出所有海洋渔区。 |
| `...` |  | 各项分析的设置，以章节名为前缀命名，如`species_h_value = 0.2`（见[⚙️ 分析的设置](#settings)） |
| `file` | `reports/FAO<code>_report_<lang>.docx` | 要写入的文件，所在文件夹不存在时会自动创建；其他格式使用相同的文件名，扩展名为`.tex`和`.pdf` |
| `format` | `"docx"` | `"docx"`、`"latex"`和`"pdf"`中的一个或多个（见[🖨️ Word、LaTeX和PDF](#formats)） |
| `lang` | `NULL`：采用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)中的语言 | `"zh"`（中文）或`"en"`（英文）；`"both"`生成中文报告 |
| `title` | 根据渔区生成 | 如“FAO 34渔区（中东大西洋）海洋捕捞产量评估” |
| `report_no` | `1` | 报告编号：“报告一”（中文数字，最大到99；英文报告为“Report 1”），同时是图表编号的第一个数字（图1-1、表1-1） |
| `author` | `NULL`：“作者” | 封面、标题下方和文件属性中的作者 |
| `affiliation` | `NULL`：不显示 | 封面上的单位 |
| `project` | `NULL`：“课题报告” | 封面顶部的文字（英文报告为“Project Report”） |
| `date` | [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) | 封面上的日期，格式为YYYY-MM-DD；也是R和faofishingreport两条参考文献中的引用日期 |
| `font` | `NULL`：中文报告为“Times New Roman”，英文报告为“Arial” | 西文和数字的字体；英文报告中为全部文字的字体 |
| `font_zh` | `"FangSong"` | 正文、标题、表格和参考文献的中文字体；各级标题和题注仍用黑体和楷体 |
| `citation` | `"apa"` | 英文报告的引文格式：`"apa"`或`"fao"`。中文报告一律采用顺序编码制（GB/T 7714）。 |
| `top_n` | `10` | 表格和图中主要种类和国家的数量，也是CMSY++评估的物种数（未指定`cmsy_top`时） |
| `data_version` | `"latest"` | 使用的FAO数据版本，同[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md) |
| `total`、`species`、`countries`、`groups`、`status`、`quality`、`diversity`、`fleets` | `TRUE` | 默认开启的章节（见[🧩 分析章节](#sections)） |
| `shocks`、`collapse`、`synchrony`、`pelagic`、`mtc`、`distant` | `FALSE` | 默认关闭的章节 |
| `cmsy` | `FALSE` | `TRUE`时加入主要种类的CMSY++资源评估；也可以传入之前的运行结果（见[📉 CMSY++资源评估](#cmsy)） |

## 🧩 分析章节

每项分析是报告中的一个章节，用与章节同名的参数开启或关闭。该分析函数的参数以倒数第二列的前缀传入，最后一列是该章节正文引用的文献（完整条目见文末）。

| 开关 | 默认 | 报告中的章节 | 函数 | 前缀 | 引用的文献 |
|----|----|----|----|----|----|
| `total` | `TRUE` | 渔获量变化与阶段划分：总渔获量及其突变点、近10年趋势、占全球海洋捕捞产量的比例 | [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md) | `total_` | Bai和Perron（2003）、Zeileis等（2003）、Mann（1945）、Kendall（1975） |
| `species` | `TRUE` | 近5年主要渔获种类：表格、图、突变点和近10年趋势 | [`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md) | `species_` | 同`total` |
| `countries` | `TRUE` | 近5年主要捕捞国家（地区）：表格、图、地图、突变点和趋势 | [`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md) | `countries_` | 同`total` |
| `groups` | `TRUE` | 最后一年的ISSCAAP类群组成 | [`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md) | `groups_` | 无 |
| `status` | `TRUE` | 资源开发状态图 | [`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md) | `status_` | Kleisner等（2013） |
| `quality` | `TRUE` | 数据质量：正式报告与估算的渔获量、分类阶元 | [`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md) | `quality_` | 无 |
| `diversity` | `TRUE` | 渔获多样性与集中度（Shannon指数、HHI） | [`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md) | `diversity_` | Shannon（1948）、Rhoades（1993） |
| `fleets` | `TRUE` | 沿岸国与远洋捕捞国家和地区的渔获量 | [`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md) | `fleets_` | 无 |
| `shocks` | `FALSE` | 渔获骤变 | [`catch_shocks()`](https://linbojun99.github.io/faofishingreport/reference/catch_shocks.md) | `shocks_` | Cottrell等（2019）；`shocks_method = "gephart2017"`时为Gephart等（2017） |
| `collapse` | `FALSE` | 渔业崩溃，并与随机序列对照 | [`collapse_analysis()`](https://linbojun99.github.io/faofishingreport/reference/collapse_analysis.md) | `collapse_`，另有`collapse_seed` | Mullon等（2005）；`collapse_method = "worm2006"`时为Worm等（2006）；Wilberg和Miller（2007） |
| `synchrony` | `FALSE` | 渔获同步性与组合效应 | [`catch_synchrony()`](https://linbojun99.github.io/faofishingreport/reference/catch_synchrony.md) | `synchrony_` | Loreau和de Mazancourt（2008）、Thibaut和Connolly（2013）、Carlson和Satterthwaite（2011）、Ong等（2021）、Lepš等（2019） |
| `pelagic` | `FALSE` | 中上层鱼类与底层鱼类渔获比（P/D） | [`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/reference/pelagic_demersal.md) | `pelagic_` | Caddy（1993）和de Leiva Moreno等（2000）；`pelagic_definition = "grainger_garcia"`时为Grainger和Garcia（1996）；Hondorp等（2010） |
| `mtc` | `FALSE` | 渔获平均温度 | [`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md) | `mtc_` | Cheung等（2013）、FishBase（Froese和Pauly）、使用ERSST v5数据时的Huang等（2017）、Pyper和Peterman（1998）、Alajmi等（2024） |
| `distant` | `FALSE` | 在该渔区作业的主要远洋捕捞国家（地区）概况 | [`distant_water_profile()`](https://linbojun99.github.io/faofishingreport/reference/distant_water_profile.md) | `distant_`，另有`distant_countries` | Swartz等（2010）、Garibaldi和Limongelli（2003） |
| `cmsy` | `FALSE` | 主要种类资源评估，单独成为一部分 | [`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)（及[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)） | `cmsy_` | Froese等（2023）、Froese等（2017）、FishBase（Froese和Pauly）、Plummer（2003）、FAO（2026b） |

每份报告还会引用所用的FAO数据版本、FAO渔业和水产养殖统计年鉴（FAO
2026a）、R和faofishingreport。各项分析的详细介绍见本站其他文章：[🌊
渔区分析](https://linbojun99.github.io/faofishingreport/articles/zh/fishing-areas.md)、[📈
类群结构、资源状态与趋势](https://linbojun99.github.io/faofishingreport/articles/zh/status-trends.md)、[🔍
数据质量、多样性与船队](https://linbojun99.github.io/faofishingreport/articles/zh/quality-diversity-fleets.md)、[🌡️
海表温度与渔获平均温度](https://linbojun99.github.io/faofishingreport/articles/zh/sst-mtc.md)和[📉
CMSY++资源评估](https://linbojun99.github.io/faofishingreport/articles/zh/cmsy.md)。

报告的其余部分随开启的章节而变：

- **摘要**列出这些章节的分析内容和方法，并为`total`、`species`、`countries`、`status`和`cmsy`各写一句结果；
- **1.2
  分析方法**只包含这些章节所用方法的段落（`groups`、`quality`和`fleets`不需要方法段落）；
- **2 结果与结论**中的小节以及图表按章节顺序编号，**结论**每个章节一条；
- **参考文献**只列出正文引用的文献，例如只有开启`diversity`时才列出Shannon（1948）；
- **附件**只在开启`species`和`countries`时才有附表1和附表2，只在开启`cmsy`时才有CMSY++的附表。

总渔获量分析始终会运行，因为它提供该渔区的数据，但只有`total = TRUE`时才写入相应章节。

## 🎛️ 增减章节

关闭三个章节，开启中上层鱼类与底层鱼类渔获比，并在资源开发状态图中使用平滑后的渔获量：

``` r

short <- fao_report(34, file = "FAO34_short.docx", lang = "zh", quality = FALSE, diversity = FALSE,
                    fleets = FALSE, pelagic = TRUE, status_smooth = TRUE)
short
#> FAO report: /tmp/Rtmprkl2xQ/faofishingreport_site_1bf3818daf3bc/FAO34_short.docx
#> Sections: total, species, countries, groups, status, pelagic
#>   $main_species           main species of the last five years (10 rows)
#>   $main_countries         main fishing countries of the last five years (10 rows)
#>   $analyses               results of the analyses: total, species, countries, groups, status, pelagic
short_word <- officer::docx_summary(officer::read_docx(short$file))
outline(short_word)
#> 1 数据来源与分析方法
#>     1.1 数据来源
#>     1.2 分析方法
#> 2 结果与结论
#>     2.1 渔获量变化与阶段划分
#>     2.2 主要渔获种类
#>     2.3 主要捕捞国家（地区）
#>     2.4 渔获类群组成
#>     2.5 资源开发状态
#>     2.6 中上层鱼类与底层鱼类渔获比
#>     2.7 结论
#> 参考文献
#> 附件
```

结果部分变为6个小节，结论变为6条。摘要和分析方法中加入了P/D，参考文献中增加了Caddy（1993）、de
Leiva
Moreno等（2000）和Hondorp等（2010），不再有Shannon（1948）和Rhoades（1993）：

``` r

show_table(data.frame(reference = short_word$text[short_word$style_name == "FAO Reference"]), n = 30)
```

| reference |
|:---|
| \[1\] FAO. FishStat: Global capture production 1950–2024 (release 2026.1.0)\[DB/OL\]. Rome: FAO, 2026\[2026-10-10\]. <https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip>. |
| \[2\] FAO. Fishery and Aquaculture Statistics – Yearbook 2024\[M\]. Rome: FAO, 2026. DOI: 10.4060/ce1217en. |
| \[3\] Bai J, Perron P. Computation and analysis of multiple structural change models\[J\]. Journal of Applied Econometrics, 2003, 18(1): 1-22. DOI: 10.1002/jae.659. |
| \[4\] Zeileis A, Kleiber C, Krämer W, et al. Testing and dating of structural changes in practice\[J\]. Computational Statistics & Data Analysis, 2003, 44(1-2): 109-123. DOI: 10.1016/S0167-9473(03)00030-6. |
| \[5\] Mann H B. Nonparametric tests against trend\[J\]. Econometrica, 1945, 13(3): 245-259. DOI: 10.2307/1907187. |
| \[6\] Kendall M G. Rank correlation methods\[M\]. 4th ed. London: Charles Griffin, 1975. |
| \[7\] Kleisner K, Zeller D, Froese R, et al. Using global catch data for inferences on the world’s marine fisheries\[J\]. Fish and Fisheries, 2013, 14(3): 293-311. DOI: 10.1111/j.1467-2979.2012.00469.x. |
| \[8\] Caddy J F. Toward a comparative evaluation of human impacts on fishery ecosystems of enclosed and semi‐enclosed seas\[J\]. Reviews in Fisheries Science, 1993, 1(1): 57-95. DOI: 10.1080/10641269309388535. |
| \[9\] de Leiva Moreno J I, Agostini V N, Caddy J F, et al. Is the pelagic-demersal ratio from fishery landings a useful proxy for nutrient availability? A preliminary data exploration for the semi-enclosed seas around Europe\[J\]. ICES Journal of Marine Science, 2000, 57(4): 1091-1102. DOI: 10.1006/jmsc.2000.0705. |
| \[10\] R Core Team. R: A language and environment for statistical computing\[CP/OL\]. Vienna: R Foundation for Statistical Computing, 2026\[2026-10-11\]. <https://www.R-project.org/>. |
| \[11\] Lin H. faofishingreport: Analyse and report FAO Global Capture Production statistics. R package version 0.5.0\[CP/OL\]. 2026\[2026-10-11\]. <https://linbojun99.github.io/faofishingreport/>. |
| \[12\] Hondorp D W, Breitburg D L, Davias L A. Eutrophication and Fisheries: Separating the Effects of Nitrogen Loads and Hypoxia on the Pelagic-to-Demersal Ratio and other Measures of Landings Composition\[J\]. Marine and Coastal Fisheries, 2010, 2(1): 339-361. DOI: 10.1577/C09-020.1. |

分析方法段落描述的是各项分析的默认设置，例如资源开发状态图的纳入标准和突变点检测中15%的最小分段长度。修改这类设置（`status_smooth = TRUE`、`status_min_catch = 10000`、`species_h_value = 0.2`等）后，结果会随之改变，但需要自己在1.2
分析方法中说明所做的修改。

## ⚙️ 分析的设置

每个章节的分析函数都有自己的参数。把参数传给[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)时，在参数名前加上章节名和“\_”即可：`species_h_value = 0.2`就是[`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md)的`h_value = 0.2`。你的设置优先于报告自己设定的值。

| 在[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)中 | 传给 | 作用 |
|----|----|----|
| `species_h_value = 0.2` | `fishing_area_species(h_value = 0.2)` | 主要种类突变点检测的最小分段长度为序列长度的20% |
| `countries_line_linewidth = 1` | `fishing_area_country(line_linewidth = 1)` | 国家图中的线条更细 |
| `status_smooth = TRUE` | `stock_status(smooth = TRUE)` | 先对渔获量做3年滑动平均，再划分资源状态 |
| `status_min_catch = 10000` | `stock_status(min_catch = 10000)` | 只纳入累计渔获量不少于10 000 t的种群 |
| `shocks_method = "gephart2017"` | `catch_shocks(method = "gephart2017")` | 按Gephart等（2017）的定义识别骤变，方法段落和参考文献随之改变 |
| `collapse_method = "worm2006"` | `collapse_analysis(method = "worm2006")` | 按Worm等（2006）的定义识别崩溃，方法、结果、图和结论随之改变 |
| `collapse_seed = 2` | 在[`collapse_analysis()`](https://linbojun99.github.io/faofishingreport/reference/collapse_analysis.md)之前设定随机数种子 | 零模型使用另一组随机序列（默认为1，因此每次生成的报告数字相同）；运行后恢复会话原来的随机数状态 |
| `synchrony_by = "country"` | `catch_synchrony(by = "country")` | 分析各国家（地区）而不是各物种的同步性 |
| `pelagic_definition = "grainger_garcia"` | `pelagic_demersal(definition = "grainger_garcia")` | 采用Grainger和Garcia（1996）的类群划分 |
| `mtc_version = "builtin"` | `mean_temperature_catch(version = "builtin")` | 报告的默认值：使用本包自带的FishBase数据，无需下载 |
| `mtc_version = "latest"` | `mean_temperature_catch(version = "latest")` | 使用最新的FishBase版本，必要时下载 |
| `distant_countries = 5` | [`distant_water_profile()`](https://linbojun99.github.io/faofishingreport/reference/distant_water_profile.md)，每个国家运行一次 | 介绍近5年在该渔区远洋渔获量最高的5个国家（地区）（默认为3个），也可以直接给出国家名称，如`distant_countries = c("Russian Federation", "Belize")` |
| `cmsy_top = 5` | `cmsy_area(top = 5)` | 用CMSY++评估5个物种（默认为`top_n`） |
| `cmsy_settings = list(n.iter = 100000)` | `cmsy_area(settings = ...)` | CMSY++的设置，见[`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md) |

`collapse_seed`和`distant_countries`是报告本身的设置。`cmsy_`前缀也可以用于[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)的参数，如`cmsy_min_years`。报告自己设定的参数（`area`、`area_data`、`area_name`、`lang`、`pptx`和`table`）不能指定。已关闭章节的设置不起作用。

设置必须有名称，以章节名开头，并且是该章节函数的参数，否则[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)会在运行任何分析之前停止，报错信息列出可以使用的名称：

``` r

fao_report(34, species_h = 0.2)
#> Error:
#> ! fao_report(): species_h would set h, which is not an argument of fishing_area_species(). Its arguments: rank_range, plot, timeseries_analysis, line_width, point_size, point_alpha, point_shape, line_alpha, axis_text_size, axis_title_size, strip_text_size, axis_text_face, axis_title_face, strip_text_face, smooth_method, smooth_linetype, line_linewidth, text_hjust, text_vjust, text_size, text_color, facet_ncol, h_value, ZH
fao_report(34, speceis_h_value = 0.2)
#> Error:
#> ! Unknown argument of fao_report(): speceis_h_value. Settings of an analysis start with its section: total_, species_, countries_, groups_, status_, quality_, diversity_, fleets_, shocks_, collapse_, synchrony_, pelagic_, mtc_, distant_, cmsy_
```

## 📐 格式

中文报告采用课题报告的常用格式：

| 项目 | 格式 |
|----|----|
| 页面 | A4纵向；上下页边距2.54 cm，左右3.17 cm |
| 正文 | 中文仿宋、西文和数字Times New Roman，小四号（12磅），1.5倍行距，首行缩进2字符，两端对齐 |
| 标题部分 | “报告一”和标题：仿宋，小二号（18磅），加粗，居中；其下为作者，小四号 |
| “摘　要” | 黑体，小三号（15磅），居中 |
| 一级标题（1、2……） | 黑体，四号（14磅） |
| 二级标题（1.1、2.1……） | 楷体，四号（14磅） |
| 题注 | 黑体，小四号（12磅），居中，按报告编号：图题“图1-1”在图下方，表题“表1-1”在表上方；附件中为“附表1” |
| 表格 | 三线表（顶线和底线1.5磅，表头下方的线0.75磅），仿宋和Times New Roman五号（10.5磅），单倍行距 |
| 引文 | 方括号上标数字，按首次引用的顺序编号：\[1\]、\[3,4\]、\[5-7\]（GB/T 7714） |
| 参考文献 | 按引用顺序编号\[1\]、\[2\]……，采用GB/T 7714格式，悬挂缩进 |
| 空格 | 中文与西文字母、数字之间不加空格，标题和题注的编号后保留空格（“2.1 渔获量变化与阶段划分”“图1-1 ……”） |
| 页码 | 页脚居中，五号（10.5磅）；封面不编页码，标题页为第1页 |
| 封面 | 项目名称黑体一号（26磅）加粗，标题二号（22磅）加粗，其他各行三号（16磅） |

英文报告的页面、结构、编号和表格与中文报告相同，全部文字使用同一种字体，默认为Arial（可用`font`修改，如`font = "Times New Roman"`）。各级标题加粗，一级标题14磅，二级标题12磅。题注以加粗的标签开头：“**Figure
1-1.** Catch in …”“**Table 1-1.** Main species items
…”。引文采用著者-出版年制，默认为APA格式，也可以用`citation = "fao"`改为FAO出版物的格式。

无论中英文，物种和属的学名在正文和表格中都用斜体：*Sardina
pilchardus*、*Trachurus*
spp.（“spp.”为正体）；科及更高分类阶元的名称用正体，如Actinopterygii。符号同样用斜体（*P*\<0.05、*H*）。灰色备注和参考文献按原样写入，不做中文空格处理。

各级标题设有大纲级别1和2，因此会显示在Word的导航窗格中，也会出现在Word中插入的目录里。标题和作者还写入了文件属性。

## 🖨️ Word、LaTeX和PDF

`format`决定输出哪些文件：`"docx"`（默认）、`"latex"`和`"pdf"`，可以选一个或多个。各项分析只运行一次，各种格式的文字、图表、表格、图表标题和参考文献完全相同。

``` r

fao_report(34, lang = "zh", format = c("docx", "pdf"))      # Word和PDF
fao_report(34, lang = "en", format = "latex")                # 只输出LaTeX源文件
report <- fao_report(34, format = c("docx", "latex", "pdf"))
report$file                                                  # 按docx、latex、pdf命名的文件路径
```

| `format` | 输出的文件（以`fao_report(34, lang = "zh")`为例） | 说明 |
|----|----|----|
| `"docx"` | `reports/FAO34_report_zh.docx` | 本文介绍的Word文件 |
| `"latex"` | `reports/FAO34_report_zh.tex`和文件夹`reports/FAO34_report_zh_figures/` | 同一份报告的XeLaTeX源文件，图片为PNG文件。中文报告使用`ctexart`文档类，英文报告使用`article`文档类。 |
| `"pdf"` | `reports/FAO34_report_zh.pdf`（以及`.tex`） | 用XeLaTeX编译的PDF，带有各级标题的书签 |

LaTeX版本沿用Word文件的格式（见[📐
格式](#format)）：A4纸张和相同的页边距，相同的字体和字号，1.5倍行距，三线表（booktabs，单倍行距，列宽按内容确定，可以跨页），带编号的图表标题，封面不编页码，标题页从第1页开始。字体取自本机已安装的字体：

|  | Windows | macOS | Linux |
|----|----|----|----|
| 中文正文（仿宋） | FangSong | STFangsong | FandolFang（TeX Live自带） |
| 中文标题和图表标题（黑体） | SimHei | Heiti SC | FandolHei |
| 中文二级标题（楷体） | KaiTi | Kaiti SC | FandolKai |
| 西文和数字 | `font`：中文报告为Times New Roman，英文报告为Arial（没有时用Liberation Serif或Sans，再没有时用Latin Modern） |  |  |

中文字体没有粗体字形：与Word一样，加粗的中文（如标题）通过加粗笔画实现。

**编译。**安装了tinytex包（`install.packages("tinytex")`；如果本机没有LaTeX，再运行[`tinytex::install_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html)）时，用`tinytex::latexmk(engine = "xelatex")`生成PDF，第一次运行时会自动安装缺少的LaTeX宏包（ctex、xeCJK、booktabs、longtable、needspace、xurl等）；没有tinytex时，系统路径中须有`xelatex`。要修改LaTeX版本的文字，编辑`.tex`文件后重新编译即可，例如`tinytex::xelatex("reports/FAO34_report_zh.tex")`。

如果tinytex因为本机的TeX Live比在线仓库旧而无法安装宏包（提示”Local TeX
Live (2024) is older than remote repository”），可以更新TeX
Live（[`tinytex::reinstall_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html)），或从对应年份的存档安装宏包：`tlmgr --repository https://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2024/tlnet-final install ctex`。

## 🔖 引文与参考文献

参考文献只列出正文引用过的文献。所用的FAO数据版本、R和faofishingreport三条文献根据数据和R会话自动生成（版本号、发布日期和引用日期）；报告包含CMSY++或渔获平均温度时，会引用FishBase及所用的版本；其他文献都来自`inst/extdata/report_references.csv`。

| 格式 | 正文中的引文 | 参考文献列表 |
|----|----|----|
| 中文报告，GB/T 7714 | 上标\[7\] | 按引用顺序编号 |
| `citation = "apa"`，APA第7版 | (Kleisner et al., 2013)；(Bai & Perron, 2003; Zeileis et al., 2003) | 按字母顺序；同一作者同一年份的文献标为2026a、2026b |
| `citation = "fao"`，FAO出版物 | (Kleisner et al., 2013)；(Bai and Perron, 2003) | 按字母顺序；同一作者同一年份的文献标为2026a、2026b |

同样两条文献在三种格式下的写法（与报告中相同；FAO数据为2026.1.0版，于2026年10月10日下载）：

| 格式 | Kleisner等（2013） | FAO数据 |
|----|----|----|
| GB/T 7714 | Kleisner K, Zeller D, Froese R, et al. Using global catch data for inferences on the world’s marine fisheries\[J\]. Fish and Fisheries, 2013, 14(3): 293-311. DOI: 10.1111/j.1467-2979.2012.00469.x. | FAO. FishStat: Global capture production 1950–2024 (release 2026.1.0)\[DB/OL\]. Rome: FAO, 2026\[2026-10-10\]. <https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip>. |
| APA | Kleisner, K., Zeller, D., Froese, R., & Pauly, D. (2013). Using global catch data for inferences on the world’s marine fisheries. *Fish and Fisheries*, *14*(3), 293–311. <https://doi.org/10.1111/j.1467-2979.2012.00469.x> | FAO. (2026). *FishStat: Global capture production 1950–2024* (Release 2026.1.0) \[Data set\]. <https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip> |
| FAO | Kleisner, K., Zeller, D., Froese, R. & Pauly, D. 2013. Using global catch data for inferences on the world’s marine fisheries. *Fish and Fisheries*, 14(3): 293–311. <https://doi.org/10.1111/j.1467-2979.2012.00469.x> | FAO. 2026. FishStat: Global capture production 1950–2024 (release 2026.1.0). \[Accessed on 10 October 2026\]. In: FishStatJ. Available at: <https://www.fao.org/fishery/statistics/software/fishstatj/en>. Licence: CC-BY-4.0. |

GB/T
7714中方括号内的字母是文献类型标识：\[J\]期刊、\[M\]专著、\[C\]论文集、\[R\]报告、\[DB/OL\]联机数据库、\[CP/OL\]联机计算机程序；作者不超过三位时全部列出，超过三位时只列前三位，其后加“et
al.”。FAO格式参照FAO出版物（如FAO
2024）的参考文献列表，FishStat数据的引用方式也与这些出版物相同，注明引用日期、FishStatJ和许可证。

## 📉 CMSY++资源评估

`cmsy = TRUE`时，主要种类还会用CMSY++（[`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)）进行资源评估：评估对象为近10年渔获量最高的`top_n`个鉴定到种的物种条目，或指定的`cmsy_top`个。CMSY++需要JAGS，用[`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)安装一次即可；未安装时[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)会报错并给出提示。评估需要几分钟。

``` r

cmsy_setup()                          # 只需运行一次：安装JAGS和CMSY++所需的R包
report <- fao_report(34, lang = "zh", cmsy = TRUE)
report
#> FAO report: .../reports/FAO34_report_zh.docx
#> Sections: total, species, countries, groups, status, quality, diversity, fleets, cmsy
#>   $main_species           main species of the last five years (10 rows)
#>   $main_countries         main fishing countries of the last five years (10 rows)
#>   $cmsy_reference_points  CMSY++ biological reference points with 95% confidence limits (10 rows)
#>   $cmsy_priors            CMSY++ priors, their sources and links (10 rows)
#>   $cmsy                   CMSY++ result (folder .../reports/cmsy/Atlantic__Eastern_Central)
#>   $analyses               results of the analyses: total, species, ..., fleets, cmsy

View(report$cmsy_reference_points)    # r、k、MSY、Fmsy、Bmsy、B/Bmsy和F/Fmsy及其95%置信区间
View(report$cmsy_priors)              # 先验参数、来源和链接

# 表格中列出15个主要种类和国家，评估其中5个物种，MCMC链更长
fao_report(34, lang = "zh", top_n = 15, cmsy = TRUE, cmsy_top = 5,
           cmsy_settings = list(n.iter = 100000))
```

此时，摘要中会增加一句种群状况，分析方法中增加一段CMSY++方法，结果之后还有单独的一部分：

    3 主要渔获种类资源评估（CMSY++）
        3.1 评估概况
        3.2 沙丁鱼（Sardina pilchardus）
        3.3 金色小沙丁鱼（Sardinella aurita）
        ...
        3.12 小结

- **评估概况**：完成评估的物种数（以及未能评估的物种），参照FAO（2026b）的划分，最后一年处于最大可持续捕捞（B/Bmsy≥0.8）和过度捕捞（B/Bmsy\<0.8）状态的种群数，以及位于Kobe图绿区（B/Bmsy≥1且F/Fmsy≤1）和红区（B/Bmsy\<1且F/Fmsy\>1）的种群数；附评估结果表（年份、r、k、MSY、B/Bmsy、F/Fmsy、资源状态）、全部种群的Kobe图和B/Bmsy图。
- **每个物种一个小节**：r、k和MSY及其95%置信区间，最后一年的B/Bmsy和F/Fmsy，渔获量占MSY的百分比，资源状态，位于Kobe图四个区的概率，该物种的CMSY++结果图，以及一条灰色备注，提示与其他评估结果比较并讨论先验设置。
- **小结**；附件中还有**附表3**生物学参考点（r、k、MSY、Fmsy、Bmsy、B/Bmsy、F/Fmsy）及其95%置信区间，**附表4**先验参数（恢复力、r和k的范围、首年、中间年份和末年的B/k先验），**附表5**先验参数的来源（r先验来源、B/k先验来源、物种在FishBase或SeaLifeBase中的页面），其后列出FishBase数据、CMSY++代码与用户指南以及FAO数据的链接。

CMSY++的全部结果（输入文件、运行结果、图和表）保存在报告旁边的`cmsy/`文件夹中，如`reports/cmsy/Atlantic__Eastern_Central/`，其中有`cmsy_result.rds`。如需重新生成报告（例如改变章节或修改模板之后），把这个结果传给`cmsy`即可，不必再运行CMSY++，此时`cmsy_`设置不再起作用。[`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)的结果也可以同样传入：

``` r

cmsy34 <- readRDS(file.path("reports", "cmsy", "Atlantic__Eastern_Central", "cmsy_result.rds"))
fao_report(34, lang = "zh", cmsy = cmsy34, pelagic = TRUE)

area34 <- cmsy_area("Atlantic, Eastern Central", top = 10)
fao_report(34, lang = "zh", cmsy = area34)
```

CMSY++部分的图来自那次运行，语言与运行时相同。方法、设置和局限性见[📉
CMSY++资源评估](https://linbojun99.github.io/faofishingreport/articles/zh/cmsy.md)：FAO数据只有渔获量，评估结果仅供参考。

## 📦 fao_report()的返回结果

一个`fao_report`类的列表，以不可见的方式返回：

| 元素 | 内容 |
|----|----|
| `file` | 所写文件的路径，按`docx`、`latex`、`pdf`命名 |
| `sections` | 开启的章节 |
| `main_species` | 近5年主要渔获种类：报告语言的名称、英文名、学名、年均渔获量（t）、占比（%）、峰值年份、最后一年渔获量为峰值的百分比、近10年趋势（“up”、“down”或”none”） |
| `main_countries` | 主要捕捞国家（地区）的同类信息 |
| `mtc_species` | `mtc = TRUE`时：渔获平均温度所用的物种、FishBase匹配结果和偏好温度 |
| `analyses` | 各项分析的结果，按章节命名，例如`analyses$status`是[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)的结果 |
| `cmsy` | 有CMSY++时：[`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)的完整结果 |
| `cmsy_reference_points` | 有CMSY++时：每个种群一行，包括r、k、MSY、Fmsy、Bmsy、B/Bmsy和F/Fmsy，各附95%置信区间的下限和上限（`_lcl`、`_ucl`），以及最后一年渔获量、位于Kobe图各区的概率和资源状态 |
| `cmsy_priors` | 有CMSY++时：恢复力及其来源、r和k的先验范围、MSY先验、B/k先验及其来源，以及物种在FishBase或SeaLifeBase中的页面、FishBase数据、CMSY++用户指南和FAO数据的链接 |

``` r

names(report)
#> [1] "file"           "sections"       "main_species"   "main_countries"
#> [5] "analyses"
show_table(report$main_countries, digits = 1)
```

| country | country_en | mean_catch_t | share_pct | peak_year | pct_of_peak | trend_10_years |
|:---|:---|---:|---:|:---|---:|:---|
| 摩洛哥 | Morocco | 1,418,927.3 | 27.7 | 2022 | 91.0 | none |
| 毛里塔尼亚 | Mauritania | 674,299.1 | 13.2 | 2018 | 65.4 | none |
| 塞内加尔 | Senegal | 464,480.7 | 9.1 | 2017 | 93.6 | none |
| 尼日利亚 | Nigeria | 441,470.9 | 8.6 | 2017 | 94.4 | none |
| 几内亚 | Guinea | 343,164.0 | 6.7 | 2021 | 81.0 | up |
| 加纳 | Ghana | 333,652.8 | 6.5 | 1999 | 98.1 | up |
| 塞拉利昂 | Sierra Leone | 205,312.0 | 4.0 | 2022 | 97.1 | up |
| 喀麦隆 | Cameroon | 198,094.6 | 3.9 | 2019 | 78.8 | none |
| 伯利兹 | Belize | 190,814.7 | 3.7 | 2010 | 51.1 | none |
| 格鲁吉亚 | Georgia | 127,022.0 | 2.5 | 2019 | 79.9 | up |

``` r

names(report$analyses)
#> [1] "total"     "species"   "countries" "groups"    "status"    "quality"  
#> [7] "diversity" "fleets"
stocks <- report$analyses$status$stocks
show_table(stocks[, c("species_name", "peak_year", "peak_tonnes", "pct_of_peak", "status_last_year")],
           n = 5, digits = 0)
```

| species_name   | peak_year | peak_tonnes | pct_of_peak | status_last_year |
|:---------------|:----------|------------:|------------:|:-----------------|
| 沙丁鱼         | 2018      |   1,354,617 |          53 | 充分开发         |
| 竹筴鱼属物种   | 2010      |     574,747 |          59 | 充分开发         |
| 未分类的沙丁鱼 | 2018      |     542,915 |          33 | 过度开发         |
| 科利鲭         | 2019      |     485,443 |          97 | 充分开发         |
| 金色小沙丁鱼   | 1991      |     453,410 |          35 | 过度开发         |

## ✏️ 修改报告措辞

报告的全部文字都来自`inst/extdata/report_text.csv`，每段文字一行，列为`key`、`en`和`zh`：

``` r

texts <- read.csv(system.file("extdata", "report_text.csv", package = "faofishingreport"),
                  fileEncoding = "UTF-8", colClasses = "character")
nrow(texts)
#> [1] 269
show_table(data.frame(texts[texts$key %in% c("h_status", "status_p2", "concl_status"), ], row.names = NULL))
```

| key | en | zh |
|:---|:---|:---|
| h_status | Stock status | 资源开发状态 |
| status_p2 | Stock status plots rely on catch data only: a falling catch may also result from management measures, markets or changes in reporting {cite:kleisner2013}, so the results show the general state of exploitation and do not replace stock assessments. | 需要指出的是，资源开发状态图仅基于渔获量数据，渔获量的下降也可能源于管理措施、市场需求或统计方式的变化{cite:kleisner2013}，其结果仅反映资源开发的大致状况，不能替代基于资源评估模型的结论。 |
| concl_status | In {last_year}, {oc_pct}% of the stocks were over-exploited or collapsed. | {last_year} 年处于过度开发和崩溃状态的资源占 {oc_pct}%。 |

- `{last_year}`、`{oc_pct}`等是占位符，由分析结果填入。请只使用原文中已有的占位符，未知的占位符会使报告报错停止。
- `{cite:kleisner2013}`按键名引用一条文献，`{cite:bai2003,zeileis2003}`同时引用多条。在中文报告中变为上标编号，在英文报告中变为“(Kleisner
  et al., 2013)”。
- `*...*`写为斜体（`*P*`、学名），`^...^`写为上标，`~...~`写为下标，例如`HHI = ∑(100*p*~i~)^2^`。

参考文献在`inst/extdata/report_references.csv`中：

``` r

refs <- read.csv(system.file("extdata", "report_references.csv", package = "faofishingreport"),
                 fileEncoding = "UTF-8", colClasses = "character")
show_table(refs[, c("key", "type", "author", "year", "title")], n = 5)
```

| key | type | author | year | title |
|:---|:---|:---|:---|:---|
| fao_yearbook | M | FAO | 2026 | Fishery and Aquaculture Statistics – Yearbook 2024 |
| bai2003 | J | Bai, J.; Perron, P. | 2003 | Computation and analysis of multiple structural change models |
| zeileis2003 | J | Zeileis, A.; Kleiber, C.; Krämer, W.; Hornik, K. | 2003 | Testing and dating of structural changes in practice |
| kleisner2013 | J | Kleisner, K.; Zeller, D.; Froese, R.; Pauly, D. | 2013 | Using global catch data for inferences on the world’s marine fisheries |
| mann1945 | J | Mann, H. B. | 1945 | Nonparametric tests against trend |

| 列 | 内容 |
|----|----|
| `key` | `{cite:key}`中使用的键名 |
| `type` | `J`期刊论文、`M`专著、`C`论文集中的论文、`R`丛刊中的报告（如FAO Fisheries Technical Paper）。`DB`（数据库）和`CP`（计算机程序）是函数自动生成的文献所用的类型：FAO数据、FishBase、R和faofishingreport。 |
| `author` | 个人作者写作“Family, I. I.”，多位作者用“;”分隔；机构作者直接写名称，如FAO |
| `year`、`title` | 年份和题名 |
| `container` | 期刊名或论文集名 |
| `volume`、`issue`、`pages` | 卷、期和页码，如`293-311` |
| `edition` | 版次，如`4`表示第4版 |
| `series` | 报告或专著所属丛刊，如“FAO Fisheries Technical Paper No. 359” |
| `place`、`publisher` | 出版地和出版者 |
| `doi`、`url` | DOI（不含“<https://doi.org/>”）和网址 |

如需修改措辞或增加文献，在包源代码中编辑这两个文件，重新安装本包（如在源代码文件夹中运行`R CMD INSTALL .`），然后重启R：这两个文件在每个会话中只读取一次。

## 📖 参考文献

**标准与引文格式**

中华人民共和国国家质量监督检验检疫总局，中国国家标准化管理委员会.
信息与文献　参考文献著录规则：GB/T 7714—2015\[S\].
北京：中国标准出版社，2015.（Information and documentation—Rules for
bibliographic references and citations to information
resources；2015年5月15日发布，2015年12月1日实施）

American Psychological Association (2020). *Publication manual of the
American Psychological Association*. 7th edition. Washington, DC:
American Psychological Association.
[doi:10.1037/0000165-000](https://doi.org/10.1037/0000165-000)

FAO (2024). *The State of World Fisheries and Aquaculture 2024 – Blue
Transformation in action*. Rome: FAO.
[doi:10.4060/cd0683en](https://doi.org/10.4060/cd0683en)（FAO出版物参考文献格式的范例）

FAO. FishStatJ. Rome: FAO.
<https://www.fao.org/fishery/statistics/software/fishstatj/en>（FAO出版物引用FishStat数据的格式为“FAO.
*年份*. FishStat: *数据集名称和年份*. \[Accessed on *日期*\]. In:
FishStatJ. Licence: CC-BY-4.0.”）

FAO. Global Capture Production. In: Fisheries and Aquaculture. Rome:
FAO.
<https://www.fao.org/fishery/collection/capture/en>（`fao_data_info()$citation`采用该页面“Cite
this content as”的格式）

**每份报告都引用的数据**

FAO. 2026. Global Capture Production 1950-2024, release 2026.1.0. In:
Fisheries and Aquaculture \[online\]. Rome. \[Cited 10 October 2026\].
<https://www.fao.org/fishery/collection/capture/en>. Licence: CC BY 4.0.

FAO (2026a). *Fishery and Aquaculture Statistics – Yearbook 2024*. FAO
Yearbook of Fishery and Aquaculture Statistics. Rome: FAO.
[doi:10.4060/ce1217en](https://doi.org/10.4060/ce1217en)

**默认章节引用的方法和软件**

Bai, J. and Perron, P. (2003). Computation and analysis of multiple
structural change models. *Journal of Applied Econometrics* 18(1): 1–22.
[doi:10.1002/jae.659](https://doi.org/10.1002/jae.659)

Kendall, M.G. (1975). *Rank correlation methods*. 4th edition. London:
Charles Griffin.

Kleisner, K., Zeller, D., Froese, R. and Pauly, D. (2013). Using global
catch data for inferences on the world’s marine fisheries. *Fish and
Fisheries* 14(3): 293–311.
[doi:10.1111/j.1467-2979.2012.00469.x](https://doi.org/10.1111/j.1467-2979.2012.00469.x)

Lin, H. (2026). faofishingreport: Analyse and report FAO Global Capture
Production statistics. R package version 0.5.0.
<https://linbojun99.github.io/faofishingreport/>

Mann, H.B. (1945). Nonparametric tests against trend. *Econometrica*
13(3): 245–259. [doi:10.2307/1907187](https://doi.org/10.2307/1907187)

R Core Team (2026). R: A language and environment for statistical
computing. Vienna: R Foundation for Statistical Computing.
<https://www.R-project.org/>

Rhoades, S.A. (1993). The Herfindahl-Hirschman index. *Federal Reserve
Bulletin* 79(3): 188–189.

Shannon, C.E. (1948). A mathematical theory of communication. *Bell
System Technical Journal* 27(3): 379–423.
[doi:10.1002/j.1538-7305.1948.tb01338.x](https://doi.org/10.1002/j.1538-7305.1948.tb01338.x)

Zeileis, A., Kleiber, C., Krämer, W. and Hornik, K. (2003). Testing and
dating of structural changes in practice. *Computational Statistics &
Data Analysis* 44(1–2): 109–123.
[doi:10.1016/S0167-9473(03)00030-6](https://doi.org/10.1016/S0167-9473(03)00030-6)

**其他章节引用的方法**

Alajmi, A., Ben-Hasan, A., Alrushaid, T., Vahabnezhad, A. and Pauly, D.
(2024). Mean temperature of the catch index can be masked by changes in
catch composition unrelated to ocean warming. *Fisheries Management and
Ecology* 31(1): e12659.
[doi:10.1111/fme.12659](https://doi.org/10.1111/fme.12659)

Caddy, J.F. (1993). Toward a comparative evaluation of human impacts on
fishery ecosystems of enclosed and semi-enclosed seas. *Reviews in
Fisheries Science* 1(1): 57–95.
[doi:10.1080/10641269309388535](https://doi.org/10.1080/10641269309388535)

Carlson, S.M. and Satterthwaite, W.H. (2011). Weakened portfolio effect
in a collapsed salmon population complex. *Canadian Journal of Fisheries
and Aquatic Sciences* 68(9): 1579–1589.
[doi:10.1139/f2011-084](https://doi.org/10.1139/f2011-084)

Cheung, W.W.L., Watson, R. and Pauly, D. (2013). Signature of ocean
warming in global fisheries catch. *Nature* 497(7449): 365–368.
[doi:10.1038/nature12156](https://doi.org/10.1038/nature12156)

Cottrell, R.S., Nash, K.L., Halpern, B.S., Remenyi, T.A., Corney, S.P.,
Fleming, A., Fulton, E.A., Hornborg, S., Johne, A., Watson, R.A. and
Blanchard, J.L. (2019). Food production shocks across land and sea.
*Nature Sustainability* 2(2): 130–137.
[doi:10.1038/s41893-018-0210-1](https://doi.org/10.1038/s41893-018-0210-1)

de Leiva Moreno, J.I., Agostini, V.N., Caddy, J.F. and Carocci, F.
(2000). Is the pelagic-demersal ratio from fishery landings a useful
proxy for nutrient availability? A preliminary data exploration for the
semi-enclosed seas around Europe. *ICES Journal of Marine Science*
57(4): 1091–1102.
[doi:10.1006/jmsc.2000.0705](https://doi.org/10.1006/jmsc.2000.0705)

FAO (2026b). *The State of World Fisheries and Aquaculture 2026 – Blue
Transformation: turning vision into impact*. Rome: FAO.
[doi:10.4060/cd8357en](https://doi.org/10.4060/cd8357en)

Froese, R., Demirel, N., Coro, G., Kleisner, K.M. and Winker, H. (2017).
Estimating fisheries reference points from catch and resilience. *Fish
and Fisheries* 18(3): 506–526.
[doi:10.1111/faf.12190](https://doi.org/10.1111/faf.12190)

Froese, R. and Pauly, D. (eds). FishBase. World Wide Web electronic
publication. <https://www.fishbase.se>（报告中引用实际使用的版本）

Froese, R., Winker, H., Coro, G., Palomares, M.L.D., Tsikliras, A.C.,
Dimarchopoulou, D., Touloumis, K., Demirel, N., Vianna, G.M.S.,
Scarcella, G., Schijns, R., Liang, C. and Pauly, D. (2023). New
developments in the analysis of catch time series as the basis for fish
stock assessments: The CMSY++ method. *Acta Ichthyologica et Piscatoria*
53: 173–189.
[doi:10.3897/aiep.53.105910](https://doi.org/10.3897/aiep.53.105910)

Garibaldi, L. and Limongelli, L. (2003). *Trends in oceanic captures and
clustering of large marine ecosystems: two studies based on the FAO
capture database*. FAO Fisheries Technical Paper No. 435. Rome: FAO.
<https://www.fao.org/4/y4449e/y4449e00.htm>

Gephart, J.A., Deutsch, L., Pace, M.L., Troell, M. and Seekell, D.A.
(2017). Shocks to fish production: Identification, trends, and
consequences. *Global Environmental Change* 42: 24–32.
[doi:10.1016/j.gloenvcha.2016.11.003](https://doi.org/10.1016/j.gloenvcha.2016.11.003)

Grainger, R.J.R. and Garcia, S.M. (1996). *Chronicles of marine fishery
landings (1950–1994): trend analysis and fisheries potential*. FAO
Fisheries Technical Paper No. 359. Rome: FAO.
<https://www.fao.org/4/w3244e/w3244e00.htm>

Hondorp, D.W., Breitburg, D.L. and Davias, L.A. (2010). Eutrophication
and Fisheries: Separating the Effects of Nitrogen Loads and Hypoxia on
the Pelagic-to-Demersal Ratio and other Measures of Landings
Composition. *Marine and Coastal Fisheries* 2(1): 339–361.
[doi:10.1577/C09-020.1](https://doi.org/10.1577/C09-020.1)

Huang, B., Thorne, P.W., Banzon, V.F., Boyer, T., Chepurin, G.,
Lawrimore, J.H., Menne, M.J., Smith, T.M., Vose, R.S. and Zhang, H.-M.
(2017). Extended Reconstructed Sea Surface Temperature, Version 5
(ERSSTv5): Upgrades, Validations, and Intercomparisons. *Journal of
Climate* 30(20): 8179–8205.
[doi:10.1175/JCLI-D-16-0836.1](https://doi.org/10.1175/JCLI-D-16-0836.1)

Lepš, J., Götzenberger, L., Valencia, E. and de Bello, F. (2019).
Accounting for long-term directional trends on year-to-year synchrony in
species fluctuations. *Ecography* 42(10): 1728–1741.
[doi:10.1111/ecog.04528](https://doi.org/10.1111/ecog.04528)

Loreau, M. and de Mazancourt, C. (2008). Species Synchrony and Its
Drivers: Neutral and Nonneutral Community Dynamics in Fluctuating
Environments. *The American Naturalist* 172(2): E48–E66.
[doi:10.1086/589746](https://doi.org/10.1086/589746)

Mullon, C., Fréon, P. and Cury, P. (2005). The dynamics of collapse in
world fisheries. *Fish and Fisheries* 6(2): 111–120.
[doi:10.1111/j.1467-2979.2005.00181.x](https://doi.org/10.1111/j.1467-2979.2005.00181.x)

Ong, J.J.L., Walter, J.A., Jensen, O.P. and Pinsky, M.L. (2021). Global
hotspots of coherent marine fishery catches. *Ecological Applications*
31(5): e02321. [doi:10.1002/eap.2321](https://doi.org/10.1002/eap.2321)

Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical
models using Gibbs sampling. In: *Proceedings of the 3rd International
Workshop on Distributed Statistical Computing (DSC 2003)*, Vienna,
pp. 1–10.
<https://www.r-project.org/conferences/DSC-2003/Proceedings/Plummer.pdf>

Pyper, B.J. and Peterman, R.M. (1998). Comparison of methods to account
for autocorrelation in correlation analyses of fish data. *Canadian
Journal of Fisheries and Aquatic Sciences* 55(9): 2127–2140.
[doi:10.1139/f98-104](https://doi.org/10.1139/f98-104)

Swartz, W., Sala, E., Tracey, S., Watson, R. and Pauly, D. (2010). The
Spatial Expansion and Ecological Footprint of Fisheries (1950 to
Present). *PLoS ONE* 5(12): e15143.
[doi:10.1371/journal.pone.0015143](https://doi.org/10.1371/journal.pone.0015143)

Thibaut, L.M. and Connolly, S.R. (2013). Understanding
diversity–stability relationships: towards a unified model of portfolio
effects. *Ecology Letters* 16(2): 140–150.
[doi:10.1111/ele.12019](https://doi.org/10.1111/ele.12019)

Wilberg, M.J. and Miller, T.J. (2007). Comment on “Impacts of
Biodiversity Loss on Ocean Ecosystem Services”. *Science* 316(5829):
1285.
[doi:10.1126/science.1137946](https://doi.org/10.1126/science.1137946)

Worm, B., Barbier, E.B., Beaumont, N., Duffy, J.E., Folke, C., Halpern,
B.S., Jackson, J.B.C., Lotze, H.K., Micheli, F., Palumbi, S.R., Sala,
E., Selkoe, K.A., Stachowicz, J.J. and Watson, R. (2006). Impacts of
Biodiversity Loss on Ocean Ecosystem Services. *Science* 314(5800):
787–790.
[doi:10.1126/science.1132294](https://doi.org/10.1126/science.1132294)
