# fao_report(): FAO渔区评估报告

英文原页：[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)

## 说明

该函数对一个FAO主要渔区运行本包的各项分析，并生成中文或英文的Word报告：封面、标题、摘要、数据来源与分析方法、结果与结论（含图表和表格）、参考文献和附表。报告文字由模板填入分析结果生成，是一份初稿：请核对文字，并补充以灰色标出的部分，如其他数据来源、讨论和其他参考文献。

English

This function runs the analyses of the package for one FAO major fishing
area and writes a Word report in Chinese or English: cover, title,
abstract, data sources and methods, results and conclusions with charts
and tables, references and appendix tables. The text is written from
templates filled in with the results, as a first draft: check it, and
complete the parts marked in grey, such as other data sources, the
discussion and other references.

## 用法

``` r

fao_report(
  area,
  ...,
  file = NULL,
  format = "docx",
  lang = NULL,
  title = NULL,
  report_no = 1,
  author = NULL,
  affiliation = NULL,
  project = NULL,
  date = Sys.Date(),
  font = NULL,
  font_zh = "FangSong",
  citation = c("apa", "fao"),
  top_n = 10,
  data_version = "latest",
  total = TRUE,
  species = TRUE,
  countries = TRUE,
  groups = TRUE,
  status = TRUE,
  quality = TRUE,
  diversity = TRUE,
  fleets = TRUE,
  shocks = FALSE,
  collapse = FALSE,
  synchrony = FALSE,
  pelagic = FALSE,
  mtc = FALSE,
  distant = FALSE,
  cmsy = FALSE
)
```

## 参数

- `area`:

  FAO主要渔区：名称（例如”Atlantic, Eastern Central”）或代码（例如34）。

  English

  The FAO major fishing area: its name, e.g. “Atlantic, Eastern
  Central”, or its code, e.g. 34.

- `...`:

  各项分析的设置，以章节名作为前缀命名：`total_`对应[`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md)，`species_`对应[`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md)，`countries_`对应[`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md)，`groups_`对应[`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md)，`status_`对应[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)，`quality_`对应[`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md)，`diversity_`对应[`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md)，`fleets_`对应[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md)，`shocks_`对应[`catch_shocks()`](https://linbojun99.github.io/faofishingreport/reference/catch_shocks.md)，`collapse_`对应[`collapse_analysis()`](https://linbojun99.github.io/faofishingreport/reference/collapse_analysis.md)（另有`collapse_seed`，即其随机序列的随机数种子，默认为1），`synchrony_`对应[`catch_synchrony()`](https://linbojun99.github.io/faofishingreport/reference/catch_synchrony.md)，`pelagic_`对应[`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/reference/pelagic_demersal.md)，`mtc_`对应[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md)（除非指定了其他版本，报告使用`mtc_version = "builtin"`，即本包自带的FishBase数据），`distant_`对应[`distant_water_profile()`](https://linbojun99.github.io/faofishingreport/reference/distant_water_profile.md)（另有`distant_countries`，即要介绍的远洋捕捞国数量，默认为3，即近5年在该渔区远洋渔获量最大的3个国家；也可以给出这些国家的英文名称），`cmsy_`对应[`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)（以及[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)）。例如`species_h_value = 0.2`、`countries_line_linewidth = 1`、`status_smooth = TRUE`、`cmsy_top = 5`或`cmsy_settings = list(n.iter = 100000)`。如果某个名称不是对应函数的参数，会报错并列出该函数的参数。报告自行设定的参数（`area_data`、`area_name`、`area`、`lang`、`table`和`pptx`）不能通过前缀更改，`pelagic_by`和`mtc_by`也不能设置（按分区的分析使用[`load_fao_divisions()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_divisions.md)的数据，属于不纳入报告的扩展功能）。

  English

  Settings of the analyses, named with the section as prefix: `total_`
  for
  [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md),
  `species_` for
  [`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md),
  `countries_` for
  [`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md),
  `groups_` for
  [`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md),
  `status_` for
  [`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md),
  `quality_` for
  [`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md),
  `diversity_` for
  [`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md),
  `fleets_` for
  [`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md),
  `shocks_` for
  [`catch_shocks()`](https://linbojun99.github.io/faofishingreport/reference/catch_shocks.md),
  `collapse_` for
  [`collapse_analysis()`](https://linbojun99.github.io/faofishingreport/reference/collapse_analysis.md)
  (and `collapse_seed`, the seed of its random series, default 1),
  `synchrony_` for
  [`catch_synchrony()`](https://linbojun99.github.io/faofishingreport/reference/catch_synchrony.md),
  `pelagic_` for
  [`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/reference/pelagic_demersal.md),
  `mtc_` for
  [`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md)
  (the report uses `mtc_version = "builtin"`, the FishBase data bundled
  with the package, unless another version is given), `distant_` for
  [`distant_water_profile()`](https://linbojun99.github.io/faofishingreport/reference/distant_water_profile.md)
  (and `distant_countries`, the number of distant-water countries
  profiled, default 3, or their names) and `cmsy_` for
  [`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)
  (and
  [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)).
  For example `species_h_value = 0.2`, `countries_line_linewidth = 1`,
  `status_smooth = TRUE`, `cmsy_top = 5` or
  `cmsy_settings = list(n.iter = 100000)`. A name that is not an
  argument of the function gives an error listing its arguments. The
  arguments the report sets itself (`area_data`, `area_name`, `area`,
  `lang`, `table` and `pptx`) cannot be changed, nor can `pelagic_by`
  and `mtc_by` (the analyses by division use the data of
  [`load_fao_divisions()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_divisions.md),
  an extension that is not part of the report).

- `file`:

  要写入的文件。默认为工作目录下的`reports/FAO<code>_report_<lang>.docx`。其他格式使用相同的文件名，扩展名为`.tex`和`.pdf`。

  English

  The file to write. Default is `reports/FAO<code>_report_<lang>.docx`
  in the working directory. The other formats get the same name with
  `.tex` and `.pdf`.

- `format`:

  要输出的格式，可以是”docx”（默认，Word）、“latex”（`.tex`文件，其图片保存在旁边的`<name>_figures/`文件夹中，用XeLaTeX编译；中文报告使用ctexart文档类）和”pdf”（用XeLaTeX把LaTeX文件编译为PDF；安装了tinytex包时通过它编译，并自动安装缺少的LaTeX宏包）中的一个或多个。TinyTeX只需安装一次：`install.packages("tinytex")`，然后[`tinytex::install_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html)；较旧的TinyTeX用[`tinytex::reinstall_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html)更新。各项分析只运行一次，各种格式的文字、图表、表格和参考文献完全相同。字体取自本机已安装的字体：中文在Windows上为仿宋、黑体和楷体（FangSong、SimHei、KaiTi），在macOS上为STFangsong、Heiti
  SC和Kaiti SC，其他系统使用TeX Live自带的Fandol字体。

  English

  The formats to write, one or more of “docx” (default: Word), “latex”
  (a `.tex` file with its figures in the folder `<name>_figures/` next
  to it, compiled with XeLaTeX; Chinese reports use the ctexart class)
  and “pdf” (the LaTeX file compiled to PDF with XeLaTeX, through the
  tinytex package when it is installed, which also installs missing
  LaTeX packages). Install TinyTeX once with
  `install.packages("tinytex")` and
  [`tinytex::install_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html);
  an old TinyTeX is updated with
  [`tinytex::reinstall_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html).
  The analyses run once for all formats, and the formats have the same
  text, figures, tables and references. Fonts are taken from those of
  the computer: for Chinese, FangSong, SimHei and KaiTi on Windows,
  STFangsong, Heiti SC and Kaiti SC on macOS, else the Fandol fonts of
  TeX Live.

- `lang`:

  报告的语言：“zh”（中文）或”en”（英文）。默认为NULL，即使用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)设置的语言（“both”生成中文报告）。

  English

  The language of the report: “zh” (Chinese) or “en” (English). Default
  is NULL, the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (“both” gives a Chinese report).

- `title`:

  报告标题。默认根据渔区生成，例如”FAO
  34渔区（中东大西洋）海洋捕捞产量评估”（英文报告为”Assessment of Marine
  Capture Production in FAO Major Fishing Area 34 (Atlantic, Eastern
  Central)“）。

  English

  The report title. Default is generated from the area, e.g. “Assessment
  of Marine Capture Production in FAO Major Fishing Area 34 (Atlantic,
  Eastern Central)”.

- `report_no`:

  报告编号，显示为”报告一”（中文报告使用汉字数字；英文报告为”Report
  1”），并用于图和表格的编号（1-1、1-2……）。默认为1。

  English

  The number of the report, shown as “Report 1” (in Chinese reports with
  a Chinese numeral) and used in the figure and table numbers (1-1, 1-2,
  …). Default is 1.

- `author`:

  封面和标题下方的作者。默认为NULL，此时写入占位文字”作者”（英文报告为”Author”）。

  English

  The author on the cover and below the title. Default is NULL, which
  writes “Author” (“作者” in Chinese reports) as a placeholder.

- `affiliation`:

  封面上的单位。默认为NULL，即不写单位。

  English

  The affiliation on the cover. Default is NULL, none.

- `project`:

  封面顶部的文字。默认为NULL，此时写入”课题报告”（英文报告为”Project
  Report”）。

  English

  The text at the top of the cover. Default is NULL, which writes
  “Project Report” (“课题报告” in Chinese reports).

- `date`:

  封面上的日期。默认为当天。

  English

  The date on the cover. Default is today.

- `font`:

  拉丁字母文字（西文和数字）的字体；英文报告的全部文字都使用该字体。默认中文报告为”Times
  New Roman”，英文报告为”Arial”。

  English

  The font of the text in Latin script. Default is “Times New Roman” in
  Chinese reports and “Arial” in English reports.

- `font_zh`:

  中文正文的字体（也用于标题、表格和参考文献；各级标题和题注使用黑体和楷体）。默认为”FangSong”（仿宋）。

  English

  The font of the Chinese body text. Default is “FangSong”.

- `citation`:

  英文报告的引文格式：“apa”（默认）或”fao”（FAO出版物的格式）。中文报告使用顺序编码制引文（GB/T
  7714）。

  English

  The citation style of English reports: “apa” (default) or “fao” (the
  style of FAO publications). Chinese reports use numbered citations
  (GB/T 7714).

- `top_n`:

  图表和表格中主要物种和国家的数量，以及用CMSY++评估的物种数（未指定`cmsy_top`时）。默认为10。

  English

  The number of main species and countries in the charts and tables, and
  of species assessed with CMSY++ (unless `cmsy_top` is given). Default
  is 10.

- `data_version`:

  使用的FAO数据版本，同[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)。默认为”latest”。

  English

  The FAO release to use, as in
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).
  Default is “latest”.

- `total, species, countries, groups, status, quality, diversity, fleets`:

  逻辑值：报告是否包含以下章节：总渔获量及其阶段划分（[`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md)）、主要物种（[`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md)）、主要捕捞国家（[`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md)）、ISSCAAP物种类群（[`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md)）、资源状态图（[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)）、数据质量（[`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md)）、渔获量的多样性与集中度（[`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md)），以及沿岸国与远洋捕捞国（[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md)）。默认均为TRUE。

  English

  Logical values: whether the report has the section on the total catch
  and its stages
  ([`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md)),
  the main species
  ([`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md)),
  the main fishing countries
  ([`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md)),
  the ISSCAAP groups
  ([`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md)),
  the stock status plots
  ([`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)),
  the data quality
  ([`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md)),
  the diversity and concentration of the catch
  ([`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md))
  and the coastal and distant-water fleets
  ([`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md)).
  All TRUE by default.

- `shocks, collapse, synchrony, pelagic, mtc, distant`:

  逻辑值：报告是否包含以下章节：渔获骤变（[`catch_shocks()`](https://linbojun99.github.io/faofishingreport/reference/catch_shocks.md)）、渔业崩溃（[`collapse_analysis()`](https://linbojun99.github.io/faofishingreport/reference/collapse_analysis.md)）、渔获量的同步性与组合效应（[`catch_synchrony()`](https://linbojun99.github.io/faofishingreport/reference/catch_synchrony.md)）、中上层/底层比（[`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/reference/pelagic_demersal.md)）、渔获平均温度（[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md)），以及该渔区主要远洋捕捞国的概况（[`distant_water_profile()`](https://linbojun99.github.io/faofishingreport/reference/distant_water_profile.md)）。默认均为FALSE。

  English

  Logical values: whether the report has the sections on catch shocks
  ([`catch_shocks()`](https://linbojun99.github.io/faofishingreport/reference/catch_shocks.md)),
  fishery collapses
  ([`collapse_analysis()`](https://linbojun99.github.io/faofishingreport/reference/collapse_analysis.md)),
  the synchrony of the catch and the portfolio effect
  ([`catch_synchrony()`](https://linbojun99.github.io/faofishingreport/reference/catch_synchrony.md)),
  the pelagic to demersal ratio
  ([`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/reference/pelagic_demersal.md)),
  the mean temperature of the catch
  ([`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md))
  and the profiles of the main distant-water fleets in the area
  ([`distant_water_profile()`](https://linbojun99.github.io/faofishingreport/reference/distant_water_profile.md)).
  All FALSE by default.

- `cmsy`:

  逻辑值。为TRUE时，还会用CMSY++（[`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)）评估主要物种，报告中增加资源评估部分，包括每个物种的结果，以及生物学参考点、先验及其来源的附表。默认为FALSE。也可以传入之前[`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)运行的结果，这样无需再次运行CMSY++即可生成报告；该结果必须与报告属于同一渔区和同一FAO数据版本（`data_version`），否则停止生成报告。CMSY++需要JAGS：见[`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)。

  English

  A logical value. If TRUE, the main species are also assessed with
  CMSY++
  ([`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)),
  and the report has a part on the assessment, with the results of every
  species and appendix tables of the reference points, priors and their
  sources. Default is FALSE. The result of an earlier
  [`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)
  run can also be given, to write the report without running CMSY++
  again; it must be of the same area and FAO release as the report
  (`data_version`), else the report stops. CMSY++ needs JAGS: see
  [`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md).

## 返回值

以不可见方式返回一个`fao_report`类的列表，包含所写文件的路径（`file`，按格式命名：`docx`、`latex`、`pdf`）、报告中包含的章节（`sections`），以及报告中以数据框形式给出的表格：`main_species`（主要物种）和`main_countries`（主要捕捞国家），渔获平均温度所用的物种及其FishBase匹配结果（`mtc_species`），全部分析的结果（`analyses`）；有CMSY++评估时，还包括`cmsy_reference_points`（r、k、MSY、Fmsy、Bmsy、B/Bmsy和F/Fmsy及其95%置信限）、`cmsy_priors`（恢复力、r、k和B/k先验及其来源，以及指向FishBase或SeaLifeBase、FishBase数据、CMSY++代码和FAO数据的链接）和完整的CMSY++结果`cmsy`。将结果赋值给一个变量，例如`report <- fao_report(...)`，即可查看这些表格（`View(report$cmsy_priors)`）。`cmsy = TRUE`时，CMSY++的全部结果（各次运行、图表和表格）保存在报告旁边的`cmsy/`文件夹中，其中的`cmsy_result.rds`可用于不再运行CMSY++而重新生成报告（`cmsy = readRDS("cmsy/<area>/cmsy_result.rds")`）。

English

Invisibly, a list of class `fao_report` with the paths of the files
written (`file`, named by format: `docx`, `latex`, `pdf`), the sections
in the report (`sections`) and the tables of the report as data frames:
`main_species` and `main_countries`, the species of the mean temperature
of the catch with their FishBase matches (`mtc_species`), the results of
all analyses (`analyses`), and with CMSY++ `cmsy_reference_points` (r,
k, MSY, Fmsy, Bmsy, B/Bmsy and F/Fmsy with 95% confidence limits),
`cmsy_priors` (resilience, r, k and B/k priors, their sources and the
links to FishBase or SeaLifeBase, the FishBase data, the CMSY++ code and
the FAO data) and the full CMSY++ result `cmsy`. Assign it,
e.g. `report <- fao_report(...)`, to look at the tables
(`View(report$cmsy_priors)`). With `cmsy = TRUE`, all CMSY++ results
(runs, charts and tables) are kept in the folder `cmsy/` next to the
report, with `cmsy_result.rds` to write the report again without running
CMSY++ (`cmsy = readRDS("cmsy/<area>/cmsy_result.rds")`).

## 详细说明

每项分析是报告中的一个章节，可以单独开启或关闭（`total`、`species`……`cmsy`）。章节所用函数的设置以章节名作为前缀传入，例如`species_h_value = 0.2`即[`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md)的`h_value = 0.2`，`cmsy_top = 5`即[`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)的`top = 5`。摘要、分析方法、结论和参考文献随开启的章节而变化。

中文报告采用课题报告的格式：A4纸；正文中文为仿宋、西文为Times New
Roman，小四号（12磅），1.5倍行距，首行缩进2字符；标题为仿宋加粗，小二号（18磅）；“摘要”为黑体，小三号（15磅）；一级标题为黑体、二级标题为楷体，均为四号（14磅）；题注为黑体，小四号（12磅），按报告编号（图1-1、表1-1），图题在图下方，表题在表上方；三线表，表内文字为仿宋，五号（10.5磅）；参考文献按引用顺序编号，正文中以上标方括号标注（GB/T
7714）。英文报告结构相同，只用一种字体（默认为Arial），采用APA格式或FAO出版物格式的著者-出版年制引文。物种和属的学名用斜体。

报告的文字模板位于本包源代码的`inst/extdata/report_text.csv`，参考文献位于`inst/extdata/report_references.csv`：修改这些文件即可更改措辞。

English

Every analysis is a section that can be switched on or off (`total`,
`species`, …, `cmsy`). The settings of the function of a section are
given with the section name as prefix, e.g. `species_h_value = 0.2` is
`h_value = 0.2` of
[`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md)
and `cmsy_top = 5` is `top = 5` of
[`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md).
The abstract, the methods, the conclusions and the references follow the
sections that are switched on.

Chinese reports follow the format of project reports: A4 pages; body
text in FangSong and Times New Roman, size 12 (xiao si), 1.5 line
spacing and a first-line indent of two characters; title in bold
FangSong, size 18; “abstract” in SimHei, size 15; section headings in
SimHei and subsection headings in KaiTi, size 14; captions in SimHei,
size 12, numbered by report (figure 1-1, table 1-1), below figures and
above tables; three-line tables with text in FangSong, size 10.5;
numbered references in superscript square brackets, in the order of
citation (GB/T 7714). English reports have the same structure, in one
font (Arial by default), with author-date citations in APA style or in
the style of FAO publications. Scientific names of species and genera
are in italics.

The text templates are in `inst/extdata/report_text.csv` and the
references in `inst/extdata/report_references.csv` of the package
source: edit them to change the wording.

## 示例

``` r

fao_report(34)                                  # Chinese or English, from fao_settings()
fao_report("Atlantic, Eastern Central", lang = "zh", author = "Hongyu Lin")
fao_report(34, lang = "en", font = "Times New Roman", citation = "fao")

# The other analyses of the package, with their settings
fao_report(34, lang = "zh", shocks = TRUE, collapse = TRUE, synchrony = TRUE, pelagic = TRUE,
           mtc = TRUE, distant = TRUE, collapse_method = "worm2006", distant_countries = 5)

# Only some sections, with settings of their functions
fao_report(34, lang = "zh", quality = FALSE, diversity = FALSE,
           species_h_value = 0.2, status_smooth = TRUE)

# With a CMSY++ assessment of the 10 main species (a few minutes)
report <- fao_report(34, lang = "zh", cmsy = TRUE)
report                                         # the tables it returns
View(report$cmsy_reference_points)             # r, k, MSY, B/Bmsy, ... with 95% limits
View(report$cmsy_priors)                       # priors, sources and links
fao_report(34, lang = "zh", top_n = 15, cmsy = TRUE, cmsy_top = 5)

# Reuse CMSY++ results to rewrite the report
fao_report(34, lang = "zh", cmsy = readRDS(file.path("reports", "cmsy", "Atlantic__Eastern_Central",
                                                      "cmsy_result.rds")))
```
