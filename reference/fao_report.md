# Assessment Report of an FAO Fishing Area

This function runs the analyses of the package for one FAO major fishing
area and writes a Word report in Chinese or English: cover, title,
abstract, data sources and methods, results and conclusions with charts
and tables, references and appendix tables. The text is written from
templates filled in with the results, as a first draft: check it, and
complete the parts marked in grey, such as other data sources, the
discussion and other references.

## Usage

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

## Arguments

- area:

  The FAO major fishing area: its name, e.g. "Atlantic, Eastern
  Central", or its code, e.g. 34.

- ...:

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

- file:

  The file to write. Default is `reports/FAO<code>_report_<lang>.docx`
  in the working directory. The other formats get the same name with
  `.tex` and `.pdf`.

- format:

  The formats to write, one or more of "docx" (default: Word), "latex"
  (a `.tex` file with its figures in the folder `<name>_figures/` next
  to it, compiled with XeLaTeX; Chinese reports use the ctexart class)
  and "pdf" (the LaTeX file compiled to PDF with XeLaTeX, through the
  tinytex package when it is installed, which also installs missing
  LaTeX packages). The analyses run once for all formats, and the
  formats have the same text, figures, tables and references. Fonts are
  taken from those of the computer: for Chinese, FangSong, SimHei and
  KaiTi on Windows, STFangsong, Heiti SC and Kaiti SC on macOS, else the
  Fandol fonts of TeX Live.

- lang:

  The language of the report: "zh" (Chinese) or "en" (English). Default
  is NULL, the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("both" gives a Chinese report).

- title:

  The report title. Default is generated from the area, e.g. "Assessment
  of Marine Capture Production in FAO Major Fishing Area 34 (Atlantic,
  Eastern Central)".

- report_no:

  The number of the report, shown as "Report 1" (in Chinese reports with
  a Chinese numeral) and used in the figure and table numbers (1-1, 1-2,
  ...). Default is 1.

- author:

  The author on the cover and below the title. Default is NULL, which
  writes "Author" ("作者" in Chinese reports) as a placeholder.

- affiliation:

  The affiliation on the cover. Default is NULL, none.

- project:

  The text at the top of the cover. Default is NULL, which writes
  "Project Report" ("课题报告" in Chinese reports).

- date:

  The date on the cover. Default is today.

- font:

  The font of the text in Latin script. Default is "Times New Roman" in
  Chinese reports and "Arial" in English reports.

- font_zh:

  The font of the Chinese body text. Default is "FangSong".

- citation:

  The citation style of English reports: "apa" (default) or "fao" (the
  style of FAO publications). Chinese reports use numbered citations
  (GB/T 7714).

- top_n:

  The number of main species and countries in the charts and tables, and
  of species assessed with CMSY++ (unless `cmsy_top` is given). Default
  is 10.

- data_version:

  The FAO release to use, as in
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).
  Default is "latest".

- total, species, countries, groups, status, quality, diversity, fleets:

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

- shocks, collapse, synchrony, pelagic, mtc, distant:

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

- cmsy:

  A logical value. If TRUE, the main species are also assessed with
  CMSY++
  ([`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)),
  and the report has a part on the assessment, with the results of every
  species and appendix tables of the reference points, priors and their
  sources. Default is FALSE. The result of an earlier
  [`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)
  run can also be given, to write the report without running CMSY++
  again. CMSY++ needs JAGS: see
  [`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md).

## Value

Invisibly, a list of class `fao_report` with the paths of the files
written (`file`, named by format: `docx`, `latex`, `pdf`), the sections
in the report (`sections`) and the tables of the report as data frames:
`main_species` and `main_countries`, the species of the mean temperature
of the catch with their FishBase matches (`mtc_species`), the results of
all analyses (`analyses`), and with CMSY++ `cmsy_reference_points` (r,
k, MSY, Fmsy, Bmsy, B/Bmsy and F/Fmsy with 95% confidence limits),
`cmsy_priors` (resilience, r, k and B/k priors, their sources and the
links to FishBase or SeaLifeBase, the FishBase data, the CMSY++ code and
the FAO data) and the full CMSY++ result `cmsy`. Assign it, e.g.
`report <- fao_report(...)`, to look at the tables
(`View(report$cmsy_priors)`). With `cmsy = TRUE`, all CMSY++ results
(runs, charts and tables) are kept in the folder `cmsy/` next to the
report, with `cmsy_result.rds` to write the report again without running
CMSY++ (`cmsy = readRDS("cmsy/<area>/cmsy_result.rds")`).

## Details

Every analysis is a section that can be switched on or off (`total`,
`species`, ..., `cmsy`). The settings of the function of a section are
given with the section name as prefix, e.g. `species_h_value = 0.2` is
`h_value = 0.2` of
[`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md)
and `cmsy_top = 5` is `top = 5` of
[`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md).
The abstract, the methods, the conclusions and the references follow the
sections that are switched on.

Chinese reports follow the format of project reports: A4 pages; body
text in FangSong and Times New Roman, size 12 (xiao si), 1.5 line
spacing and a first-line indent of two characters; title in bold
FangSong, size 18; "abstract" in SimHei, size 15; section headings in
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

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
