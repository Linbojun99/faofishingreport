# 📝 Assessment report: Word, LaTeX and PDF

📦 **Data**: [FAO Global Capture
Production](https://www.fao.org/fishery/collection/capture/en), release
**2026.1.0** (published by FAO on 2026-03-30; catches 1950–2024),
downloaded from
[Capture_2026.1.0.zip](https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip).
Licence [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
[`fao_data_info()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_info.md)
shows the release you use and how to cite it.

[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)
writes the assessment report of one FAO major fishing area as a Word
file (and, if asked, as LaTeX and PDF), in Chinese or English, in one
call. It runs the analyses of the package on the area, writes their
results into text made from templates, adds the charts and tables with
numbered captions, and lists the references it cites.

``` r

fao_report(34)                                                  # language of fao_settings()
fao_report("Atlantic, Eastern Central", lang = "zh", author = "Hongyu Lin")
fao_report(34, lang = "en", font = "Times New Roman", citation = "fao")
```

The report is a first draft. The parts only the author can write, such
as other data sources, the discussion of the conclusions and other
references, are marked in grey with “To be completed” (“待补充” in
Chinese reports). Check the generated text before you use it.

## 🚀 A first report

``` r

report <- fao_report(34, file = "FAO34_report_en.docx", lang = "en")
report
#> FAO report: /tmp/RtmpgRZd8i/faofishingreport_site_1bcd1c9c5875/FAO34_report_en.docx
#> Sections: total, species, countries, groups, status, quality, diversity, fleets
#>   $main_species           main species of the last five years (10 rows)
#>   $main_countries         main fishing countries of the last five years (10 rows)
#>   $analyses               results of the analyses: total, species, countries, groups, status, quality, diversity, fleets
```

The area is given by its code (`34`) or its English name
(`"Atlantic, Eastern Central"`). The call takes about half a minute and
reports its progress in messages (hidden on this page): each analysis,
then the file written. Without `file`, the report is saved as
`reports/FAO34_report_en.docx` in the working directory; on this page
the working directory is a temporary folder.

The analyses run in a temporary folder of their own, with charts at 300
dpi and no PowerPoint files. The charts end up in the Word file only, so
the report leaves no chart files in your folder; run the analysis
functions themselves to keep them (see [🌊 Fishing area
analysis](https://linbojun99.github.io/faofishingreport/articles/fishing-areas.md)).

[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)
returns the tables of the report invisibly, so assign the result. The
main species of the last five years:

``` r

show_table(report$main_species, digits = 1)
```

| species | species_en | scientific_name | mean_catch_t | share_pct | peak_year | pct_of_peak | trend_10_years |
|:---|:---|:---|---:|---:|:---|---:|:---|
| European pilchard(=Sardine) | European pilchard(=Sardine) | Sardina pilchardus | 1,041,854.9 | 20.4 | 2018 | 52.6 | none |
| Atlantic chub mackerel | Atlantic chub mackerel | Scomber colias | 414,307.8 | 8.1 | 2019 | 96.6 | up |
| Bonga shad | Bonga shad | Ethmalosa fimbriata | 348,662.7 | 6.8 | 2017 | 87.1 | none |
| Jack and horse mackerels nei | Jack and horse mackerels nei | Trachurus spp. | 263,295.6 | 5.1 | 2010 | 58.7 | none |
| Sardinellas nei | Sardinellas nei | Sardinella spp. | 246,900.5 | 4.8 | 2018 | 32.8 | down |
| Marine fishes nei | Marine fishes nei | Actinopterygii | 243,873.7 | 4.8 | 1995 | 60.5 | none |
| Madeiran sardinella | Madeiran sardinella | Sardinella maderensis | 243,476.6 | 4.8 | 2021 | 73.4 | none |
| Skipjack tuna | Skipjack tuna | Katsuwonus pelamis | 195,951.7 | 3.8 | 2018 | 69.2 | none |
| Round sardinella | Round sardinella | Sardinella aurita | 127,328.2 | 2.5 | 1991 | 34.9 | down |
| European anchovy | European anchovy | Engraulis encrasicolus | 124,334.4 | 2.4 | 1990 | 48.9 | up |

## 📑 What the report contains

In this order:

1.  **Cover**, on its own page without a page number: the project
    (“Project Report” unless `project` is given), the title, the report
    number, the author, the affiliation (only if given) and the date.
2.  **Title block**: “Report 1” (“报告一” in Chinese reports, with
    Chinese numerals), the title and the author, centred.
3.  **Abstract**: one paragraph with the area, the years and release of
    the FAO data, the topics and methods of the sections switched on,
    one sentence of results for the total catch, the main species, the
    main country, the stock status and CMSY++ (when these sections are
    on), and a closing recommendation.
4.  **1 Data and methods**
    - **1.1 Data sources**: the FAO Global Capture Production release
      used, the date FAO published it, the date it was downloaded and
      the link to the file; the years, and the numbers of countries and
      species items in the area; the ASFIS list of species, the FAO
      yearbook of statistics, and how coastal countries are defined. A
      grey note asks for other data sources (regional fisheries
      management organisations, national agencies, Sea Around Us, …).
    - **1.2 Methods**: one paragraph for each method used by the
      sections switched on (breakpoints, Mann-Kendall trend tests, stock
      status plots, diversity indices, …), with its references, and a
      last paragraph on the software (R and faofishingreport).
5.  **2 Results and conclusions**: one numbered subsection (2.1, 2.2, …)
    for each section switched on, with its text, figures and tables. The
    last subsection, **Conclusions**, has one numbered conclusion per
    section, (1), (2), …, and a grey note asking to complete them with
    stock assessments, the literature and management measures.
6.  **3 Stock assessment of the main species (CMSY++)**, with
    `cmsy = TRUE` only: an overview, one subsection per species and a
    summary (see [📉 CMSY++ assessment](#cmsy)).
7.  **References**: the references cited in the text, then a grey note
    on how to add others.
8.  **Appendix**: mean yearly catch by decade of the main species (Table
    A1) and of the main countries (Table A2), and with CMSY++ the
    reference points, the priors and their sources (Tables A3 to A5).

The grey notes are the paragraphs the author has to write or check:
other data sources, the conclusions, other references, the comparison of
each CMSY++ assessment with other assessments, and the preferred
temperatures of the species of the mean temperature of the catch. If a
chart could not be drawn, the report has “\[Figure not available\]” in
its place.

## 🔎 Inside the Word file

Every paragraph of the report has a named Word style (“FAO Body”, “FAO
Heading 1”, …). The format of the whole report can be changed in Word by
modifying a style, and the file can be read back in R with the officer
package:

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

The headings, styles “FAO Heading 1” and “FAO Heading 2”:

``` r

outline <- function(word) {
  headings <- word[word$style_name %in% c("FAO Heading 1", "FAO Heading 2"), ]
  cat(paste0(ifelse(headings$style_name == "FAO Heading 2", "    ", ""), headings$text), sep = "\n")
}
outline(word)
#> 1 Data and methods
#>     1.1 Data sources
#>     1.2 Methods
#> 2 Results and conclusions
#>     2.1 Catch trends and stages
#>     2.2 Main species
#>     2.3 Main fishing countries
#>     2.4 Composition by species group
#>     2.5 Stock status
#>     2.6 Data quality
#>     2.7 Diversity and concentration of the catch
#>     2.8 Coastal and distant-water fleets
#>     2.9 Conclusions
#> References
#> Appendix
```

The abstract, the first “FAO Body” paragraph after the heading
“Abstract”:

``` r

after_heading <- seq_len(nrow(word)) > which(word$style_name == "FAO Abstract Heading")
abstract <- word$text[after_heading & word$style_name == "FAO Body"][1]
show_table(data.frame(abstract))
```

| abstract |
|:---|
| FAO Major Fishing Area 34 (Atlantic, Eastern Central) is one of the important marine fishing areas of the world. Using the FAO Global Capture Production statistics for 1950–2024 (release 2026.1.0), this report analyses the catch trends, the main species, the main fishing countries, the composition of the catch, the status of the stocks, the quality of the data, the diversity of the catch and the catch of coastal and distant-water fleets, with breakpoint analysis, Mann-Kendall trend tests, stock status plots and diversity and concentration indices. In 2024, the catch in the area was 5.10 million t, 6.3% of the world’s marine capture production. The catch peaked in 2018 at 5.52 million t, and the catch of 2024 was 92.4% of the peak. Breakpoints divide the catch series into 4 stages (1950–1968, 1969–1987, 1988–2009 and 2010–2024). In the last five years, European pilchard(=Sardine) was the main species item, with 20.4% of the catch. Morocco was the main fishing country, with 27.7% of the catch of the last five years. In 2024, 36.1% of the stocks were over-exploited or collapsed. Monitoring and assessment of the main stocks, and the reporting of catch at species level, should be strengthened. |

The captions of the figures and tables, numbered by report:

``` r

captions <- word[word$style_name %in% c("FAO Caption", "FAO Table Caption"), ]
show_table(data.frame(style = captions$style_name, caption = captions$text), n = 20)
```

| style | caption |
|:---|:---|
| FAO Caption | Figure 1-1. Catch in FAO Major Fishing Area 34 (Atlantic, Eastern Central) and its breakpoints, 1950–2024 |
| FAO Caption | Figure 1-2. Share of FAO Major Fishing Area 34 (Atlantic, Eastern Central) in the world’s marine capture production |
| FAO Table Caption | Table 1-1. Main species items in FAO Major Fishing Area 34 (Atlantic, Eastern Central), 2020–2024 |
| FAO Caption | Figure 1-3. Catch of the main species items in FAO Major Fishing Area 34 (Atlantic, Eastern Central) |
| FAO Caption | Figure 1-4. Catch of the main species items in FAO Major Fishing Area 34 (Atlantic, Eastern Central) and their breakpoints |
| FAO Table Caption | Table 1-2. Main fishing countries in FAO Major Fishing Area 34 (Atlantic, Eastern Central), 2020–2024 |
| FAO Caption | Figure 1-5. Catch of the main fishing countries in FAO Major Fishing Area 34 (Atlantic, Eastern Central) |
| FAO Caption | Figure 1-6. Total catch of each country in FAO Major Fishing Area 34 (Atlantic, Eastern Central) |
| FAO Caption | Figure 1-7. Catch of the main fishing countries in FAO Major Fishing Area 34 (Atlantic, Eastern Central) and their breakpoints |
| FAO Caption | Figure 1-8. Catch in FAO Major Fishing Area 34 (Atlantic, Eastern Central) by ISSCAAP species group |
| FAO Caption | Figure 1-9. Stock status plot of FAO Major Fishing Area 34 (Atlantic, Eastern Central) (share of stocks) |
| FAO Caption | Figure 1-10. Stock status plot of FAO Major Fishing Area 34 (Atlantic, Eastern Central) (share of catch) |
| FAO Caption | Figure 1-11. Officially reported and estimated catch in FAO Major Fishing Area 34 (Atlantic, Eastern Central) |
| FAO Caption | Figure 1-12. Catch in FAO Major Fishing Area 34 (Atlantic, Eastern Central) by taxonomic level of identification |
| FAO Caption | Figure 1-13. Diversity and concentration of the catch in FAO Major Fishing Area 34 (Atlantic, Eastern Central) |
| FAO Caption | Figure 1-14. Catch of coastal countries and distant-water fleets in FAO Major Fishing Area 34 (Atlantic, Eastern Central) |
| FAO Table Caption | Table A1. Mean yearly catch (thousand t) of the main species items in FAO Major Fishing Area 34 (Atlantic, Eastern Central), by decade |
| FAO Table Caption | Table A2. Mean yearly catch (thousand t) of the main fishing countries in FAO Major Fishing Area 34 (Atlantic, Eastern Central), by decade |

The grey notes:

``` r

show_table(data.frame(note = word$text[word$style_name == "FAO Note"]))
```

| note |
|:---|
| \[To be completed: describe any other data used, e.g. from regional fisheries management organisations (ICCAT, CECAF, WCPFC, …), national agencies (e.g. NOAA), Sea Around Us, FishBase, Global Fishing Watch, distant-water fleet or commercial data, or the literature, with their period and processing.\] |
| \[To be completed: these conclusions were generated automatically from the FAO statistics; complete them with the stock assessments of regional fisheries management organisations, the literature and management measures.\] |
| \[To be completed: add the other references in alphabetical order of the first author, in APA style, e.g.: Author, A. A., Author, B. B., & Author, C. C. (Year). Title of the article. Journal Name, Volume(Issue), Pages. <https://doi.org/xx.xxx>\] |

And the list of references, style “FAO Reference”:

``` r

show_table(data.frame(reference = word$text[word$style_name == "FAO Reference"]), n = 30)
```

| reference |
|:---|
| Bai, J., & Perron, P. (2003). Computation and analysis of multiple structural change models. Journal of Applied Econometrics, 18(1), 1–22. <https://doi.org/10.1002/jae.659> |
| FAO. (2026a). Fishery and Aquaculture Statistics – Yearbook 2024. <https://doi.org/10.4060/ce1217en> |
| FAO. (2026b). FishStat: Global capture production 1950–2024 (Release 2026.1.0) \[Data set\]. <https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip> |
| Kendall, M. G. (1975). Rank correlation methods (4th ed.). Charles Griffin. |
| Kleisner, K., Zeller, D., Froese, R., & Pauly, D. (2013). Using global catch data for inferences on the world’s marine fisheries. Fish and Fisheries, 14(3), 293–311. <https://doi.org/10.1111/j.1467-2979.2012.00469.x> |
| Lin, H. (2026). faofishingreport: Analyse and report FAO Global Capture Production statistics. R package version 0.5.0 \[Computer software\]. <https://linbojun99.github.io/faofishingreport/> |
| Mann, H. B. (1945). Nonparametric tests against trend. Econometrica, 13(3), 245–259. <https://doi.org/10.2307/1907187> |
| R Core Team. (2026). R: A language and environment for statistical computing \[Computer software\]. R Foundation for Statistical Computing. <https://www.R-project.org/> |
| Rhoades, S. A. (1993). The Herfindahl-Hirschman index. Federal Reserve Bulletin, 79(3), 188–189. |
| Shannon, C. E. (1948). A mathematical theory of communication. Bell System Technical Journal, 27(3), 379–423. <https://doi.org/10.1002/j.1538-7305.1948.tb01338.x> |
| Zeileis, A., Kleiber, C., Krämer, W., & Hornik, K. (2003). Testing and dating of structural changes in practice. Computational Statistics & Data Analysis, 44(1-2), 109–123. <https://doi.org/10.1016/S0167-9473(03)00030-6> |

## 🧾 Arguments

| Argument | Default | Meaning |
|----|----|----|
| `area` | (required) | The FAO major fishing area: its code, e.g. `34`, or its English name, e.g. `"Atlantic, Eastern Central"`. An unknown area gives an error that lists the marine areas. |
| `...` |  | Settings of the analyses, named with the section as prefix, e.g. `species_h_value = 0.2` (see [⚙️ Settings of the analyses](#settings)) |
| `file` | `reports/FAO<code>_report_<lang>.docx` | The file to write; its folder is created if needed. The other formats get the same name with `.tex` and `.pdf`. |
| `format` | `"docx"` | One or more of `"docx"`, `"latex"` and `"pdf"` (see [🖨️ Word, LaTeX and PDF](#formats)) |
| `lang` | `NULL`: the language of [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md) | `"zh"` (Chinese) or `"en"` (English); `"both"` gives a Chinese report |
| `title` | made from the area | e.g. “Assessment of Marine Capture Production in FAO Major Fishing Area 34 (Atlantic, Eastern Central)” |
| `report_no` | `1` | Number of the report: “Report 1” (“报告一” in Chinese reports, with Chinese numerals up to 99), and the first number of the figures and tables (Figure 1-1, Table 1-1) |
| `author` | `NULL`: “Author” | The author on the cover, under the title and in the properties of the file |
| `affiliation` | `NULL`: no line | The affiliation on the cover |
| `project` | `NULL`: “Project Report” | The text at the top of the cover (“课题报告” in Chinese reports) |
| `date` | [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) | The date on the cover, written as YYYY-MM-DD; also the access date in the references to R and faofishingreport |
| `font` | `NULL`: “Times New Roman” in Chinese reports, “Arial” in English reports | The font of Latin script and numbers; in English reports, of all text |
| `font_zh` | `"FangSong"` | The Chinese font of the body text, title, tables and references; headings and captions stay in SimHei and KaiTi |
| `citation` | `"apa"` | Citation style of English reports: `"apa"` or `"fao"`. Chinese reports always use numbered citations (GB/T 7714). |
| `top_n` | `10` | Number of main species and countries in the tables and charts, and of species assessed with CMSY++ (unless `cmsy_top` is given) |
| `data_version` | `"latest"` | The FAO release to use, as in [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md) |
| `total`, `species`, `countries`, `groups`, `status`, `quality`, `diversity`, `fleets` | `TRUE` | Sections switched on by default (see [🧩 Sections](#sections)) |
| `shocks`, `collapse`, `synchrony`, `pelagic`, `mtc`, `distant` | `FALSE` | Sections switched off by default |
| `cmsy` | `FALSE` | `TRUE` adds a CMSY++ assessment of the main species; the result of an earlier run can also be given (see [📉 CMSY++ assessment](#cmsy)) |

## 🧩 Sections

Each analysis is a section of the report, switched on or off with the
argument of the same name. The settings of its function are given with
the prefix in the last but one column, and the last column lists the
references its text cites (full references at the end of this page).

| Switch | Default | Section of the report | Function | Prefix | References cited |
|----|----|----|----|----|----|
| `total` | `TRUE` | Catch trends and stages: total catch with its breakpoints, 10-year trend, share of the world’s marine catch | [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md) | `total_` | Bai and Perron (2003), Zeileis et al. (2003), Mann (1945), Kendall (1975) |
| `species` | `TRUE` | Main species of the last five years: table, charts, breakpoints and 10-year trends | [`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md) | `species_` | as `total` |
| `countries` | `TRUE` | Main fishing countries of the last five years: table, charts, map, breakpoints and trends | [`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md) | `countries_` | as `total` |
| `groups` | `TRUE` | Composition by ISSCAAP species group in the last year | [`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md) | `groups_` | none |
| `status` | `TRUE` | Stock status plots | [`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md) | `status_` | Kleisner et al. (2013) |
| `quality` | `TRUE` | Data quality: reported and estimated catch, taxonomic level | [`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md) | `quality_` | none |
| `diversity` | `TRUE` | Diversity and concentration of the catch (Shannon index, HHI) | [`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md) | `diversity_` | Shannon (1948), Rhoades (1993) |
| `fleets` | `TRUE` | Catch of coastal countries and distant-water fleets | [`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md) | `fleets_` | none |
| `shocks` | `FALSE` | Catch shocks | [`catch_shocks()`](https://linbojun99.github.io/faofishingreport/reference/catch_shocks.md) | `shocks_` | Cottrell et al. (2019), or Gephart et al. (2017) with `shocks_method = "gephart2017"` |
| `collapse` | `FALSE` | Fishery collapses, compared with random series | [`collapse_analysis()`](https://linbojun99.github.io/faofishingreport/reference/collapse_analysis.md) | `collapse_`, and `collapse_seed` | Mullon et al. (2005), or Worm et al. (2006) with `collapse_method = "worm2006"`; Wilberg and Miller (2007) |
| `synchrony` | `FALSE` | Synchrony of the catch and portfolio effect | [`catch_synchrony()`](https://linbojun99.github.io/faofishingreport/reference/catch_synchrony.md) | `synchrony_` | Loreau and de Mazancourt (2008), Thibaut and Connolly (2013), Carlson and Satterthwaite (2011), Ong et al. (2021), Lepš et al. (2019) |
| `pelagic` | `FALSE` | Pelagic to demersal ratio (P/D) | [`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/reference/pelagic_demersal.md) | `pelagic_` | Caddy (1993) and de Leiva Moreno et al. (2000), or Grainger and Garcia (1996) with `pelagic_definition = "grainger_garcia"`; Hondorp et al. (2010) |
| `mtc` | `FALSE` | Mean temperature of the catch | [`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md) | `mtc_` | Cheung et al. (2013), FishBase (Froese and Pauly), Huang et al. (2017) with the ERSST v5 data, Pyper and Peterman (1998), Alajmi et al. (2024) |
| `distant` | `FALSE` | Profiles of the main distant-water countries in the area | [`distant_water_profile()`](https://linbojun99.github.io/faofishingreport/reference/distant_water_profile.md) | `distant_`, and `distant_countries` | Swartz et al. (2010), Garibaldi and Limongelli (2003) |
| `cmsy` | `FALSE` | Stock assessment of the main species, a part of its own | [`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md) (and [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)) | `cmsy_` | Froese et al. (2023), Froese et al. (2017), FishBase (Froese and Pauly), Plummer (2003), FAO (2026b) |

Every report also cites the FAO release used, the FAO yearbook of
statistics (FAO 2026a), R and faofishingreport. The analyses themselves
are described on the other pages of this site: [🌊 Fishing area
analysis](https://linbojun99.github.io/faofishingreport/articles/fishing-areas.md),
[📈 Composition, stock status and
trends](https://linbojun99.github.io/faofishingreport/articles/status-trends.md),
[🔍 Data quality, diversity and
fleets](https://linbojun99.github.io/faofishingreport/articles/quality-diversity-fleets.md),
[🌡️ Sea temperature and catch
temperature](https://linbojun99.github.io/faofishingreport/articles/sst-mtc.md)
and [📉 CMSY++ stock
assessment](https://linbojun99.github.io/faofishingreport/articles/cmsy.md).

The rest of the report follows the sections switched on:

- the **abstract** lists their topics and methods, and has a sentence of
  results for `total`, `species`, `countries`, `status` and `cmsy`;
- **1.2 Methods** has the paragraphs of their methods only (`groups`,
  `quality` and `fleets` need none);
- the subsections of **2 Results and conclusions** and the figures and
  tables are numbered in the order of the sections, and the
  **conclusions** have one item per section;
- the **references** list only what the text cites, e.g. Shannon (1948)
  only with `diversity`;
- the **appendix** has Tables A1 and A2 only with `species` and
  `countries`, and the CMSY++ tables only with `cmsy`.

The total catch is always analysed, as it gives the data of the area,
but its section is written only with `total = TRUE`.

## 🎛️ Fewer or more sections

Three sections switched off, the pelagic to demersal ratio switched on,
and stock status plots with smoothed catches:

``` r

short <- fao_report(34, file = "FAO34_short.docx", lang = "en", quality = FALSE, diversity = FALSE,
                    fleets = FALSE, pelagic = TRUE, status_smooth = TRUE)
short
#> FAO report: /tmp/RtmpgRZd8i/faofishingreport_site_1bcd1c9c5875/FAO34_short.docx
#> Sections: total, species, countries, groups, status, pelagic
#>   $main_species           main species of the last five years (10 rows)
#>   $main_countries         main fishing countries of the last five years (10 rows)
#>   $analyses               results of the analyses: total, species, countries, groups, status, pelagic
short_word <- officer::docx_summary(officer::read_docx(short$file))
outline(short_word)
#> 1 Data and methods
#>     1.1 Data sources
#>     1.2 Methods
#> 2 Results and conclusions
#>     2.1 Catch trends and stages
#>     2.2 Main species
#>     2.3 Main fishing countries
#>     2.4 Composition by species group
#>     2.5 Stock status
#>     2.6 Pelagic to demersal ratio
#>     2.7 Conclusions
#> References
#> Appendix
```

The results now have six subsections and the conclusions six items. The
abstract and the methods name the P/D ratio, and the references have
Caddy (1993), de Leiva Moreno et al. (2000) and Hondorp et al. (2010)
instead of Shannon (1948) and Rhoades (1993):

``` r

show_table(data.frame(reference = short_word$text[short_word$style_name == "FAO Reference"]), n = 30)
```

| reference |
|:---|
| Bai, J., & Perron, P. (2003). Computation and analysis of multiple structural change models. Journal of Applied Econometrics, 18(1), 1–22. <https://doi.org/10.1002/jae.659> |
| Caddy, J. F. (1993). Toward a comparative evaluation of human impacts on fishery ecosystems of enclosed and semi‐enclosed seas. Reviews in Fisheries Science, 1(1), 57–95. <https://doi.org/10.1080/10641269309388535> |
| de Leiva Moreno, J. I., Agostini, V. N., Caddy, J. F., & Carocci, F. (2000). Is the pelagic-demersal ratio from fishery landings a useful proxy for nutrient availability? A preliminary data exploration for the semi-enclosed seas around Europe. ICES Journal of Marine Science, 57(4), 1091–1102. <https://doi.org/10.1006/jmsc.2000.0705> |
| FAO. (2026a). Fishery and Aquaculture Statistics – Yearbook 2024. <https://doi.org/10.4060/ce1217en> |
| FAO. (2026b). FishStat: Global capture production 1950–2024 (Release 2026.1.0) \[Data set\]. <https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip> |
| Hondorp, D. W., Breitburg, D. L., & Davias, L. A. (2010). Eutrophication and Fisheries: Separating the Effects of Nitrogen Loads and Hypoxia on the Pelagic-to-Demersal Ratio and other Measures of Landings Composition. Marine and Coastal Fisheries, 2(1), 339–361. <https://doi.org/10.1577/C09-020.1> |
| Kendall, M. G. (1975). Rank correlation methods (4th ed.). Charles Griffin. |
| Kleisner, K., Zeller, D., Froese, R., & Pauly, D. (2013). Using global catch data for inferences on the world’s marine fisheries. Fish and Fisheries, 14(3), 293–311. <https://doi.org/10.1111/j.1467-2979.2012.00469.x> |
| Lin, H. (2026). faofishingreport: Analyse and report FAO Global Capture Production statistics. R package version 0.5.0 \[Computer software\]. <https://linbojun99.github.io/faofishingreport/> |
| Mann, H. B. (1945). Nonparametric tests against trend. Econometrica, 13(3), 245–259. <https://doi.org/10.2307/1907187> |
| R Core Team. (2026). R: A language and environment for statistical computing \[Computer software\]. R Foundation for Statistical Computing. <https://www.R-project.org/> |
| Zeileis, A., Kleiber, C., Krämer, W., & Hornik, K. (2003). Testing and dating of structural changes in practice. Computational Statistics & Data Analysis, 44(1-2), 109–123. <https://doi.org/10.1016/S0167-9473(03)00030-6> |

The methods paragraphs describe the default settings of each analysis,
e.g. the inclusion criteria of the stock status plots and a minimum
segment of 15% for the breakpoints. When you change such settings
(`status_smooth = TRUE`, `status_min_catch = 10000`,
`species_h_value = 0.2`, …), the results follow them; describe the
change in 1.2 Methods yourself.

## ⚙️ Settings of the analyses

The function of each section has its own arguments. Give them to
[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)
with the name of the section and “\_” in front: `species_h_value = 0.2`
is `h_value = 0.2` of
[`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md).
Your settings win over the values the report sets.

| In [`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md) | Goes to | Effect |
|----|----|----|
| `species_h_value = 0.2` | `fishing_area_species(h_value = 0.2)` | Breakpoints of the main species with a minimum segment of 20% of the series |
| `countries_line_linewidth = 1` | `fishing_area_country(line_linewidth = 1)` | Thinner lines in the charts of the countries |
| `status_smooth = TRUE` | `stock_status(smooth = TRUE)` | 3-year running mean of the catch before the stocks are classified |
| `status_min_catch = 10000` | `stock_status(min_catch = 10000)` | Only stocks with a total catch of at least 10 000 t |
| `shocks_method = "gephart2017"` | `catch_shocks(method = "gephart2017")` | Shocks as defined by Gephart et al. (2017), with its methods text and reference |
| `collapse_method = "worm2006"` | `collapse_analysis(method = "worm2006")` | Collapses as defined by Worm et al. (2006): methods, results, chart and conclusion change |
| `collapse_seed = 2` | the random number generator, before [`collapse_analysis()`](https://linbojun99.github.io/faofishingreport/reference/collapse_analysis.md) | Other random series for the null model (default 1, so the report gives the same numbers every time); the random state of the session is restored afterwards |
| `synchrony_by = "country"` | `catch_synchrony(by = "country")` | Synchrony of the countries instead of the species |
| `pelagic_definition = "grainger_garcia"` | `pelagic_demersal(definition = "grainger_garcia")` | Groups of Grainger and Garcia (1996) |
| `mtc_version = "builtin"` | `mean_temperature_catch(version = "builtin")` | The report default: FishBase data bundled with the package, nothing downloaded |
| `mtc_version = "latest"` | `mean_temperature_catch(version = "latest")` | The latest FishBase release, downloaded if needed |
| `distant_countries = 5` | [`distant_water_profile()`](https://linbojun99.github.io/faofishingreport/reference/distant_water_profile.md), once per country | Profiles of the five countries with the largest distant-water catch in the area over the last five years (default 3), or of the countries named, e.g. `distant_countries = c("Russian Federation", "Belize")` |
| `cmsy_top = 5` | `cmsy_area(top = 5)` | Five species assessed with CMSY++ (default `top_n`) |
| `cmsy_settings = list(n.iter = 100000)` | `cmsy_area(settings = ...)` | CMSY++ settings, see [`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md) |

`collapse_seed` and `distant_countries` are settings of the report
itself. The `cmsy_` prefix also takes the arguments of
[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md),
such as `cmsy_min_years`. The arguments that the report sets itself
(`area`, `area_data`, `area_name`, `lang`, `pptx` and `table`) cannot be
given. Settings of a section that is switched off have no effect.

A setting must be named, start with the name of a section, and be an
argument of the section’s function. Otherwise
[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)
stops before any analysis is run, and the error lists what is allowed:

``` r

fao_report(34, species_h = 0.2)
#> Error:
#> ! fao_report(): species_h would set h, which is not an argument of fishing_area_species(). Its arguments: rank_range, plot, timeseries_analysis, line_width, point_size, point_alpha, point_shape, line_alpha, axis_text_size, axis_title_size, strip_text_size, axis_text_face, axis_title_face, strip_text_face, smooth_method, smooth_linetype, line_linewidth, text_hjust, text_vjust, text_size, text_color, facet_ncol, h_value, ZH
fao_report(34, speceis_h_value = 0.2)
#> Error:
#> ! Unknown argument of fao_report(): speceis_h_value. Settings of an analysis start with its section: total_, species_, countries_, groups_, status_, quality_, diversity_, fleets_, shocks_, collapse_, synchrony_, pelagic_, mtc_, distant_, cmsy_
```

## 📐 Format

Chinese reports follow the usual format of Chinese project reports:

| Element | Format |
|----|----|
| Page | A4, portrait; margins 2.54 cm at top and bottom, 3.17 cm left and right |
| Body text | FangSong for Chinese and Times New Roman for Latin script and numbers, size 12 (小四), line spacing 1.5, first-line indent of two characters, justified |
| Title block | “报告一” and the title in FangSong, size 18 (小二), bold, centred; the author below in size 12 |
| “摘　要” | SimHei, size 15 (小三), centred |
| Section headings (1, 2, …) | SimHei, size 14 (四号) |
| Subsection headings (1.1, 2.1, …) | KaiTi, size 14 (四号) |
| Captions | SimHei, size 12 (小四), centred, numbered by report: 图1-1 below the figure, 表1-1 above the table; 附表1 in the appendix |
| Tables | Three-line tables (top and bottom rules of 1.5 pt, a rule of 0.75 pt below the header), FangSong and Times New Roman size 10.5 (五号), single line spacing |
| Citations | Superscript numbers in square brackets, in the order of first citation: \[1\], \[3,4\], \[5-7\] (GB/T 7714) |
| References | Numbered \[1\], \[2\], … in the order of citation, GB/T 7714 format, with a hanging indent |
| Spaces | No spaces between Chinese characters and Latin letters or numbers, except after the number of a heading or caption (“2.1 渔获量变化与阶段划分”, “图1-1 …”) |
| Page numbers | Centred in the footer, size 10.5 (五号); none on the cover, 1 on the title page |
| Cover | Project in SimHei size 26 (一号) bold, title size 22 (二号) bold, other lines size 16 (三号) |

English reports have the same page, structure, numbering and tables, in
one font for all text, Arial unless `font` is given
(e.g. `font = "Times New Roman"`). Headings are bold, size 14 for
sections and 12 for subsections. Captions start with a bold label:
“**Figure 1-1.** Catch in …”, “**Table 1-1.** Main species items …”.
Citations are author-date, in APA style (default) or in the style of FAO
publications (`citation = "fao"`).

In both languages, scientific names of species and genera are in
italics, in the text and in the tables: *Sardina pilchardus*,
*Trachurus* spp. (with “spp.” upright). Names of families and higher
groups stay upright, e.g. Actinopterygii. Symbols are in italics as well
(*P* \< 0.05, *H*). The grey notes and the references are written as
they are, without the Chinese spacing rule.

The headings have outline levels 1 and 2, so they show in the navigation
pane of Word and in a table of contents inserted in Word. The title and
author are also stored in the properties of the file.

## 🖨️ Word, LaTeX and PDF

`format` chooses the files to write: `"docx"` (default), `"latex"` and
`"pdf"`, one or more. The analyses run once, and all formats have the
same text, figures, tables, captions and references.

``` r

fao_report(34, lang = "zh", format = c("docx", "pdf"))      # Word and PDF
fao_report(34, lang = "en", format = "latex")                # the LaTeX source only
report <- fao_report(34, format = c("docx", "latex", "pdf"))
report$file                                                  # named docx, latex and pdf
```

| `format` | Files written (for `fao_report(34, lang = "zh")`) | What it is |
|----|----|----|
| `"docx"` | `reports/FAO34_report_zh.docx` | The Word file described in this article |
| `"latex"` | `reports/FAO34_report_zh.tex` and the folder `reports/FAO34_report_zh_figures/` | The XeLaTeX source of the same report, with its figures as PNG files. Chinese reports use the `ctexart` class, English reports the `article` class. |
| `"pdf"` | `reports/FAO34_report_zh.pdf` (and the `.tex`) | The LaTeX source compiled with XeLaTeX, with bookmarks for the headings |

The LaTeX version follows the format of the Word file (see [📐
Format](#format)): A4 pages with the same margins, the same fonts and
sizes, line spacing 1.5, three-line tables (booktabs, single line
spacing, columns sized to their contents and running over pages),
numbered captions, the cover without page number and pages numbered from
1 on the title page. The fonts are those installed on the computer:

|  | Windows | macOS | Linux |
|----|----|----|----|
| Chinese body text (仿宋) | FangSong | STFangsong | FandolFang (TeX Live) |
| Chinese headings and captions (黑体) | SimHei | Heiti SC | FandolHei |
| Chinese subsection headings (楷体) | KaiTi | Kaiti SC | FandolKai |
| Latin text and numbers | `font`: Times New Roman in Chinese reports, Arial in English reports (else Liberation Serif or Sans, else Latin Modern) |  |  |

Chinese fonts have no bold face: as in Word, bold Chinese text (the
titles) is made by thickening the strokes.

**Compiling.** With the tinytex package (`install.packages("tinytex")`,
then
[`tinytex::install_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html)
if you have no LaTeX), the PDF is made with
`tinytex::latexmk(engine = "xelatex")`, which installs missing LaTeX
packages (ctex, xeCJK, booktabs, longtable, needspace, xurl, …) on the
first run. Without tinytex, `xelatex` must be on the system path. To
change the text of the LaTeX version, edit the `.tex` file and compile
it again, e.g. `tinytex::xelatex("reports/FAO34_report_zh.tex")`.

If tinytex cannot install a package because your TeX Live is older than
the online repository (“Local TeX Live (2024) is older than remote
repository”), update TeX Live
([`tinytex::reinstall_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html)),
or install the packages from the archive of your TeX Live year:
`tlmgr --repository https://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2024/tlnet-final install ctex`.

## 🔖 Citations and references

The references are listed only when the text cites them. The FAO release
used, R and faofishingreport are made from the data and the R session
(release, publication and access dates, versions); FishBase is cited
with the version used when the report has CMSY++ or the mean temperature
of the catch; all other references come from
`inst/extdata/report_references.csv`.

| Style | Citation in the text | List of references |
|----|----|----|
| Chinese reports, GB/T 7714 | superscript \[7\] | in the order of citation, numbered |
| `citation = "apa"`, APA 7th edition | (Kleisner et al., 2013); (Bai & Perron, 2003; Zeileis et al., 2003) | alphabetical; same authors and year as 2026a, 2026b |
| `citation = "fao"`, FAO publications | (Kleisner et al., 2013); (Bai and Perron, 2003) | alphabetical; same authors and year as 2026a, 2026b |

The same two references in each style, as the report writes them (FAO
release 2026.1.0, downloaded on 10 October 2026):

| Style | Kleisner et al. (2013) | FAO data |
|----|----|----|
| GB/T 7714 | Kleisner K, Zeller D, Froese R, et al. Using global catch data for inferences on the world’s marine fisheries\[J\]. Fish and Fisheries, 2013, 14(3): 293-311. DOI: 10.1111/j.1467-2979.2012.00469.x. | FAO. FishStat: Global capture production 1950–2024 (release 2026.1.0)\[DB/OL\]. Rome: FAO, 2026\[2026-10-10\]. <https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip>. |
| APA | Kleisner, K., Zeller, D., Froese, R., & Pauly, D. (2013). Using global catch data for inferences on the world’s marine fisheries. *Fish and Fisheries*, *14*(3), 293–311. <https://doi.org/10.1111/j.1467-2979.2012.00469.x> | FAO. (2026). *FishStat: Global capture production 1950–2024* (Release 2026.1.0) \[Data set\]. <https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip> |
| FAO | Kleisner, K., Zeller, D., Froese, R. & Pauly, D. 2013. Using global catch data for inferences on the world’s marine fisheries. *Fish and Fisheries*, 14(3): 293–311. <https://doi.org/10.1111/j.1467-2979.2012.00469.x> | FAO. 2026. FishStat: Global capture production 1950–2024 (release 2026.1.0). \[Accessed on 10 October 2026\]. In: FishStatJ. Available at: <https://www.fao.org/fishery/statistics/software/fishstatj/en>. Licence: CC-BY-4.0. |

In GB/T 7714, the letter in square brackets gives the type of document:
\[J\] journal article, \[M\] book, \[C\] proceedings, \[R\] report,
\[DB/OL\] online database, \[CP/OL\] online computer program; up to
three authors are listed, more as the first three and “et al.”. The FAO
style follows the reference lists of FAO publications (e.g. FAO 2024),
and cites FishStat data as these publications do, with the access date,
FishStatJ and the licence.

## 📉 CMSY++ assessment

With `cmsy = TRUE`, the main species are also assessed with CMSY++
([`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)):
the `top_n` species items identified to species with the largest catch
of the last ten years, or `cmsy_top` of them. CMSY++ needs JAGS,
installed once with
[`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md);
without it,
[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)
stops with a message. The assessment takes a few minutes.

``` r

cmsy_setup()                          # once: JAGS and the R packages of CMSY++
report <- fao_report(34, lang = "en", cmsy = TRUE)
report
#> FAO report: .../reports/FAO34_report_en.docx
#> Sections: total, species, countries, groups, status, quality, diversity, fleets, cmsy
#>   $main_species           main species of the last five years (10 rows)
#>   $main_countries         main fishing countries of the last five years (10 rows)
#>   $cmsy_reference_points  CMSY++ biological reference points with 95% confidence limits (10 rows)
#>   $cmsy_priors            CMSY++ priors, their sources and links (10 rows)
#>   $cmsy                   CMSY++ result (folder .../reports/cmsy/Atlantic__Eastern_Central)
#>   $analyses               results of the analyses: total, species, ..., fleets, cmsy

View(report$cmsy_reference_points)    # r, k, MSY, Fmsy, Bmsy, B/Bmsy and F/Fmsy with 95% limits
View(report$cmsy_priors)              # priors, their sources and links

# 15 main species and countries in the tables, 5 species assessed, longer MCMC chains
fao_report(34, lang = "en", top_n = 15, cmsy = TRUE, cmsy_top = 5,
           cmsy_settings = list(n.iter = 100000))
```

The report then has an abstract sentence on the stocks, a methods
paragraph on CMSY++, and a part of its own after the results:

    3 Stock assessment of the main species (CMSY++)
        3.1 Overview
        3.2 European pilchard(=Sardine) (Sardina pilchardus)
        3.3 Round sardinella (Sardinella aurita)
        ...
        3.12 Summary

- **Overview**: how many species were assessed (and which could not be),
  how many stocks were maximally sustainably fished (B/Bmsy ≥ 0.8) or
  overfished (B/Bmsy \< 0.8) in their last year, as in FAO (2026b), and
  how many lay in the green (B/Bmsy ≥ 1 and F/Fmsy ≤ 1) and red (B/Bmsy
  \< 1 and F/Fmsy \> 1) quadrants of the Kobe plot; a table of the
  results (years, r, k, MSY, B/Bmsy, F/Fmsy, status), the Kobe plot and
  B/Bmsy of all stocks.
- **One subsection per species**: r, k and MSY with their 95% confidence
  limits, B/Bmsy and F/Fmsy in the last year, the catch as a percentage
  of MSY, the status, the probabilities of the four Kobe quadrants, the
  CMSY++ chart of the species, and a grey note asking to compare the
  result with other assessments and discuss the priors.
- **Summary**, and in the appendix: **Table A3** biological reference
  points (r, k, MSY, Fmsy, Bmsy, B/Bmsy, F/Fmsy) with 95% confidence
  limits, **Table A4** the priors (resilience, ranges of r and k, B/k
  priors of the first, an intermediate and the last year) and **Table
  A5** their sources (source of the r prior, source of the B/k priors,
  FishBase or SeaLifeBase page of the species), followed by the links to
  the FishBase data, the CMSY++ code and user guide, and the FAO data.

All CMSY++ results (input files, runs, charts and tables) are kept in
the folder `cmsy/` next to the report,
e.g. `reports/cmsy/Atlantic__Eastern_Central/`, with `cmsy_result.rds`.
To write the report again, e.g. with other sections or after editing the
templates, give that result to `cmsy` instead of `TRUE`; CMSY++ does not
run again and the `cmsy_` settings are not used. The result of
[`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)
can be given in the same way:

``` r

cmsy34 <- readRDS(file.path("reports", "cmsy", "Atlantic__Eastern_Central", "cmsy_result.rds"))
fao_report(34, lang = "en", cmsy = cmsy34, pelagic = TRUE)

area34 <- cmsy_area("Atlantic, Eastern Central", top = 10)
fao_report(34, lang = "en", cmsy = area34)
```

The charts of the CMSY++ part come from the run, in the language it was
run in. See [📉 CMSY++ stock
assessment](https://linbojun99.github.io/faofishingreport/articles/cmsy.md)
for the method, its settings and its limits: FAO data have catches only,
so the results are indicative.

## 📦 What fao_report() returns

A list of class `fao_report`, returned invisibly:

| Element | Content |
|----|----|
| `file` | Paths of the files written, named `docx`, `latex` and `pdf` |
| `sections` | The sections switched on |
| `main_species` | Main species of the last five years: name in the report language, English and scientific names, mean yearly catch (t), share of the catch (%), peak year, catch of the last year as % of the peak, 10-year trend (“up”, “down” or “none”) |
| `main_countries` | The same for the main fishing countries |
| `mtc_species` | With `mtc = TRUE`: the species of the mean temperature of the catch, their FishBase matches and preferred temperatures |
| `analyses` | The result of each analysis, named by section, e.g. `analyses$status` is the result of [`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md) |
| `cmsy` | With CMSY++: the full result of [`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md) |
| `cmsy_reference_points` | With CMSY++: one row per stock with r, k, MSY, Fmsy, Bmsy, B/Bmsy and F/Fmsy, each with its lower and upper 95% confidence limits (`_lcl`, `_ucl`), the last catch, the probabilities of the Kobe quadrants and the status |
| `cmsy_priors` | With CMSY++: resilience and its source, prior ranges of r and k, the MSY prior, the B/k priors with their sources, and links to the species page in FishBase or SeaLifeBase, the FishBase data, the CMSY++ user guide and the FAO data |

``` r

names(report)
#> [1] "file"           "sections"       "main_species"   "main_countries"
#> [5] "analyses"
show_table(report$main_countries, digits = 1)
```

| country | country_en | mean_catch_t | share_pct | peak_year | pct_of_peak | trend_10_years |
|:---|:---|---:|---:|:---|---:|:---|
| Morocco | Morocco | 1,418,927.3 | 27.7 | 2022 | 91.0 | none |
| Mauritania | Mauritania | 674,299.1 | 13.2 | 2018 | 65.4 | none |
| Senegal | Senegal | 464,480.7 | 9.1 | 2017 | 93.6 | none |
| Nigeria | Nigeria | 441,470.9 | 8.6 | 2017 | 94.4 | none |
| Guinea | Guinea | 343,164.0 | 6.7 | 2021 | 81.0 | up |
| Ghana | Ghana | 333,652.8 | 6.5 | 1999 | 98.1 | up |
| Sierra Leone | Sierra Leone | 205,312.0 | 4.0 | 2022 | 97.1 | up |
| Cameroon | Cameroon | 198,094.6 | 3.9 | 2019 | 78.8 | none |
| Belize | Belize | 190,814.7 | 3.7 | 2010 | 51.1 | none |
| Georgia | Georgia | 127,022.0 | 2.5 | 2019 | 79.9 | up |

``` r

names(report$analyses)
#> [1] "total"     "species"   "countries" "groups"    "status"    "quality"  
#> [7] "diversity" "fleets"
stocks <- report$analyses$status$stocks
show_table(stocks[, c("species_name", "peak_year", "peak_tonnes", "pct_of_peak", "status_last_year")],
           n = 5, digits = 0)
```

| species_name | peak_year | peak_tonnes | pct_of_peak | status_last_year |
|:---|:---|---:|---:|:---|
| European pilchard(=Sardine) | 2018 | 1,354,617 | 53 | Exploited |
| Jack and horse mackerels nei | 2010 | 574,747 | 59 | Exploited |
| Sardinellas nei | 2018 | 542,915 | 33 | Over-exploited |
| Atlantic chub mackerel | 2019 | 485,443 | 97 | Exploited |
| Round sardinella | 1991 | 453,410 | 35 | Over-exploited |

## ✏️ Change the wording

All the text of the report comes from `inst/extdata/report_text.csv`,
one row per piece of text with the columns `key`, `en` and `zh`:

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

- `{last_year}`, `{oc_pct}`, … are placeholders, filled in with the
  results. Keep to the placeholders of the original text: an unknown one
  stops the report with an error.
- `{cite:kleisner2013}` cites a reference by its key,
  `{cite:bai2003,zeileis2003}` several. It becomes a superscript number
  in Chinese reports and “(Kleisner et al., 2013)” in English reports.
- `*...*` is written in italics (`*P*`, scientific names), `^...^` as
  superscript and `~...~` as subscript, e.g. `HHI = ∑(100*p*~i~)^2^`.

The references are in `inst/extdata/report_references.csv`:

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

| Column | Content |
|----|----|
| `key` | The key used in `{cite:key}` |
| `type` | `J` journal article, `M` book, `C` paper in proceedings, `R` report in a series (e.g. FAO Fisheries Technical Paper). `DB` (database) and `CP` (computer program) are the types of the references the function makes itself: the FAO data, FishBase, R and faofishingreport. |
| `author` | Persons as “Family, I. I.” separated by “;”, or an organisation such as FAO |
| `year`, `title` | Year and title |
| `container` | Journal or proceedings |
| `volume`, `issue`, `pages` | Volume, issue and pages, e.g. `293-311` |
| `edition` | Number of the edition, e.g. `4` for “4th ed.” |
| `series` | Series of a report or book, e.g. “FAO Fisheries Technical Paper No. 359” |
| `place`, `publisher` | Place and publisher |
| `doi`, `url` | DOI without “<https://doi.org/>”, and web address |

To change the wording or add references, edit these files in the package
source and reinstall the package (e.g. `R CMD INSTALL .` in the source
folder), then restart R: the files are read once per session.

## 📖 References

**Standards and citation styles**

American Psychological Association (2020). *Publication manual of the
American Psychological Association*. 7th edition. Washington, DC:
American Psychological Association.
[doi:10.1037/0000165-000](https://doi.org/10.1037/0000165-000)

FAO (2024). *The State of World Fisheries and Aquaculture 2024 – Blue
Transformation in action*. Rome: FAO.
[doi:10.4060/cd0683en](https://doi.org/10.4060/cd0683en) (model of the
reference lists in the style of FAO publications)

FAO. FishStatJ. Rome: FAO.
<https://www.fao.org/fishery/statistics/software/fishstatj/en> (FAO
publications cite FishStat data as “FAO. *Year*. FishStat: *dataset and
years*. \[Accessed on *date*\]. In: FishStatJ. Licence: CC-BY-4.0.”)

FAO. Global Capture Production. In: Fisheries and Aquaculture. Rome:
FAO. <https://www.fao.org/fishery/collection/capture/en> (its “Cite this
content as” format is used by `fao_data_info()$citation`)

General Administration of Quality Supervision, Inspection and Quarantine
of the People’s Republic of China and Standardization Administration of
China (2015). *GB/T 7714—2015 Information and documentation—Rules for
bibliographic references and citations to information resources*.
Beijing: Standards Press of China. Issued 15 May 2015, in force from 1
December 2015.

**Data cited by every report**

FAO. 2026. Global Capture Production 1950-2024, release 2026.1.0. In:
Fisheries and Aquaculture \[online\]. Rome. \[Cited 10 October 2026\].
<https://www.fao.org/fishery/collection/capture/en>. Licence: CC BY 4.0.

FAO (2026a). *Fishery and Aquaculture Statistics – Yearbook 2024*. FAO
Yearbook of Fishery and Aquaculture Statistics. Rome: FAO.
[doi:10.4060/ce1217en](https://doi.org/10.4060/ce1217en)

**Methods and software cited by the default sections**

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

**Methods cited by the other sections**

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
publication. <https://www.fishbase.se> (the report cites the version
used)

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
