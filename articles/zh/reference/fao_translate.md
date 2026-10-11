# fao_translate(): 翻译FAO的国家、物种或渔区名称

英文原页：[`fao_translate()`](https://linbojun99.github.io/faofishingreport/reference/fao_translate.md)

## 说明

该函数把FAO数据中使用的英文名称翻译为中文名称，或“中文
英文”形式的中英双语名称。译名来自FAO的官方代码表（随FAO数据发布的国家、物种和水域代码表，以及ASFIS物种名录）。没有官方中文名称的名称用本包自带的名称表补充，仍然没有的则保留英文。

English

This function translates English names, as used in the FAO data, to
Chinese or to bilingual “Chinese English” names. Translations come from
the FAO official code lists (country, species and water area lists
published with the FAO data, and the ASFIS species list). Names without
an official Chinese name are filled from the tables bundled with the
package, and otherwise kept in English.

## 用法

``` r

fao_translate(x, type = c("country", "species", "area"), lang = "zh")
```

## 参数

- `x`:

  英文名称组成的字符向量。

  English

  A character vector of English names.

- `type`:

  名称的类别：“country”（国家）、“species”（物种）或”area”（渔区）。默认为”country”。

  English

  The kind of name: “country”, “species” or “area”.

- `lang`:

  输出的语言：“en”（英文，名称保持不变）、“zh”（中文）或”both”（中英双语）。默认为”zh”。

  English

  The output language: “en” (English), “zh” (Chinese) or “both” (Chinese
  and English).

## 返回值

翻译后的名称组成的字符向量。

English

A character vector of translated names.

## 详细说明

FAO名录中有许多物种条目没有中文名。对于只对应一个物种的条目，使用该物种在FishBase或SeaLifeBase中的中文俗名（Froese和Pauly，2026；Palomares和Pauly，2026）：即其中给中国、以普通话记录、用简体字书写的名称；优先使用FishBase标为首选的名称，不使用用于同物异名的名称，也不使用有多个同等常用名称的物种。这张名称表，连同其他名称和每个名称的来源，保存在`inst/extdata/species_names_zh_fishbase.csv`中，由`data-raw/build_species_names_zh.R`生成。FAO的名称始终优先。

English

Many FAO species items have no Chinese name in the FAO lists. For those
identified to one species, the Chinese common name of the species in
FishBase or SeaLifeBase is used (Froese and Pauly 2026; Palomares and
Pauly 2026): the name given for China in Mandarin Chinese, in simplified
characters, preferring the name FishBase marks as preferred and leaving
out names applied to synonyms and species with several equally used
names. The table, with the other names and the source of each, is
`inst/extdata/species_names_zh_fishbase.csv`, made by
`data-raw/build_species_names_zh.R`. FAO names always come first.

## 参考文献

Froese, R. and Pauly, D. (Eds.) (2026). FishBase. World Wide Web
electronic publication, version 06/2026. <https://www.fishbase.se>

Palomares, M.L.D. and Pauly, D. (Eds.) (2026). SeaLifeBase. World Wide
Web electronic publication, version 06/2026.
<https://www.sealifebase.se>

## 另请参阅

Other settings and language:
[`fao_font()`](https://linbojun99.github.io/faofishingreport/reference/fao_font.md),
[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)

## 示例

``` r

fao_translate(c("Morocco", "Spain"), "country", "both")
```
