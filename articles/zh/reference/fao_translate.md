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

## 另请参阅

Other settings and language:
[`fao_font()`](https://linbojun99.github.io/faofishingreport/reference/fao_font.md),
[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)

## 示例

``` r

fao_translate(c("Morocco", "Spain"), "country", "both")
```
