# Translate FAO Country, Species or Fishing Area Names

This function translates English names, as used in the FAO data, to
Chinese or to bilingual "Chinese English" names. Translations come from
the FAO official code lists (country, species and water area lists
published with the FAO data, and the ASFIS species list). Names without
an official Chinese name are filled from the tables bundled with the
package, and otherwise kept in English.

## Usage

``` r
fao_translate(x, type = c("country", "species", "area"), lang = "zh")
```

## Arguments

- x:

  A character vector of English names.

- type:

  The kind of name: "country", "species" or "area".

- lang:

  The output language: "en" (English), "zh" (Chinese) or "both" (Chinese
  and English).

## Value

A character vector of translated names.

## Details

Many FAO species items have no Chinese name in the FAO lists. For those
identified to one species, the Chinese common name of the species in
FishBase or SeaLifeBase is used (Froese and Pauly 2026; Palomares and
Pauly 2026): the name given for China in Mandarin Chinese, in simplified
characters, preferring the name FishBase marks as preferred and leaving
out names applied to synonyms and species with several equally used
names. The table, with the other names and the source of each, is
`inst/extdata/species_names_zh_fishbase.csv`, made by
`data-raw/build_species_names_zh.R`. FAO names always come first.

## References

Froese, R. and Pauly, D. (Eds.) (2026). FishBase. World Wide Web
electronic publication, version 06/2026. <https://www.fishbase.se>

Palomares, M.L.D. and Pauly, D. (Eds.) (2026). SeaLifeBase. World Wide
Web electronic publication, version 06/2026.
<https://www.sealifebase.se>

## See also

Other settings and language:
[`fao_font()`](https://linbojun99.github.io/faofishingreport/reference/fao_font.md),
[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fao_translate(c("Morocco", "Spain"), "country", "both")
} # }
```
