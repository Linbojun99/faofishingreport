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

## Examples

``` r
if (FALSE) { # \dontrun{
fao_translate(c("Morocco", "Spain"), "country", "both")
} # }
```
