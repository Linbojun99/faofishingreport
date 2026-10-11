# Font Used for Chart Text

This function returns the font used for all chart text. The font is
chosen for the operating system so that Chinese text displays correctly:
"Songti SC" on macOS, "SimSun" on Windows and "Noto Serif CJK SC" on
Linux. If that font is not installed, the next available Chinese font is
used.

## Usage

``` r
fao_font(family = NULL)
```

## Arguments

- family:

  Optional font family to use from now on, e.g. "Microsoft YaHei". The
  choice can also be set with `options(faofishingreport.font = "...")`.

## Value

The font family name.

## See also

Other settings and language:
[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md),
[`fao_translate()`](https://linbojun99.github.io/faofishingreport/reference/fao_translate.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fao_font()                    # font chosen for this computer
fao_font("Microsoft YaHei")   # use another font
} # }
```
