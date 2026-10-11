# Settings for Charts, Maps and Output

This function sets, for the rest of the R session, how all functions of
the package draw and save charts and maps. Call it without arguments to
see the current settings, or with `reset = TRUE` to go back to the
defaults. Each setting is stored as the R option
`faofishingreport.<name>`, so it can also be set with
[`options()`](https://rdrr.io/r/base/options.html).

## Usage

``` r
fao_settings(..., reset = FALSE)
```

## Arguments

- ...:

  Settings to change, as `name = value`:

  lang

  :   Default language of charts: "en" (default), "zh" or "both". The
      `lang` argument of a function still takes precedence.

  theme

  :   Chart theme: "bw" (default), "classic", "minimal", "light",
      "linedraw", "gray" or "dark", or a ggplot2 theme object such as
      `ggplot2::theme_bw(base_size = 14)`. Maps and pie charts keep a
      minimal theme.

  base_size

  :   Base font size of the chart theme. Default is 12.

  font

  :   Font family for chart text; see
      [`fao_font()`](https://linbojun99.github.io/faofishingreport/reference/fao_font.md).

  palette

  :   Colours of categories (countries, species, groups): a palette of
      the ggsci package ("npg" (default), "lancet", "nejm", "jama",
      "aaas", "jco", "d3", ...), of RColorBrewer ("Set1", "Set2",
      "Dark2", "Paired", ...), of base R
      ([`grDevices::palette.pals()`](https://rdrr.io/r/grDevices/palette.html),
      e.g. "Okabe-Ito" or "Tableau 10"; or
      [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html),
      e.g. "viridis" or "Dark 3"), or a vector of colours.

  map_palette

  :   Colours of map values: a Material Design palette of ggsci
      ("deep-orange" (default), "blue", "teal", "red", ...), an
      RColorBrewer palette ("YlOrRd", "Blues", ...), a viridis option
      ("viridis", "magma", "plasma", "inferno", "cividis", "mako",
      "rocket", "turbo"), a
      [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html)
      name, or a vector of colours.

  map_center

  :   Central longitude of maps: 0 (default) for longitudes -180 to 180
      (Atlantic in the centre), or 180 for longitudes 0 to 360 (Pacific
      in the centre). The strings "-180-180" and "0-360" are also
      accepted.

  map_projection

  :   Map projection: "longlat" (default, plain longitude-latitude),
      "robinson", "mollweide", "equal_earth", "winkel_tripel" or
      "mercator", or any PROJ string or EPSG code (e.g. "EPSG:3857";
      then `map_center` is not applied).

  map_land

  :   Fill colour of countries without data on maps. Default is
      "grey90".

  map_ocean

  :   Fill colour of the ocean on maps. Default is NA (no fill).

  grid

  :   Whether charts have background grid lines (and maps graticules).
      Default is FALSE.

  dpi

  :   Resolution of the PNG files. Default is 600.

  pptx

  :   Whether charts are also saved as editable PowerPoint files (needs
      the eoffice package). Default is FALSE.

  data_dir

  :   Folder where
      [`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)
      saves FAO releases and where all functions look for them, e.g. a
      folder shared by a team. Default is NULL, the user data folder of
      R; see
      [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- reset:

  A logical value. If TRUE, all settings go back to their defaults
  first.

## Value

Invisibly, a list of the current settings.

## See also

Other settings and language:
[`fao_font()`](https://linbojun99.github.io/faofishingreport/reference/fao_font.md),
[`fao_translate()`](https://linbojun99.github.io/faofishingreport/reference/fao_translate.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fao_settings()                                   # show the current settings
fao_settings(lang = "zh", palette = "lancet", theme = "classic")
fao_settings(map_center = 180, map_projection = "robinson", map_palette = "YlOrRd")
fao_settings(reset = TRUE)                       # back to the defaults
} # }
```
