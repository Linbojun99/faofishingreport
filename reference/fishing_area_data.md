# Extract Data for a Specified Fishing Area and Optionally Generate Plots

This function extracts and returns data for a specified FAO major
fishing area. If the plot parameter is TRUE, it will also generate and
save plots to the figures directory.

## Usage

``` r
fishing_area_data(
  area_name,
  plot = TRUE,
  timeseries_analysis = TRUE,
  area_color1 = "grey80",
  area_alpha = 0.7,
  area_color2 = "black",
  line_color = "black",
  line_alpha = 0.8,
  point_size = 2,
  hline_color = "red",
  hline_alpha = 0.7,
  hline_linewidth = 1.5,
  line_linewidth = 1.5,
  axis_text_face = "plain",
  axis_title_face = "bold",
  strip_text_face = "bold",
  axis_text_size = 10,
  axis_title_size = 12,
  strip_text_size = 12,
  polygon_color = "grey90",
  fill_scale = NULL,
  axis_title_size_map = 14,
  axis_text_size_map = 12,
  legend_text_size_map = 12,
  ZH = FALSE,
  lang = NULL,
  data_version = "latest",
  pptx = getOption("faofishingreport.pptx", FALSE)
)
```

## Arguments

- area_name:

  The name of the FAO major fishing area.

- plot:

  A logical indicating whether to generate and save plots. Default is
  TRUE.

- timeseries_analysis:

  A logical value indicating whether to analysis timeseries.

- area_color1:

  The color to use for the area plot. Default is "grey80".

- area_alpha:

  The alpha transparency for the area plot. Default is 0.7.

- area_color2:

  The color to use for the area plot. Default is "black".

- line_color:

  The color to use for line plots. Default is "black".

- line_alpha:

  The alpha transparency for line plots. Default is 0.8.

- point_size:

  The size of points in the percentage plot. Default is 2.

- hline_color:

  The color of the horizontal line in the percentage plot. Default is
  "red".

- hline_alpha:

  The alpha transparency of the horizontal line in the percentage plot.
  Default is 0.7.

- hline_linewidth:

  Line width for the horizontal line. Default is 1.5.

- line_linewidth:

  Line width for the line geom. Default is 1.5.

- axis_text_face:

  The font face of the axis text. Default is "plain".

- axis_title_face:

  The font face of the axis title. Default is "bold".

- strip_text_face:

  The font face of the strip text. Default is "bold".

- axis_text_size:

  The size of the axis text. Default is 10.

- axis_title_size:

  The size of the axis title text. Default is 12.

- strip_text_size:

  The size of the strip text. Default is 12.

- polygon_color:

  The color of the borders of the polygons in the map plot. Default is
  "grey90".

- fill_scale:

  The colour palette of the map. Default is NULL, which uses
  \`map_palette\` of \`fao_settings()\` ("deep-orange" unless changed);
  see \`?fao_settings\` for the choices.

- axis_title_size_map:

  The size of the axis title text in the map plot. Default is 14.

- axis_text_size_map:

  The size of the axis text in the map plot. Default is 12.

- legend_text_size_map:

  The size of the legend text in the map plot. Default is 12.

- ZH:

  A logical value to Chinese. Default is "FALSE". \`ZH = TRUE\` is the
  same as \`lang = "zh"\`.

- lang:

  The language of the chart text: "en" (English), "zh" (Chinese) or
  "both" (Chinese and English). Default is NULL, which uses "zh" if \`ZH
  = TRUE\`, and otherwise the language set with \`fao_settings()\` ("en"
  unless changed).

- data_version:

  The FAO data release to use: "latest" (default) uses the newest of the
  releases downloaded with \`download_fao_data()\` and the data bundled
  with the package; "bundled" uses the data bundled with the package; or
  a release such as "2026.1.0".

- pptx:

  A logical value. Charts are always saved as PNG in \`figures/png/\`;
  if TRUE they are also saved as editable PowerPoint files in
  \`figures/pptx/\`. Default is FALSE, which can be changed for all
  functions with \`fao_settings(pptx = TRUE)\`.

## Value

A data frame containing data for the specified fishing area.

## Examples

``` r
if (FALSE) { # \dontrun{
# Firstly, data preparing and loading.
FAO34 <- fishing_area_data(area_name="Atlantic, Eastern Central", plot = TRUE, timeseries_analysis=TRUE)
} # }
```
