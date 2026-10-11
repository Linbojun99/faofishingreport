# Fishing Area Species Analysis

This function analyzes the fishing area data and generates plots for
specified species based on their ranking in terms of total tonnes.

## Usage

``` r
fishing_area_species(
  area_data,
  rank_range = c(1, 10),
  plot = TRUE,
  table = TRUE,
  timeseries_analysis = TRUE,
  line_width = 1.5,
  point_size = 1.8,
  point_alpha = 0.7,
  point_shape = 1,
  line_alpha = 0.7,
  axis_text_size = 10,
  axis_title_size = 10,
  strip_text_size = 10,
  axis_text_face = "plain",
  axis_title_face = "bold",
  strip_text_face = "bold",
  smooth_method = "loess",
  smooth_linetype = "dashed",
  line_linewidth = 1.5,
  text_hjust = 0.9,
  text_vjust = -4,
  text_size = 4.5,
  text_color = "black",
  facet_ncol = 2,
  h_value = 0.15,
  ZH = FALSE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE)
)
```

## Arguments

- area_data:

  A data frame containing the fishing area data.

- rank_range:

  A numeric vector of length 2 specifying the rank range of species to
  be analyzed.

- plot:

  A logical value indicating whether to generate and save plots.

- table:

  A logical value indicating whether to generate and save tables

- timeseries_analysis:

  A logical value indicating whether to analysis timeseries.

- line_width:

  A numeric value specifying the line width. Default is 1.5.

- point_size:

  A numeric value specifying the point size. Default is 1.8.

- point_alpha:

  A numeric value specifying the point alpha. Default is 0.7.

- point_shape:

  A numeric or character value specifying the shape of the points.
  Default is 1.

- line_alpha:

  A numeric value specifying the line alpha. Default is 0.7.

- axis_text_size:

  A numeric value specifying the size of the axis text. Default is 10.

- axis_title_size:

  A numeric value specifying the size of the axis title. Default is 10.

- strip_text_size:

  A numeric value specifying the size of the strip text. Default is 10.

- axis_text_face:

  The font face of the axis text. Default is "plain".

- axis_title_face:

  The font face of the axis title. Default is "bold".

- strip_text_face:

  The font face of the strip text. Default is "bold".

- smooth_method:

  A character string specifying the smoothing method to be used. Default
  is "loess".

- smooth_linetype:

  A character string specifying the line type for the smoothing curve.
  Default is "dashed".

- line_linewidth:

  A numeric value specifying the line width for the line geom. Default
  is 1.5.

- text_hjust:

  A numeric value specifying the horizontal justification of the text
  geom. Default is 0.8.

- text_vjust:

  A numeric value specifying the vertical justification of the text
  geom. Default is -8.

- text_size:

  A numeric value specifying the size of the text geom. Default is 4.5.

- text_color:

  A character string specifying the color of the text geom. Default is
  "black".

- facet_ncol:

  A numeric value specifying the number of columns to be used in
  facet_wrap. Default is 2.

- h_value:

  The minimum segment size, which must be greater than the number of
  regressors. For the model \`ts_data ~ 1\`, this means \`h_value\`
  should be greater than 1. The default value is 0.15.

- ZH:

  A logical value to Chinese. Default is "FALSE". \`ZH = TRUE\` is the
  same as \`lang = "zh"\`.

- lang:

  The language of the chart text and names: "en" (English), "zh"
  (Chinese) or "both" (Chinese and English). Default is NULL, which uses
  "zh" if \`ZH = TRUE\`, and otherwise the language set with
  \`fao_settings()\` ("en" unless changed).

- pptx:

  A logical value. Charts are always saved as PNG in \`figures/png/\`;
  if TRUE they are also saved as editable PowerPoint files in
  \`figures/pptx/\`. Default is FALSE, which can be changed for all
  functions with \`fao_settings(pptx = TRUE)\`.

## Value

NULL. The function saves the generated plots to the specified directory
if plot = TRUE.

## Examples

``` r
if (FALSE) { # \dontrun{
fishing_area_species(FAO34[["area_data"]], c(1, 10), plot=TRUE,table=TRUE,h_value = 0.15) #area_data=Fao34
} # }
```
