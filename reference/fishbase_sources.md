# Sources, Licences and Matching Rules of the FishBase Data

This function lists where the FishBase, SeaLifeBase and WoRMS data used
by the package come from, under which licence, how to cite them, which
tables are downloaded and what they hold, and the rules (with their
published sources) by which
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)
matches FAO species items to them.
[`download_fishbase_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fishbase_data.md)
prints a summary of the same information.

## Usage

``` r
fishbase_sources(print = TRUE)
```

## Arguments

- print:

  A logical value. If TRUE (default), the list is printed.

## Value

Invisibly, a data frame with one row per data source, table or rule:
`type`, `item`, `description`, `source` (citation or licence) and
`link`.

## References

Boettiger, C., Lang, D.T. and Wainwright, P.C. (2012). rfishbase:
exploring, manipulating and visualizing FishBase data from R. Journal of
Fish Biology 81(6): 2030-2039.
[doi:10.1111/j.1095-8649.2012.03464.x](https://doi.org/10.1111/j.1095-8649.2012.03464.x)
. Source code: <https://github.com/ropensci/rfishbase>

Branch, T.A., Watson, R., Fulton, E.A. et al. (2010). The trophic
fingerprint of marine fisheries. Nature 468: 431-435.
[doi:10.1038/nature09528](https://doi.org/10.1038/nature09528)

FAO (2026). Nomenclature, revision, and references of the ASFIS list
(version 2026). Rome, FAO.
<https://www.fao.org/fishery/static/ASFIS/ASFIS_References_2026_EN.pdf>

Froese, R. and Pauly, D. (Eds.) (2000). FishBase 2000: concepts, design
and data sources. ICLARM, Los Baños, Philippines.
<https://www.fishbase.se/manual/fishbasethe_food_items_table.htm>,
<https://www.fishbase.se/manual/fishbasethe_synonyms_table.htm>

Froese, R. and Pauly, D. (Eds.) FishBase. World Wide Web electronic
publication. <https://www.fishbase.se>

Liang, C. and Pauly, D. (2017). Fisheries impacts on China's coastal
ecosystems: unmasking a pervasive 'fishing down' effect. PLoS ONE 12:
e0173296.
[doi:10.1371/journal.pone.0173296](https://doi.org/10.1371/journal.pone.0173296)

Liang, C., Xian, W. and Pauly, D. (2018). Impacts of ocean warming on
China's fisheries catches: an application of "mean temperature of the
catch" concept. Frontiers in Marine Science 5: 26.
[doi:10.3389/fmars.2018.00026](https://doi.org/10.3389/fmars.2018.00026)

Palomares, M.L.D. and Pauly, D. (Eds.) SeaLifeBase. World Wide Web
electronic publication. <https://www.sealifebase.se>

Pauly, D., Christensen, V., Dalsgaard, J., Froese, R. and Torres, F.
(1998). Fishing down marine food webs. Science 279: 860-863.
[doi:10.1126/science.279.5352.860](https://doi.org/10.1126/science.279.5352.860)

Tsikliras, A.C. and Stergiou, K.I. (2014). Mean temperature of the catch
increases quickly in the Mediterranean Sea. Marine Ecology Progress
Series 515: 281-284.
[doi:10.3354/meps11005](https://doi.org/10.3354/meps11005)

WoRMS Editorial Board. World Register of Marine Species. Flanders Marine
Institute (VLIZ). <https://www.marinespecies.org>,
[doi:10.14284/170](https://doi.org/10.14284/170)

## Examples

``` r
if (FALSE) { # \dontrun{
fishbase_sources()
} # }
```
