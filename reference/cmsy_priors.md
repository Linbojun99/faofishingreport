# FishBase and SeaLifeBase Priors for CMSY++

This function searches the built-in table of CMSY++ priors for the FAO
species items: the resilience of the species and, where available, the
prior range of r ("Prior r = ..., 95% CL = ...", from the "Estimates
based on models" section of the FishBase or SeaLifeBase species page).
The table works offline. Every row gives the sources of its values as
links, so each value can be checked online (`open = TRUE` opens the
species pages):

- url:

  the FishBase or SeaLifeBase page of the species, which shows its
  resilience and prior r (or a search of the database when the species
  was not found);

- data_url:

  the table of the FishBase or SeaLifeBase release the value was read
  from (`estimate` for prior r, `stocks` for resilience);

- rule_url:

  the CMSY++ user guide, whose Tables 1 and 2 turn these values into the
  r range.

## Usage

``` r
cmsy_priors(species = NULL, open = FALSE, lang = NULL)
```

## Arguments

- species:

  Optional names to search for: English, Chinese or scientific names, or
  FAO 3-alpha codes, or parts of them (e.g. "Atlantic cod", "Gadus
  morhua", "COD", "cod" or the Chinese name). Default is NULL, all rows.

- open:

  A logical value. If TRUE, the FishBase or SeaLifeBase pages of the
  rows found (at most 10) are opened in the browser. Default is FALSE.

- lang:

  The language of the `species_name` column: "en" (English), "zh"
  (Chinese) or "both". Default is NULL, the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md).
  The `species_en` and `species_zh` columns always give both names.

## Value

A data frame with one row per FAO species item.

## Details

Species can be given by their English or Chinese FAO name, both together
as shown with `lang = "both"` (e.g. "Chinese (English)"), scientific
name, FishBase valid name or FAO 3-alpha code. Each value is first
matched exactly (ignoring case); otherwise every species whose names
contain it is returned.

The table was made from FishBase and SeaLifeBase release 26.06 for the
species-level items of the FAO capture statistics. Each FAO scientific
name is looked up in FishBase (fishes) or SeaLifeBase (other animals) as
a valid species name, then as a synonym in the database's own synonym
table; ambiguous synonyms and genus, family or higher groups get no
values. The `match_type` column tells how each item was matched.

The r range given to CMSY++ follows its user guide (Froese et al. 2021):

- the 95% confidence limits of FishBase's prior r when the species has
  one ("Alternatively and better, get the prior range of r from the
  section 'Estimates based on models'", user guide Table 1);

- otherwise the range of the species' resilience (user guide Table 2):
  High 0.6-1.5, Medium 0.2-0.8, Low 0.05-0.5, Very low 0.015-0.1;

- otherwise none: the user guide gives no default, so the prior has to
  come from the user (e.g. r close to 2 M or 2 Fmsy, user guide Table
  1); see `priors` in
  [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md).

When FishBase lists several stocks of a species with different
resilience, the value of its species-wide stock (the first stock record,
shown on the species page) is used and the other values are given in
`resilience_note`.

## References

Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for
CMSY++. GEOMAR, Germany, 17 pp.
<https://oceanrep.geomar.de/id/eprint/52147/>

Froese, R. and Pauly, D. (Eds.) (2026). FishBase. Version (06/2026).
<https://www.fishbase.se>

Palomares, M.L.D. and Pauly, D. (Eds.) (2026). SeaLifeBase. Version
(06/2026). <https://www.sealifebase.se>

## Examples

``` r
# By scientific name, English name or FAO 3-alpha code (Chinese names work as well)
cmsy_priors("Gadus morhua")
#>   species_name alpha3   species_en species_zh scientific_name major_en database
#> 1 Atlantic cod    COD Atlantic cod   大西洋鳕    Gadus morhua   PISCES FishBase
#>   SpecCode matched_name match_type match_note resilience resilience_note
#> 1       69 Gadus morhua      exact       <NA>     Medium            <NA>
#>   prior_r prior_r_lcl prior_r_ucl prior_r_n                        prior_r_note
#> 1   0.509       0.336       0.764        66 Based on 66 full stock assessments.
#>                                  url        release r_low r_high
#> 1 https://www.fishbase.se/summary/69 FishBase 26.06 0.336  0.764
#>                                                         r_source
#> 1 FishBase prior r, 95% CL (Based on 66 full stock assessments.)
#>                                                                        data_url
#> 1 https://data.source.coop/cboettig/fishbase/fb/v26.06/parquet/estimate.parquet
#>                                      rule_url
#> 1 https://oceanrep.geomar.de/id/eprint/52147/
cmsy_priors(c("COD", "European pilchard(=Sardine)"), lang = "both")
#>                         species_name alpha3                  species_en
#> 1              大西洋鳕 Atlantic cod    COD                Atlantic cod
#> 2 沙丁鱼 European pilchard(=Sardine)    PIL European pilchard(=Sardine)
#>   species_zh    scientific_name major_en database SpecCode       matched_name
#> 1   大西洋鳕       Gadus morhua   PISCES FishBase       69       Gadus morhua
#> 2     沙丁鱼 Sardina pilchardus   PISCES FishBase     1350 Sardina pilchardus
#>   match_type match_note resilience resilience_note prior_r prior_r_lcl
#> 1      exact       <NA>     Medium            <NA>   0.509       0.336
#> 2      exact       <NA>     Medium            <NA>   0.628       0.415
#>   prior_r_ucl prior_r_n                        prior_r_note
#> 1       0.764        66 Based on 66 full stock assessments.
#> 2       0.942        26 Based on 26 full stock assessments.
#>                                    url        release r_low r_high
#> 1   https://www.fishbase.se/summary/69 FishBase 26.06 0.336  0.764
#> 2 https://www.fishbase.se/summary/1350 FishBase 26.06 0.415  0.942
#>                                                         r_source
#> 1 FishBase prior r, 95% CL (Based on 66 full stock assessments.)
#> 2 FishBase prior r, 95% CL (Based on 26 full stock assessments.)
#>                                                                        data_url
#> 1 https://data.source.coop/cboettig/fishbase/fb/v26.06/parquet/estimate.parquet
#> 2 https://data.source.coop/cboettig/fishbase/fb/v26.06/parquet/estimate.parquet
#>                                      rule_url
#> 1 https://oceanrep.geomar.de/id/eprint/52147/
#> 2 https://oceanrep.geomar.de/id/eprint/52147/

# Part of a name finds every matching species
cmsy_priors("sardin")[, c("alpha3", "species_en", "scientific_name", "resilience", "r_low", "r_high")]
#>    alpha3                  species_en            scientific_name resilience
#> 1     SAM             Bali sardinella          Sardinella lemuru       High
#> 2     BSR        Brazilian sardinella    Sardinella brasiliensis       High
#> 3     PIL European pilchard(=Sardine)         Sardina pilchardus     Medium
#> 4     SAG       Goldstripe sardinella         Sardinella gibbosa       High
#> 5     IOS          Indian oil sardine       Sardinella longiceps     Medium
#> 6     JSS         Japanese sardinella          Sardinella zunasi       High
#> 7     ENL         Lake Malawi sardine    Engraulicypris sardella       High
#> 8     LTM     Lake Tanganyika sardine        Limnothrissa miodon       High
#> 9     SAE         Madeiran sardinella      Sardinella maderensis     Medium
#> 10    CHP             Pacific sardine            Sardinops sagax     Medium
#> 11    RAS             Rainbow sardine          Dussumieria acuta       High
#> 12    MSB               River sardine Engraulicypris brevianalis       High
#> 13    SAA            Round sardinella          Sardinella aurita       High
#> 14    COL              Sardinia coral           Corallium rubrum        Low
#> 15    RAL     Slender rainbow sardine     Dussumieria elopsoides       High
#> 16    AGS          Spotted sardinella           Amblygaster sirm       High
#> 17    SDB            White sardinella         Sardinella albella       High
#>    r_low r_high
#> 1  0.786  1.787
#> 2  0.263  0.598
#> 3  0.415  0.942
#> 4  0.705  1.603
#> 5  0.305  0.693
#> 6  0.600  1.500
#> 7  0.600  1.500
#> 8  0.600  1.500
#> 9  0.379  0.862
#> 10 0.358  0.813
#> 11 0.786  1.787
#> 12 0.600  1.500
#> 13 0.429  0.976
#> 14 0.050  0.500
#> 15 0.600  1.500
#> 16 0.600  1.500
#> 17 0.600  1.500

# The sources of a value, to check it online
cmsy_priors("Gadus morhua")[, c("url", "data_url", "rule_url")]
#>                                  url
#> 1 https://www.fishbase.se/summary/69
#>                                                                        data_url
#> 1 https://data.source.coop/cboettig/fishbase/fb/v26.06/parquet/estimate.parquet
#>                                      rule_url
#> 1 https://oceanrep.geomar.de/id/eprint/52147/

# How the FAO names were matched, and the species without a prior for r
p <- cmsy_priors()
table(p$match_type)
#> 
#> ambiguous synonym             exact     not a species         not found 
#>                 3              3071                 2                43 
#>           synonym 
#>                36 
head(p[is.na(p$r_source), c("alpha3", "species_en", "scientific_name")])
#>    alpha3               species_en        scientific_name
#> 2     GNU    [Ageneiosus dentatus]    Ageneiosus dentatus
#> 7     AWZ       [Anodus elongatus]       Anodus elongatus
#> 8     WAG [Anthothela grandiflora] Anthothela grandiflora
#> 9     FPJ [Aplodactylus punctatus] Aplodactylus punctatus
#> 11    ARL   [Argonectes longiceps]   Argonectes longiceps
#> 12    AIQ  [Astacopsis franklinii]  Astacopsis franklinii
```
