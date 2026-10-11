# Species Groups Mostly Caught, Not Farmed

This function gives, for each ISSCAAP species group of aquatic animals,
the share of capture in the world production (capture and aquaculture):
the lowest share over `years` and its year, and the shares in the first
and last year, and whether the group counts as mostly caught: capture
made up at least `min_capture_pct` percent of its world production in
every year of `years`.
[`catch_trade()`](https://linbojun99.github.io/faofishingreport/reference/catch_trade.md)
compares trade with catch only for these groups, because trade
statistics do not tell whether a product was caught or farmed.

## Usage

``` r
fao_capture_groups(min_capture_pct = 90, years = NULL)
```

## Arguments

- min_capture_pct:

  The lowest share of capture (percent of world production) in every
  year for a group to count. Default is 90.

- years:

  The years. Default is NULL, all years of the table (1976 to 2024).

## Value

A data frame with one row per ISSCAAP group of aquatic animals with
production: `isscaap_code`, `isscaap_en`, `isscaap_zh`,
`capture_pct_min` and `year_min` (the lowest share and its year),
`capture_pct_first` and `capture_pct_last` (the shares in the first and
last year), and `counted` (TRUE for the groups mostly caught), sorted by
code.

## Details

The shares come from the FAO global production data (capture and
aquaculture, tonnes live weight), release 2026.1.0, built into the
package from 1976 (the first year of the trade data). The groups FAO
leaves out of its summary tables of aquatic animals (mammals,
crocodiles, pearls, corals, sponges and aquatic plants) are not counted.

With the default 90 percent and the years 1976 to 2024, the groups
counted are the cods, herrings and sardines, tunas, miscellaneous
pelagic and demersal fishes, sharks and rays, shads, lobsters, king
crabs, krill, miscellaneous marine crustaceans, squids and octopuses,
and horseshoe crabs. Groups once mostly caught but now partly or largely
farmed are not counted: shrimps (capture below 90 percent from 1986),
crabs (from 2000), flounders (from 2004), miscellaneous coastal fishes
(from 2005) and salmons (81 percent caught in 1976, 12 percent in 2024).
"Marine fishes not identified" is not counted because capture made up
less than 90 percent of its production in each year from 2022 to 2024
(89.65 to 89.97 percent).

## References

FAO. Global production statistics (capture and aquaculture), release
2026.1.0. <https://www.fao.org/fishery/static/Data/>

## Examples

``` r
fao_capture_groups()
#>    isscaap_code                           isscaap_en             isscaap_zh
#> 1            11   Carps, barbels and other cyprinids   鲤、鲃及其他鲤科鱼类
#> 2            12          Tilapias and other cichlids 罗非鱼及其他丽鱼科鱼类
#> 3            13      Miscellaneous freshwater fishes           其他淡水鱼类
#> 4            21              Sturgeons, paddlefishes           鲟、匙吻鲟类
#> 5            22                           River eels                 河鳗类
#> 6            23              Salmons, trouts, smelts       鲑、鳟、胡瓜鱼类
#> 7            24                                Shads                 西鲱类
#> 8            25      Miscellaneous diadromous fishes         其他洄游性鱼类
#> 9            31           Flounders, halibuts, soles           鲆、鲽、鳎类
#> 10           32                Cods, hakes, haddocks   鳕、无须鳕、黑线鳕类
#> 11           33         Miscellaneous coastal fishes           其他沿岸鱼类
#> 12           34        Miscellaneous demersal fishes           其他底层鱼类
#> 13           35        Herrings, sardines, anchovies       鲱、沙丁鱼、鳀类
#> 14           36           Tunas, bonitos, billfishes   金枪鱼、狐鲣、旗鱼类
#> 15           37         Miscellaneous pelagic fishes         其他中上层鱼类
#> 16           38              Sharks, rays, chimaeras         鲨、鳐、银鲛类
#> 17           39         Marine fishes not identified       未鉴定的海洋鱼类
#> 18           41               Freshwater crustaceans             淡水甲壳类
#> 19           42                   Crabs, sea-spiders           蟹、蜘蛛蟹类
#> 20           43        Lobsters, spiny-rock lobsters         螯龙虾、龙虾类
#> 21           44           King crabs, squat-lobsters       帝王蟹、铠甲虾类
#> 22           45                      Shrimps, prawns                   虾类
#> 23           46        Krill, planktonic crustaceans       磷虾及浮游甲壳类
#> 24           47     Miscellaneous marine crustaceans         其他海洋甲壳类
#> 25           51                  Freshwater molluscs           淡水软体动物
#> 26           52            Abalones, winkles, conchs     鲍、玉黍螺、凤螺类
#> 27           53                              Oysters                 牡蛎类
#> 28           54                              Mussels                 贻贝类
#> 29           55                    Scallops, pectens                 扇贝类
#> 30           56            Clams, cockles, arkshells         蛤、鸟蛤、蚶类
#> 31           57      Squids, cuttlefishes, octopuses     鱿鱼、乌贼、章鱼类
#> 32           58        Miscellaneous marine molluscs       其他海洋软体动物
#> 33           71           Frogs and other amphibians         蛙及其他两栖类
#> 34           72                              Turtles                 龟鳖类
#> 35           74      Sea-squirts and other tunicates       海鞘及其他被囊类
#> 36           75 Horseshoe crabs and other arachnoids         鲎及其他蛛形类
#> 37           76    Sea-urchins and other echinoderms     海胆及其他棘皮动物
#> 38           77  Miscellaneous aquatic invertebrates     其他水生无脊椎动物
#>    capture_pct_min year_min capture_pct_first capture_pct_last counted
#> 1     3.500302e+00     2004         17.463357     5.494816e+00   FALSE
#> 2     1.098374e+01     2023         82.449209     1.117275e+01   FALSE
#> 3     3.660237e+01     2024         91.470498     3.660237e+01   FALSE
#> 4     4.576254e-02     2024        100.000000     4.576254e-02   FALSE
#> 5     1.848278e+00     2022         31.660540     2.542243e+00   FALSE
#> 6     1.170463e+01     2024         80.776834     1.170463e+01   FALSE
#> 7     9.954115e+01     2006        100.000000     9.994857e+01    TRUE
#> 8     3.788331e+00     2024          9.335222     3.788331e+00   FALSE
#> 9     8.029265e+01     2024        100.000000     8.029265e+01   FALSE
#> 10    9.967430e+01     2009        100.000000     9.984749e+01    TRUE
#> 11    7.620304e+01     2024         99.652880     7.620304e+01   FALSE
#> 12    9.879630e+01     2007        100.000000     9.949538e+01    TRUE
#> 13    9.999986e+01     2021        100.000000     1.000000e+02    TRUE
#> 14    9.912622e+01     2022        100.000000     9.926642e+01    TRUE
#> 15    9.556641e+01     2023         99.023924     9.563638e+01    TRUE
#> 16    1.000000e+02     1976        100.000000     1.000000e+02    TRUE
#> 17    8.965114e+01     2024         99.926418     8.965114e+01   FALSE
#> 18    4.987947e+00     2024         93.067780     4.987947e+00   FALSE
#> 19    7.723249e+01     2020         99.700015     7.900581e+01   FALSE
#> 20    9.892969e+01     2024         99.999320     9.892969e+01    TRUE
#> 21    9.984214e+01     2022        100.000000     1.000000e+02    TRUE
#> 22    2.624801e+01     2024         97.912022     2.624801e+01   FALSE
#> 23    1.000000e+02     1976        100.000000     1.000000e+02    TRUE
#> 24    9.995792e+01     2019        100.000000     9.997898e+01    TRUE
#> 25    4.013641e+01     2014         98.055974     4.418623e+01   FALSE
#> 26    1.910830e+01     2024         99.985034     1.910830e+01   FALSE
#> 27    1.211109e+00     2020         23.611671     1.291648e+00   FALSE
#> 28    2.127680e+00     2024         25.040631     2.127680e+00   FALSE
#> 29    2.124901e+01     2016         80.015019     2.368591e+01   FALSE
#> 30    5.212017e+00     2024         83.985648     5.212017e+00   FALSE
#> 31    9.999884e+01     1998        100.000000     1.000000e+02    TRUE
#> 32    4.631016e+01     1996         95.617812     9.972968e+01   FALSE
#> 33    1.087872e-01     2024         99.839840     1.087872e-01   FALSE
#> 34    3.711057e-03     2022         94.673645     5.910427e-03   FALSE
#> 35    6.304089e+00     2020         47.683482     1.674852e+01   FALSE
#> 36    1.000000e+02     1976        100.000000     1.000000e+02    TRUE
#> 37    2.203183e+01     2024        100.000000     2.203183e+01   FALSE
#> 38    5.123515e+01     2023         95.627724     5.527221e+01   FALSE
fao_capture_groups(years = 1976:1990)      # shrimps were then mostly caught
#>    isscaap_code                           isscaap_en             isscaap_zh
#> 1            11   Carps, barbels and other cyprinids   鲤、鲃及其他鲤科鱼类
#> 2            12          Tilapias and other cichlids 罗非鱼及其他丽鱼科鱼类
#> 3            13      Miscellaneous freshwater fishes           其他淡水鱼类
#> 4            21              Sturgeons, paddlefishes           鲟、匙吻鲟类
#> 5            22                           River eels                 河鳗类
#> 6            23              Salmons, trouts, smelts       鲑、鳟、胡瓜鱼类
#> 7            24                                Shads                 西鲱类
#> 8            25      Miscellaneous diadromous fishes         其他洄游性鱼类
#> 9            31           Flounders, halibuts, soles           鲆、鲽、鳎类
#> 10           32                Cods, hakes, haddocks   鳕、无须鳕、黑线鳕类
#> 11           33         Miscellaneous coastal fishes           其他沿岸鱼类
#> 12           34        Miscellaneous demersal fishes           其他底层鱼类
#> 13           35        Herrings, sardines, anchovies       鲱、沙丁鱼、鳀类
#> 14           36           Tunas, bonitos, billfishes   金枪鱼、狐鲣、旗鱼类
#> 15           37         Miscellaneous pelagic fishes         其他中上层鱼类
#> 16           38              Sharks, rays, chimaeras         鲨、鳐、银鲛类
#> 17           39         Marine fishes not identified       未鉴定的海洋鱼类
#> 18           41               Freshwater crustaceans             淡水甲壳类
#> 19           42                   Crabs, sea-spiders           蟹、蜘蛛蟹类
#> 20           43        Lobsters, spiny-rock lobsters         螯龙虾、龙虾类
#> 21           44           King crabs, squat-lobsters       帝王蟹、铠甲虾类
#> 22           45                      Shrimps, prawns                   虾类
#> 23           46        Krill, planktonic crustaceans       磷虾及浮游甲壳类
#> 24           47     Miscellaneous marine crustaceans         其他海洋甲壳类
#> 25           51                  Freshwater molluscs           淡水软体动物
#> 26           52            Abalones, winkles, conchs     鲍、玉黍螺、凤螺类
#> 27           53                              Oysters                 牡蛎类
#> 28           54                              Mussels                 贻贝类
#> 29           55                    Scallops, pectens                 扇贝类
#> 30           56            Clams, cockles, arkshells         蛤、鸟蛤、蚶类
#> 31           57      Squids, cuttlefishes, octopuses     鱿鱼、乌贼、章鱼类
#> 32           58        Miscellaneous marine molluscs       其他海洋软体动物
#> 33           71           Frogs and other amphibians         蛙及其他两栖类
#> 34           72                              Turtles                 龟鳖类
#> 35           74      Sea-squirts and other tunicates       海鞘及其他被囊类
#> 36           75 Horseshoe crabs and other arachnoids         鲎及其他蛛形类
#> 37           76    Sea-urchins and other echinoderms     海胆及其他棘皮动物
#> 38           77  Miscellaneous aquatic invertebrates     其他水生无脊椎动物
#>    capture_pct_min year_min capture_pct_first capture_pct_last counted
#> 1        11.329992     1987         17.463357        11.952998   FALSE
#> 2        54.849198     1989         82.449209        56.709757   FALSE
#> 3        77.360296     1989         91.470498        78.428817   FALSE
#> 4        94.897295     1988        100.000000        98.228942    TRUE
#> 5         9.834754     1990         31.660540         9.834754   FALSE
#> 6        60.887542     1990         80.776834        60.887542   FALSE
#> 7       100.000000     1976        100.000000       100.000000    TRUE
#> 8         4.523969     1983          9.335222         8.996830   FALSE
#> 9        99.367974     1990        100.000000        99.367974    TRUE
#> 10       99.994472     1990        100.000000        99.994472    TRUE
#> 11       98.106476     1990         99.652880        98.106476    TRUE
#> 12      100.000000     1976        100.000000       100.000000    TRUE
#> 13      100.000000     1976        100.000000       100.000000    TRUE
#> 14       99.991805     1990        100.000000        99.991805    TRUE
#> 15       98.458226     1987         99.023924        98.590723    TRUE
#> 16      100.000000     1976        100.000000       100.000000    TRUE
#> 17       99.347468     1990         99.926418        99.347468    TRUE
#> 18       63.966619     1987         93.067780        65.061106   FALSE
#> 19       99.282410     1986         99.700015        99.438375    TRUE
#> 20       99.957797     1988         99.999320        99.963169    TRUE
#> 21      100.000000     1976        100.000000       100.000000    TRUE
#> 22       74.204938     1990         97.912022        74.204938   FALSE
#> 23      100.000000     1976        100.000000       100.000000    TRUE
#> 24       99.975727     1987        100.000000        99.991855    TRUE
#> 25       95.635263     1990         98.055974        95.635263    TRUE
#> 26       98.178575     1990         99.985034        98.178575    TRUE
#> 27        9.408851     1988         23.611671        11.115724   FALSE
#> 28       17.492395     1989         25.040631        18.487944   FALSE
#> 29       62.531553     1990         80.015019        62.531553   FALSE
#> 30       60.619035     1990         83.985648        60.619035   FALSE
#> 31       99.998861     1990        100.000000        99.998861    TRUE
#> 32       71.173807     1979         95.617812        73.018127   FALSE
#> 33       91.870079     1989         99.839840        92.167322    TRUE
#> 34       64.715317     1990         94.673645        64.715317   FALSE
#> 35        9.606283     1989         47.683482        13.091957   FALSE
#> 36      100.000000     1976        100.000000       100.000000    TRUE
#> 37      100.000000     1976        100.000000       100.000000    TRUE
#> 38       75.754190     1980         95.627724        90.376045   FALSE
fao_capture_groups(min_capture_pct = 80)
#>    isscaap_code                           isscaap_en             isscaap_zh
#> 1            11   Carps, barbels and other cyprinids   鲤、鲃及其他鲤科鱼类
#> 2            12          Tilapias and other cichlids 罗非鱼及其他丽鱼科鱼类
#> 3            13      Miscellaneous freshwater fishes           其他淡水鱼类
#> 4            21              Sturgeons, paddlefishes           鲟、匙吻鲟类
#> 5            22                           River eels                 河鳗类
#> 6            23              Salmons, trouts, smelts       鲑、鳟、胡瓜鱼类
#> 7            24                                Shads                 西鲱类
#> 8            25      Miscellaneous diadromous fishes         其他洄游性鱼类
#> 9            31           Flounders, halibuts, soles           鲆、鲽、鳎类
#> 10           32                Cods, hakes, haddocks   鳕、无须鳕、黑线鳕类
#> 11           33         Miscellaneous coastal fishes           其他沿岸鱼类
#> 12           34        Miscellaneous demersal fishes           其他底层鱼类
#> 13           35        Herrings, sardines, anchovies       鲱、沙丁鱼、鳀类
#> 14           36           Tunas, bonitos, billfishes   金枪鱼、狐鲣、旗鱼类
#> 15           37         Miscellaneous pelagic fishes         其他中上层鱼类
#> 16           38              Sharks, rays, chimaeras         鲨、鳐、银鲛类
#> 17           39         Marine fishes not identified       未鉴定的海洋鱼类
#> 18           41               Freshwater crustaceans             淡水甲壳类
#> 19           42                   Crabs, sea-spiders           蟹、蜘蛛蟹类
#> 20           43        Lobsters, spiny-rock lobsters         螯龙虾、龙虾类
#> 21           44           King crabs, squat-lobsters       帝王蟹、铠甲虾类
#> 22           45                      Shrimps, prawns                   虾类
#> 23           46        Krill, planktonic crustaceans       磷虾及浮游甲壳类
#> 24           47     Miscellaneous marine crustaceans         其他海洋甲壳类
#> 25           51                  Freshwater molluscs           淡水软体动物
#> 26           52            Abalones, winkles, conchs     鲍、玉黍螺、凤螺类
#> 27           53                              Oysters                 牡蛎类
#> 28           54                              Mussels                 贻贝类
#> 29           55                    Scallops, pectens                 扇贝类
#> 30           56            Clams, cockles, arkshells         蛤、鸟蛤、蚶类
#> 31           57      Squids, cuttlefishes, octopuses     鱿鱼、乌贼、章鱼类
#> 32           58        Miscellaneous marine molluscs       其他海洋软体动物
#> 33           71           Frogs and other amphibians         蛙及其他两栖类
#> 34           72                              Turtles                 龟鳖类
#> 35           74      Sea-squirts and other tunicates       海鞘及其他被囊类
#> 36           75 Horseshoe crabs and other arachnoids         鲎及其他蛛形类
#> 37           76    Sea-urchins and other echinoderms     海胆及其他棘皮动物
#> 38           77  Miscellaneous aquatic invertebrates     其他水生无脊椎动物
#>    capture_pct_min year_min capture_pct_first capture_pct_last counted
#> 1     3.500302e+00     2004         17.463357     5.494816e+00   FALSE
#> 2     1.098374e+01     2023         82.449209     1.117275e+01   FALSE
#> 3     3.660237e+01     2024         91.470498     3.660237e+01   FALSE
#> 4     4.576254e-02     2024        100.000000     4.576254e-02   FALSE
#> 5     1.848278e+00     2022         31.660540     2.542243e+00   FALSE
#> 6     1.170463e+01     2024         80.776834     1.170463e+01   FALSE
#> 7     9.954115e+01     2006        100.000000     9.994857e+01    TRUE
#> 8     3.788331e+00     2024          9.335222     3.788331e+00   FALSE
#> 9     8.029265e+01     2024        100.000000     8.029265e+01    TRUE
#> 10    9.967430e+01     2009        100.000000     9.984749e+01    TRUE
#> 11    7.620304e+01     2024         99.652880     7.620304e+01   FALSE
#> 12    9.879630e+01     2007        100.000000     9.949538e+01    TRUE
#> 13    9.999986e+01     2021        100.000000     1.000000e+02    TRUE
#> 14    9.912622e+01     2022        100.000000     9.926642e+01    TRUE
#> 15    9.556641e+01     2023         99.023924     9.563638e+01    TRUE
#> 16    1.000000e+02     1976        100.000000     1.000000e+02    TRUE
#> 17    8.965114e+01     2024         99.926418     8.965114e+01    TRUE
#> 18    4.987947e+00     2024         93.067780     4.987947e+00   FALSE
#> 19    7.723249e+01     2020         99.700015     7.900581e+01   FALSE
#> 20    9.892969e+01     2024         99.999320     9.892969e+01    TRUE
#> 21    9.984214e+01     2022        100.000000     1.000000e+02    TRUE
#> 22    2.624801e+01     2024         97.912022     2.624801e+01   FALSE
#> 23    1.000000e+02     1976        100.000000     1.000000e+02    TRUE
#> 24    9.995792e+01     2019        100.000000     9.997898e+01    TRUE
#> 25    4.013641e+01     2014         98.055974     4.418623e+01   FALSE
#> 26    1.910830e+01     2024         99.985034     1.910830e+01   FALSE
#> 27    1.211109e+00     2020         23.611671     1.291648e+00   FALSE
#> 28    2.127680e+00     2024         25.040631     2.127680e+00   FALSE
#> 29    2.124901e+01     2016         80.015019     2.368591e+01   FALSE
#> 30    5.212017e+00     2024         83.985648     5.212017e+00   FALSE
#> 31    9.999884e+01     1998        100.000000     1.000000e+02    TRUE
#> 32    4.631016e+01     1996         95.617812     9.972968e+01   FALSE
#> 33    1.087872e-01     2024         99.839840     1.087872e-01   FALSE
#> 34    3.711057e-03     2022         94.673645     5.910427e-03   FALSE
#> 35    6.304089e+00     2020         47.683482     1.674852e+01   FALSE
#> 36    1.000000e+02     1976        100.000000     1.000000e+02    TRUE
#> 37    2.203183e+01     2024        100.000000     2.203183e+01   FALSE
#> 38    5.123515e+01     2023         95.627724     5.527221e+01   FALSE
```
