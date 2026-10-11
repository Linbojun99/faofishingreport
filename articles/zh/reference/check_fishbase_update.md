# check_fishbase_update(): 检查FishBase和SeaLifeBase的最新版本

英文原页：[`check_fishbase_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fishbase_update.md)

## 说明

本函数列出网上发布的FishBase和SeaLifeBase版本（rfishbase包使用的静态快照），并把其中最新的版本与本机已下载的版本进行比较。

English

This function lists the FishBase and SeaLifeBase releases published
online (the static snapshots used by the rfishbase package) and compares
the newest with the releases downloaded on this computer.

## 用法

``` r

check_fishbase_update(
  server = c("fishbase", "sealifebase"),
  data_dir = fao_data_dir(),
  quiet = FALSE
)
```

## 参数

- `server`:

  “fishbase”、“sealifebase”或两者（默认）。

  English

  “fishbase”, “sealifebase” or both (default).

- `data_dir`:

  下载数据的保存目录。默认为[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)。

  English

  The directory where downloaded data is stored. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- `quiet`:

  逻辑值。为TRUE时不显示任何信息。默认为FALSE。

  English

  A logical value. If TRUE, no message is printed. Default is FALSE.

## 返回值

以不可见方式返回一个数据框，每个数据库（`server`）一行：网上最新的版本（离线时为NA）、本机已下载的最新版本、网上和本机已下载的数据表数量，以及是否有可用的更新（`update_available`：本机没有已下载的版本或网上有更新的版本时为TRUE；某个版本只下载了本包所需的数据表时，也视为最新）。

English

Invisibly, a data frame with one row per server: newest online release
(NA when offline), newest downloaded release, number of tables online
and downloaded, and whether an update is available (`update_available`:
TRUE when nothing has been downloaded or a newer release is online; a
release of which only the tables the package needs were downloaded is up
to date).

## 参考文献

Froese, R. and Pauly, D. (Eds.) FishBase. World Wide Web electronic
publication. <https://www.fishbase.se>

Palomares, M.L.D. and Pauly, D. (Eds.) SeaLifeBase. World Wide Web
electronic publication. <https://www.sealifebase.se>

Boettiger, C., Lang, D.T. and Wainwright, P.C. (2012). rfishbase:
exploring, manipulating and visualizing FishBase data from R. Journal of
Fish Biology 81(6): 2030-2039.
[doi:10.1111/j.1095-8649.2012.03464.x](https://doi.org/10.1111/j.1095-8649.2012.03464.x).
Source code: <https://github.com/ropensci/rfishbase>

## 示例

``` r

check_fishbase_update()
```
