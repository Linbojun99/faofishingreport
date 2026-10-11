# fishbase_table(): 读取FishBase或SeaLifeBase数据表

英文原页：[`fishbase_table()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_table.md)

## 说明

本函数返回FishBase或SeaLifeBase的一张数据表，例如”species”（名称、栖息类型、最大体长……）或”estimate”（营养级、温度偏好……）。数据表从本机已下载的副本读取：

- 电脑联网时，先查询网上最新的版本；本机还没有这张表时，先下载；
- 电脑离线时（或设置了`fao_settings(online = FALSE)`时），使用本机已下载、且含有这张表的最新版本，并给出提示信息。

各版本都是固定的快照，所以下载的数据表与网上的完全相同。

English

This function returns one table of FishBase or SeaLifeBase, such as
“species” (names, habitat, maximum length, …) or “estimate” (trophic
level, temperature preference, …). It is read from the copy downloaded
on this computer:

- when the computer is online, the newest release is looked up first,
  and the table is downloaded if it is not on this computer yet;
- when the computer is offline (or with `fao_settings(online = FALSE)`),
  the newest downloaded release that has the table is used, with a
  message.

Releases are fixed snapshots, so a downloaded table is identical to the
one online.

## 用法

``` r

fishbase_table(
  table,
  server = c("fishbase", "sealifebase"),
  version = "latest",
  data_dir = fao_data_dir()
)
```

## 参数

- `table`:

  表名，例如”species”、“estimate”、“ecology”、“synonyms”或”families”。[`check_fishbase_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fishbase_update.md)可以显示一个版本有多少张数据表；各表的内容见rfishbase包的文档。

  English

  The table name, e.g. “species”, “estimate”, “ecology”, “synonyms” or
  “families”.
  [`check_fishbase_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fishbase_update.md)
  shows how many tables a release has; see the rfishbase documentation
  for what they contain.

- `server`:

  “fishbase”（默认）或”sealifebase”。

  English

  “fishbase” (default) or “sealifebase”.

- `version`:

  版本，例如”26.06”。默认为”latest”。

  English

  The release, e.g. “26.06”. Default is “latest”.

- `data_dir`:

  下载数据的保存目录。默认为[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)。

  English

  The directory where downloaded data is stored. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

## 返回值

一个数据框，带有属性`fishbase_server`、`fishbase_version`和`source_url`。

English

A data frame, with the attributes `fishbase_server`, `fishbase_version`
and `source_url`.

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

estimate <- fishbase_table("estimate")
attr(estimate, "fishbase_version")
```
