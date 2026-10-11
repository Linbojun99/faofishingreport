# download_fao_data(): 下载FAO全球捕捞产量数据的某个版本

英文原页：[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)

## 说明

该函数从FAO下载全球捕捞产量（Global Capture
Production）的某个版本，将其转换为与包自带数据（`Global_Production_long`）相同的长格式，并以`.rds`文件保存在`data_dir`中。

English

This function downloads a Global Capture Production release from FAO,
converts it to the same long format as the data bundled with the package
(`Global_Production_long`), and saves it as an `.rds` file in
`data_dir`.

## 用法

``` r

download_fao_data(
  version = "latest",
  data_dir = fao_data_dir(),
  overwrite = FALSE,
  quiet = FALSE
)
```

## 参数

- `version`:

  要下载的版本，如”2026.1.0”。默认为”latest”，即线上最新版本。FAO服务器上没有的版本会报错，并列出可用的版本。

  English

  The release to download, e.g. “2026.1.0”. Default is “latest”.

- `data_dir`:

  转换后数据的保存目录。默认为[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)。

  English

  The directory where the converted data is saved. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- `overwrite`:

  逻辑值。为FALSE（默认）时，已经下载过的版本直接从磁盘读取，不再重新下载；为TRUE时重新下载。

  English

  A logical value. If FALSE (default), a release that was already
  downloaded is read from disk instead of being downloaded again.

- `quiet`:

  逻辑值。为TRUE时不显示进度信息。默认为FALSE。

  English

  A logical value. If TRUE, progress messages are not printed. Default
  is FALSE.

## 返回值

以不可见方式返回一个数据框，其列与`Global_Production_long`相同。

English

Invisibly, a data frame with the columns of `Global_Production_long`.

## 详细说明

国家和物种名称通过其FAO代码匹配到包自带数据所用的名称，因此结果可以直接用于现有的分析函数和中文名称表。与包自带数据一样，只保留以活重吨计的产量。

English

Country and species names are matched through their FAO codes to the
names used by the bundled data, so the result works with the existing
analysis functions and Chinese name tables. Only quantities in tonnes
live weight are kept, as in the bundled data.

## 另请参阅

Other FAO data:
[`check_fao_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fao_update.md),
[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md),
[`fao_data_info()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_info.md),
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)

## 示例

``` r

check_fao_update()
new_data <- download_fao_data()
```
