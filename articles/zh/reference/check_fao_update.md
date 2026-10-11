# check_fao_update(): 检查FAO全球捕捞产量数据的最新版本

英文原页：[`check_fao_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fao_update.md)

## 说明

该函数读取FAO渔业数据索引，列出线上发布的全球捕捞产量（Global Capture
Production）各个版本，并把最新版本与包自带的数据以及本机已下载的数据进行比较。

English

This function reads the FAO fishery data index, lists the Global Capture
Production releases published online, and compares the newest release
with the data bundled with the package and the data downloaded on this
computer.

## 用法

``` r

check_fao_update(data_dir = fao_data_dir(), quiet = FALSE)
```

## 参数

- `data_dir`:

  下载的数据版本的保存目录。默认为[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)。

  English

  The directory where downloaded releases are stored. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- `quiet`:

  逻辑值。为TRUE时不显示任何信息。默认为FALSE。

  English

  A logical value. If TRUE, no message is printed. Default is FALSE.

## 返回值

以不可见方式返回一个列表，包含`latest`（线上最新版本）、`bundled`（包自带数据的版本）、`local`（已下载的最新版本，没有则为NA）、`update_available`（逻辑值，线上最新版本比包自带数据和已下载数据都新时为TRUE）和`releases`（线上所有版本组成的数据框：版本号`version`、发布日期`date`、文件大小`size`和下载地址`url`）。

English

Invisibly, a list with `latest` (newest online version), `bundled`
(version bundled with the package), `local` (newest downloaded version,
or NA if none), `update_available` (logical) and `releases` (a data
frame of all online releases).

## 另请参阅

Other FAO data:
[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md),
[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md),
[`fao_data_info()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_info.md),
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)

## 示例

``` r

check_fao_update()
```
