# fao_data_info(): 所用FAO数据的信息

英文原页：[`fao_data_info()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_info.md)

## 说明

显示分析所用的FAO全球捕捞产量数据版本（即[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)读取的版本）、FAO发布该版本的日期、数据覆盖的年份、下载地址、许可证和引用格式，例如用于报告的数据部分。

English

Shows which FAO Global Capture Production release the analyses use (the
release that
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
loads), when FAO published it, the years it covers, where it was
downloaded from, its licence and a citation, e.g. for the data section
of a report.

## 用法

``` r

fao_data_info(version = "latest", data_dir = fao_data_dir())
```

## 参数

- `version`:

  要读取的版本，如”2026.1.0”。默认为”latest”，即已下载的各版本与包自带数据中最新的一个。使用”bundled”读取包自带的数据。指定的版本尚未下载时会报错。

  English

  The release to load, e.g. “2026.1.0”. Default is “latest”, the newest
  of the downloaded releases and the bundled data. Use “bundled” for the
  data bundled with the package.

- `data_dir`:

  下载的数据版本的保存目录。默认为[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)。

  English

  The directory where downloaded releases are stored. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

## 返回值

一个`fao_data_info`类的列表，包含`release`（版本号）、`release_date`（在FAO网站上的发布日期）、`years`（首末年份）、`source`（“bundled”表示包自带，“downloaded”表示已下载）、`url`（下载地址）、`download_date`（下载日期）、`licence`（许可证）、`terms`（使用条款）和`citation`（引用格式）。打印该对象时显示一份摘要。

English

A list of class `fao_data_info` with `release`, `release_date`
(publication date on the FAO website), `years`, `source` (“bundled” with
the package or “downloaded”), `url`, `download_date`, `licence`, `terms`
and `citation`. Printing it shows a summary.

## 另请参阅

Other FAO data:
[`check_fao_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fao_update.md),
[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md),
[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md),
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)

## 示例

``` r

fao_data_info()
fao_data_info()$citation
```
