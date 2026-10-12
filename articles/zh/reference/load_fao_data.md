# load_fao_data(): 读取FAO全球捕捞产量数据

英文原页：[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)

## 说明

该函数读取用[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)下载的某个版本，或包自带的数据。

English

This function loads a release downloaded with
[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md),
or the data bundled with the package.

## 用法

``` r

load_fao_data(version = "latest", data_dir = fao_data_dir())
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

一个数据框，其列与`Global_Production_long`相同。

English

A data frame with the columns of `Global_Production_long`.

## 另请参阅

Other FAO data:
[`check_fao_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fao_update.md),
[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md),
[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md),
[`fao_data_info()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_info.md)

## 示例

``` r

data <- load_fao_data()
FAO27 <- subset(data, FAO.major.fishing.area..Name. == "Atlantic, Northeast")
```
