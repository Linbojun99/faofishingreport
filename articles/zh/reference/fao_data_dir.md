# fao_data_dir(): 下载的FAO数据的保存目录

英文原页：[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)

## 说明

返回[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)和[`download_fishbase_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fishbase_data.md)保存数据、所有函数查找数据的目录：优先使用`fao_settings(data_dir = ...)`设置的文件夹，其次是环境变量`FAOFISHINGREPORT_DATA_DIR`，否则为R的用户数据文件夹（`tools::R_user_dir("faofishingreport", "data")`）。

English

Returns the directory where
[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)
and
[`download_fishbase_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fishbase_data.md)
save data and where all functions look for it: the folder set with
`fao_settings(data_dir = ...)`, else the environment variable
`FAOFISHINGREPORT_DATA_DIR`, else the user data folder of R
(`tools::R_user_dir("faofishingreport", "data")`).

## 用法

``` r

fao_data_dir()
```

## 返回值

一个字符串，即该目录的路径。

English

A character string with the directory path.

## 详细说明

如果要在每个R会话中都使用同一个文件夹，例如由多台电脑或一个团队共享的云端文件夹，可以在用户主目录的`.Renviron`文件（`file.path(Sys.getenv("HOME"), ".Renviron")`）中加一行`FAOFISHINGREPORT_DATA_DIR=<文件夹>`，然后重启R。

English

To use the same folder in every R session, e.g. a cloud folder shared by
several computers or a team, add a line
`FAOFISHINGREPORT_DATA_DIR=<folder>` to the `.Renviron` file in your
home folder (`file.path(Sys.getenv("HOME"), ".Renviron")`) and restart
R.

## 另请参阅

Other FAO data:
[`check_fao_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fao_update.md),
[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md),
[`fao_data_info()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_info.md),
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)

## 示例

``` r

fao_data_dir()
fao_settings(data_dir = "D:/shared/fao_data")   # e.g. a folder shared by a team
```
