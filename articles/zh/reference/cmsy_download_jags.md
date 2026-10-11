# cmsy_download_jags(): 下载适合本机的JAGS安装程序

英文原页：[`cmsy_download_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_download_jags.md)

## 说明

本函数选出与操作系统、R版本和处理器匹配的JAGS安装程序（见[`cmsy_jags_versions()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_jags_versions.md)），从其固定链接下载，并核对文件大小和校验值（MD5，以及R能够计算时的SHA-256）。对于JAGS
5，还会下载配套的rjags
5-3包。也可以下载其他系统或其他R版本的安装程序，例如用于在无法联网的电脑上安装JAGS，或者为同事下载。

English

This function picks the JAGS installer that matches the operating
system, the R version and the processor (see
[`cmsy_jags_versions()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_jags_versions.md)),
downloads it from its pinned link, and checks its size and checksums
(MD5, and SHA-256 when R can compute it). For JAGS 5 it also downloads
the matching rjags 5-3 package. It can also download the installers of
other systems or R versions, e.g. to install JAGS on computers without
internet or for colleagues.

## 用法

``` r

cmsy_download_jags(
  version = "recommended",
  os = "auto",
  r_version = getRversion(),
  arch = "auto",
  dest = NULL,
  rjags = TRUE,
  data_dir = fao_data_dir(),
  silent = fao_setting("silent")
)
```

## 参数

- `version`:

  “recommended”（默认：适合当前R版本的JAGS
  4，可与CRAN上的rjags配合使用）、“latest”（JAGS 5.0.0和rjags
  5-3）或JAGS版本号，例如”4.3.1”。

  English

  “recommended” (default: JAGS 4 for this R version, which works with
  rjags from CRAN), “latest” (JAGS 5.0.0 with rjags 5-3) or a JAGS
  version such as “4.3.1”.

- `os`:

  “auto”（默认，本机）、“windows”、“macos”、“linux”或”all”（`r_version`对应的全部三个系统的安装程序）。

  English

  “auto” (default, this computer), “windows”, “macos”, “linux” or “all”
  (the installers of all three systems for `r_version`).

- `r_version`:

  安装程序所针对的R版本。默认为当前运行的R版本。

  English

  The R version the installer is for. Default is the running R.

- `arch`:

  处理器类型，用于选择macOS上rjags
  5的二进制包：“auto”（默认）、“arm64”（Apple芯片）或”x86_64”（Intel）。

  English

  The processor, for the rjags 5 binary on macOS: “auto” (default),
  “arm64” (Apple silicon) or “x86_64” (Intel).

- `dest`:

  可选，把文件复制到的文件夹。

  English

  Optional folder to copy the files to.

- `rjags`:

  逻辑值。为TRUE（默认）时，还下载JAGS 5所需的rjags包（JAGS 4所用的rjags
  4-17由[`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)从CRAN安装）。

  English

  A logical value. If TRUE (default), the rjags package needed by JAGS 5
  is also downloaded (rjags 4-17 for JAGS 4 is installed from CRAN by
  [`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)).

- `data_dir`:

  下载文件的保存目录。默认为[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)。

  English

  The directory where downloads are kept. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- `silent`:

  逻辑值。为FALSE（默认）时，报告匹配结果和下载情况。

  English

  A logical value. If FALSE (default), the matching and the download are
  reported.

## 返回值

以不可见方式返回一个数据框，每个文件一行：系统、R版本、组件、版本、文件名、本地路径、链接、SHA-256，以及校验值是否相符（即已核对的项目；不相符时会报错）。

English

Invisibly, a data frame with one row per file: system, R version,
component, version, file, local path, link, SHA-256 and whether the
checksums matched.

## 详细说明

文件保存在本包的下载文件夹（`fao_data_dir()/cmsy/downloads`）中，因此只需下载一次；给出`dest`时还会复制到`dest`。打开安装程序即可安装JAGS（[`cmsy_install_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_install_jags.md)或[`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)会自动打开），安装后请重启R。

English

The files are kept in the download folder of the package
(`fao_data_dir()/cmsy/downloads`), so they are downloaded only once, and
copied to `dest` when given. Install JAGS by opening the installer
([`cmsy_install_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_install_jags.md)
or
[`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)
do this) and restart R.

## 参考文献

Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical
models using Gibbs sampling. Proceedings of the 3rd International
Workshop on Distributed Statistical Computing (DSC 2003), Vienna.
<https://mcmc-jags.sourceforge.io/>

## 示例

``` r

# The installer for this computer (JAGS 4 for this R version)
cmsy_download_jags()

# JAGS 5.0.0 and the matching rjags 5-3
cmsy_download_jags(version = "latest")

# All three systems for R 4.4, copied to a folder to share in a lab
cmsy_download_jags(os = "all", r_version = "4.4.1", dest = "jags_installers")
```
