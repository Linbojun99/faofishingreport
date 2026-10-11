# cmsy_install_jags(): 下载并打开JAGS安装程序

英文原页：[`cmsy_install_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_install_jags.md)

## 说明

本函数下载与操作系统和R版本匹配的JAGS安装程序（见[`cmsy_jags_versions()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_jags_versions.md)和[`cmsy_download_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_download_jags.md)），核对文件大小和校验值，然后打开它：Windows上是`.exe`安装程序，macOS上是`.pkg`安装程序。在Linux上则下载源代码，并说明如何用包管理器或从源代码安装JAGS。如果在包的`inst/jags/`文件夹（安装后为`jags/`）中放有安装程序，则直接使用该文件，不再下载。

English

This function downloads the JAGS installer that matches the operating
system and R version (see
[`cmsy_jags_versions()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_jags_versions.md)
and
[`cmsy_download_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_download_jags.md)),
checks its size and checksums, and opens it: the `.exe` installer on
Windows and the `.pkg` installer on macOS. On Linux it downloads the
source code and shows how to install JAGS with the package manager or
from source. An installer placed in the package folder `inst/jags/`
(installed as `jags/`) is used instead of downloading.

## 用法

``` r

cmsy_install_jags(
  version = "recommended",
  os = "auto",
  open = TRUE,
  data_dir = fao_data_dir(),
  silent = fao_setting("silent")
)
```

## 参数

- `version`:

  JAGS版本：“recommended”（默认）、“latest”或版本号。

  English

  The JAGS version: “recommended” (default), “latest” or a version
  number.

- `os`:

  采用哪个操作系统的设置：“auto”（默认，自动检测）、“windows”、“macos”或”linux”。

  English

  The operating system whose settings are used: “auto” (default, detect
  it), “windows”, “macos” or “linux”.

- `open`:

  逻辑值。为TRUE（默认）时打开安装程序。

  English

  A logical value. If TRUE (default), the installer is opened.

- `data_dir`:

  下载文件的保存目录。默认为[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)。

  English

  The directory where downloads are kept. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- `silent`:

  逻辑值。为TRUE时不输出汇总信息。默认为FALSE，或`fao_settings(silent = )`设置的值。

  English

  A logical value. If TRUE, no summary is printed. Default is FALSE, or
  the value set with `fao_settings(silent = )`.

## 返回值

以不可见方式返回所下载安装程序的路径。

English

Invisibly, the path of the downloaded installer.

## 参考文献

Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical
models using Gibbs sampling. Proceedings of the 3rd International
Workshop on Distributed Statistical Computing (DSC 2003), Vienna.
<https://mcmc-jags.sourceforge.io/>

## 示例

``` r

# Download the installer for this system (size and checksum are checked) and open it
cmsy_install_jags()

# Only download it, e.g. to install it later
path <- cmsy_install_jags(open = FALSE)

# Download the Windows installer on a Mac, e.g. for a colleague
cmsy_install_jags(os = "windows", open = FALSE)
```
