# fao_font(): 图表文字的字体

英文原页：[`fao_font()`](https://linbojun99.github.io/faofishingreport/reference/fao_font.md)

## 说明

该函数返回所有图表文字使用的字体。字体按操作系统选择，以便正确显示中文：macOS上为”Songti
SC”，Windows上为”SimSun”，Linux上为”Noto Serif CJK
SC”。如果该字体未安装，则使用下一个可用的中文字体（都没有时使用”sans”）。

English

This function returns the font used for all chart text. The font is
chosen for the operating system so that Chinese text displays correctly:
“Songti SC” on macOS, “SimSun” on Windows and “Noto Serif CJK SC” on
Linux. If that font is not installed, the next available Chinese font is
used.

## 用法

``` r

fao_font(family = NULL)
```

## 参数

- `family`:

  可选，从现在起（在本次R会话中）使用的字体族，如”Microsoft
  YaHei”。默认为NULL，即不改变字体，只返回当前字体。也可以用`options(faofishingreport.font = "...")`设置字体。

  English

  Optional font family to use from now on, e.g. “Microsoft YaHei”. The
  choice can also be set with `options(faofishingreport.font = "...")`.

## 返回值

字体族的名称。

English

The font family name.

## 另请参阅

Other settings and language:
[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md),
[`fao_translate()`](https://linbojun99.github.io/faofishingreport/reference/fao_translate.md)

## 示例

``` r

fao_font()                    # font chosen for this computer
fao_font("Microsoft YaHei")   # use another font
```
