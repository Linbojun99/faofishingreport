# 函数参考

每个函数的中文说明，附英文原文对照；用法、示例和参考文献与英文帮助页面相同。在R中可以用`?函数名`查看英文帮助。

## 💾 FAO数据

| 函数 | 说明 |
|----|----|
| [`check_fao_update()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/check_fao_update.md) | 检查FAO全球捕捞产量数据的最新版本 |
| [`download_fao_data()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/download_fao_data.md) | 下载FAO全球捕捞产量数据的某个版本 |
| [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/load_fao_data.md) | 读取FAO全球捕捞产量数据 |
| [`fao_data_info()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fao_data_info.md) | 所用FAO数据的信息 |
| [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fao_data_dir.md) | 下载的FAO数据的保存目录 |

## 🌊 渔区分析

| 函数 | 说明 |
|----|----|
| [`fao_areas()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fao_areas.md) | FAO主要渔区 |
| [`fishing_area_map()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fishing_area_map.md) | FAO主要渔区及其分区地图 |
| [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fishing_area_data.md) | 提取指定渔区的数据并绘图（可选） |
| [`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fishing_area_species.md) | 渔区主要物种分析 |
| [`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fishing_area_country.md) | 渔区主要国家分析 |

## 🐟 物种与国家

| 函数 | 说明 |
|----|----|
| [`species_country()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/species_country.md) | 按国家产量排名分析指定物种并绘图 |
| [`country_species()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/country_species.md) | 分析并绘制某个国家在渔区内的主要捕捞物种 |
| [`country_analysis()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/country_analysis.md) | 分析并绘制某个国家的全球渔获量 |

## 📊 汇总分析

| 函数 | 说明 |
|----|----|
| [`species_group_composition()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/species_group_composition.md) | 按物种类群的渔获量组成 |
| [`stock_status()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/stock_status.md) | 资源状态图 |
| [`peak_trend()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/peak_trend.md) | 峰值渔获量与近期趋势 |
| [`area_overview()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/area_overview.md) | 全部FAO海洋渔区概览 |
| [`data_quality()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/data_quality.md) | 捕捞产量统计的数据质量 |
| [`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/diversity_concentration.md) | 渔获量的多样性与集中度 |
| [`country_group_summary()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/country_group_summary.md) | 按国家分组的渔获量 |
| [`regime_shift()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/regime_shift.md) | 主要物种渔获量的同步突变点 |
| [`coastal_distant()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/coastal_distant.md) | 沿岸国与远洋捕捞国的渔获量 |

## 🔬 文献方法分析

| 函数 | 说明 |
|----|----|
| [`catch_shocks()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/catch_shocks.md) | 渔获骤变 |
| [`collapse_analysis()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/collapse_analysis.md) | 渔业崩溃 |
| [`catch_synchrony()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/catch_synchrony.md) | 渔获量的同步性与稳定性 |
| [`distant_water_profile()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/distant_water_profile.md) | 一个国家的远洋捕捞概况 |
| [`compare_releases()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/compare_releases.md) | 比较两个FAO数据版本 |
| [`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/pelagic_demersal.md) | 中上层/底层渔获比 |
| [`fao_pd_groups()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fao_pd_groups.md) | 中上层与底层物种类群 |

## 🌡️ FishBase与温度

| 函数 | 说明 |
|----|----|
| [`check_fishbase_update()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/check_fishbase_update.md) | 检查FishBase和SeaLifeBase的最新版本 |
| [`download_fishbase_data()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/download_fishbase_data.md) | 下载FishBase和SeaLifeBase数据表 |
| [`fishbase_table()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fishbase_table.md) | 读取FishBase或SeaLifeBase数据表 |
| [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fishbase_match.md) | 将FAO物种条目与FishBase、SeaLifeBase匹配 |
| [`fishbase_sources()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fishbase_sources.md) | FishBase数据的来源、许可协议和匹配规则 |
| [`fao_sst_products()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fao_sst_products.md) | 海表温度数据产品 |
| [`download_sst_data()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/download_sst_data.md) | 下载海表温度数据产品 |
| [`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fao_area_sst.md) | FAO主要渔区的海表温度 |
| [`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/mean_temperature_catch.md) | 渔获平均温度 |

## 📉 CMSY++资源评估

| 函数 | 说明 |
|----|----|
| [`cmsy_area()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_area.md) | 一次完成某个渔区主要物种的CMSY++评估 |
| [`cmsy_system()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_system.md) | 检查本机的CMSY++运行环境并选择相应设置 |
| [`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_setup.md) | 安装JAGS和CMSY++所需的R包 |
| [`cmsy_jags_versions()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_jags_versions.md) | 各R版本对应的JAGS和rjags版本 |
| [`cmsy_download_jags()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_download_jags.md) | 下载适合本机的JAGS安装程序 |
| [`cmsy_install_jags()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_install_jags.md) | 下载并打开JAGS安装程序 |
| [`cmsy_sources()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_sources.md) | CMSY++及其所需软件的来源 |
| [`cmsy_priors()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_priors.md) | 来自FishBase和SeaLifeBase的CMSY++先验 |
| [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_input.md) | 将FAO渔获数据转换为CMSY++输入文件 |
| [`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_defaults.md) | CMSY++设置及其默认值 |
| [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_run.md) | 用FAO渔获数据运行CMSY++ |
| [`cmsy_plot()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_plot.md) | CMSY++结果图表 |
| [`cmsy_mcmc()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_mcmc.md) | CMSY++运行的MCMC设置与收敛诊断 |
| [`cmsy_forecast()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_forecast.md) | 不同捕捞情景下的CMSY++预测 |
| [`cmsy_compare()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_compare.md) | 比较多次CMSY++运行结果，或与其他评估比较 |
| [`cmsy_tables()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/cmsy_tables.md) | 报告用的CMSY++先验参数表和结果表 |

## 📝 报告

| 函数 | 说明 |
|----|----|
| [`fao_report()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fao_report.md) | FAO渔区评估报告 |

## 🧩 扩展：FAO其他数据集

| 函数 | 说明 |
|----|----|
| [`load_fao_divisions()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/load_fao_divisions.md) | 按分区的渔获量：区域渔业组织的FAO数据 |
| [`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/load_fao_trade.md) | 水产品贸易：FAO数据 |
| [`catch_trade()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/catch_trade.md) | 以捕捞为主的物种类群的渔获量与贸易 |
| [`fao_capture_groups()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fao_capture_groups.md) | 以捕捞为主（而非养殖）的物种类群 |
| [`load_fao_employment()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/load_fao_employment.md) | 渔业与水产养殖就业：FAO数据 |
| [`employment_summary()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/employment_summary.md) | 渔业与水产养殖逐年就业情况 |
| [`catch_per_fisher()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/catch_per_fisher.md) | 人均渔获 |

## 🎨 设置与语言

| 函数 | 说明 |
|----|----|
| [`fao_settings()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fao_settings.md) | 图表、地图和输出的设置 |
| [`fao_font()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fao_font.md) | 图表文字的字体 |
| [`fao_translate()`](https://linbojun99.github.io/faofishingreport/articles/zh/reference/fao_translate.md) | 翻译FAO的国家、物种或渔区名称 |
