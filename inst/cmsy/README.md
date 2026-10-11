# CMSY++ files bundled with faofishingreport

These files are the unmodified March 2021 release of CMSY++ from GEOMAR's repository OceanRep,
record 52147 (<https://oceanrep.geomar.de/id/eprint/52147/>), file
`UserGuideCodeDataMarch2021.zip` (<https://oceanrep.geomar.de/id/eprint/52147/4/UserGuideCodeDataMarch2021.zip>,
SHA-256 `568ad918dce4136a5e6a45c5af5a459c5ecb98578ca8dd8de0989aefbdda77fe`), published under the
Creative Commons Attribution 4.0 International licence (CC BY 4.0,
<https://creativecommons.org/licenses/by/4.0/>). The same code is on GitHub at
<https://github.com/SISTA16/cmsyPlusPlus> (no licence stated there).

| File | Content | MD5 |
|---|---|---|
| `CMSY++16.R` | CMSY++ R code, version of January 2021 | `9ed00c053d05a10024170be1a48f8d85` |
| `ffnn.bin` | Trained neural network for the default B/k priors | `367498b9060d29649ec4b06cc6fe16bd` |
| `CMSYUserGuideMarch2021.pdf` | User Guide for CMSY++ (17 pp.) | `acba624286bd29ce43469888291d0b1e` |
| `Train_Catch_9e.csv`, `Train_ID_9e.csv` | Example catch and ID files (400 training stocks) | `936b35fbf9483d7693a6cd1f5af89b0a`, `4b497654e8df98a41ad9821b47ae13b1` |

`faofishingreport::cmsy_run()` never changes these files. It writes a changed copy,
`CMSY++16_faofishingreport.R`, into each run folder, together with `code_changes.csv`, which lists
every change (see `cmsy_system()`): the operating-system changes of the user guide (step 12:
`windows()` to `quartz()` and `antialias = "cleartype"` to `"default"` on macOS), the removal of
the automatic package installation, of `setwd(rstudioapi::...)` and of the archived snpar package
(whose runs test is reproduced), and the settings of the run.

Please cite:

- Froese R, Winker H, Coro G, Palomares MLD, Tsikliras AC, Dimarchopoulou D, Touloumis K, Demirel N,
  Vianna GMS, Scarcella G, Schijns R, Liang C, Pauly D (2023). New developments in the analysis of
  catch time series as the basis for fish stock assessments: the CMSY++ method. Acta Ichthyologica
  et Piscatoria 53: 173-189. <https://doi.org/10.3897/aiep.53.105910>
- Froese R, Demirel N, Coro G, Winker H (2021). User Guide for CMSY++. GEOMAR, Germany, 17 pp.
  <https://oceanrep.geomar.de/id/eprint/52147/>

CMSY++ needs JAGS (<https://mcmc-jags.sourceforge.io/>), which is not bundled: `cmsy_setup()`
downloads the installer for the operating system from the links listed by `cmsy_sources()` and
checks its size and checksum. Installers copied into a folder `inst/jags/` of the package source
are used instead of downloading.
