# Sources of CMSY++ and the Software It Needs

This function lists every component used by the CMSY++ functions of the
package, with its version, download link, file size, checksums (MD5 and
SHA-256), licence and the reference to cite: the CMSY++ code, neural
network, user guide and training data (bundled with the package), the
JAGS installers for each operating system, the rjags package, the other
R packages CMSY++ uses, and the FishBase and SeaLifeBase releases behind
the built-in table of priors (see
[`cmsy_priors()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_priors.md)).

## Usage

``` r
cmsy_sources(component = NULL)
```

## Arguments

- component:

  Optional text to keep only matching components, e.g. "JAGS" or "CMSY".

## Value

A data frame with one row per file or package.

## Details

The CMSY++ files bundled with the package are the unmodified March 2021
release from GEOMAR's repository OceanRep (record 52147), published
under the CC BY 4.0 licence; the same code is on GitHub at
<https://github.com/SISTA16/cmsyPlusPlus>.
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
applies a few documented changes to a copy of the code before running it
(see
[`cmsy_system()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_system.md));
they do not change the method. faofishingreport is not affiliated with
or endorsed by the authors of CMSY++.

## References

Froese, R., Winker, H., Coro, G., Palomares, M.L.D., Tsikliras, A.C.,
Dimarchopoulou, D., Touloumis, K., Demirel, N., Vianna, G.M.S.,
Scarcella, G., Schijns, R., Liang, C. and Pauly, D. (2023). New
developments in the analysis of catch time series as the basis for fish
stock assessments: the CMSY++ method. Acta Ichthyologica et Piscatoria
53: 173-189.
[doi:10.3897/aiep.53.105910](https://doi.org/10.3897/aiep.53.105910)

Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for
CMSY++. GEOMAR, Germany, 17 pp.
<https://oceanrep.geomar.de/id/eprint/52147/>

Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical
models using Gibbs sampling. Proceedings of the 3rd International
Workshop on Distributed Statistical Computing (DSC 2003), Vienna.
<https://mcmc-jags.sourceforge.io/>

## Examples

``` r
# Every component, with version, file, licence and link
src <- cmsy_sources()
src[, c("component", "version", "platform", "file", "license")]
#>                                     component           version     platform
#> 1                                 CMSY++ code 16 (January 2021)          all
#> 2                       CMSY++ neural network 16 (January 2021)          all
#> 3                           CMSY++ user guide        March 2021          all
#> 4                CMSY++ training data (catch)                9e          all
#> 5               CMSY++ training data (stocks)                9e          all
#> 6                                        JAGS             4.3.2      windows
#> 7                                        JAGS             4.3.1      windows
#> 8                                        JAGS             4.3.2        macos
#> 9                                        JAGS             4.3.1        macos
#> 10                                       JAGS             4.3.2       source
#> 11                                       JAGS             5.0.0      windows
#> 12                                       JAGS             5.0.0        macos
#> 13                                       JAGS             5.0.0       source
#> 14                                      rjags              4-17          all
#> 15                                      rjags               5-3      windows
#> 16                                      rjags               5-3  macos-arm64
#> 17                                      rjags               5-3 macos-x86_64
#> 18                                      rjags               5-3       source
#> 19 snpar (runs test reproduced in cmsy_run())               1.0          all
#> 20                                     R2jags             0.8-9          all
#> 21                                       coda          0.19-4.1          all
#> 22                                     gplots             3.3.0          all
#> 23                                    mvtnorm             1.4-2          all
#> 24                                  neuralnet            1.44.2          all
#> 25                                   conicfit             1.0.4          all
#> 26                                   FishBase             26.06          all
#> 27                                SeaLifeBase             26.06          all
#>                                   file
#> 1                           CMSY++16.R
#> 2                             ffnn.bin
#> 3           CMSYUserGuideMarch2021.pdf
#> 4                   Train_Catch_9e.csv
#> 5                      Train_ID_9e.csv
#> 6                       JAGS-4.3.2.exe
#> 7                       JAGS-4.3.1.exe
#> 8                       JAGS-4.3.2.pkg
#> 9                       JAGS-4.3.1.pkg
#> 10                   JAGS-4.3.2.tar.gz
#> 11                      JAGS-5.0.0.exe
#> 12            JAGS-5.0.0-universal.pkg
#> 13                   JAGS-5.0.0.tar.gz
#> 14                               rjags
#> 15                       rjags_5-3.zip
#> 16          rjags_5-3-R4.6.1-arm64.tgz
#> 17         rjags_5-3-R4.6.1-x86_64.tgz
#> 18                    rjags_5-3.tar.gz
#> 19                    snpar_1.0.tar.gz
#> 20                              R2jags
#> 21                                coda
#> 22                              gplots
#> 23                             mvtnorm
#> 24                           neuralnet
#> 25                            conicfit
#> 26 species, synonyms, stocks, estimate
#> 27 species, synonyms, stocks, estimate
#>                                                          license
#> 1                                                      CC BY 4.0
#> 2                                                      CC BY 4.0
#> 3                                                      CC BY 4.0
#> 4                                                      CC BY 4.0
#> 5                                                      CC BY 4.0
#> 6                                                          GPL-2
#> 7                                                          GPL-2
#> 8                                                          GPL-2
#> 9                                                          GPL-2
#> 10                                                         GPL-2
#> 11                                                         GPL-2
#> 12                                                         GPL-2
#> 13                                                         GPL-2
#> 14                                                         GPL-2
#> 15                                                         GPL-2
#> 16                                                         GPL-2
#> 17                                                         GPL-2
#> 18                                                         GPL-2
#> 19                                                    GPL (>= 2)
#> 20                                                     GPL (> 2)
#> 21                                                    GPL (>= 2)
#> 22                                                         GPL-2
#> 23                                                         GPL-2
#> 24                                                    GPL (>= 2)
#> 25                                                    GPL (>= 3)
#> 26 CC BY-NC 4.0 (FishBase.org team, as stated for the snapshots)
#> 27 CC BY-NC 4.0 (FishBase.org team, as stated for the snapshots)

# The JAGS installers for each system, with their checksums
cmsy_sources("JAGS")[, c("component", "version", "platform", "url", "sha256")]
#>    component version     platform
#> 1       JAGS   4.3.2      windows
#> 2       JAGS   4.3.1      windows
#> 3       JAGS   4.3.2        macos
#> 4       JAGS   4.3.1        macos
#> 5       JAGS   4.3.2       source
#> 6       JAGS   5.0.0      windows
#> 7       JAGS   5.0.0        macos
#> 8       JAGS   5.0.0       source
#> 9      rjags    4-17          all
#> 10     rjags     5-3      windows
#> 11     rjags     5-3  macos-arm64
#> 12     rjags     5-3 macos-x86_64
#> 13     rjags     5-3       source
#> 14    R2jags   0.8-9          all
#>                                                                                              url
#> 1            https://downloads.sourceforge.net/project/mcmc-jags/JAGS/4.x/Windows/JAGS-4.3.2.exe
#> 2            https://downloads.sourceforge.net/project/mcmc-jags/JAGS/4.x/Windows/JAGS-4.3.1.exe
#> 3       https://downloads.sourceforge.net/project/mcmc-jags/JAGS/4.x/Mac%20OS%20X/JAGS-4.3.2.pkg
#> 4       https://downloads.sourceforge.net/project/mcmc-jags/JAGS/4.x/Mac%20OS%20X/JAGS-4.3.1.pkg
#> 5          https://downloads.sourceforge.net/project/mcmc-jags/JAGS/4.x/Source/JAGS-4.3.2.tar.gz
#> 6   https://downloads.sourceforge.net/project/mcmc-jags/JAGS/5.x/Windows/Rtools45/JAGS-5.0.0.exe
#> 7    https://downloads.sourceforge.net/project/mcmc-jags/JAGS/5.x/macOS/JAGS-5.0.0-universal.pkg
#> 8          https://downloads.sourceforge.net/project/mcmc-jags/JAGS/5.x/Source/JAGS-5.0.0.tar.gz
#> 9                                                       https://cran.r-project.org/package=rjags
#> 10    https://downloads.sourceforge.net/project/mcmc-jags/rjags/5/Windows/Rtools45/rjags_5-3.zip
#> 11  https://downloads.sourceforge.net/project/mcmc-jags/rjags/5/macOS/rjags_5-3-R4.6.1-arm64.tgz
#> 12 https://downloads.sourceforge.net/project/mcmc-jags/rjags/5/macOS/rjags_5-3-R4.6.1-x86_64.tgz
#> 13           https://downloads.sourceforge.net/project/mcmc-jags/rjags/5/Source/rjags_5-3.tar.gz
#> 14                                                     https://cran.r-project.org/package=R2jags
#>                                                              sha256
#> 1  197f3b32f35462fdbbb566035f459bade6c61207fb2bada265be1504c0fa414f
#> 2  e7f1ea777749b17c0d64f07d80f9c3a93a5f95fa6b0b009f1568858b0106a2b3
#> 3  14d171621a582d29a7e6ce11eccf853170367c17d41554e07b88abed76a2fb7e
#> 4  6f8f220f975556aec82275c93a4556184476f4d32ee828ee8f839590fe7c94bb
#> 5  871f556af403a7c2ce6a0f02f15cf85a572763e093d26658ebac55c4ab472fc8
#> 6  f79f4af3da180f9e9a7c1c1ff036e806063d883ee485b5e7a458c50b0315acd4
#> 7  49753af665f4ebe309c4b1b7b7d53fd10009b5b19ed766e040e5581842772316
#> 8  64fcd4883b8a8ee907722f49366cc9f277477a0647ada61356f17568f84ffff8
#> 9                                                              <NA>
#> 10 2e063131bd7200de18268d7579c7f927d22b2745c178b809b160e9213970abee
#> 11 1fe67195947f9cb0df085b8d434e348a6bf4c69108bf4fa78ceee0cc5d322d5c
#> 12 c39a3527e2580271113c76c4ea504b6553d2eed7f17dd271c8ff966223b82d72
#> 13 dac0e7f2da817a15d8a796025ce2f36f0aa3154921bd520be4f98df2753f617a
#> 14                                                             <NA>

# The references to cite
unique(src$citation[nzchar(src$citation)])
#>  [1] "Froese R, Winker H, Coro G, Palomares MLD, Tsikliras AC, Dimarchopoulou D, Touloumis K, Demirel N, Vianna GMS, Scarcella G, Schijns R, Liang C, Pauly D (2023). New developments in the analysis of catch time series as the basis for fish stock assessments: the CMSY++ method. Acta Ichthyologica et Piscatoria 53: 173-189. https://doi.org/10.3897/aiep.53.105910"
#>  [2] "Froese R, Demirel N, Coro G, Winker H (2021). User Guide for CMSY++. GEOMAR, Germany, 17 pp. https://oceanrep.geomar.de/id/eprint/52147/"                                                                                                                                                                                                                              
#>  [3] "Plummer M (2003). JAGS: A program for analysis of Bayesian graphical models using Gibbs sampling. Proceedings of DSC 2003, Vienna. https://mcmc-jags.sourceforge.io/"                                                                                                                                                                                                  
#>  [4] "citation(\"rjags\")"                                                                                                                                                                                                                                                                                                                                                   
#>  [5] "Qiu D (2014). snpar: Supplementary Non-parametric Statistics Methods. R package version 1.0."                                                                                                                                                                                                                                                                          
#>  [6] "citation(\"R2jags\")"                                                                                                                                                                                                                                                                                                                                                  
#>  [7] "citation(\"coda\")"                                                                                                                                                                                                                                                                                                                                                    
#>  [8] "citation(\"gplots\")"                                                                                                                                                                                                                                                                                                                                                  
#>  [9] "citation(\"mvtnorm\")"                                                                                                                                                                                                                                                                                                                                                 
#> [10] "citation(\"neuralnet\")"                                                                                                                                                                                                                                                                                                                                               
#> [11] "citation(\"conicfit\")"                                                                                                                                                                                                                                                                                                                                                
#> [12] "Froese R, Pauly D (Eds.) (2026). FishBase. World Wide Web electronic publication, version (06/2026). https://www.fishbase.se"                                                                                                                                                                                                                                          
#> [13] "Palomares MLD, Pauly D (Eds.) (2026). SeaLifeBase. World Wide Web electronic publication, version (06/2026). https://www.sealifebase.se"                                                                                                                                                                                                                               
```
