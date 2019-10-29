# phisStatR

A set of statistical functions and rmarkdown scripts to analyze experiment in the phenoarch greenhouse - phis OpenSilex system information.

* Spatial representation of a greenhouse or a lattice platform
* Detection of outliers in a set of points using the smoothing of a local regression and calculating a confidence interval of the prediction. see vignette _detectOutlierPoints_
* Detection of outlier time courses using a nonparametric spline (Gu, 2014). see vignette _gssAnalysisReport_
* Detection of outlier plant, defined as a biological replicate deviating from the overall distribution of plants on a
multi-criteria basis, regardless of the quality of measurements. see vignette _detectOutlierCurves_

__Package In progress...__

# Installation

To install the **phisStatR** package, the easiest is to install it directly from GitHub. Open an R session and run the following commands:

```R
library(remotes) 
install_github("sanchezi/phisStatR", build_vignettes=TRUE)
```

# Usage

Once the package is installed on your computer, it can be loaded into a R session:

```R
library(phisStatR)
help(package="phisStatR")
```

# Citation

* https://github.com/OpenSILEX/opensilex-ws-client-R

* https://github.com/sanchezi/phisWSClientR

* Alvarez Prado, S., Sanchez, I., Cabrera Bosquet, L., Grau, A., Welcker, C., Tardieu, F., Hilgert, N. (2019). To clean or not to clean phenotypic datasets for outlier plants in genetic analyses?. Journal of Experimental Botany, 70 (15), 3693-3698. , DOI : 10.1093/jxb/erz191 https://prodinra.inra.fr/record/481355

* Neveu, P., Tireau, A., Hilgert, N., Negre, V., Mineau-Cesari, J., Brichet, N., Chapuis, R., Sanchez, I., Pommier, C., Charnomordic, B., Tardieu, F., Cabrera Bosquet, L. (2019). Dealing with multi-source and multi-scale information in plant phenomics: the ontology-driven Phenotyping Hybrid Information System. New Phytologist, 221 (1), 588-601. , DOI : 10.1111/nph.15385 https://prodinra.inra.fr/record/442308

You should also cite the **phisStatR** package:

```R
citation("phisStatR")
```

See also citation() for citing R itself.