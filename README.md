# phisStatR

A set of statistical functions and rmarkdown scripts to analyze experiment in the phenoarch greenhouse - phis OpenSilex system information.
  
* Spatial representation of a greenhouse or a lattice platform
* Create a video with imageGreenhouse() function. see vignette _videoImageGreenhouse_
* Detection of outliers in a set of points using the smoothing of a local regression (Loader 2013) and calculating a confidence interval of the prediction. see vignette _detectOutlierPoints_
* Detection of outliers in a set of points using a bayesian spatio-temporal ANOVA model (Lee, 2018). see vignette _CARBayesSTReport_
* Detection of outlier time courses using a nonparametric spline (Gu, 2014). see vignette _gssAnalysisReport_
* Detection of outlier plant, defined as a biological replicate deviating from the overall distribution of plants on a
multi-criteria basis, regardless of the quality of measurements. see vignette _detectOutlierCurves_

__Package In progress...__

The functions of the package are constructed according to a data set structure of a lattice experiment (spatial coordinates) and for some with temporal informations and therefore are not entirely generic. Please have a look to the structure of the example datasets provided by the package (plant1, plant2, plant3 and plant4). Mostly, the following columns are required:

* the Line and Position columns (coordinates in a greenhouse)
* Ref: a unique identifiant
* genotypeAlias: genotypes used in the experiment
* scenario: scenario applied to the experiment

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

# References

1. Maria Xose Rodriguez-Alvarez, Martin P. Boer, Fred A. van Eeuwijk, Paul H.C. Eilers (2018). Correcting
for spatial heterogeneity in plant breeding experiments with P-splines. Spatial Statistics 23 52 - 71
URL https://doi.org/10.1016/j.spasta.2017.10.003
2. Gu, C. (2013), Smoothing Spline ANOVA Models (2nd Ed). New York: Springer-Verlag.
3. Gu, C. (2014), Smoothing Spline ANOVA Models: R Package gss. Journal of Statistical Software, 58(5),
1-25. URL http://www.jstatsoft.org/v58/i05/.
4. Lee D, Rushworth A, Napier G (2018). “Spatio-Temporal Areal Unit Modeling in R with Conditional Autoregressive Priors Using the CARBayesST Package.” Journal of Statistical Software, 84(9), 1–39. doi: 10.18637/jss.v084.i09.
5. Catherine Loader (2013). locfit: Local Regression, Likelihood and Density Estimation.. R package version 1.5-9.1. https://CRAN.R-project.org/package=locfit