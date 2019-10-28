#' Visible leafs of plant over thermal time in an experiment
#'
#' A dataset containing the number of leafs over thermal time for each plant
#' of an experiment
#' @format a data frame with XX rows and 10 variables:
#' \describe{
#'   \item{experimentAlias}{Id of an experiment}
#'   \item{F_visible}{Number of leafs}
#'   \item{DAS}{DAS}
#'   \item{Line}{Line position in the greenhouse}
#'   \item{Position}{Position in the line in the greenhouse}
#'   \item{scenario}{Scenarios of the experiment}
#'   \item{genotypeAlias}{Genotypes used in the experiment}
#'   \item{Genetic}{Genetic}
#'   \item{thermalTime}{Thermal Time}
#'   \item{Ref}{Reference of each plant in the greenhouse}
#' }
"plant2"

#' Parameters of interest of plant over thermal time in an experiment
#'
#' A dataset containing biovolume, plant height (PH) and leaf area (LA) over thermal time
#' for each plant of an experiment
#' @format a data frame with XX rows and 14 variables:
#' \describe{
#'   \item{experimentAlias}{Id of an experiment}
#'   \item{Day}{date}
#'   \item{potAlias}{Id of the pot in the greenhouse}
#'   \item{scenario}{Scenarios of the experiment}
#'   \item{genotypeAlias}{Genotypes used in the experiment}
#'   \item{repetition}{Repetition}
#'   \item{Line}{Line position in the greenhouse}
#'   \item{Position}{Position in the line in the greenhouse}
#'   \item{thermalTime}{Thermal Time}
#'   \item{plantHeight}{plant height (mm)}
#'   \item{leafArea}{Leaf area}
#'   \item{biovolume}{biovolume (g)}
#'   \item{Repsce}{repetion-scenario}
#'   \item{Ref}{Reference of each plant in the greenhouse}
#' }
"plant1"

#' Parameters of interest of plants over thermal time in an experiment
#'
#' A dataset containing biovolume, plant height (PH) and leaf area (LA) over thermal time
#' for each plant of an experiment
#' @format a data frame with XX rows and 15 variables:
#' \describe{
#'   \item{Ref}{Reference of each plant in the greenhouse}
#'   \item{experimentAlias}{Id of an experiment}
#'   \item{Day}{date}
#'   \item{potAlias}{Id of the pot in the greenhouse}
#'   \item{scenario}{Scenarios of the experiment}
#'   \item{genotypeAlias}{Genotypes used in the experiment}
#'   \item{repetition}{Repetition}
#'   \item{Line}{Line position in the greenhouse}
#'   \item{Position}{Position in the line in the greenhouse}
#'   \item{thermalTime}{Thermal Time}
#'   \item{plantHeight}{plant height (mm)}
#'   \item{leafArea}{Leaf area}
#'   \item{biovolume}{biovolume (g)}
#'   \item{Repsce}{repetion-scenario}
#' }
"plant3"

#' Parameters of interest of plants in an experiment for outlier detection purpose
#'
#' A dataset containing biovolume (biovolume24), plant height (PH24) and phyllocron (phy)
#' for each plant of an experiment for outlier detection purpose
#' @format a data frame with 1680 rows and 11 variables:
#' \describe{
#'   \item{Line}{Line position in the greenhouse}
#'   \item{Position}{Position in the line in the greenhouse}
#'   \item{Ref}{Reference of each plant in the greenhouse}
#'   \item{potAlias}{Id of the pot in the greenhouse}
#'   \item{scenario}{Scenarios of the experiment}
#'   \item{genotypeAlias}{Genotypes used in the experiment}
#'   \item{repetition}{Repetition}
#'   \item{Repsce}{repetion-scenario}
#'   \item{Biomass24}{biomass at 24 TT day (g)}
#'   \item{PH24}{plant height at 24 TT day (mm)}
#'   \item{Phy}{Phyllocron}
#' }
"plant4"
