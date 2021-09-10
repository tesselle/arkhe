#' @details
#'  \tabular{ll}{
#'   **Package:** \tab arkhe \cr
#'   **Type:** \tab Package \cr
#'   **Version:** \tab 0.4.0 \cr
#'   **License:** \tab GPL-3 \cr
#'   **Zenodo:** \tab \doi{10.5281/zenodo.3526659} \cr
#'  }
#'
#' @section Package options:
#'  `arkhe` uses the following [options()] to configure behavior:
#'  * `arkhe.autodetect`: a [`logical`] scalar. Try to automatically assign
#'    values to the corresponding slot of a `*Matrix` object when coercing a
#'    `data.frame`? Defaults to `TRUE`.
#'  * `arkhe.verbose`: a [`logical`] scalar. Should \R report extra information
#'    on progress? Defaults to `TRUE`.
#'
#' @author
#'  **Full list of authors and contributors** (alphabetic order):
#'
#'  \tabular{ll}{
#'   Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
#'   Brice Lebrun \tab *Université Bordeaux Montaigne, France* \cr
#'  }
#'
#'  **Package maintainer**
#'
#'  Nicolas Frerebeau\cr
#'  \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#'  IRAMAT-CRP2A (UMR 5060)\cr
#'  Maison de l'Archéologie\cr
#'  Université Bordeaux Montaigne\cr
#'  F-33607 Pessac cedex\cr
#'  France
#' @name arkhe-package
#' @aliases arkhe
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @importFrom methods Arith as as<- callGeneric callNextMethod Compare
#' .hasSlot initialize is Logic Math Math2 new Ops setClass setClassUnion
#' setGeneric setMethod slot slot<- slotNames Summary validObject
#' .valueClassTest
#' @importFrom stats quantile na.omit rmultinom sd xtabs
#' @importFrom utils capture.output combn
NULL
