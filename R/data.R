#' The Garden Party
#'
#' A character vector containing the full text of "The Garden Party, and Other Stories" by 
#' Katherine Mansfield.
#'
#' @docType data
#'
#' @usage katherinemansfieldr(theGardenParty)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references Moore et al. (2013) Genetics 195:1077-1086
#' (\href{http://www.ncbi.nlm.nih.gov/pubmed/23979570}{PubMed})
#'
#' @source \href{http://qtlarchive.org/db/q?pg=projdetails&proj=moore_2013b}{QTL Archive}
#'
#' @examples
#' data(grav)
#' times <- attr(grav, "time")
#' phe <- grav$pheno
#' \donttest{iplotCurves(phe, times)}
"theGardenParty"