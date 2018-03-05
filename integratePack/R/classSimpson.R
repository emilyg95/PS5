#' Class Simpson
#'
#' An S4 class to represent a set of points to be integrated using Simpson's rule
#'
#' @slot x A numeric object
#' @slot y A numeric object with the same dimensionality as \code{x}.
#' @slot rule A character object "Simpson" indicating class 
#'
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#'
#' @seealso classTrapezoid, classIntegral
#' @rdname classSimpson
#' @export
#' @include classIntegral.R
setClass(Class = "Simpson", ## creates a new class "Simpson"
         contains = "Integral"
) ## inherits inputs from class Integral
