#' Class Trapezoid
#'
#' An S4 class to represent a set of points to be integrated using trapezoid rule
#'
#' @slot x A numeric object
#' @slot y A numeric object with the same dimensionality as \code{x}.
#' @slot rule A character object "Trapezoid" indicating class 
#'
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#'
#' @seealso classIntegral, classSimpson
#' @rdname classTrapezoid
#' @export
#' @include classIntegral.R
setClass(Class = "Trapezoid", ## creates a new class "Trapezoid"
         contains = "Integral"
) ## inherits inputs from class Integral
