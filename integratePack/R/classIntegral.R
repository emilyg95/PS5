#' Class Integral
#'
#' An S4 class to represent a set of points to be integrated
#'
#' @slot x A numeric object
#' @slot y A numeric object with the same dimensionality as \code{x}.
#' @slot rule A character object "Trapezoid" or "Simpson" indicating subclass 
#'
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#'
#' @seealso classTrapezoid, classSimpson
#' @rdname classIntegral
#' @export
setClass(Class = "Integral", ## creates a new class "Integral"
         representation = representation(
           x = "numeric",
           y = "numeric",
           rule = "character"
         ) ## indicates 3 inputs; x and y, which must both be numeric, and rule, which must be a character
)
