#' Creating a new Integral
#'
#' Creates a new object of class Integral
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#' @param z A character object "Trapezoid" or "Simpson" indicating the rule to be integrated over 
#'
#'@return An object of class Integral
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#'  \item{z}{The third object input}
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#' @examples
#' 
#' myX <- c(3, 20) 
#' myY <- c(15, 40) 
#' newIntegral(myX, myY, "Trapezoid")
#' @seealso classSimpson, classTrapezoid, classIntegral, newSimpson, newTrapezoid
#' @rdname newIntegral
#' @include classIntegral.R
#' @export
newIntegral = function(x, y, z){ ## construction function takes in 3 arguments; x, y, and rule
  object = new("Integral", x = x, y = y, rule = z) ## creates a new object setting the 3 inputs as defined for class Integral
  return(object) ## returns the object
}
