#' Creating a new Trapezoid
#'
#' Creates a new object of class Trapezoid
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#' @param z A character object "Trapezoid" indicating the rule to be integrated over 
#'
#'@return An object of class Trapezoid
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#'  \item{z}{The third object input}
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#' @examples
#' 
#' myX <- c(3, 20) 
#' myY <- c(15, 40) 
#' newIntegral(myX, myY, "Trapezoid")
#' @seealso classSimpson, classTrapezoid, classIntegral, newIntegral, newSimpson
#' @rdname newTrapezoid
#' @export
#' @include classTrapezoid.R
newTrapezoid = function(x, y, z){ ## construction function takes in 3 arguments; x, y, and rule
  object = new("Trapezoid", x = x, y = y, rule = z) ## creates a new object setting the 3 inputs as defined for class Trapezoid
  return(object) ## returns the object
}
