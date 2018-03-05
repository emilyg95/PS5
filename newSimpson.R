#' Creating a new Simpson
#'
#' Creates a new object of class Simpson
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#' @param z A character object "Simpson" indicating the rule to be integrated over 
#'
#'@return An object of class Simpson
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#'  \item{z}{The third object input}
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#' @examples
#' 
#' myX <- c(3, 20) 
#' myY <- c(15, 40) 
#' newIntegral(myX, myY, "Simpson")
#' @seealso classSimpson, classTrapezoid, classIntegral, newIntegral, newTrapezoid
#' @rdname newSimpson
#' @include classSimpson.R
#' @export
newSimpson = function(x, y, z){ ## construction function takes in 3 arguments; x, y, and rule
  object = new("Simpson", x = x, y = y, rule = z) ## creates a new object setting the 3 inputs as defined for class Simpson
  return(object) ## returns the object
}