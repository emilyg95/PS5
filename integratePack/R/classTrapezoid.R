#' Create New Trapezoid
#'
#' Creates a new object of S4 class Trapezoid
#'
#' @slot x A numeric object
#' @slot y A numeric object with the same dimensionality as \code{x}.
#' @slot rule A character object "Trapezoid" indicating class 
#'
#'#'
#' @return A list with the elements
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#'  \item{z}{The third object input}
#'  
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#'
#' @seealso classIntegral, classSimpson
#' @rdname classTrapezoid
#' @include classIntegral.R
#' @export
setClass(Class = "Trapezoid", ## creates a new class "Trapezoid"
         contains = "Integral"
) ## inherits inputs from class Integral
#'
checkLength = function(object){ ## creates a function to check if x and y are the same length and returns error message if not
  test1 = (length(object@x) == length(object@y))
  if (!test1){
    return("x and y must be of the same length")}
  else{
    return(TRUE)}
}
#'
checkSpacing = function(object){ ## creates a function to check if x values are evenly spaced
  sub_x = setdiff(object@x, max(object@x)) ## creates a new vector of x values without the highest one
  difference = max(object@x)-max(sub_x) ## takes the difference between the highest and second highest x value
  even_spaced = seq(min(object@x), max(object@x), difference) ## creates a vector from the minimum x value to the maximum x value spaced evenly by the value of difference
  test1 = (even_spaced == object@x) ##  returns a logical vector indicating whether all the elements of x and the evenly spaced vector are the same
  if (all(test1) != TRUE){ ## tests if all the elements of the logical vector are true and sends an error message if not
    return("x values must be evenly spaced")} 
  else{
    return(TRUE)}
}
#'
checkRuleTrap = function(object){ ## creates a function that forces the rule input to be Trapezoid
  if(object@rule != "Trapezoid"){
    return("rule must be 'Trapezoid'")}
  else{
    return(TRUE)}
}
#'
checkValidityTrap = function(object){ ## feeds checkLength, checkSpacing, and checkRuleTrap into validity function for trapezoid
  if (checkLength(object) != TRUE | checkSpacing(object) != TRUE | checkRuleTrap(object) != TRUE){ ## returns an error message if any test does not pass
    return("object is not a valid value")}
  else{
    return(TRUE)}
}
#'
setValidity("Trapezoid", checkValidityTrap) ## sets checkValidityTrap as the constraints for an object to be of class trapezoid
#'
#' @export
#' setMethod("initialize", "Trapezoid",
function(.Object, ...){
  value = callNextMethod()
  validObject(value)
  return(value)
}
)
#'
newTrapezoid = function(x, y, z){ ## construction function takes in 3 arguments; x, y, and rule
  object = new("Trapezoid", x = x, y = y, rule = z) ## creates a new object setting the 3 inputs as defined for class Trapezoid
  return(object) ## returns the object
}