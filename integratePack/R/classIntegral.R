#' Create New Integral
#'
#' Creates a new object of S4 class Integral
#'
#' @slot x A numeric object
#' @slot y A numeric object with the same dimensionality as \code{x}.
#' @slot rule A character object "Trapezoid" or "Simpson" indicating subclass 
#'
#' @return A list with the elements
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#'  \item{z}{The third object input}
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
#'
checkRuleIntegral = function(object){ ## creates a function that forces the rule input to be Trapezoid or Simpson
  if(object@rule != "Simpson" & object@rule != "Trapezoid"){
    return("rule must be 'Simpson' or 'Trapezoid'")}
  else{
    return(TRUE)}
}
#'
checkValidityIntegral = function(object){ ## feeds checkRuleIntegral into validity function for Integral
  if (checkRuleIntegral(object) != TRUE){ ## returns an error message if the test does not pass
    return("object is not a valid value")}
  else{
    return(TRUE)}
}
#'
setValidity("Integral", checkValidityIntegral)
#'
#' @export
setMethod("initialize", "Integral",
          function(.Object, ...){
            value = callNextMethod()
            validObject(value)
            return(value)
          }
            )
#'
newIntegral = function(x, y, z){ ## construction function takes in 3 arguments; x, y, and rule
  object = new("Integral", x = x, y = y, rule = z) ## creates a new object setting the 3 inputs as defined for class Integral
  return(object) ## returns the object
}
