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
#' @include classIntegral.R
#' @export
setClass(Class = "Simpson", ## creates a new class "Simpson"
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
checkIntervals = function(object){ ## creates a function to check if n is odd (necessary for Simpson's rule)
  n = length(object@x) ## takes the length of the vector of x values
  test1 = ((n-1) %% 2 == 0) ## checks if n - 1 is evenly divisble by 2 and if not sends an error message
  if (test1 != TRUE){
    return("there must be an odd number of x values")}
  else{
    return(TRUE)}
}
#'
checkRuleSimp = function(object){ ## creates a function that forces the rule input to be Simpson
  if(object@rule != "Simpson"){
    return("rule must be 'Simpson'")}
  else{
    return(TRUE)}
}
#'
checkValiditySimpson = function(object){ ## feeds checkLength, checkSpacing, checkIntervals, and checkRuleSimp into validity function for simpson
  if (checkLength(object) != TRUE | checkSpacing(object) != TRUE | checkIntervals(object) != TRUE | checkRuleSimp(object) != TRUE){ ## returns an error message if either test does not pass
    return("object is not a valid value")}
  else{
    return(TRUE)}
}
#'
setValidity("Simpson", checkValiditySimpson)
#' @export
setMethod("initialize", "Simpson",
          function(.Object, ...){
            value = callNextMethod()
            validObject(value)
            return(value)
          }
)





















