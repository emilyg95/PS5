#' Integrating with Trapezoid or Simpson's Rule
#'
#' Uses Trapezoid or Simpson's Rule to approximate the area under a set of points
#'
#' @slot object An object of class Trapezoid or Integral
#'
#'
#' @return A list with the input elements and the answer
#'  \item{object@x}{The first object input} 
#'  \item{object@y}{The second object input}
#'  \item{object@z}{The third object input}
#'  \item{Answer}{The value calculated using Trapezoid or Simpson's Rule}
#'  
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#' @examples
#' 
#'integrateIt(newTrapezoid(1:4, c(3,6,12,13), "Trapezoid"))
#' @seealso newTrapezoid, newSimpson, printIntegral
#' @rdname integrateIt
#' @include newIntegral.R
#' @include newTrapezoid.R
#' @include newSimpson.R
#' @export
setGeneric("integrateIt", function(object){ ## creates the interior function for set generic
  standardGeneric("integrateIt")
})

#' @exportMethod Trapezoid
setMethod("integrateIt", signature("Trapezoid"), ## creates a method of integrateIt for objects of class Trapezoid
          function(object){ ## defines x and y inputs as numbers
            x = object@x
            y = object@y
            Trap = function(x, y){ ## formula for trapezoidal rule that takes in arguments for x values and y values
              a = min(x) ## defines minimum value of x
              b = max(x) ## defines maximum value of x
              n = length(x) ## defines the number of elements in x
              h = ((b-a)/(n-1)) ## defines h as in trapezoidal rule (n-1 because a = x0 in the rule, not x1, so it should be a measure of the number of intervals not the number of points)
              a_y = min(y) ## defines minimum value of y
              b_y = max(y) ## defines maximum value of y
              remove = c(a_y, b_y) ## creates a new vector of min and max y values
              sub_y = setdiff(y, remove) ## creates a new vector of all y values except min and max
              t = (h/2)*(a_y + b_y + sum(2*sub_y)) ## calculates t as in the trapezoidal rule
              return(t) ## returns t
            }
            Answer = Trap(x, y) ## applies Trap function to x and y
            return(list("x" = object@x, "y" = object@y, "rule" = object@rule, "Answer" = Answer)) ## returns the object and the output of Trap
          }
)
#' @exportMethod Simpson
setMethod("integrateIt", signature("Simpson"), ## creates a method of integrateIt for objects of class Simpson
          function(object){ ## defines x and y inputs as numbers
            x = object@x
            y = object@y
            Simpson = function(x, y){ ## formula for simpson's rule that takes in arguments for x values and y values
              a = min(x) ## defines minimum value of x
              b = max(x) ## defines maximum value of x
              n = length(x) ## defines the number of elements in x
              h = ((b-a)/(n-1)) ## defines h as in trapezoidal rule (n-1 because a = x0 in the rule, not x1, so it should be a measure of the number of intervals not the number of points)
              a_y = min(y) ## defines minimum value of y
              b_y = max(y) ## defines maximum value of y
              remove = c(a_y, b_y) ## creates a new vector of min and max y values
              sub_y = setdiff(y, remove) ## creates a new vector of all y values except min and max
              odd_y = seq(min(sub_y),max(sub_y),2) ## subsets the new vector of y values further into every other
              if(length(sub_y) > 1){
                even_y = seq(min(sub_y)+1,max(sub_y),2) ## takes the other half of the new vector of y values
              }
              else{
                even_y = 0 ## if else statements ensures error isn't thrown if the vector of x values only contains 3 numbers
              }              
              s = (h/3)*(a_y + b_y + sum(4*odd_y) + sum(2*even_y)) ## calculates s as in simpson's rule
              return(s)
            }
            Answer = Simpson(x, y) ## applies Simpson function to x and y
            return(list("x" = object@x, "y" = object@y, "rule" = object@rule, "Answer" = Answer)) ## returns the object and the output of Simpson
          }
)