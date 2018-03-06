#' Printing an Integral
#'
#' Uses Trapezoid or Simpson's Rule to approximate the area under a set of points and prints the output
#'
#' @slot x An object of class Trapezoid or Integral
#'
#'
#' @return The answer
#'  \item{Answer}{The value calculated using Trapezoid or Simpson's Rule}
#'  
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#' @examples
#' 
#'printIntegral(newTrapezoid(1:4, c(3,6,12,13), "Trapezoid"))
#' @seealso newTrapezoid, newSimpson, newIntegral, integrateIt
#' @rdname printIntegral
#' @include newIntegral.R
#' @include newTrapezoid.R
#' @include newSimpson.R
#' @exportMethod Integral
setMethod("print", signature("Integral"), ## creates a new method for print that takes objects of class integral (parent class of Trapezoid and Simpson)
          function(x){ 
            if(x@rule == "Trapezoid"){ ## creates a new object of class trapezoid using inputs if rule selected is trapezoid (so that validity tests designed for trapezoid will be run)
              Trapezoid = newTrapezoid(x@x, x@y, x@rule)
              x = Trapezoid@x ## defines x and y values from inputs
              y = Trapezoid@y
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
              Answer = Trap(x, y) ## runs Trap on x and y
              return(Answer) ## returns output of Trap
            }
            else if(x@rule == "Simpson"){ ## creates a new object of class simpson using inputs if rule selected is simpson (so that validity tests designed for simpson will be run)
              Simpson = newSimpson(x@x, x@y, x@rule)
              x = Simpson@x  ## defines x and y values from inputs
              y = Simpson@y
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
              Answer = Simpson(x, y) ## runs Simpson on x and y
              return(Answer) ## returns output of Simpson
            }
          }
)