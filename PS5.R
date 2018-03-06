## this is my development file

z = 1:5 ## test vector
q = 1:5 ## test vector
w = 1:6 ## test vector
p = c(2,5,7,12) ## test vector

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

Trap(q, z) ## check

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

Simpson(q, z) ## test
Simpson(1:5, 5:9) ## test

setClass(Class = "Integral", ## creates a new class "Integral"
         representation = representation(
           x = "numeric",
           y = "numeric",
           rule = "character"
         ) ## indicates 3 inputs; x and y, which must both be numeric, and rule, which must be a character
)

setClass(Class = "Trapezoid", ## creates a new class "Trapezoid"
         contains = "Integral"
         ) ## inherits inputs from class Integral

setClass(Class = "Simpson", ## creates a new class "Simpson"
         contains = "Integral"
         ) ## inherits inputs from class Integral

newSimpson = function(x, y, z){ ## construction function takes in 3 arguments; x, y, and rule
  object = new("Simpson", x = x, y = y, rule = z) ## creates a new object setting the 3 inputs as defined for class Simpson
  return(object) ## returns the object
}

newTrapezoid = function(x, y, z){ ## construction function takes in 3 arguments; x, y, and rule
  object = new("Trapezoid", x = x, y = y, rule = z) ## creates a new object setting the 3 inputs as defined for class Trapezoid
  return(object) ## returns the object
}

newIntegral = function(x, y, z){ ## construction function takes in 3 arguments; x, y, and rule
  object = new("Integral", x = x, y = y, rule = z) ## creates a new object setting the 3 inputs as defined for class Integral
  return(object) ## returns the object
}

checkRuleIntegral = function(object){ ## creates a function that forces the rule input to be Trapezoid or Simpson
  if(object@rule != "Simpson" & object@rule != "Trapezoid"){
    return("rule must be 'Simpson' or 'Trapezoid'")}
  else{
    return(TRUE)}
}

checkRuleIntegral(newIntegral(1:4, 1:4, "Trapezoid"))

checkValidityIntegral = function(object){ ## feeds checkRuleIntegral into validity function for Integral
  if (checkRuleIntegral(object) != TRUE){ ## returns an error message if the test does not pass
    return("object is not a valid value")}
  else{
    return(TRUE)}
}

setValidity("Integral", checkValidityIntegral) ## sets checkValidityIntegral as the constraints for an object to be of class Integral
## Integral does not require more constraints because the only specified constraint forces a subclass to be called whenever it is used, which has its own constraints

newIntegral(1:4, 1:4, "Trapezoid")
newIntegral(1:4, 1:4, "Face")

Simpson1 = newSimpson(q, z, "Simpson") ## check
Trap1 = newTrapezoid(q, z, "Trapezoid") ## check

checkLength = function(object){ ## creates a function to check if x and y are the same length and returns error message if not
  test1 = (length(object@x) == length(object@y))
  if (!test1){
    return("x and y must be of the same length")}
  else{
    return(TRUE)}
}

checkLength(Simpson1) ## test
checkLength(newSimpson(1:4, 1:5, "Simpson")) ## test

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

checkSpacing(Simpson1) ## test
checkSpacing(newSimpson(1:4, 1:5, "Simpson")) ## test
checkSpacing(newSimpson(c(3,6,12,14), 1:4, "Simpson")) ## test

checkIntervals = function(object){ ## creates a function to check if n is odd (necessary for Simpson's rule)
  n = length(object@x) ## takes the length of the vector of x values
  test1 = ((n-1) %% 2 == 0) ## checks if n - 1 is evenly divisble by 2 and if not sends an error message
  if (test1 != TRUE){
    return("there must be an odd number of x values")}
  else{
    return(TRUE)}
  }

checkIntervals(Simpson1) ## test
checkIntervals(newSimpson(1:4, 1:5, "Simpson")) ## test

checkRuleTrap = function(object){ ## creates a function that forces the rule input to be Trapezoid
  if(object@rule != "Trapezoid"){
    return("rule must be 'Trapezoid'")}
  else{
    return(TRUE)}
  }

checkRuleTrap(Trap1) ## test
checkRuleTrap(newTrapezoid(1:4, 1:5, "Trap")) ## test

checkRuleSimp = function(object){ ## creates a function that forces the rule input to be Simpson
  if(object@rule != "Simpson"){
    return("rule must be 'Simpson'")}
  else{
    return(TRUE)}
}

checkRuleSimp(Simpson1) ## test
checkRuleSimp(newSimpson(1:4, 1:5, "Trap")) ## test

checkValidityTrap = function(object){ ## feeds checkLength, checkSpacing, and checkRuleTrap into validity function for trapezoid
  if (checkLength(object) != TRUE | checkSpacing(object) != TRUE | checkRuleTrap(object) != TRUE){ ## returns an error message if any test does not pass
    return("object is not a valid value")}
  else{
    return(TRUE)}
  }

checkValidityTrap(Trap1) ## test
checkValidityTrap(newTrapezoid(1:4, 1:5, "Trap")) ## test

setValidity("Trapezoid", checkValidityTrap) ## sets checkValidityTrap as the constraints for an object to be of class trapezoid

validObject(Trap1) ## test
validObject(newTrapezoid(1:4, 1:5, "Trap")) ## test

Trap1 = newTrapezoid(q, z, "Trapezoid") ## test
Trap2 = newTrapezoid(z, w, "Trapezoid") ## test
Trap3 = newTrapezoid(p, q, "Trapezoid") ## test
Trap4 = newTrapezoid(1:4, 5:8, "Trapezoid")

checkValiditySimpson = function(object){ ## feeds checkLength, checkSpacing, checkIntervals, and checkRuleSimp into validity function for simpson
  if (checkLength(object) != TRUE | checkSpacing(object) != TRUE | checkIntervals(object) != TRUE | checkRuleSimp(object) != TRUE){ ## returns an error message if either test does not pass
    return("object is not a valid value")}
  else{
    return(TRUE)}
}

checkValiditySimpson(Simpson1) ## test
checkValiditySimpson(newSimpson(1:4, 1:5, "Simpson")) ## test
checkValiditySimpson(newSimpson(1:5, 1:4, "Simp")) ## test

setValidity("Simpson", checkValiditySimpson) ## sets checkValiditySimpson as the constraints for an object to be of class simpson

Simpson1 = newSimpson(1:4, 1:4, "Simpson") ## test
Simpson2 = newSimpson(1:5, 5:9, "Simpson") ## test

generic = function(object){ ## creates the interior function for set generic
  standardGeneric("integrateIt")
}

setGeneric("integrateIt", generic) ## sets the generic function of integrateIt as generic

## in my package files I define the functions Trap and Simpson within setMethod rather than calling them from the global environment as I do here
setMethod("integrateIt", signature("Trapezoid"), ## creates a method of integrateIt for objects of class Trapezoid
          function(object){ ## defines x and y inputs as numbers
            x = object@x
            y = object@y
            Answer = Trap(x, y) ## applies Trap function to x and y
            return(list(object, "Answer" = Answer)) ## returns the object and the output of Trap
          }
)

integrateIt(Trap1) ## test
integrateIt(newTrapezoid(c(3,6,12,13), 1:4, "Trapezoid")) ## test

setMethod("integrateIt", signature("Simpson"), ## creates a method of integrateIt for objects of class Simpson
          function(object){ ## defines x and y inputs as numbers
            x = object@x
            y = object@y
            Answer = Simpson(x, y) ## applies Simpson function to x and y
            return(list(object, "Answer" = Answer)) ## returns the object and the output of Simpson
          }
)

integrateIt(newSimpson(1:5, 1:5, "Simpson")) ## test

setGeneric("print", function(object){ ##  This step is actually unnecessary so I took it out in my package file
  standardGeneric("print")}
)
    
setMethod("print", signature("Integral"), ## creates a new method for print that takes objects of class integral (parent class of trapezoid and simpson)
          function(object){ ## here I use object because I overwrote print with my own generic that takes object as an input, but in my package file I use "x" which is the input the existing generic print function takes
            if(object@rule == "Trapezoid"){ ## creates a new object of class trapezoid using inputs if rule selected is trapezoid (so that validity tests designed for trapezoid will be run)
              Trapezoid = newTrapezoid(object@x, object@y, object@rule)
              x = Trapezoid@x ## defines x and y values from inputs
              y = Trapezoid@y
              Answer = Trap(x, y) ## runs Trap on x and y
              return(Answer) ## returns output of Trap
            }
            else if(object@rule == "Simpson"){ ## creates a new object of class simpson using inputs if rule selected is simpson (so that validity tests designed for simpson will be run)
              Simpson = newSimpson(object@x, object@y, object@rule)
              x = Simpson@x  ## defines x and y values from inputs
              y = Simpson@y
              Answer = Simpson(x, y) ## runs Simpson on x and y
              return(Answer) ## returns output of Simpson
            }
          }
)

Integral = newIntegral(1:4, 1:4, "Trapezoid") ## test integral
print(Integral) ## test

Integral4 = newIntegral(1:4, 1:4, "Sam") ## test integral
print(Integral) ## test

Integral1 = newIntegral(1:5, 5:9, "Simpson") ## test integral
print(Integral1) ## test

Integral2 = newIntegral(1:5, c(3,5,12,15), "Simpson") ## test integral
print(Integral2) ## test

Integral3 = newIntegral(1:5, c(3,5,12,15), "Trapezoid") ## test integral
print(Integral3) ## test

## package development tools
install.packages("devtools")
install.packages("roxygen2")

library(devtools)
library(roxygen2)

setwd("/Users/emilygarner/Documents/School/Second Sem/R/Problem Sets/PS5") ## sets wd to where I want my integratePack located

devtools::create("integratePack") ## creates the structure for my package

rm(list=ls(all=TRUE)) ## wiping environment to test package

current.code <- as.package("integratePack")
load_all(current.code)
document(current.code) ## creating package

newIntegral(c(3,12), c(3,6), "Trapezoid") ## test - works

newTrapezoid(c(3,6), c(3,6), "Simpson") ## test
newTrapezoid(c(3,6), c(3,6), "Trapezoid") ## test - works

newSimpson(c(3,6), c(3,6), "Simpson") ## test
newSimpson(c(3,6,9), c(3,12,14), "Simpson") ## test

integrateIt(newSimpson(c(3,6,9,12,15), c(3,12,14,3,5), "Simpson")) ## test
integrateIt(newTrapezoid(c(3,6,9,12,15), c(3,12,14,3,5), "Trapezoid")) ## test
integrateIt(newIntegral(c(3,6,9,12,15), c(3,12,14,3,5), "Trapezoid")) ## test
integrateIt(newSimpson(c(3,6,9), c(3,12,14), "Simpson")) ## test

print(newIntegral(c(3,6,9,12,15), c(3,12,14,3,5), "Trapezoid")) ## test
print(newIntegral(c(3,6,9,12,15), c(3,12,14,3,5), "Simpson")) ## test
