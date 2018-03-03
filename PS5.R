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
  even_y = seq(min(sub_y)+1,length(sub_y),2) ## takes the other half of the new vector of y values
  s = (h/3)*(a_y + b_y + sum(4*odd_y) + sum(2*even_y)) ## calculates s as in simpson's rule
  return(s)
}

Simpson(q,z) ## check

generic = function(x, y, rule){ ## creates the interior function for set generic which takes as an input x, y, and rule
  standardGeneric("integrateIt")
}

setGeneric("integrateIt", generic) ## sets the generic function of integrateIt as generic

setClass(Class = "Trapezoid", ## creates a new class "Trapezoid"
         representation = representation(
           x = "numeric",
           y = "numeric"
         ) ## indicates 2 inputs; x and y, which must both be numeric
)

setClass(Class = "Simpson", ## creates a new class "Simpson"
         representation = representation(
           x = "numeric",
           y = "numeric"
         ) ## indicates 2 inputs; x and y, which must both be numeric
)

newSimpson = function(x, y){ ## construction function takes in 2 arguments; x and y
  object = new("Simpson", x = x, y = y) ## creates a new object setting the 2 inputs x and y as defined for class Simpson
  return(object) ## returns the object
}

newTrapezoid = function(x, y){ ## construction function takes in 2 arguments; x and y
  object = new("Trapezoid", x = x, y = y) ## creates a new object setting the 2 inputs x and y as defined for class Trapezoid
  return(object) ## returns the object
}

checkLength = function(object){ ## creates a function to check if x and y are the same length and returns error message if not
  test1 = (length(object@x) == length(object@y))
  if (!test1){
    return("x and y must be of the same length")}
  else{
    return(TRUE)}
  }

Simpson1 = newSimpson(q,z)
Simpson2 = newSimpson(q,w)

checkLength(Simpson1) ## test
checkLength(Simpson2) ## test

Simpson3 = newSimpson(p,q)

checkX = function(object){
sub_x = setdiff(object@x, max(object@x)) ## creates a new vector of x values without the highest one
difference = max(object@x)-max(sub_x) ## takes the difference between the highest and second highest x value
even_spaced = seq(min(object@x), max(object@x), difference) ## creates a vector from the minimum x value to the maximum x value spaced evenly by the value of difference
test1 = (even_spaced == object@x) ##  returns a logical vector indicating whether all the elements of x and the evenly spaced vector are the same
if (all(test1) != TRUE){ ## tests if all the elements of the logical vector are true and send an error message if not
  return("x values must be evenly spaced")} 
else{
  return(TRUE)}
}

checkX(Simpson2) ## test
checkX(Simpson3) ## test
