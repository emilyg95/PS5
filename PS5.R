x = 1:4 ## test vector of x values
y = 1:4 ## test vector of y values
sum(2*suby)

Trap = function(x, y){ ## formula for trapezoidal rule that takes in arguments for x values and y values
  a = min(x) ## defines minimum value of x
  b = max(x) ## defines maximum value of x
  n = length(x) ## defines the number of elements in x
  h = ((b-a)/(n-1)) ## defines h as in trapezoidal rule (n-1 because a = x0 in the rule, not x1, so it should be a measure of the number of intervals not the number of points)
  ay = min(y) ## defines minimum value of y
  by = max(y) ## defines maximum value of y
  remove = c(ay, by) ## creates a new vector of min and max y values
  suby = setdiff(y, remove) ## creates a new vector of all y values except min and max
  t = (h/2)*(ay + by + sum(2*suby)) ## calculates t as in the trapezoidal rule
  return(t) ## returns t
}

Trap(x, y) ## check


