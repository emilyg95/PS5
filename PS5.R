z = 1:5 ## test vector of x values
q = 1:5 ## test vector of y values

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
