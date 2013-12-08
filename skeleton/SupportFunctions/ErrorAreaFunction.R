library(sfsmisc)

# Function: approx_integal
# sfmisc package seems to have some error, so I wrote my own integral function, but this one doesn't work that well

# Input:
# x: x axis of points
# y: y axis of points
# a: leftBound
# b: rightBound
# Output: approxiate area under the curve
approx_integral = function(x, y, a, b) {
  points_in_range = which(x >= a & x <= b)
  x = x[points_in_range]
  y = y[points_in_range]
  points_order = order(x)
  x = x[points_order]
  y = y[points_order]
  leftBound = sum(diff(x) * y[1:length(y)-1])
  rightBound = sum(diff(x) * y[2:length(y)])
  return (0.5*leftBound + 0.5*rightBound)
}


# Function: get_error_area

# Input:
# result: A list of lists of taus and error rates for different models
# leftBound: the left bound for the area, default the maxmin tau of result 
# righttBound: the right bound for the area, default the minmax tau of result
# Output: a vector of error area for different models 
get_error_rate_average = function(result, leftBound = NA, rightBound = NA ){

  # Get boundries
  maxmin = max(sapply(result, function(x){min(x$taus)}))
  minmax = min(sapply(result, function(x){max(x$taus)}))

  # Check if input is valid
  if (!(is.list(result) 
        && all(sapply(result, is.list))
        && !any(sapply(result, function(x){is.null(x$taus)}))
        && !any(sapply(result, function(x){is.null(x$vs)}))
        )) stop("result is of wrong format")
  if (is.na(leftBound)) leftBound = maxmin
  if (is.na(rightBound)) rightBound = minmax
  if (leftBound < maxmin || rightBound > minmax) stop(paste('The minimum leftBound is: ', maxmin, '; The maximum rightBound is: ', minmax, sep=''))  

  # Get errors
  errs = sapply(result, function(x){
    points_order = order(x$taus)
    taus = x$taus[points_order]
    vs = x$vs[points_order]
    return(integrate.xy(taus, vs, a = leftBound, b = rightBound))
    })
  # errs = sapply(result, function(x){approx_integral(x$taus, x$vs, a = leftBound, b = rightBound)})
  return (errs/(rightBound - leftBound))
}
