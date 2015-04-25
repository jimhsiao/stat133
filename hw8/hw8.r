xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  yVecs <- unlist(tapply(y, x, function(i) sample(i, length(i), replace = TRUE)))
  return(yVecs)
  

}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  errVec <- sample(err, replace = FALSE)
  return(fit + errVec)
 
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if(degree == 1) {
    mid <- lm(y ~ x)
  } else {
    mid <- lm(y ~ x + I(x^2))
  }
  return(coefficients(mid))
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  

 
  ### Use fitModel to fit a model to this bootstrap Y 
  x <- data$x
  y <- data$y
  if (is.null(fit)) {
    temp = genBootY(x,y)
  } else {
    temp = genBootR(fit, y - fit)
  }
  return(fitModel(x, temp, degree = degree))
 
}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic

  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  fit1 <- lm (y~x, data = data)$fitted
  fit2 <- lm (y ~ x + I(x^2), data = data)$fitted
  
  sim1 <- replicate(B, oneBoot(data = data, fit1, 1))
  sim2 <- replicate(B, oneBoot(data = data, fit2, 2))
  sim3 <- replicate(B, oneBoot(data = data, NULL, 1))
  sim4 <- replicate(B, oneBoot(data = data, NULL, 2))
  
  temp = list(sim1, sim2, sim3, sim4)
  temp = matrix(temp, ncol = 4)
  return(temp)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data

  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out
  
  plot(x, y, type = "p", col = rgb(0, 0, 1))
  if(ncol(coeff) == 2) {
    mapply(function(x,y) abline(a = x, b = y, col = rgb(0, 0, 0, 0.05)), coeff[,1], coeff[,2])
  } else {
    mapply(function(a, b, c) curve(a*(x^2) + b*x + c, add = TRUE, col = rgb(0, 0, 0, 0.05)), coeff[,3], coeff[,2], coeff[,1])
  }
  a <- trueCoeff[3]
  b <- trueCoeff[2]
  c <- trueCoeff[1]
  curve(a*(x^2) + b*x + c, add = TRUE, col = rgb(1,0,0))
  
}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}
