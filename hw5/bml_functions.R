#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  matrix(sample(c(0, 1, 2), r*c, prob = c(1-p, p/2, p/2), replace = T), nrow = r)
   
}

move.red <- function(i, j, m) {
  if(j == ncol(m)) {
    m[i,1] = 1
    m[i,j] = 0
  } else {
    m[i, j+1] = 1
    m[i, j] = 0
  }
  return(m)
}

move.blue <- function(i, j, m) {
  if(i == 1) {
    m[nrow(m), j] = 2
    m[i, j] = 0
  } else {
    m[i-1, j] = 2
    m[i, j] = 0
  }
  return(m)
}

next.red <- function(i, j, m) {
  if(j == ncol(m)) {
    return(m[i, 1])
  } else {
    return(m[i, j+1])
  }
}

next.blue <- function(i, j, m) {
  if(i == 1) {
    return(m[nrow(m), j])
  } else {
    return(m[i-1, j])
  }
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  a = m
  b = m
  for(i in 1:nrow(a)) {
    for(j in 1:ncol(a)) {
      if((a[i,j] == 2) && (next.blue(i,j,a) == 0)) {
        b = move.blue(i, j, b)
      }
    }
  }
  a = b
  for(i in 1:nrow(a)) {
    for(j in 1:ncol(a)) {
      if((a[i,j] == 1) && (next.red(i,j,a) == 0)) {
        b = move.red(i, j, b)
      }
    }
  }
  grid.new = any(b != m)
  m = b
  return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  stuck <- 0
  flow <- 0
  for(x in 1:5) {
    isStuck <- FALSE
    m = bml.init(r, c, p)
    image(t(apply(m, 2, rev)), axes = FALSE, col = c("white", "red", "blue"))
    for (i in 1:1000) {
      n = bml.step(m)
      if(n[[2]] == TRUE) {
        m = n[[1]]
        image(t(apply(m, 2, rev)), axes = FALSE, col = c("white", "red", "blue"))
      } else {
        image(t(apply(m, 2, rev)), axes = FALSE, col = c("white", "red", "blue"))
        stuck = stuck + 1
        isStuck <- TRUE
      }
    }
    if(isStuck) {
      flow <- flow + 1
    }
  }
  return(list(stuck, flow))

}
