# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output
  initial.doctors = matrix(c(initial.doctors), nrow = n.doctors, ncol = 1)
  has_adopted <- matrix(nrow = n.doctors, ncol = n.days - 1, 0)
  has_adopted <- cbind(initial.doctors, has_adopted)
  for(j in 1:n.doctors) {
    if(has_adopted[j, 1] == 1) {
      for(k in 1:n.days) {
        has_adopted[j, k] = 1
      }
    }
  }
  for(i in 1:n.days) {
    random <- sample(1:n.doctors, 2)
    if(has_adopted[random[1], i] == 0 && has_adopted[random[2], i] == 1) {
      toChange <- sample(1:2, size = 1, prob = c(p, 1-p))
      if(toChange ==  1) {
        for(x in i:n.days) {
          has_adopted[random[1], x] = 1
        }
      }
    } else if(has_adopted[random[1], i] == 1 && has_adopted[random[2], i] == 0) {
      toChange <- sample(1:2, size = 1, prob = c(p, 1-p))
      if(toChange ==  1) {
        for(x in i:n.days) {
          has_adopted[random[2], x] = 1
        }
      }
    }
  }
  
  return(has_adopted)

}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)

count.doctors <- function(end.matrix) {
  perday <- vector()
  for(i in 1:ncol(end.matrix)) {
    tempSum = 0
    for(j in 1:nrow(end.matrix)) {
      if(end.matrix[j,i] == 1) {
        tempSum <- tempSum + 1
      }
    }
    perday <- c(perday, tempSum)
  }
  return(perday)
}

# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
initDoctors = c(1, rep(0, 9), 1, rep(0, 9))
first.sim <- sim.doctors(initDoctors, 20, 20, 1)
initDoctors = c(1, rep(0, 9), 1, rep(0, 9))
second.sim <- sim.doctors(initDoctors, 20, 20, 0.8)
initDoctors = c(1, rep(0, 9), 1, rep(0, 9))
third.sim <- sim.doctors(initDoctors, 20, 20, 0.5)
initDoctors = c(1, rep(0, 9), 1, rep(0, 9))
fourth.sim <- sim.doctors(initDoctors, 20, 20, 0.3)
initDoctors = c(1, rep(0, 9), 1, rep(0, 9))
fifth.sim <- sim.doctors(initDoctors, 20, 20, 0.1)

plot(count.doctors(first.sim), type = "o", xlab = "days", ylab = "numer of doctors adopted", main = "Drug adoption simulation")
lines(count.doctors(second.sim), type = "o", col = "blue")
lines(count.doctors(third.sim), type = "o", col = "red")
lines(count.doctors(fourth.sim), type = "o", col = "green")
lines(count.doctors(fifth.sim), type = "o", col = "yellow")
