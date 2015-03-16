#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.


source("bml_functions.r")
bml.sim(100,100,0.2)
bml.sim(100,100,0.2)

bml.sim(100,100,0.8)
bml.sim(100,100,0.8)

bml.sim(50,50, 0.6)
bml.sim(10,10, 0.6)
bml.sim(5,10,0.6)
bml.sim(10,5,0.6)



