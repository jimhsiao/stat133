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

pdf(file = "bml_figures.pdf")
#1
barplot(matrix, beside= TRUE, names.arg = density2, xlab = "Density of Cars", ylab = "Gridlock Proportion", main= "Gridlock Rates of Different Car Densities", col = "purple")
#2
boxplot(my_matrix, main = "# Iterations Until Gridlock For Different Densities", xlab = "Density (in tenths)", ylab = "# Iterations", col = "orange")

#3A
barplot(matrix2, beside= TRUE, names.arg = dimensions, xlab = "Individual Side Dimension", ylab = "Gridlock Proportion", main= "Gridlock Rates of Different Sized (Square) Dimensions", col = "pink")
#3b
barplot(matrixShape, main = "Gridlock Rates of Square and Rectangle Grids", beside = TRUE, xlab = "Dimension of Grid", ylab = "Gridlock Rate", names.arg = c("5x5", "4x6", "10x10", "4x25"))
boxplot(square, main = "# Iterations Until Gridlock For Small 5x5 Square Grid", xlab = "Density", ylab = "# Iterations", col = "blue")
boxplot(rect, main = "# Iterations Until Gridlock For Small 4x6 Uneven Grid", xlab = "Density (in tenths)", ylab = "# Iterations", col = "yellow")
boxplot(big_square, main = "# Iterations Until Gridlock For Big 10x10 Square Grid", xlab = "Density (in tenths)", ylab = "# Iterations", col = "green")
boxplot(big_rect, main = "# Iterations Until Gridlock For Big 4x25 Uneven Grid", xlab = "Density (in tenths)", ylab = "# Iterations", col = "red")
dev.off()



