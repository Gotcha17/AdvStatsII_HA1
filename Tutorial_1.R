Nmc = c(10,100,1000) # number of replications of MonteCarlo Simulation
n = c(10,100,1000) # number of draws (#of elements in the sample)

# Problem 1a ---------------------------------------------------------------

path = file.path(getwd(), paste("Problem_1_a",'pdf', sep = '.')) #creating string with file in the working directory
pdf(file=path) #save output as pdf file in the path
par(mfrow = c(3,3)) #all different subgraphs into one plot

for(i in Nmc){ #we need to test all replication
  for(j in n){ #for all draws
    X = rnorm(i, mean = 1/j, sd = sqrt(1/(1+2/j))) #"i" is now "Nmc"
    Y = rnorm(i, mean = 2+1/j, sd = sqrt(1/j)) #"j" is now "n"
    P = X*Y
    hist(P, breaks = 'Scott', freq = FALSE, xlim = c(-10,10), ylim =c(0, 0.2),
         main = substitute(paste(MC, '= ', u, ', ', n, '= ', v, sep = ''), list(u=i, v=j))) #constract a histogramm with Method Scott in the x of -10 to 10 and y from 0 to 0.2
    x1 = seq(from = -10, to=10, length.out = 10000) #10**4 = 10^4
    y1 = dnorm(x1, mean = 0, sd = 2) #as calculated in the paper-pen tutorial
    lines(x1,y1, col = 'darkgreen', lwd = 2) #color and linewidth of the line from x1 to y1
    box() #creates a box around the graphic
  }
}
dev.off() #after plotting it will finish to work on this plot, next plot will then will be starting in a new plot
par(mfrow = c(1,1)) #so next plot will be "standard", so one graph in one plot

# Problem 1b --------------------------------------------------------------

path = file.path(getwd(), paste("Problem_1_b",'pdf', sep = '.')) #creating string with file in the working directory
pdf(file=path) #save output as pdf file in the path
par(mfrow = c(3,3)) #all different subgraphs into one plot

for(i in Nmc){ #we need to test all replication
  for(j in n){ #for all draws
    X = rnorm(i, mean = 1/j, sd = sqrt(1/(1+2/j))) #"i" is now "Nmc"
    Y = rnorm(i, mean = 2+1/j, sd = sqrt(1/j)) #"j" is now "n"
    P = X/Y
    hist(P, breaks = 'Scott', freq = FALSE, xlim = c(-2,2), ylim =c(0, 0.8),
         main = substitute(paste(MC, '= ', u, ', ', n, '= ', v, sep = ''), list(u=i, v=j))) #constract a histogramm with Method Scott in the x of -10 to 10 and y from 0 to 0.2
    x1 = seq(from = -2, to=2, length.out = 10000) #10**4 = 10^4
    y1 = dnorm(x1, mean = 0, sd = 1/2) #as calculated in the paper-pen tutorial
    lines(x1,y1, col = 'darkgreen', lwd = 2) #color and linewidth of the line from x1 to y1
    box() #creates a box around the graphic
  }
}
dev.off() #after plotting it will finish to work on this plot, next plot will then will be starting in a new plot
par(mfrow = c(1,1)) #so next plot will be "standard", so one graph in one plot
# Problem2 ----------------------------------------------------------------
hfnorm = function(z){
  sqrt(2/pi)*exp(-z^2/2)
}

path = file.path(getwd(), paste("Problem_2", "pdf", sep = "."))
pdf(file=path)
par(mfrow=c(2,2))

for(i in Nmc){
  X = rnorm(i, mean =0, sd = 1)
  Y = abs(X)
  hist(Y, breaks = 'Scott', freq = F, xlim = c(0,5), ylim = c(0,1),
       main = substitute(paste(MC, "=", u, sep = ""), list(u=i)))
  x1 = seq(from=0, to=5, length.out = 10**4)
  y1 = hfnorm(x1)
  lines(x1,y1, col = 'darkblue', lwd = 2)
  box()
}
dev.off()
par(mfrow=c(1,1))

# Problem3 ----------------------------------------------------------------


# Home Assignment ---------------------------------------------------------


#home assignment
#use the Beta Distr. as in the uploaded solutions
