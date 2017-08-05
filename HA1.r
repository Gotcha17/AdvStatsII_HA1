##### Comments ############################################################################################

# Probability limits

# As we can see from the produced Graphs and was veryfied in the Tutorial:
# X_minimum --> 0 as n --> Inf, hence plim(X_minimum) = 0
# X_maximum --> 1 as n --> Inf, hence plim(X_maximum) = 1 

# Influence of an increasing Number of Replications of Monte-Carlo-Simulation (Nmc): 

# For both the distributions of the sample maximum and the sample minimum we move closer to the (identical)
# Distributions of the Pen & Paper-Tutorial / Solution of the Tutorial

# Influence of an increasing Number of Draws (n): 

# We can observe that both the Probability of X_minimum and X_maximum get closer to their theoretical 
# values of 0 and 1 respectively. Also their distributions becomes more centered 
# towards these values with as the Number of Draws approaches infinity.

##### Sample Minimum ######################################################################################

Nmc = c(100,500,1000)      # Number of Replications of Monte-Carlo-Simulation
n = c(10,100,1000)         # Number of Draws

path = file.path(getwd(), paste("HA1_minimum_stu127762_stu107307",'pdf', sep = '.'))  # Creating File-Path
pdf(file=path)             # Save Output as PDF-File for given Path
par(mfrow = c(3,3))        # Put all Subgraphs into 3x3 plot 

X_minimum = matrix()       # Initialise Matrix

for (i in Nmc){            # Loop over Replications of Monte-Carlo-Simulation
  for (j in n){            # Loop over Draws
    X = matrix(runif(i*j, min=0, max=1), i, j)        # Standard Uniformly Distributed Random Draws
    X_minimum = apply(X, 1, min)                      # Select Minimum Values
    
    x1 = seq(from=0, to=1, length.out = 10000)        # Generating 10^4 long sequence of numbers in the range of 0 and 1
    y1 = j*(1-x1)^(j-1)                               # Distribution from Pen & Paper-Tutorial
    y2 = dbeta(x1, 1, j)                              # Distribution from Solution
    hist(X_minimum, breaks = "Scott", freq = F, xlim = c(0,max(X_minimum)), ylim = c(0,j), 
         main=paste("n=", j,", Nmc=", i, sep = ""), 
         xlab = "X", ylab = "Density")                               # Draw Histogram of Minimum Values using Scott Method   
    lines(x1, y1, type = "l", lty=1, col="red", lwd=2)               # Draw Distribution from Pen & Paper-Tutorial
    lines(x1, y2, type = "l", lty=4,  col="green", lwd=2)            # Draw Distribution from Solution
  }
}

dev.off()                  # Reset Plot Settings       
par(mfrow=c(1,1))          

##### Sample Maximum ######################################################################################

Nmc = c(100,500,1000)      # Number of Replications of Monte-Carlo-Simulation
n = c(10,100,1000)         # Number of Draws

path = file.path(getwd(), paste("HA1_maximum_stu127762_stu107307",'pdf', sep = '.'))  # Creating File-Path
pdf(file=path)             # Save Output as PDF-File for given Path
par(mfrow = c(3,3))        # Put all Subgraphs into 3x3 plot

X_maximum = matrix()       # Initialise Matrix

for (i in Nmc){            # Loop over Replications of Monte-Carlo-Simulation
  for (j in n){            # Loop over Draws
    X = matrix(runif(i*j, min=0, max=1), i, j)        # Standard Uniformly Distributed Random Draws
    X_maximum = apply(X, 1, max)                      # Select Maximum Values
    
    x1 = seq(from=0, to=1, length.out = 10000)        # Generating 10^4 long sequence of numbers in the range of 0 and 1
    y1 = j*x1^(j-1)                                   # Distribution from Pen & Paper-Tutorial
    y2 = dbeta(x1, j, 1)                              # Distribution from Solution
    hist(X_maximum, breaks = "Scott", freq = F, xlim = c(min(X_maximum),1), ylim = c(0,j), 
         main=paste("n=",j,", Nmc=", i, sep = ""), 
         xlab = "X", ylab = "Density")                # Draw Histogram of Minimum Values using Scott Method   
    lines(x1, y1, type = "l", lty=1, col="red", lwd=2)               # Draw Distribution from Pen & Paper-Tutorial
    lines(x1, y2, type = "l", lty=4,  col="green", lwd=2)            # Draw Distribution from Solution
  }
}

dev.off()                  # Reset Plot Settings
par(mfrow=c(1,1))          

###########################################################################################################



