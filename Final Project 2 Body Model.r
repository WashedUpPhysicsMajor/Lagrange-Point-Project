#First We must define our Two Objects in  the initial Position, velocity, acceleration and mass (Sun and Earth Conditions)
G <- 6.6743*10^(-11) # Newton's Gravitational Constant
M1 <- 1.9891*10^(30) # Sun's Mass
X1 <- 0
Vx1 <- 0
Ax1 <- 0
Y1 <- 0
Vy1 <- 0
Ay1 <- 0


M2 <- 5.972*10^(24) # Earth's Mass
X2 <- 1.4960*10^(11) # Earth's Distance
Vx2 <- 0
Ax2 <- 0
Y2 <- 0
Vy2 <- 29.78 * 10^3 # Earth's Velocity
Ay2 <- 0

#Cool Ring effect
G <- 6.6743*10^(-11) # Newton's Gravitational Constant
M1 <- 1.9891*10^(30) # Sun's Mass
X1 <- -1.5*10^(10)
Vx1 <- 0
Ax1 <- 0
Y1 <- 0
Vy1 <- -29.78 * 10^3
Ay1 <- 0


M2 <- 1.9891*10^(30) # Sun 2
X2 <- 1.5*10^(10) 
Vx2 <- 0
Ax2 <- 0
Y2 <- 0
Vy2 <- 29.78 * 10^3 # Earth's Velocity
Ay2 <- 0

#Two Circle Inital Conditions
G <- 6.6743*10^(-11) # Newton's Gravitational Constant
M1 <- 1.9891*10^(30) # Sun's Mass
X1 <- -1.5*10^(10)
Vx1 <- 0
Ax1 <- 0
Y1 <- 0
Vy1 <- -60 * 10^3
Ay1 <- 0


M2 <- 1.9891*10^(30) # Sun 2
X2 <- 1.5*10^(10) 
Vx2 <- 0
Ax2 <- 0
Y2 <- 0
Vy2 <- 60 * 10^3 # Earth's Velocity
Ay2 <- 0

#Two Circle Inital Conditions
G <- 6.6743*10^(-11) # Newton's Gravitational Constant
M1 <- 1.9891*10^(30) # Sun's Mass
X1 <- -1.5*10^(10)
Vx1 <- 12*10^2
Ax1 <- 0
Y1 <- 0
Vy1 <- -60 * 10^3
Ay1 <- 0


M2 <- 1.9891*10^(30) # Sun 2
X2 <- 1.5*10^(10) 
Vx2 <- 0
Ax2 <- 0
Y2 <- 0
Vy2 <- 60 * 10^3 # Earth's Velocity
Ay2 <- 0

#Define a time step and a length of the entire simulation:

detT <- 10000

N <- 15000


#Now we need to create force functions

FORCEMag <- function(m1, m2, x1, x2, y1, y2) {
  mag <- (G*m1*m2) / ((x1 - x2)^2 + (y1 - y2)^2)
  return(mag)
}
#Newton's Law of Gravitation (there is no need to the big constant G here)
Cosine <- function(x1,x2,y1,y2){
    (x1-x2)/sqrt((x1-x2)^2+(y1-y2)^2)
}
Sine <- function(x1,x2,y1,y2){
    (y1-y2)/sqrt((x1-x2)^2+(y1-y2)^2)
}


FORCE_X <- function(m1, m2, x1, x2, y1, y2){
    Cosine(x1,x2,y1,y2)*FORCEMag(m1, m2, x1, x2, y1, y2) 
}# Cosine (ratio) Multiplied by mag

FORCE_Y <- function(m1, m2, x1, x2, y1, y2){
    Sine(x1,x2,y1,y2)*FORCEMag(m1, m2, x1, x2, y1, y2)
}# Sine (ratio) Multiplied by mag


X1Pos <- c()
Y1Pos <- c()
X2Pos <- c()
Y2Pos <- c()
X1Vel <- c()
Y1Vel <- c()
X2Vel <- c()
Y2Vel <- c()
X1Acc <- c()
Y1Acc <- c()
X2Acc <- c()
Y2Acc <- c()

for(i in 0:N) {                    
    Vx1 <- Vx1+Ax1*detT/2 #Half Step
    Vy1 <- Vy1+Ay1*detT/2 
    Vx2 <- Vx2+Ax2*detT/2 
    Vy2 <- Vy2+Ay2*detT/2 
    X1 <- X1+Vx1*detT
    Y1 <- Y1+Vy1*detT
    X2 <- X2+Vx2*detT
    Y2 <- Y2+Vy2*detT
    Ax1 <- -FORCE_X(M1, M2, X1, X2, Y1, Y2)/M1
    Ay1 <- -FORCE_Y(M1, M2, X1, X2, Y1, Y2)/M1 # Accelerations Update 1
    Ax2 <- FORCE_X(M1, M2, X1, X2, Y1, Y2)/M2
    Ay2 <- FORCE_Y(M1, M2, X1, X2, Y1, Y2)/M2 # Accelerations Update 2
    Vx1 <- Vx1+Ax1*detT/2 #Second Half Step
    Vy1 <- Vy1+Ay1*detT/2 
    Vx2 <- Vx2+Ax2*detT/2 
    Vy2 <- Vy2+Ay2*detT/2 


#Now let us record all of our values into an array and save them for later
    X1Pos <- c(X1Pos, X1)
    Y1Pos <- c(Y1Pos, Y1)
    X2Pos <- c(X2Pos, X2)
    Y2Pos <- c(Y2Pos, Y2)
    X1Vel <- c(X1Vel, Vx1)
    Y1Vel <- c(Y1Vel, Vy1)
    X2Vel <- c(X2Vel, Vx2)
    Y2Vel <- c(Y2Vel, Vy2)
    X1Acc <- c(X1Acc, Ax1)
    Y1Acc <- c(Y1Acc, Ay1)
    X2Acc <- c(X2Acc, Ax2)
    Y2Acc <- c(Y2Acc, Ay2)
}

#plot the positions and everything

plot(X1Pos, Y1Pos, type = "p",pch='.', col = "red", xlab = "X",
     ylab = "Y", main = "2 Body Simulation",ylim = c(-5*10^11,5*10^11),xlim = c(-5*10^11,5*10^11))
points(X2Pos,Y2Pos, type = "p",pch='.', col = "blue")

# Now plotting momentum over time and energy over time (should remain constant)
TimeSteps <- seq(0,N,by=1)
plot(TimeSteps, (1/2)*M1*(X1Vel^2+Y1Vel^2)+(1/2)*M2*(X2Vel^2+Y2Vel^2)+G*M1*M2/sqrt((X1-X2)^2+(Y1-Y2)^2), type = "p", col = "red", xlab = "X",
     ylab = "Y", main = "Total Energy Over Time",xlim = c(0,N))

TimeSteps <- seq(0,N,by=1)
plot(TimeSteps, M1*sqrt((X1Vel^2+Y1Vel^2))+M2*sqrt((X2Vel^2+Y2Vel^2)), type = "p", col = "red", xlab = "X",
     ylab = "Y", main = "Total Momentum Over Time",xlim = c(0,N))


