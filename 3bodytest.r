#work in progress, gives results tho
#G <- 6.6743*10^(-11) # Newton's Gravitational Constant
G <- 1
#M1 <- 1.9891*10^(30) # Sun's Mass
M1 <- 1
X1 <- -1
#Vx1 <- 0
Vx1 <- 0.080584
Ax1 <- 0
Y1 <- 0
#Vy1 <- 0
Vy1 <- 0.588836
Ay1 <- 0


#M2 <- 5.972*10^(24) # Earth's Mass
M2 <- 1
#X2 <- 1.4960*10^(11) # Earth's Distance
X2 <- 1
#Vx2 <- 0
Vx2 <- 0.080584
Ax2 <- 0
Y2 <- 0
#Vy2 <- 29.78 * 10^3 # Earth's Velocity
Vy2 <- 0.588836
Ay2 <- 0


#M3 <- 7.347*10^(22) # Moon's Mass
M3 <- 1
#X3 <- 1.4960*10^(11) + 384000 # Moons's Distance
X3 <- 0
#Vx3 <- 0
Vx3 <- -2*0.080584
Ax3 <- 0
Y3 <- 0
#Vy3 <- 29.78 * 10^3 + 1000 # Moon's Velocity
Vy3 <- -2*0.588836
Ay3 <- 0


detT <- 0.00001

N <- 1500000


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


a <- 0


X1Pos <- c(rep(NA,N/150))
Y1Pos <- c(rep(NA,N/150))
X2Pos <- c(rep(NA,N/150))
Y2Pos <- c(rep(NA,N/150))
X3Pos <- c(rep(NA,N/150))
Y3Pos <- c(rep(NA,N/150))
#X1Vel <- c()
#Y1Vel <- c()
#X2Vel <- c()
#Y2Vel <- c()
#X3Vel <- c()
#Y3Vel <- c()
#X1Acc <- c()
#Y1Acc <- c()
#X2Acc <- c()
#Y2Acc <- c()
#X3Acc <- c()
#Y3Acc <- c()

for(i in 0:N) {                    
    F1 <- FORCE_X(M1, M2, X1, X2, Y1, Y2)
    F2 <- FORCE_X(M1, M3, X1, X3, Y1, Y3)
    F3 <- FORCE_Y(M1, M2, X1, X2, Y1, Y2)
    F4 <- FORCE_Y(M1, M3, X1, X3, Y1, Y3)
    F5 <- FORCE_X(M2, M3, X2, X3, Y2, Y3)
    F6 <- FORCE_Y(M2, M3, X2, X3, Y2, Y3)
    
    Vx1 <- Vx1+Ax1*detT/2 #Half Step
    Vy1 <- Vy1+Ay1*detT/2 
    Vx2 <- Vx2+Ax2*detT/2 
    Vy2 <- Vy2+Ay2*detT/2 
    Vx3 <- Vx3+Ax3*detT/2 
    Vy3 <- Vy3+Ay3*detT/2 
    X1 <- X1+Vx1*detT
    Y1 <- Y1+Vy1*detT
    X2 <- X2+Vx2*detT
    Y2 <- Y2+Vy2*detT
    X3 <- X3+Vx3*detT
    Y3 <- Y3+Vy3*detT
    Ax1 <- -F1/M1 - F2/M1
    Ay1 <- -F3/M1 -F4/M1# Accelerations Update 1
    Ax2 <- F1/M2 -F5/M2
    Ay2 <- F3/M2-F6/M2 # Accelerations Update 2
    Ax3 <- F5/M3 + F2/M3
    Ay3 <- F6/M3 +F4/M3 # Accelerations Update 3
    Vx1 <- Vx1+Ax1*detT/2 #Second Half Step
    Vy1 <- Vy1+Ay1*detT/2 
    Vx2 <- Vx2+Ax2*detT/2 
    Vy2 <- Vy2+Ay2*detT/2 
    Vx3 <- Vx3+Ax3*detT/2 
    Vy3 <- Vy3+Ay3*detT/2 


#Now let us record all of our values into an array and save them for later
    if(i%%150 == 0){
    a <- a+1
    X1Pos[a] <- X1
    Y1Pos[a] <- Y1
    X2Pos[a] <- X2
    Y2Pos[a] <- Y2
    X3Pos[a] <- X3
    Y3Pos[a] <- Y3
    #X1Vel <- c(X1Vel, Vx1)
    #Y1Vel <- c(Y1Vel, Vy1)
    #X2Vel <- c(X2Vel, Vx2)
    #Y2Vel <- c(Y2Vel, Vy2)
    #X3Vel <- c(X3Vel, Vx3)
    #Y3Vel <- c(Y3Vel, Vy3)
    #X1Acc <- c(X1Acc, Ax1)
    #Y1Acc <- c(Y1Acc, Ay1)
    #X2Acc <- c(X2Acc, Ax2)
    #Y2Acc <- c(Y2Acc, Ay2)
    #X3Acc <- c(X3Acc, Ax3)
    #Y3Acc <- c(Y3Acc, Ay3)
    }
}


plot(X1Pos, Y1Pos, type = "p",pch='.', col = "red", xlab = "X",
     ylab = "Y", main = "2 Body Simulation",ylim = c(-2,2),xlim = c(-2,2))
points(X2Pos,Y2Pos, type = "p",pch='.', col = "blue")
points(X3Pos,Y3Pos, type = "p",pch='.', col = "green")
