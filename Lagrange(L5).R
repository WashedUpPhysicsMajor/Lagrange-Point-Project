ACCMag <- function(m2, x1, x2, y1, y2) {
  mag <- (G*m2) / ((x1 - x2)^2 + (y1 - y2)^2)
  return(mag)
}
#Newton's Law of Gravitation (there is no need to the big constant G here)
Cosine <- function(x1,x2,y1,y2){
  (x1-x2)/sqrt((x1-x2)^2+(y1-y2)^2)
}
Sine <- function(x1,x2,y1,y2){
  (y1-y2)/sqrt((x1-x2)^2+(y1-y2)^2)
}


FORCE_X <- function(m2, x1, x2, y1, y2){
  Cosine(x1,x2,y1,y2)*ACCMag(m2, x1, x2, y1, y2) 
}# Cosine (ratio) Multiplied by mag

FORCE_Y <- function(m2, x1, x2, y1, y2){
  Sine(x1,x2,y1,y2)*ACCMag(m2, x1, x2, y1, y2)
}# Sine (ratio) Multiplied by mag

#work in progress, gives results tho
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


M3 <- 1 # Lagrange Point L2
#X3 <- X2+X2*((M2/(M1+M2))/3)^(1/3) # L2
X3 <- (X2/2)*(M1-M2)/(M1+M2) #L5
Ax3 <- 0
Y3 <- -(sqrt(3)/2)*X2 #L5
#Y3 <- 0 #L1,2 and 3
Vx3 <- Sine(X1,X3,Y1,Y3)*29.78 * 10^3 *(sqrt((X3-X2)^2+(Y3-Y2)^2))/(X2)  
Vy3 <- -Cosine(X1,X3,Y1,Y3)*29.78 * 10^3 *(sqrt((X3-X2)^2+(Y3-Y2)^2))/(X2)  
Ay3 <- 0


detT <- 100

N <- 400000

a <- 0

X1Pos <- c(rep(NA,N/4000))
Y1Pos <- c(rep(NA,N/4000))
X2Pos <- c(rep(NA,N/4000))
Y2Pos <- c(rep(NA,N/4000))
X3Pos <- c(rep(NA,N/4000))
Y3Pos <- c(rep(NA,N/4000))
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
  Ax1 <- -FORCE_X(M2, X1, X2, Y1, Y2) - FORCE_X(M3, X1, X3, Y1, Y3)
  Ay1 <- -FORCE_Y(M2, X1, X2, Y1, Y2) - FORCE_Y(M3, X1, X3, Y1, Y3)# Accelerations Update 1
  Ax2 <- FORCE_X(M1, X1, X2, Y1, Y2) - FORCE_X(M3, X2, X3, Y2, Y3)
  Ay2 <- FORCE_Y(M1, X1, X2, Y1, Y2) - FORCE_Y(M3, X2, X3, Y2, Y3) # Accelerations Update 2
  Ax3 <- FORCE_X(M2, X2, X3, Y2, Y3) + FORCE_X(M1, X1, X3, Y1, Y3)
  Ay3 <- FORCE_Y(M2, X2, X3, Y2, Y3) + FORCE_Y(M1, X1, X3, Y1, Y3) # Accelerations Update 3
  Vx1 <- Vx1+Ax1*detT/2 #Second Half Step
  Vy1 <- Vy1+Ay1*detT/2 
  Vx2 <- Vx2+Ax2*detT/2 
  Vy2 <- Vy2+Ay2*detT/2 
  Vx3 <- Vx3+Ax3*detT/2 
  Vy3 <- Vy3+Ay3*detT/2 
  
  
  #Now let us record all of our values into an array and save them for later
  if(i%%4000 == 0){
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


plot(X1Pos, Y1Pos, type = "p", col = "red", xlab = "X",
     ylab = "Y", main = "3 Body Simulation",ylim = c(-2*10^11,2*10^11),xlim = c(-2*10^11,2*10^11))
points(X2Pos,Y2Pos, type = "p", col = "blue")
points(X3Pos,Y3Pos, type = "p", col = "green")
plot(seq(0,N/4000,by=1),sqrt((X3Pos-X2Pos)^2+(Y3Pos-Y2Pos)^2), type ="p", col = "blue", xlab = "Time (Arb)", ylab = "Distance Between L5 - Earth")

