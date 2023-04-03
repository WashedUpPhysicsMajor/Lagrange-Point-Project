G <- 6.6743*10^(-11) # Newton's Gravitational Constant

M1 <- 1.9891 * 10^(30)
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


M3 <- 3.285*10^(23) # Mercury's Mass
X3 <- 61.115*10^(9) #mercury distance to sun
#Vx3 <- 0
Vy3 <- 47870
Ax3 <- 0
Y3 <- 0
Vx3 <- 0
Ay3 <- 0

M4 <- 4.867*10^(24) # Venus' Mass
X4 <- 107.89*10^(9) # venus distance to sun
#Vx3 <- 0
Vy4 <- 35020
Ax4 <- 0
Y4 <- 0
Vx4 <- 0
Ay4 <- 0

M5 <- 6.39*10^(23) # Mars' Mass
X5 <- 245.68*10^(9) # Mars distance to sun
#Vx3 <- 0
Vy5 <- 24077
Ax5 <- 0
Y5 <- 0
Vx5 <- 0
Ay5 <- 0

detT <- 100

N <- 800000


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


X1Pos <- c(rep(NA,N/4000))
Y1Pos <- c(rep(NA,N/4000))
X2Pos <- c(rep(NA,N/4000))
Y2Pos <- c(rep(NA,N/4000))
X3Pos <- c(rep(NA,N/4000))
Y3Pos <- c(rep(NA,N/4000))
X4Pos <- c(rep(NA,N/4000))
Y4Pos <- c(rep(NA,N/4000))
X5Pos <- c(rep(NA,N/4000))
Y5Pos <- c(rep(NA,N/4000))


for(i in 0:N) {                    
  
  
  Vx1 <- Vx1+Ax1*detT/2 #Half Step
  Vy1 <- Vy1+Ay1*detT/2 
  Vx2 <- Vx2+Ax2*detT/2 
  Vy2 <- Vy2+Ay2*detT/2 
  Vx3 <- Vx3+Ax3*detT/2 
  Vy3 <- Vy3+Ay3*detT/2 
  Vx4 <- Vx4+Ax4*detT/2 #Half Step
  Vy4 <- Vy4+Ay4*detT/2 
  Vx5 <- Vx5+Ax5*detT/2 
  Vy5 <- Vy5+Ay5*detT/2 
  
  X1 <- X1+Vx1*detT
  Y1 <- Y1+Vy1*detT
  X2 <- X2+Vx2*detT
  Y2 <- Y2+Vy2*detT
  X3 <- X3+Vx3*detT
  Y3 <- Y3+Vy3*detT
  X4 <- X4+Vx4*detT
  Y4 <- Y4+Vy4*detT
  X5 <- X5+Vx5*detT
  Y5 <- Y5+Vy5*detT
  
  SunF1x <- FORCE_X(M1,M3,X1,X3,Y1,Y3)#mercury force x
  SunF1y <- FORCE_Y(M1,M3,X1,X3,Y1,Y3)#mercury force y
  SunF2x <- FORCE_X(M1,M2,X1,X2,Y1,Y2)#earth force x
  SunF2y <- FORCE_Y(M1,M2,X1,X2,Y1,Y2)#earth force y
  SunF3x <- FORCE_X(M1,M4,X1,X4,Y1,Y4)#venus force x
  SunF3y <- FORCE_Y(M1,M4,X1,X4,Y1,Y4)#venus force y
  SunF4x <- FORCE_X(M1,M5,X1,X5,Y1,Y5)#mars force x
  SunF4y <- FORCE_Y(M1,M5,X1,X5,Y1,Y5)#mars force y
  SunF5x <- FORCE_X(M1,M6,X1,X6,Y1,Y6)#jupyter force x
  SunF5y <- FORCE_Y(M1,M6,X1,X6,Y1,Y6)#jupyter force y
  
  Ax1 <- -SunF1x/M1 - SunF2x/M1 -SunF3x/M1 - SunF4x/M1 -SunF5x/M1
  Ay1 <- -SunF1y/M1 - SunF2y/M1 -SunF3y/M1 - SunF4y/M1 -SunF5y/M1
  
  Ax2 <- SunF2x/M2#earth
  Ay2 <- SunF2y/M2
  
  Ax3 <- SunF1x/M3#mercury
  Ay3 <- SunF1y/M3 
  
  Ax4 <- SunF3x/M4
  Ay4 <- SunF3y/M4 #venus
  
  Ax5 <- SunF4x/M5#mars
  Ay5 <- SunF4y/M5
  
  
  Vx1 <- Vx1+Ax1*detT/2 #Half Step
  Vy1 <- Vy1+Ay1*detT/2 
  Vx2 <- Vx2+Ax2*detT/2 
  Vy2 <- Vy2+Ay2*detT/2 
  Vx3 <- Vx3+Ax3*detT/2 
  Vy3 <- Vy3+Ay3*detT/2 
  Vx4 <- Vx4+Ax4*detT/2 #Half Step
  Vy4 <- Vy4+Ay4*detT/2 
  Vx5 <- Vx5+Ax5*detT/2 
  Vy5 <- Vy5+Ay5*detT/2 
  
  
  #Now let us record all of our values into an array and save them for later
  if(i%%4000 == 0){
  a <- a+1
  X1Pos[a] <- X1
  Y1Pos[a] <- Y1
  X2Pos[a] <- X2
  Y2Pos[a] <- Y2
  X3Pos[a] <- X3
  Y3Pos[a] <- Y3
  X4Pos[a] <- X4
  Y4Pos[a] <- Y4
  X5Pos[a] <- X5
  Y5Pos[a] <- Y5
  
  }
}


plot(X1Pos, Y1Pos, type = "p", col = "red", xlab = "X",
     ylab = "Y", main = "2 Body Simulation", xlim = c(-2.5*10^(11),2.5*10^(11)), ylim = c(-2.5*10^(11),2.5*10^(11)))
points(X2Pos,Y2Pos, type = "p", col = "blue")
points(X3Pos,Y3Pos, type = "p", col = "green")
points(X4Pos,Y4Pos, type = "p", col = "orange")
points(X5Pos,Y5Pos, type = "p", col = "#ee00ff")
