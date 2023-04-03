ACCMag <- function(m2, x1, x2, y1, y2,z1,z2) {
  mag <- (G*m2) / ((x1 - x2)^2 + (y1 - y2)^2+(z1-z2)^2)
  return(mag)
}
#Newton's Law of Gravitation (there is no need to the big constant G here)
Cosine <- function(x1,x2,y1,y2){
  (x1-x2)/sqrt((x1-x2)^2+(y1-y2)^2)
}
Sine <- function(x1,x2,y1,y2){
  (y1-y2)/sqrt((x1-x2)^2+(y1-y2)^2)
}

SinPhi <- function(x1,x2,y1,y2,z1,z2){
  (z1-z2)/sqrt((x1-x2)^2+(y1-y2)^2+(z1-z2)^2) 
}

CosPhi <- function(x1,x2,y1,y2,z1,z2){
  sqrt((x1-x2)^2+(y1-y2)^2)/sqrt((x1-x2)^2+(y1-y2)^2+(z1-z2)^2) 
}

FORCE_X <- function(m2, x1, x2, y1, y2,z1,z2){
  Cosine(x1,x2,y1,y2)*ACCMag(m2, x1, x2, y1, y2, z1, z2)*CosPhi(x1,x2,y1,y2,z1,z2)
}# Cosine (ratio) Multiplied by mag

FORCE_Y <- function(m2, x1, x2, y1, y2,z1,z2){
  Sine(x1,x2,y1,y2)*ACCMag(m2, x1, x2, y1, y2, z1, z2)*CosPhi(x1,x2,y1,y2,z1,z2)
}# Sine (ratio) Multiplied by mag

FORCE_Z <- function(m2, x1, x2, y1, y2,z1,z2){
  SinPhi(x1,x2,y1,y2,z1,z2)*ACCMag(m2, x1, x2, y1, y2, z1, z2)
}
#work in progress, gives results tho
G <- 6.6743*10^(-11) # Newton's Gravitational Constant
M1 <- 1.9891*10^(30) # Sun's Mass
X1 <- 0
Vx1 <- 0
Ax1 <- 0
Y1 <- 0
Vy1 <- 0
Ay1 <- 0
Z1 <- 0
Vz1 <- 0
Az1 <- 0


M2 <- 5.972*10^(24) # Earth's Mass
X2 <- 1.4960*10^(11) # Earth's Distance
Vx2 <- 0
Ax2 <- 0
Y2 <- 0
Vy2 <- 29.78 * 10^3 # Earth's Velocity
Ay2 <- 0
Z2 <- 0
#Vz2 <- 1000
Vz2 <- 0
Az2 <- 0
#------
#-----
M3 <- 6200 # Lagrange Point L2
X3 <- 1.4960*10^(11) + 1500000000 #L2 point of orbit
Ax3 <- 0
Y3 <- -800000000
Vx3 <- 0  
Vy3 <- 30.2 * 10^3#roughly speed of L2 i got off reddit <- LMAO
Ay3 <- 0
Z3 <- 0
Vz3 <-500 #L2
Az3 <- 0

detT <- 40

L2x <- 1.4960*10^(11) + 1500000000
L2y <- 0
L2z <- 0

N <- 3.154*0.25*10^6 #good for 1 full orbit around sun

#N<-800000 #good for 1 full orbit around L2

a <- 0

ratio<-X3/X2#ratio used to determine L2 position in space, since L2 is 1.5m km behind earth we can use this
#ratio to find the x and y position of L2 at later times

count<-0#counter for how many times we store fuel

C<-c(rep(NA,N/4000))#array to store fuel used over time

X1Pos <- c(rep(NA,N/4000))
Y1Pos <- c(rep(NA,N/4000))
Z1Pos <- c(rep(NA,N/4000))
X2Pos <- c(rep(NA,N/4000))
Y2Pos <- c(rep(NA,N/4000))
Z2Pos <- c(rep(NA,N/4000))
X3Pos <- c(rep(NA,N/4000))
Y3Pos <- c(rep(NA,N/4000))
Z3Pos <- c(rep(NA,N/4000))

boost<-0.000434#best boost there is

for(i in 0:N) {                    
  Vx1 <- Vx1+Ax1*detT/2 #Half Step
  Vy1 <- Vy1+Ay1*detT/2 
  Vz1 <- Vz1+Az1*detT/2 
  Vx2 <- Vx2+Ax2*detT/2 
  Vy2 <- Vy2+Ay2*detT/2 
  Vz2 <- Vz2+Az2*detT/2 
  Vx3 <- Vx3+Ax3*detT/2 
  Vy3 <- Vy3+Ay3*detT/2 
  Vz3 <- Vz3+Az3*detT/2 
  X1 <- X1+Vx1*detT
  Y1 <- Y1+Vy1*detT
  Z1 <- Z1+Vz1*detT
  X2 <- X2+Vx2*detT
  Y2 <- Y2+Vy2*detT
  Z2 <- Z2+Vz2*detT
  X3 <- X3+Vx3*detT
  Y3 <- Y3+Vy3*detT
  Z3 <- Z3+Vz3*detT

  #Edited the code to make a sphere around the point instead and boost the opposide direction of the radius.
  #Boosts in the unit vector pointed towards the L2 Point from the satellite's position
  
  if (((ratio*X2-X3)^2+(ratio*Y2-Y3)^2+Z3^2)^(1/2) > 800000000){
    Ax3 <- FORCE_X(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_X(M1, X1, X3, Y1, Y3,Z1,Z3) + boost*(((ratio*X2-X3))/((ratio*X2-X3)^2+(ratio*Y2-Y3)^2+Z3^2)^(1/2))
    Ay3 <- FORCE_Y(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Y(M1, X1, X3, Y1, Y3,Z1,Z3) + boost*(((ratio*Y2-Y3))/((ratio*X2-X3)^2+(ratio*Y2-Y3)^2+Z3^2)^(1/2))
    Az3 <- FORCE_Z(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Z(M1, X1, X3, Y1, Y3,Z1,Z3) + boost*(((-Z3))/((ratio*X2-X3)^2+(ratio*Y2-Y3)^2+Z3^2)^(1/2))
    count <- count+1
  }
  else{
    Ax3 <- FORCE_X(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_X(M1, X1, X3, Y1, Y3,Z1,Z3)
    Ay3 <- FORCE_Y(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Y(M1, X1, X3, Y1, Y3,Z1,Z3)
    Az3 <- FORCE_Z(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Z(M1, X1, X3, Y1, Y3,Z1,Z3)
  }
  
  Ax1 <- -FORCE_X(M2, X1, X2, Y1, Y2,Z1,Z2) - FORCE_X(M3, X1, X3, Y1, Y3,Z1,Z3)
  Ay1 <- -FORCE_Y(M2, X1, X2, Y1, Y2,Z1,Z2) - FORCE_Y(M3, X1, X3, Y1, Y3,Z1,Z3)# Accelerations Update 1
  Az1 <- -FORCE_Z(M2, X1, X2, Y1, Y2,Z1,Z2) - FORCE_Z(M3, X1, X3, Y1, Y3,Z1,Z3)
  Ax2 <- FORCE_X(M1, X1, X2, Y1, Y2,Z1,Z2) - FORCE_X(M3, X2, X3, Y2, Y3,Z2,Z3)
  Ay2 <- FORCE_Y(M1, X1, X2, Y1, Y2,Z1,Z2) - FORCE_Y(M3, X2, X3, Y2, Y3,Z2,Z3) # Accelerations Update 2
  Az2 <- FORCE_Z(M1, X1, X2, Y1, Y2,Z1,Z2) - FORCE_Z(M3, X2, X3, Y2, Y3,Z2,Z3)
  
  Vx1 <- Vx1+Ax1*detT/2 #Half Step
  Vy1 <- Vy1+Ay1*detT/2 
  Vz1 <- Vz1+Az1*detT/2 
  Vx2 <- Vx2+Ax2*detT/2 
  Vy2 <- Vy2+Ay2*detT/2 
  Vz2 <- Vz2+Az2*detT/2 
  Vx3 <- Vx3+Ax3*detT/2 
  Vy3 <- Vy3+Ay3*detT/2 
  Vz3 <- Vz3+Az3*detT/2
  
  
  #Now let us record all of our values into an array and save them for later
  if(i%%4000 == 0){
    a <- a+1
    X1Pos[a] <- X1
    Y1Pos[a] <- Y1
    Z1Pos[a] <- Z1
    X2Pos[a] <- X2
    Y2Pos[a] <- Y2
    Z2Pos[a] <- Z2
    X3Pos[a] <- X3
    Y3Pos[a] <- Y3
    Z3Pos[a] <- Z3
    C[a]<- count
  }
  
  
}

pdf("1yearorbitNickSmall.pdf")
plot(X1Pos, Y1Pos, type = "p", col = "red", xlab = "X",
     ylab = "Y", main = "3 Body Simulation",ylim = c(-2*10^11,2*10^11),xlim = c(-2*10^11,2*10^11))
points(X2Pos,Y2Pos, type = "p", col = "blue")
points(X3Pos,Y3Pos, type = "p", col = "green")
dev.off()
pdf("distance.pdf")
plot(seq(0,N/4000,by=1),sqrt((X3Pos-X2Pos)^2+(Y3Pos-Y2Pos)^2+(Z3Pos-Z2Pos)^2), type ="p", col = "blue", xlab = "Time (Arb)", ylab = "Distance Between Satellite - Earth")
dev.off()
#plot(X3Pos,Z3Pos,type = "p")

#plotting the total acceleration given to satellite in cardinal direction as function of time
# CX gives the count, xboost gives the acceleration boost per count so we multiply them to get acceleration
pdf("FuelRadiusMethodSmall.pdf")
plot(seq(0,N/4000,by=1),C*boost,ylab = "Acceleration",xlab = "Time",main = "Total Acceleration Needed")#plotting total acceleration used
dev.off()



