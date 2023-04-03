ACCMag <- function(m2, x1, x2, y1, y2,z1,z2) {
  mag <- (G*m2) / ((x1 - x2)^2 + (y1 - y2)^2+(z1-z2)^2)
  return(mag)
}
#Newton's Law of Gravitation
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
Vy3 <- 30.2 * 10^3 #roughly speed of L2 from NASA
Ay3 <- 0
Z3 <- 0
Vz3 <-500 #L2
Az3 <- 0

detT <- 40 #Seconds per time step

L2x <- 1.4960*10^(11) + 1500000000
L2y <- 0
L2z <- 0

N <- 3.154*0.25*10^6 #good for 1 full orbit around sun (this is how many seconds in a year (N*DetT = seconds in 1 year)

#N<-800000 #good for 1 full orbit around L2

a <- 0

ratio<-X3/X2#ratio used to determine L2 position in space, since L2 is 1.5m km behind earth we can use this
#ratio to find the x and y position of L2 at later times

countz<-0#counter for how many times we store fuel
countx<-0
county<-0

CZ<-c(rep(NA,N/400))#array to store fuel used over time
CY<-c(rep(NA,N/400))
CX<-c(rep(NA,N/400))

X1Pos <- c(rep(NA,N/400)) # More arrays for postions
Y1Pos <- c(rep(NA,N/400))
Z1Pos <- c(rep(NA,N/400))
X2Pos <- c(rep(NA,N/400))
Y2Pos <- c(rep(NA,N/400))
Z2Pos <- c(rep(NA,N/400))
X3Pos <- c(rep(NA,N/400))
Y3Pos <- c(rep(NA,N/400))
Z3Pos <- c(rep(NA,N/400))

xboost<-0.0025*30#boosts
yboost<-0.0025*30
zboost<-0.00025/2

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
  #below we create a 3D box, if the satellite leaves 3D box we give it a push back to the box,
  #the push back is given by xboost which is determined before simulation (this is the box JWT is more
  #or less in)
  #we count how many times we do this for each direction and store them when we store position
  if (Z3 > 400000000){
    Az3 <- FORCE_Z(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Z(M1, X1, X3, Y1, Y3,Z1,Z3) - zboost
    countz <- countz+1 #Now checking if satellite leaves box, if so, boost in opposite direction
  }
  else if (Z3 < -400000000){
    Az3 <- FORCE_Z(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Z(M1, X1, X3, Y1, Y3,Z1,Z3) + zboost
    countz <- countz+1
  }
  else{
    Az3 <- FORCE_Z(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Z(M1, X1, X3, Y1, Y3,Z1,Z3)
  }

  if (X3 > X2*ratio+400000000){
    Ax3 <- FORCE_X(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_X(M1, X1, X3, Y1, Y3,Z1,Z3) - xboost
    countx <- countx+1
  }
  else if (X3 < X2*ratio-400000000){
    Ax3 <- FORCE_X(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_X(M1, X1, X3, Y1, Y3,Z1,Z3) + xboost
    countx <- countx+1
  }
  else{
    Ax3 <- FORCE_X(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_X(M1, X1, X3, Y1, Y3,Z1,Z3)
  }

  if (Y3 > Y2*ratio+800000000){
    Ay3 <- FORCE_Y(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Y(M1, X1, X3, Y1, Y3,Z1,Z3) - yboost
    county <- county+1
  }
  else if (Y3 < Y2*ratio-800000000){
    Ay3 <- FORCE_Y(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Y(M1, X1, X3, Y1, Y3,Z1,Z3) + yboost
    county <- county+1
  }
  else{
    Ay3 <- FORCE_Y(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Y(M1, X1, X3, Y1, Y3,Z1,Z3)
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
  if(i%%400 == 0){ # the Mod 400 is used because we dont need an array of size N to understand the movement of the bodies when we plot these points. 
    #This saves on computational time.
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
    CZ[a]<-countz#storing count
    CX[a]<-countx
    CY[a]<-county
  }

  
}
# Now we plot and save all of the images
pdf("1yearorbit52.pdf")
plot(X1Pos, Y1Pos, type = "p", col = "red", xlab = "X",
     ylab = "Y", main = "3 Body Simulation",ylim = c(-2*10^11,2*10^11),xlim = c(-2*10^11,2*10^11))
points(X2Pos,Y2Pos, type = "p", col = "blue")
points(X3Pos,Y3Pos, type = "p", col = "green")
dev.off()
#plot(seq(0,N/400,by=1),sqrt((X3Pos-X2Pos)^2+(Y3Pos-Y2Pos)^2), type ="p", col = "blue", xlab = "Time (Arb)", ylab = "Distance Between L5 - Earth")
#plot(X3Pos,Z3Pos,type = "p")

#plotting the total acceleration given to satellite in cardinal direction as function of time
# CX gives the count, xboost gives the acceleration boost per count so we multiply them to get acceleration
pdf("yaxisfuel52.pdf")
plot(seq(0,N/400,by=1),CY*yboost,ylab = "Acceleration",xlab = "Time",main = "Total Y Acceleration Needed")#plotting total acceleration used
dev.off()
pdf("xaxisfuel52.pdf")
plot(seq(0,N/400,by=1),CX*xboost,ylab = "Acceleration",xlab = "Time",main = "Total X Acceleration Needed")
dev.off()
pdf("zaxisfuel52.pdf")
plot(seq(0,N/400,by=1),CZ*zboost,ylab = "Acceleration",xlab = "Time",main = "Total Z Acceleration Needed")
dev.off()


#first set of fuel graphs gave 0.0025 boosts to x and y, 0.00025 boosts to z.

#second set is a write off, i tried to make a gradient boost where
#boosts would kick in at the half way mark to the boundary and again at the boundary.

#third boost i gave x and y 0.0025/2 boosts and z a 0.00025/2 boosts. It seems to stay
#in orbit, z and y fuel decrease but x fuel increased overall.


#as seen in the 1yearorbit images, all these boosts keep the satellite in orbit, we just need
#to minimize it while keeping it in orbit, looking at the fourth boost it seems like bigger boosts
#work better, it is also obvious that the boosts in one direction effect other directions
#we cant just treat this as 3 minimization problems

#the idea is to probably do a binary search of some sorts to decrease the final value
#of CX,CZ and CY as much as possible. this would take forever to run in r so maybe we switch
#to c++ for the binary search aspect. 

#fourth boost we had xboost=yboost=zboost = 0.0025*20, this lowered x and y but increased z by alot 
#so z should be lower than x and y boost

#fifth boost xboost<-0.0025*30, yboost<-0.0025*30, zboost<-0.00025/2 greatly reduced overall
#fuel needed. -- worked the best so far

#sixth boost used xboost<-0.0025*30 yboost<-0.0025*40 zboost<-0.00025/4, x stayed same, y increased
#a lot, z decreased

#seventh boost used xboost<-0.0025*30 yboost<-0.0025*30 zboost<-0.00025/4, x decrease, y z stayed same

#8th boost used xboost<-0.0025*30 yboost<-0.0025*20 zboost<-0.00025/4, didnt do shit

#9th boost used xboost<-0.0025*30 yboost<-0.0025*100 zboost<-0.00025/4, fucked

#5 works the best, looks like we need to minimize around that

#52 had detT = 40 but same as #5


