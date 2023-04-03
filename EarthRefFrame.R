install.packages("ggforce")
library(ggforce)
library(gganimate)
library(ggplot2)
library(ggthemes)

#ONLY RUN THIS CODE AFTER YOU HAVE RUN THE MAIN 2 BODY SIMULATION OTHERWISE IT WILL NOT WORK

df <- data.frame(time = 1:length(X1Pos), 
                 Objects = rep(c("Earth","L4"), each = length(X1Pos)),
                 x = c(X2Pos-X2Pos,X3Pos-X2Pos), y = c(Y2Pos-Y2Pos,Y3Pos-Y2Pos))

df <- data.frame(time = 1:length(X1Pos), 
                 Objects = rep(c("Earth","L2"), each = length(X1Pos)),
                 x = c(X2Pos-X2Pos,X3Pos-X2Pos), y = c(Z2Pos-Z2Pos,Z3Pos-Z2Pos))

df <- data.frame(time = 1:length(X1Pos), 
                 Objects = rep(c("Earth","L2"), each = length(X1Pos)),
                 x = c(Y2Pos-Y2Pos,Y3Pos-Y2Pos), y = c(Z2Pos-Z2Pos,Z3Pos-Z2Pos))

df <- data.frame(time = 1:length(X1Pos), 
                 Objects = rep(c("Earth","L2"), each = length(X1Pos)),
                 x = c(X2Pos,X3Pos), y = c(Y2Pos,Y3Pos))

p <- ggplot(df, aes(x = x, y = y, frame = time, colour = Objects)) +
  geom_point(size=2) +
  transition_time(time) +
  labs(subtitle = "Time Step: {frame_time}", Title = "Zoom Of Earth and Lagrange", x = "X Cordinate (m)", y = "Y Cordinate (m)")+
  shadow_wake(wake_length = 0.2)+
  theme_clean() #+ 
  #xlim(-1700000000,1700000000)


ant <- animate(p, renderer = gifski_renderer(), height = 500, width = 500, fps = 15, duration = 10,
               end_pause = 30)
anim_save('EarthFrameL2mark12xy.gif',ant)

install.packages("plot3D")
library(plot3D)
pdf("3d.pdf")
scatter3D(X3Pos,Y3Pos,Z3Pos)
dev.off()
scatter3D(X2Pos,Y2Pos,Z2Pos)
