#If you are running this on you local RStudio you must first uncomment the install lines and install them

install.packages("gganimate")
install.packages("gifski")
library(gganimate)
library(ggplot2)
library(ggthemes)

#ONLY RUN THIS CODE AFTER YOU HAVE RUN THE MAIN 2 BODY SIMULATION OTHERWISE IT WILL NOT WORK

df <- data.frame(time = 1:length(X1Pos), 
                 Objects = rep(c("1", "2","3","4","5","6","7","8","9"), each = length(X1Pos)),
                 x = c(X1Pos, X2Pos,X3Pos,X4Pos, X5Pos,X6Pos,X7Pos, X8Pos,X9Pos), y = c(Y1Pos, Y2Pos,Y3Pos,Y4Pos, Y5Pos,Y6Pos,Y7Pos, Y8Pos,Y9Pos))

p <- ggplot(df, aes(x = x, y = y, frame = time, colour = Objects)) +
  geom_point(size=2) +
  transition_time(time) +
  labs(subtitle = "Time Step: {frame_time}", Title = "2 Body Orbiter", x = "X Cordinate (m)", y = "Y Cordinate (m)")+
  shadow_wake(wake_length = 0.1)+
  theme_clean()

ant <- animate(p, renderer = gifski_renderer(), height = 500, width = 500, fps = 30, duration = 10,
        end_pause = 30)




anim_save('solar4.gif',ant)

