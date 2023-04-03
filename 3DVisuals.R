library(rgl)

mycolors <- c('royalblue1')

plot3d( 
  x=c(X3Pos,X2Pos), y= c(Y3Pos,Y2Pos), z=c(Z3Pos,Z2Pos), 
  col = mycolors, 
  type = 's', 
  radius = 2000000000,
  xlab="x", ylab="y", zlab="z")

htmlwidgets::saveWidget(rglwidget(width = 500, height = 500), 
                        selfcontained = FALSE
)