##my first R-program
f1 <-function(){
  x<-0:9
  print(x)
  as.logical(x)
  car <- c(1,2,3,5,5,6,7,9)
  plot(car,type="h",col="red")
  pie(car,main="Car DISTRO",col=rainbow(length(car)))
  sp <- read.csv("speed.csv")
  plot(sp,type="l",col="blue")
}