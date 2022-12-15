xp <- c(3.05,3.86,4.67,5.48,6.29,7.10,7.91)
fp<- c(0.3619,-3.0480,-10.0123,-3.4302,0.0272,2.7344,3.6877)
DATOS <- data.frame(xp,fp)

MCO4<- lm(fp~xp + I(xp^2)+ I(xp^3)+ I(xp^4),data = DATOS)
  
summary(MCO4)  

m<-length(MCO4$coefficients)

MCO_P4 <- paste("f(x)=", round(MCO4$coefficients[1],4))
for (i in 2:m) {
  MCO_P4<- paste(MCO_P4,"+",round(MCO4$coefficients[i],4),"x^",(i-1))
}

MCO_P4