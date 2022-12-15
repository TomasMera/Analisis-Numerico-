rm(list=ls())
graphics.off()

#Newton Cotes Abiertas con Tabla

#NC Abierta n=0
#el x0 es el del medio siempre

ncx<-c(1,2,3)
ncy<-c(1,2,3)

nccero<-function(a,b){
  h<-(b-a)/2
  nc<-(2*h)*ncy[2]
  return(nc)
}

#NC Abierta n=1

nc1x<-c(1,2,3,4)
nc1y<-c(1,2,3,4)

ncuno<-function(a,b){
  h<-(b-a)/3
  nc<-((3*h)/2)*(nc1y[2]+nc1y[3])
  return(nc)
}

#NC Abierta n=2

nc2x<-c(1,2,3,4,5)
nc2y<-c(1,2,3,4,5)

ncdos<-function(a,b){
  h<-(b-a)/4
  nc<-((4*h)/3)*(2*nc2y[2]-nc2y[3]+2*nc2y[4])
  return(nc)
}


#NC Abierta n=3

nc3x<-c(1,2,3,4,5,6)
nc3y<-c(1,2,3,4,5,6)

nctres<-function(a,b){
  h<-(b-a)/5
  nc<-((5*h)/24)*(11*nc3y[2]+nc3y[3]+nc3y[4]+11*nc3y[5])
  return(nc)
}
