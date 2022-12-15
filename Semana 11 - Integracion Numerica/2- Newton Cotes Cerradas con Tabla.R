rm(list=ls())
graphics.off()

#Newton Cotes Cerradas con Tabla

#Regla Trapecio n=1

vxt<-c(1,2)
vyt<-c(1,2)

trap<-function(a,b){
  h<-(b-a)
  t<-(h/2)*(vyt[1]+vyt[2])
  return(t)
}

#Regla Simpson n=2

vxs<-c(1,2,3)
vys<-c(1,2,3)

simp<-function(a,b){
  h<-(b-a)/2
  s<-(h/3)*(vys[1]+4*vys[2]+vys[3])
  return(s)
}


#Tres Octavos de Simpson n=3

vxsoct<-c(1,2,3,4)
vysoct<-c(1,2,3,4)

simptresoc<-function(a,b){
  h<-(b-a)/3
  st<-((3*h)/8)*(vysoct[1]+3*vysoct[2]+3*vysoct[3]+vysoct[4])
  return(st)
}


#NC con n=4

vx4<-c(1,2,3,4,5)
vy4<-c(1,2,3,4,5)

newtgrado4<-function(a,b){
  h<-(b-a)/4
  n4<-((2*h)/45)*(7*vy4[1]+32*vy4[2]+12*vy4[3]+32*vy4[4]+7*vy4[5])
  return(n4)
}
