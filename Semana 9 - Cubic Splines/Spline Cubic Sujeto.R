rm(list = ls())
graphics.off()


precio <- c(125, 119.41372, 114.14038, 109.15941, 104.45182, 100,  95.78764,  91.79961,  88.02187,  84.44139, 81.04607)
tasas <- seq(0,0.1,0.01)

ClampedCubicSpline_3.5 <- function(xp,fp,FP0,FPN,x){
  n <- length(xp)
  h <- matrix(NA,(n-1),1)
  a <- matrix(NA,(n-2),1)
  l <- matrix(NA,n,1)
  mu <- matrix(NA,(n-1),1)
  z <- matrix(NA,n,1)
  b <- matrix(NA,(n-1),1)
  c <- matrix(NA,(n-2),1)
  d <- matrix(NA,(n-1),1)
  
  #Paso 1
  for (i in 1:(n-1)){
    h[i] <- xp[i+1]-xp[i]
  }
  
  #Paso 2
  a[1] <- ((3*(fp[2]-fp[1]))/h[1])-3*FP0
  a[n] <- 3*FPN-((3*(fp[n]-fp[n-1]))/h[n-1])
  
  #Paso 3
  for (i in 2:(n-1)){
    a[i] = (3/h[i])*(fp[i+1]-fp[i])-(3/h[i-1])*(fp[i]-fp[i-1])
  }
  
  #Paso 4
  l[1]=2*h[1]
  mu[1]=0.5
  z[1]=a[1]/l[1]
  
  #Paso 5
  for (i in 2:(n-1)){
    l[i]=2*(xp[i+1]-xp[i-1])-h[i-1]*mu[i-1]
    mu[i]=h[i]/l[i]
    z[i]=(a[i]-h[i-1]*z[i-1])/l[i]
  }  
  #Paso 6
  l[n]=h[n-1]*(2-mu[n-1])
  z[n]=(a[n]-h[n-1]*z[n-1])/l[n]
  c[n]=z[n]
  
  #Paso 7
  i=1
  for (j in (n-i):0){
    c[j]=z[j]-mu[j]*c[j+1]
    b[j]=((fp[j+1]-fp[j])/h[j])-(h[j]*(c[j+1]+2*c[j])/3)
    d[j]=(c[j+1]-c[j])/(3*h[j])
  }
  
  #Paso 7
  fp <- fp[1:(n-1)]
  c <- c[1:(n-1)]
  
  res <- matrix(cbind(fp,b,c,d),(n-1),4)
  
  if(missing(x)){
    return(res)
  }
  else if(x>xp[1] & x<tail(xp, n=1)){
    
    fila<-min(which(xp > x))-1
    
    v_x<-rep((x-xp[fila]),4)
    for(i in 1:4){
      v_x[i]<-v_x[i]^(i-1)
      
    }
    
    rdo<-res[fila,]%*%v_x
    print(rdo)
    
  }
  else if (x==xp[1]) {
    v_x<-rep(x-xp[1],4)
    for(i in 1:4){
      v_x[i]<-v_x[i]^(i-1)
    }  
    rdo<-res[1,]%*%v_x
    print(rdo)
    
  }
  else if (x==xp[length(xp)]) {
    v_x<-rep(x-xp[length(xp)-1],4)
    for(i in 1:4){
      v_x[i]<-v_x[i]^(i-1)
    }  
    rdo<-res[length(xp)-1,]%*%v_x
    print(rdo)
  }
  
  else {
    print('valores por fuera del intervalo')
  }
}

ClampedCubicSpline_3.5(tasas,precio,-575.0002,-330.6541)


M<-as.matrix(round(ClampedCubicSpline_3.5(tasas,precio,-575.0002,-330.6541),20))
M

##PROCESO DE GENERACION DE POLINOMIOS

xp <- seq(0,0.1,0.01)
fp <- c(125, 119.41372, 114.14038, 109.15941, 104.45182, 100,  95.78764,  91.79961,  88.02187,  84.44139, 81.04607)


for(i in 1:(length(xp)-1)){
  a<-paste("(x-",xp[i],")",sep="")
  signo_b<-sign(M[i,2])
  if(signo_b==-1){
    signo_b<-"-"
  }else{
    signo_b<-"+"
  }
  signo_c<-sign(M[i,3])
  if(signo_c==-1){
    signo_c<-"-"
  }else{
    signo_c<-"+"
  }
  signo_d<-sign(M[i,4])
  if(signo_d==-1){
    signo_d<-"-"
  }else{
    signo_d<-"+"
  }
  b<-paste(M[i,1],signo_b,abs(M[i,2]),"*",a,sep="")
  b<-paste(b,signo_c,abs(M[i,3]),"*",a,"^",2,sep="")
  b<-paste(b,signo_d,abs(M[i,4]),"*",a,"^",3,sep="")
  assign(paste("p",i,"spline_sujeto(x)",sep="_"),b)
  c<-paste("Para el intervalo (",xp[i],",",xp[i+1],")",sep="")
  c<-paste(c,b,sep="\n")
  cat(c,sep = '\n')
}
rm(a,b,c,signo_b,signo_c,signo_d)

# Generar funciones a partir de polinomios --------------------------------


p_1_sp_sujeto<-function(x){
  r_poli<-eval(parse(text=`p_1_spline_sujeto(x)`))
  return(r_poli)
}

p_2_sp_sujeto<-function(x){
  r_poli<-eval(parse(text=`p_2_spline_sujeto(x)`))
  return(r_poli)
}

p_3_sp_sujeto<-function(x){
  r_poli<-eval(parse(text=`p_3_spline_sujeto(x)`))
  return(r_poli)
}

p_4_sp_sujeto<-function(x){
  r_poli<-eval(parse(text=`p_4_spline_sujeto(x)`))
  return(r_poli)
}

p_5_sp_sujeto<-function(x){
  r_poli<-eval(parse(text=`p_5_spline_sujeto(x)`))
  return(r_poli)
}

p_6_sp_sujeto<-function(x){
  r_poli<-eval(parse(text=`p_6_spline_sujeto(x)`))
  return(r_poli)
}

p_7_sp_sujeto<-function(x){
  r_poli<-eval(parse(text=`p_7_spline_sujeto(x)`))
  return(r_poli)
}

p_8_sp_sujeto<-function(x){
  r_poli<-eval(parse(text=`p_8_spline_sujeto(x)`))
  return(r_poli)
}

p_9_sp_sujeto<-function(x){
  r_poli<-eval(parse(text=`p_9_spline_sujeto(x)`))
  return(r_poli)
}

p_10_sp_sujeto<-function(x){
  r_poli<-eval(parse(text=`p_10_spline_sujeto(x)`))
  return(r_poli)
}

p_10_sp_sujeto(0.095)

ClampedCubicSpline_3.5(tasas,precio,-575.0002,-330.6541,0.095)