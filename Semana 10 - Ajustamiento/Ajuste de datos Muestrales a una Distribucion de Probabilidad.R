set

Datos = read.csv(file.choose(), header = T, row.names = 1)
plot(Datos)
abline(h=c(0,1), col = 'lightgrey')
Modelo = nls(fx ~ pnorm(x,mu,sigma),
             data = Datos,
             start=list(mu=120,sigma=50))
summary(Modelo)

AjusteNL = data.frame(x=seq(from = min(Datos$x), to = max(Datos$x), lenght.out = 1000))
AjusteNL$fit = predict(Modelo, newdata = list(x=AjusteNL$x))
lines(AjusteNL$x,AjusteNL$fit,col='red', lty= 'dashed')

mu_muestra = mean(Datos$x)
sigma_muestra = sd(Datos$x)
curve(pnorm(x,mu_muestra,sigma_muestra), col = 'blue', add = T,
      from = min(Datos$x), to = max(Datos$x))

legend('bottomright',
       legend = c('Datos','NLS','Momentos'),
       lty = c(0,2,1), pch = c(1,NA,NA),
       col= c('black','red','blue'))
title('Ajuste de datos a Distribucion Normal', cex= 0.8)