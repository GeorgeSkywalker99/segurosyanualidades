
library(dplyr)
#Primero importaremos la base y de preferencia veamos como estan los nombres de ellas
#Si se cambian las etiquetas las funciones no van a funcionar asi que deben estar estandarizadas

#Todos son seguros y anualidades anticipadas
seguro_ordinario<-function(edad, base){
  #MX/DX
  fila_x<-filter(base, Edad==edad)
  fila_x$Mx/fila_x$Dx
}

seguro_temporal<-function(edad, temporalidad, base){
  #(MX-MX+N)/DX
  fila_x<-filter(base, Edad==edad)
  fila_xn<-filter(base, Edad==edad+temporalidad)
  (fila_x$Mx-fila_xn$Mx)/fila_x$Dx
}

dotal_puro<-function(edad, temporalidad, base){
  #Dx+n/Dx
  fila_x<-filter(base, Edad==edad)
  fila_xn<-filter(base, Edad==edad+temporalidad)
  fila_xn$Dx/fila_x$Dx
}
dotal_mixto<-function(edad, temporalidad, base){
  #(Mx-Mx+n+Dx+n)/Dx
  dotal_puro(edad, temporalidad, base)+seguro_temporal(edad, temporalidad, base)
}

anualidad_vitalicia<-function(edad, base){
  #NX/DX
  fila_x<-filter(base, Edad==edad)
  fila_x$Nx/fila_x$Dx
}

anualidad_temporal<-function(edad, temporalidad, base){
  #(NX-NX+N)/DX
  fila_x<-filter(base, Edad==edad)
  fila_xn<-filter(base, Edad==edad+temporalidad)
  (fila_x$Nx-fila_xn$Nx)/fila_x$Dx
}

#Ejemplo: 
#Quiero un hallar la prima nivelada para un ordinario a edad 30, pagos limitados 10.CNSF 2013-i=0.03
#Supongamos que importe la base al objeto: base2013
#Calculo el seguro con s<-seguro_ordinario(30, base2013)
#Calculo la anualidad con a<-anualidad_temporal(30,10,base2013)
#P<-s/a



#Este codigo es para generar una tabla de seguros temporales 
#V<-array(0,dim=(c(99,98))) 
#for(i in 12:110){
#  for(j in 1:98){
#    if(i+j<111){
#      V[i-11,j]<-seguro_temporal(i,j,base2013)
#    }
#    else{
#      V[i-11,j]<-0
#    }
#  }
#}
#write.csv(V, "anualidadestemporales.csv")
#Creo que estaban afectando los numeric(0)

