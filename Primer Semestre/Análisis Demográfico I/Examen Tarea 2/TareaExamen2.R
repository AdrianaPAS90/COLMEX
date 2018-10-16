rm(list=ls())
NxMedia_H<-read.csv("NxMedia_EdoMex_H.csv", header = TRUE, row.names =1)
NxMedia_M<-read.csv("NxMedia_EdoMex_M.csv", header = TRUE, row.names =1)
Dx_H<-read.csv("Dx_EdoMex_H.csv", header = TRUE, row.names =1)
Dx_M<-read.csv("Dx_EdoMex_M.csv", header = TRUE, row.names =1)
Nx_H<-read.csv("Nx_EdoMex_H.csv", header = TRUE, row.names =1)
Nx_M<-read.csv("Nx_EdoMex_M.csv", header = TRUE, row.names =1)
NxNacional_H<-read.csv("NxMedia_Nacional_H.csv", header = TRUE, row.names =1)
NxNacional_M<-read.csv("NxMedia_Nacional_M.csv", header = TRUE, row.names =1)

#Años-Persona
AP_H<-matrix(0,110,46)
AP_M<-matrix(0,110,46)
for(i in 1:46){
  AP_H[,i]<-(Nx_H[,i+1]-Nx_H[,i])/(log(Nx_H[,i+1]/Nx_H[,i]))
}
for(i in 1:46){
  AP_M[,i]<-(Nx_M[,i+1]-Nx_M[,i])/(log(Nx_M[,i+1]/Nx_M[,i]))
}
AP_H[is.nan(AP_H)] <- 1
AP_H[is.infinite(AP_H)] <- 1
AP_M[is.nan(AP_M)] <- 1
AP_M[is.infinite(AP_M)] <- 1

for(j in 1:110){
for(i in 1:46){
  if (AP_H[j,i] == 0){
    AP_H[j,i] <- 1
  }
  if (AP_M[j,i] == 0){
    AP_M[j,i]<-1
  }   
}
}

#Tasas específicas por edad
MxH<-(Dx_H)/(AP_H)
MxM<-(Dx_M)/(AP_M)

#Estructura por edad
CxH<-matrix(0,110,46)
CxM<-matrix(0,110,46)

for(j in 1:46){
  CxH[,j]<-AP_H[,j]/colSums(AP_H)[j]
}
for(j in 1:46){
  CxM[,j]<-AP_M[,j]/colSums(AP_M)[j]
}
CxH[is.nan(CxH)] <- 1
CxH[is.infinite(CxH)] <- 1
CxM[is.nan(CxM)] <- 1
CxM[is.infinite(CxM)] <- 1

tbm_H<-1000*colSums(MxH*CxH)
tbm_M<-1000*colSums(MxM*CxM)

#O 
TBM_H<-1000*(colSums(Dx_H)/colSums(AP_H))
TBM_M<-1000*(colSums(Dx_M)/colSums(AP_M))

#Estructura de edades Nacional
CxNacionalH<-NxNacional_H[,46]/colSums(NxNacional_H)[46]
CxNacionalM<-NxNacional_M[,46]/colSums(NxNacional_M)[46]

#Tasas Estandarizadas de Mortalidad
tem_H<-1000*colSums(MxH*CxNacionalH)
tem_M<-1000*colSums(MxM*CxNacionalM)

matplot(tbm_H,type="l",lwd=2, log ="y", xaxt="n", col="green", main="Tasas brutas de mortalidad de hombres en el Estado de México en 1970-2015")+
    axis(side=1,at=c(0,10,20,30,40),labels=c("1970","1980","1990","2000","2010"))
matplot(tbm_M,type="l",lwd=2, log ="y", xaxt="n", col="red", main="Tasas brutas de mortalidad de mujeres en el Estado de México en 1970-2015")+
  axis(side=1,at=c(0,10,20,30,40),labels=c("1970","1980","1990","2000","2010"))

matplot(tem_H,type="l",lwd=2, log ="y", xaxt="n", col="green", main="Tasas estandarizadas de mortalidad de hombres en el Estado de México en 1970-2015")+
  axis(side=1,at=c(0,10,20,30,40),labels=c("1970","1980","1990","2000","2010"))
matplot(tem_M,type="l",lwd=2, log ="y", xaxt="n", col="red", main="Tasas estandarizadas de mortalidad de mujeres en el Estado de México en 1970-2015")+
  axis(side=1,at=c(0,10,20,30,40),labels=c("1970","1980","1990","2000","2010"))