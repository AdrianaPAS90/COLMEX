rm(list=ls())

Dx <- read.csv("Dx.csv",header = T)
N <- read.csv("Nx.csv",header = T)
Nx<-N[-c(49)]

DxM <- Dx[Dx$Sexo=="Mujeres",-c(1,2)]
DxH <- Dx[Dx$Sexo=="Hombres",-c(1,2)]

NxM <- Nx[Nx$Sexo=="Mujeres",-c(1,2)]
NxH <- Nx[Nx$Sexo=="Hombres",-c(1,2)]

mx <- as.matrix(DxM/NxM)
mx[is.nan(mx)] <- 1
mx[is.infinite(mx)] <- 1
mx[mx==0] <- 1

n <- matrix(1,dim(mx)[1],dim(mx)[2])
edad <- matrix(c(0:(dim(mx)[1]-1)),
               dim(mx)[1],dim(mx)[2])

ax <- n/2

for(i in 1:dim(mx)[2]){
  if(mx[1,i]>=0.107){
    ax[1,i] <- 0.35
  }else{
    ax[1,i] <- 0.053 + 2.8*mx[1,i]
  }
}


ax[dim(mx)[1],] <- 1/mx[dim(mx)[1],]

qx <- n*mx/(1+(n-ax)*mx)

px <- 1-qx

lx <- matrix(100000,dim(mx)[1],
             dim(mx)[2])

for(i in 2:dim(mx)[1]){
  lx[i,] <- lx[i-1,]*px[i-1,]
}

dx <- qx*lx


ax1 <- ax
ax[2:(dim(mx)[1]-1),] <-
  ((-n[1:(dim(mx)[1]-2),]/24)*
     dx[1:(dim(mx)[1]-2),]+
     (n[2:(dim(mx)[1]-1),]/2)*
     dx[2:(dim(mx)[1]-1),]+
     (n[3:dim(mx)[1],]/24)*
     dx[3:dim(mx)[1],])/dx[2:(dim(mx)[1]-1),]

ax[is.nan(ax)] <- 0
ax[is.infinite(ax)] <- 0


Lx <- dx/mx


Tx <- matrix(0,dim(mx)[1],
             dim(mx)[2])
for(i in 1:dim(mx)[2]){
  Tx[,i] <- rev(cumsum(rev(Lx[,i])))
}

ex <- Tx/lx



# Gini

temp1 <- dx/lx[1,]
temp2 <- matrix(0,dim(mx)[1],dim(mx)[2])
for(i in 1:dim(mx)[2]){
  temp2[,i] <- (dx[,i]*(edad[,i]+ax[,i]))/Tx[1,i]
}

Fx <- matrix(0,(dim(mx)[1]+1),dim(mx)[2])
for (i in 1:dim(mx)[2]) {
  Fx[,i] <- c(0,cumsum(temp1[,i]))
}

FFx <- matrix(0,(dim(mx)[1]+1),dim(mx)[2])
for (i in 1:dim(mx)[2]) {
  FFx[,i] <- c(0,cumsum(temp2[,i]))
}


int <- matrix(0,dim(mx)[1],dim(mx)[2])
for(i in 1:dim(mx)[1]){
  int[i,] <- (Fx[i+1,]-Fx[i,])*(FFx[i+1,]-FFx[i,])
}

Gini <- 1-colSums(int)

edadmort <- edad + ax
lxsc <- lx
lxsc[lxsc == 0] <- 0.0000001

SDVx <- matrix(0, dim(mx)[1],dim(mx)[2])
for(i in 1:dim(mx)[2]){
  SDVx[,i] <- 
    sqrt(rev(cumsum(rev((edadmort[,i]^2)*dx[,i])))/lxsc[,i]-
           (edad[,i]+ex[,i])^2)
}
SDVx[is.nan(SDVx)] <- NA

CVx <- SDVx/(edad+ex)

# matplot(CVx, type="l")

### e-dagger y entropia

e_daga <- matrix(0,dim(mx)[1],dim(mx)[2])
for(i in 1:dim(mx)[2]){
  e_daga[1:(dim(mx)[1]-1),i] <- 
    (rev(cumsum(rev(dx[1:(dim(mx)[1]-1),i]*
                      (ex[2:dim(mx)[1],i]+
                         1-ax[1:(dim(mx)[1]-1),i]))))+
       0.5*ex[dim(mx)[1],i]*lx[dim(mx)[1],i])/
    lx[1:(dim(mx)[1]-1),i]
}

e_daga[dim(mx)[1],] <- 0.5*ex[dim(mx)[1],i]*
  lx[dim(mx)[1],i]/lxsc[dim(mx)[1],i]


#a)

ex.Nacimiento=ex[1,]
Años=seq(1970,2015)
plot(Años,ex.Nacimiento,type="l", main="Esperanza de vida de las mujeres en el Estado de México")

#b)
ex.Nacimiento=ex[18,]
Años=seq(1970,2015)
plot(Años,ex.Nacimiento,type="l", main="Esperanza de vida de las mujeres en el Estado de México a los 17 años")


#c)
matplot(dx,type="l", main="Edad modal a la muerte de las mujeres en el Estado de México")

#d)
matplot(qx,type="l",log="y", main="Probabilidades de Muerte para Mujeres en el Estado de México")

#e)
matplot(e_daga,type="l", main="E-daga para mujeres en el Estado de México")
matplot(Gini,type="l", main="Índice Gini para mujeres en el Estado de México")
