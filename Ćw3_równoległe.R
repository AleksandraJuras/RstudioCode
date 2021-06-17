library (foreach)
library (doSNOW)


#funkcja dla odlegolosci
getR <- function(j,i,k)
{
  #Napisaæ sami
  if(k==j)
  {
    return(i)
  }
  if(k<j)
  {
    ii = j -k
    odl = sqrt(ii^2 +i^2)
    return (odl)
  }
  
  if (k>j)
  {
    ii = (k-j)
    odl = sqrt(ii^2 +i^2)
    return (odl)
    
  }
  
}

#funkcja dla kata
getAngle <- function(j,i,k)
{
  if(k==j)
  {
    return(90.0)
    
  }
  
  if(k<j)
  {
    ii = j -k
    tg = i/ii
    rad = atan(tg)
    deg = (rad/pi)*180
    return (deg)
  }
  
  if (k>j)
  {
    ii = (k-j)
    tg = i/ii
    rad = atan(tg)
    deg = (rad/pi)*180
    return (deg)
    
  }
}


#wymiary
N=500
M=250

#gestosc
rho1 <-3500
rho2 <- 5500

#stala grawitacji
gamma <- 6.67e-11

#macierz gestiosci
rho <- matrix(nrow=M, ncol=N, rho1)

#generacja modelu
for (k in 1:3) 
{
  x <- round(100+runif(1)*(N-100))
  z <- round(50+runif(2)*(M-100))
  for (i in 1:M) 
  {
    for (j in 1:N) 
    {
      if(sqrt((i-z)^2 + (j-x)^2)<50)
        #ustawiamy dyfuzje cieplna
        rho[i,j] <- rho2
      
    }
  }
}

#model
image(t(apply(rho, 2, rev)), asp =0.5)
box()

#wektor dla wyniku
g<- rep(0,N)

c1 <- makeCluster(10)
registerDoSNOW(c1)

#pomiar czasu
start<-Sys.time()
#to sie dlugo liczy
#rho nie jest stale jak we wrzoze wiec weszlo pod sume

out <- foreach(k=seq(20,480,20)) %dopar%   # równolegy 
  {
    suma <- 0
    for(i in 1:M)
    {
      for(j in 1:N)
      {
        beta = (i+1-i)/(j+1-j)
        r = getR(j, i, k)
        r1 = getR(j, i+1, k)
        z = r1/r
        logarytm = log(z)
        a = getAngle(j, i, k)
        a1 = getAngle(j, i+1, k)
        suma <- suma + (rho[i,j]*beta*(logarytm - (a-a1))) 
      }
    }
    g <- 2*gamma*suma;
  }

#pomiar czasu
stop<-Sys.time ()
#wypisanie ró¿nicy czasu
stop-start

m<-matrix(nrow=1, ncol=1000,0)
#przypisanie do macierzy "rozwinietej listy"
m<-unlist(out)
plot(m)
stopCluster(c1)


