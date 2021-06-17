library(animation)
library(plotrix)
#pomiar czasu wykonania zadania
start<-Sys.time()
#wymiar
N=200
#krok
h<-5
#inicjalizacja macierzy dle kroku nowego i przesz³ego
Lnew<-matrix(nrow=N, ncol=N, 0)
L<-matrix (nrow=N, ncol=N, 0)

#macierz z dyfuzja
a <- matrix(nrow = N, ncol = N, 1.15)

#trzy losowo zlokalizowane ciala o srednicy 100metr
for (k in 1:3) 
{
  x[k] <- round(runif(1)*N)
  y[k] <- round(runif(2)*N)
  for (i in 1:N) 
  {
    for (j in 1:N) 
    {
      if(sqrt((i-y[k])^2 + (j-x[k])^2)<10)
        #ustawiamy dyfuzje cieplna
        a[i,j] <- 4
      
    }
  }
}

image(t(apply(a, 2, rev)))

#max a do stabilnoœci
a_max<-max(a)
a_max

#krok czasowy
dt<-h^2/(4*a_max)
dt

#czas symulaci
t<-0

#warunki brzegowe
L[,1]<-rep(0,N)
L[,N]<-rep(0,N)
L[1,]<-rep(0,N)
L[N,]<-rep(50,N)
#chcemy miec warunki brzegowe te¿ w nowym kroku
Lnew <- L
#wypisanie stanu pola w kroku 0
L
#ile bedzie iteracji wstepnie DUZO ZA MALO
niter<-2000

#inicjalizacja paska postepu
prog_bar <- txtProgressBar(min=0,max=niter, style = 3)

#blok do zapiu w animacji gif
saveGIF({
  #pasek postepu nie umie w iterator petli for :(
  stepi <-(-1)
  #petla po iteracjach k
  for(k in 1:niter)
  {
    t<-t+dt
    #pasek postepu i jego osobista zmianna
    stepi <- stepi+1
    setTxtProgressBar(prog_bar, stepi)
    
    #petla po wierszach (i) i kolumnach (i)
    for (i in 2: (N-1))
      for (j in 2: (N-1)) {
        Lnew[i, j]<-(1-(4*dt*a[i,j])/(h^2))*L[i,j]+dt*a[i,j]*
          ((L[i-1,j]+L[i+1,j]+L[i,j-1]+L[i, j+1])/(h^2))
      }
    #naiwny gradient 0 ale dzia³a :)
    Lnew[,1]<-L[,2]
    Lnew[,N]<-L[,N-1]
    #przejœcie o krok do przodu w iteracji
    #auxl to zachowana macierz do przetestowania czy pole jest stab
    auxL<-L
    L<-Lnew
    
    if(k%%100==0)
    {
      #image po rotacji. apply zadaje funkcje (tu rev) do kolejnych
      #kolumn (2) lub wierszy (1) dla zadanej macierzy (1)
      #rev odwraca kolejnoœæ
      Limg <- apply(L, 2, rev)
      image (t(Limg))
      #dodanie w lewym górnym rogu czasu w dniach!
      text(0.2, 0.9, round(t/(3600*24)))
      
      #lines zarys modelu (wy macie trudniej)
      for (m in 1:3) 
      {
        draw.circle(x[m]*0.008, y[m]*0.008, radius = 0.05, border = "black", lwd =1)
      }
      #ladna czarna ramka
      box()
    } #if
    
  } #po k
  
}) #SaveGif

#koniec pomiaru czasu
stop <- Sys.time()

#wypisanie czasu 
stop-start
#wypisanie wyniku 
L
image(L)

