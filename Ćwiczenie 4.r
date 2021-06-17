library(animation)
#parametry modelu
nz <- 500
nx <- 500
fpeak <- 30.0 #czestotliwosc dominujaca sygnalu Rickera
dt <- 0.002 #najmniejszy krok czasowy, nie wynika z warunkow stabilnosci
et <- 0.5 #czas koncowy
ds <- 1.0

#położenie źródła 
xs <- nx/2.0
zs <- 25

#ile będzie kroków czasowych dla sejsmogramu
nt <- et/dt+1

#model
V <- matrix(nrow=nz, ncol=nx, 2000)

for (i in 1:nz/2) {
  for (j in 1:nx) {
    V[i,j] <- 1000
  }
}

#macierze dla pól ciśnień w czasie t+1, t i t-1
p <- matrix(nrow=nz, ncol=nx, 0)
pm <- matrix(nrow=nz, ncol=nx, 0)
pp <- matrix(nrow=nz, ncol=nx, 0)

#macierz do zapisu probek sejsmogramu
xx <- matrix(nrow = nt, ncol = nx, 0)

#trzeba znaleźć vmax do warunku stabilności
vmax <- 2000

#dtr - realny krok próbkowania który jest dzielnikiem dt
dtr <- ds/(2.0*vmax)
w2 <- 0

while (1) {
  w2 <- w2+1
  w1 <- dt/w2
  if(w1<=dtr)
  {
    dtr <- w1
    break
  }
}

#inicjalizacja paska postępu
niter=et/dtr+1
prog_bar <- txtProgressBar(min=0, max=niter, style=3)


x <- 1  #iterator do zapisu probek sejsmogramu

kk <- 1 #ilosc dtr n ajedna dt
kkk <- 0 #ilosc dt
k <- 1 #ilosc dtr


saveGIF({
while(1)
{
  k <- k+1
  kk <- kk+1
  t <- k*dtr


#pasek postepu
setTxtProgressBar(prog_bar, k)

#główna pętla do modelowania
for (i in 2:(nz-1)) 
{
 for (j in 2:(nx-1)) 
  {
   pp[i,j]= 2.0*p[i,j]-pm[i,j] + ((dtr*dtr)/(ds*ds))*V[i,j]*V[i,j]*(p[i+1,j]+p[i-1,j]+p[i,j+1]+p[i,j-1]-4.0*p[i,j])
  } 
}

#dodanie źródła Rickera
pp[zs,xs] <- pp[zs,xs]+exp(-(((pi*fpeak*(t-(1.0/fpeak)))*(pi*fpeak*(t-(1.0/fpeak))))))*(1.0-2.0*((pi*fpeak*(t-(1.0/fpeak)))*(pi*fpeak*(t-(1.0/fpeak)))))

#transparent lewa - warunek brzegowy dla lewej krawędzi modelu
for (i in 1:nz) 
{
  pp[i,1]=p[i,1]+p[i,2]-pm[i,2]+V[i,1]*(dtr/ds)*(p[i,2]-p[i,1]-(pm[i,3]-pm[i,2]))  
}

#warunek brzegowy dla prawej strony
for (i in 1:nz) 
{
  pp[i,500]=p[i,500]+p[i,499]-pm[i,499]+V[i,500]*(dtr/ds)*(p[i,499]-p[i,500]-(pm[i,498]-pm[i,499]))  
}

#warunek brzegowy dla góry
for (i in 1:nx) 
{
  pp[1,i]=p[1,i]+p[2,i]-pm[2,i]+V[1,i]*(dtr/ds)*(p[2,i]-p[1,i]-(pm[3,i]-pm[2,i]))  
}

#warunek brzegowy dla dołu
for (i in 1:nx) 
{
  pp[500,i]=p[500,i]+p[499,i]-pm[499,i]+V[500,i]*(dtr/ds)*(p[499,i]-p[500,i]-(pm[498,i]-pm[499,i]))  
}

#przejscie o krok do przodu z macirzami
pm <- p
p <- pp

#warunek do zapisania próbki sejsmogramu (taki sam jak dla animacji)
if(kk*dtr + dtr/10.0 >= dt)
{
  kk <- 0
  kkk <- kkk+1
  
  #dodanie probek do sejsmogramu
  for(i in 1:nx)
  {
    xx[x, i] <- pp[1, i]
  }
  x <- x + 1
  
  Limg <- (apply(xx, 2, rev))
  image(t(Limg))
  text(0.2, 0.9, k)
  
  #przerwanie po czasie
  if(kkk*dt > et) break
  
  
  
}
}#koniec petli while
}, interval =0.2, movie.name = "lab5.gif") #SAVE GIF

image(t(apply(p, 2, rev)))
text(0.2, 0.9, kkk*dt)






