#CWICZENIE 1
library(animation)
library(rmutil)

N = 100
#inicjalizacja macierzy dla kroku nowego i przeszlego
Lnew <- matrix(nrow=N, ncol=N,0)
L <- matrix(nrow = N, ncol = N, 0)

#warunki brzegowe
L[,1] <= rep(0,N)
L[,N] <= rep(0,N)
L[1,] <= rep(0,N)
L[N,] <= rep(0,N)


#chcemy mieæ warunki brzegowe te¿ w nowym kroku
Lnew<-L
#wypisanie stanu pola w kroku 0
L

#pozycja punktu w obszarze
pos <- round(runif(2)*(N-2)+1)

#blok do zapisu w animacji GIF o domyœlnych: interwale 1s i nazwie animation.gif
saveGIF({
  #pêtla po iteracjach (k)
  for(k in 1:1000){
  
    #nowa pozycja punktu zz gwarancja bycia w obszarze
    pom <- pos+round(runif(2)*2-1)
    while (pom[1]<=0 || pom[1]>=N || pom[2]<=0 || pom[2]>=N)
    {
      pom <- pos+round(runif(2)*2-1)
    }
    pos <- pom
    pos
    
    #przypisanie punktu
    L[pos[1], pos[2]] <- 1
    
    #(2*rbinom(2,1,0.5)-1) zwraca 1 lub -1
    #rlevy dystrybucje Levy'ego (rbinom bo musimy zadbaæ o znak)
    pom <- pos + round((2*rbinom(2,1,0.5)-1)*rlevy(2,m=0,s=1))
    pom
    
    #animacja co 10 iteracji
    if(k%% 10 == 0){
    #image po rotacji. apply zadaje funkcjê (tu rev) do kolejnych
    #kolumn (2) lub wierszy (1) dla zadanej macierzy (L)
    #rev odwraca kolejnoœæ
    Limg<- apply(L, 2, rev)
    image(t(Limg))
    #dodanie w lewym górnym rogu numeru iteracji
    text(0.05,0.95,k)}}
  #po k
  }) #SaveGIF

