#zaladowanie biblioteki
library(animation)
#inicjalizacja macierzy dla kroku nowego i przeszlego
Lnew<- matrix(nrow=10,ncol=10,0)
L <- matrix(nrow=10,ncol=10,0)
#warunki brzegowe
L[1,] = rep(0,10)
L[,1] = rep(0,10)
L[,10] = rep(0,10)
L[10,] = rep(1,10)
#chcemy miec warunki brzegowe te¿ w nowym kroku
Lnew <- L
#wypisanie stanu pola w kroku 0
L
k=1
#zmienna epsilon do momentu, gdy ró¿nica w polu macierzy zmieni siê w danej
#iteracji o mniejsz¹ wartoœæ ni¿ zadany epsilon wychodzimy z pêtli. Mo¿emy 
#zaobserwowaæ, w której iteracji gif siê nie zmienia
eps = 0.0001
isEnd = FALSE
#blok do zapisu w animacji GIF o domyslnych: interwale 1s i nazwie animation.gif
saveGIF({
  #petla po iteracjach (k)
  while(!isEnd)
  {
    #petla po wierszach (i) i kolumnach (j)
    for (i in 2:9){
      for (j in 2:9){
        Lnew[i,j]<-0.25*(L[i-1,j]+L[i+1,j]+L[i,j-1]+L[i,j+1])
        if(Lnew[i,j] - L[i,j] <= eps){
          isEnd = TRUE
        }else{
          isEnd = FALSE
        }
      }
    }
    #przejecie o krok do przodu w iteracji
    L<-Lnew
    print(k)
    print(L)
    if(k %% 10 == 0 || k == 1){
      #image po rotacji. apply zadaje funkcji (tu rev) do kolejnych
      #kolumn (2) lub wierszy (1) dla zadanej macierzy (L)
      #rev odwraca kolejno
      Limg <- apply(L, 2, rev)
      image(t(Limg))
      #dodanie w lewym gornym rogu numeru iteracji
      text(0,1,k)
    }
    k=k+1
  } #po k
}) #SaveGIF


