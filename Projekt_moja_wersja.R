#inf o zasiêgu lodu morskiego w oko³o Antarktydy dla wszystkich d³ugoœci geograficznych
ice_edge<-read.csv("D:/Documents/Aleksandra_Juras/Studia/Modelowanie w naukach o ziemi/Projekt/daily_ice_edge.csv",header=TRUE)

open_water<-read.csv("D:/Documents/Aleksandra_Juras/Studia/Modelowanie w naukach o ziemi/Projekt/daily_open_water.csv",header=TRUE)


library(plotrix)
#Wyswietla kontur antarktydy w roku 1978
ice_edge_1978 <- ice_edge[1,-1]
polar.plot(ice_edge_1978,grid.col="gray",clockwise = TRUE, start = 90, main ="zasieg lodu w 1978 rou",
           rp.type = "polygon", radial.lim = c(-90,-50),line.col=4)

#Wyswietla kontur przedstawiaj¹cy minimalny zasiêg lodu
#analizowanym okresie dla wszystkich k¹tów
#install.packages("matrixStats")
library(matrixStats)

ice_matrix <- data.matrix(ice_edge, rownames.force = NA)
ice_min <- colMins(ice_matrix)
ice_min <- data.frame(ice_min)
ice_min <- ice_min[-1,]
polar.plot(ice_min, grid.col="gray",clockwise = TRUE, start = 90, main ="minimalny zasiêg lodu dla wszystkich k¹tów",
           rp.type = "polygon",radial.lim = c(-90,-50),line.col=4)


############################### model matematyczny###############################


library(dplyr)
library(readr)
# wczytujemy dane

ice_edge_poprawione2 <- read_csv("D:/Documents/Aleksandra_Juras/Studia/Modelowanie w naukach o ziemi/Projekt/daily_ice_edge_poprawione2.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y")))
ice_edge_poprawione1 <- read_csv("D:/Documents/Aleksandra_Juras/Studia/Modelowanie w naukach o ziemi/Projekt/daily_ice_edge_poprawione1.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y")))
ice_edge_poprawione <- read_csv("D:/Documents/Aleksandra_Juras/Studia/Modelowanie w naukach o ziemi/Projekt/daily_ice_edge_poprawione.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y")))

#macierz wynikowa z wartoœciami wymodelowanymi
wynik <- ice_edge_poprawione



for (k in 2:362){
  
  # dla pomiarów co 2 dni
  t <- 1:1590
  y = ice_edge_poprawione2[, k, drop=TRUE] #zamienia na ramke danych na wektor

  
  ssp <- spectrum(y)  #Funkcja widma szacuje gêstoœæ widmow¹ szeregu czasowego.
  per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]    #bierzemy najwy¿sz¹ wartoœæ i odczytujemy x>>(war odwrotna)
  reslm <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t))
  summary(reslm)
  
  rg <- diff(range(y))   #przedzia³ miêdzy max i min wektora y
  plot(y~t,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg))
  lines(fitted(reslm)~t,col=4,lty=2)   # dashed blue line is sin fit
  
  # including 2nd harmonic really improves the fit
  reslm2 <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
  summary(reslm2)
  lines(fitted(reslm2)~t,col=3)    # solid green line is periodic with second harmonic
  
  wynik1 <- data.frame(reslm2$fitted.values)
  
  
  # dla pomiarów co 1 dzieñ
  
  t <- 1591:9530
  y <- ice_edge_poprawione1[, k, drop=TRUE]
  
  ssp <- spectrum(y)  
  per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
  reslm <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t))
  summary(reslm)
  
  rg <- diff(range(y))
  plot(y~t,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg))
  lines(fitted(reslm)~t,col=4,lty=2)   # dashed blue line is sin fit
  
  # including 2nd harmonic really improves the fit
  reslm2 <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
  summary(reslm2)
  lines(fitted(reslm2)~t,col=3)    # solid green line is periodic with second harmonic
  
  wynik2 <- data.frame(reslm2$fitted.values)
  
  wynik_kolumna <- rbind(wynik1, wynik2)
  
  wynik[,k] <- wynik_kolumna
  
}




############nr.3 wizualizajca pomiarów dla ka¿dego roku z osobna###############

#usuwamy kolumnê z dat¹
ice_edge_new <- ice_edge[,-1]
wynik_new <- wynik[,-1]

n <- as.matrix(wynik_new)
nn <- as.matrix(ice_edge_new)

####1979
im <- as.matrix(ice_edge[35:216, 2:361])
Lnew <- im
L <- im

for(i in 1:181){
  for(i in 2:(181-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}


dni <- 2*(1:183) # co dwa dni 1:366 co dwa
azymuty <- (1:ncol(L)) #361 azymutów 361 dlatego bo pierwsz y to zero:)
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1979", font.main = 4)


dni <- 2*(1:183) # co dwa dni 1:366 co dwa
azymuty <- (1:ncol(n)) #361 azymutów 361 dlatego bo pierwsz y to zero:)
image(dni,azymuty,n[35:216, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1979", font.main = 4)

dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[35:216, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1979", font.main = 4)
####1980
im <- as.matrix(ice_edge[217:399, 2:361])
Lnew <- im
L <- im

for(i in 1:182){
  for(i in 2:(182-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- 2*(1:183)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1980", font.main = 4)

dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[217:399, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1980", font.main = 4)

dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[217:399, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1980", font.main = 4)

####1981
im <- as.matrix(ice_edge[400:582, 2:361])
Lnew <- im
L <- im

for(i in 1:182){
  for(i in 2:(182-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- 2*(1:183)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1981", font.main = 4)

n <- as.matrix(wynik_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[400:582, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1981", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[400:582, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1981", font.main = 4)

####1982
im <- as.matrix(ice_edge[583:764, 2:361])
Lnew <- im
L <- im

for(i in 1:181){
  for(i in 2:(181-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- 2*(1:182)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1982", font.main = 4)

n <- as.matrix(wynik_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[583:764, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1982", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[583:764, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1982", font.main = 4)

####1983
im <- as.matrix(ice_edge[765:947, 2:361])
Lnew <- im
L <- im

for(i in 1:182){
  for(i in 2:(182-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- 2*(1:183)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1983", font.main = 4)

n <- as.matrix(wynik_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[765:947, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1983", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[765:947, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1983", font.main = 4)

###1984
im <- as.matrix(ice_edge[948:1130, 2:361])
Lnew <- im
L <- im

for(i in 1:182){
  for(i in 2:(182-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- 2*(1:183)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1984", font.main = 4)


n <- as.matrix(wynik_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[948:1130, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1984", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[948:1130, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1984", font.main = 4)
###1985
im <- as.matrix(ice_edge[1131:1312, 2:361])
Lnew <- im
L <- im

for(i in 1:181){
  for(i in 2:(181-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- 2*(1:182)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1985", font.main = 4)

n <- as.matrix(wynik_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[1131:1312, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1985", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[1131:1312, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1985", font.main = 4)

###1986
im <- as.matrix(ice_edge[1313:1495, 2:361])
Lnew <- im
L <- im

for(i in 1:182){
  for(i in 2:(182-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- 2*(1:183)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1986", font.main = 4)

n <- as.matrix(wynik_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[1313:1495, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1986", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- 2*(1:183)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[1313:1495, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1986", font.main = 4)

#1987 - tutaj siê zmienia czêstotliwoœæ!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
n <- as.matrix(wynik_new)
dni <- 1:95
azymuty <- (1:ncol(n))
image(dni,azymuty,n[1496:1590, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1987 - cz.1", font.main = 4)

dni <- 95:242
azymuty <- (1:ncol(n))
image(dni,azymuty,n[1590:1736, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1987 - cz.2", font.main = 4)

dni <- 1:95
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[1496:1590, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1987 - cz.1", font.main = 4)

dni <- 95:242
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[1590:1736, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1987 - cz.2", font.main = 4)

#1988 - brakuje kilkunastu dni w styczniu :o
n <- as.matrix(wynik_new)
dni <- (13:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[1737:2089, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1988", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- 13:366
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[1737:2089, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1988", font.main = 4)
####1989
im <- as.matrix(ice_edge[2090:2454, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1989", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[2090:2454, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1989", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[2090:2454, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1989", font.main = 4)

###1990
im <- as.matrix(ice_edge[2455:2819, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1990", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[2455:2819, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1990", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[2455:2819, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1990", font.main = 4)
###1991
im <- as.matrix(ice_edge[2820:3184, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1991", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[2820:3184, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1991", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[2820:3184, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1991", font.main = 4)
###1992
im <- as.matrix(ice_edge[3185:3550, 2:361])
Lnew <- im
L <- im

for(i in 1:365){
  for(i in 2:(365-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:366)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1992", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[3185:3550, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1992", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[3185:3550, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1992", font.main = 4)
###1993
im <- as.matrix(ice_edge[3551:3915, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1993", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[3551:3915, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1993", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[3551:3915, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1993", font.main = 4)

###1994
im <- as.matrix(ice_edge[3916:4280, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1994", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[3916:4280, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1994", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[3916:4280, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1994", font.main = 4)
###1995
im <- as.matrix(ice_edge[4281:4645, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1995", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[4281:4645, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1995", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[4281:4645, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1995", font.main = 4)
###1996
im <- as.matrix(ice_edge[4646:5011, 2:361])
Lnew <- im
L <- im

for(i in 1:365){
  for(i in 2:(365-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:366)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1996", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[4646:5011, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1996", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[4646:5011, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1996", font.main = 4)
###1997
im <- as.matrix(ice_edge[5012:5376, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1997", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[5012:5376, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1997", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[5012:5376, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1997", font.main = 4)

###1998
im <- as.matrix(ice_edge[5377:5741, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1998", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[5377:5741, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1998", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[5377:5741, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1998", font.main = 4)

###1999
im <- as.matrix(ice_edge[5742:6106, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 1999", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[5742:6106, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 1999", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[5742:6106, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 1999", font.main = 4)
###2000
im <- as.matrix(ice_edge[6107:6472, 2:361])
Lnew <- im
L <- im

for(i in 1:365){
  for(i in 2:(365-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:366)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 2000", font.main = 4)


n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[6107:6472, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 2000", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[6107:6472, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 2000", font.main = 4)

###2001
im <- as.matrix(ice_edge[6473:6837, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 2001", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[6473:6837, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 2001", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[6473:6837, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 2001", font.main = 4)

###2002
im <- as.matrix(ice_edge[6838:7202, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 2002", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[6838:7202, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 2002", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[6838:7202, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 2002", font.main = 4)

###2003
im <- as.matrix(ice_edge[7203:7567, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 2003", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[7203:7567, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 2003", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[7203:7567, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 2003", font.main = 4)

###2004
im <- as.matrix(ice_edge[7568:7933, 2:361])
Lnew <- im
L <- im

for(i in 1:365){
  for(i in 2:(365-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:366)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 2004", font.main = 4)


n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[7568:7933, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 2004", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[7568:7933, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 2004", font.main = 4)

####2005
im <- as.matrix(ice_edge[7934:8298, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 2005", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[7934:8298, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 2005", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[7934:8298, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 2005", font.main = 4)

###2006
im <- as.matrix(ice_edge[8299:8663, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:365)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 2006", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[8299:8663, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 2006", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[8299:8663, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 2006", font.main = 4)

###2007
im <- as.matrix(ice_edge[8664:9028, 2:361])
Lnew <- im
L <- im

for(i in 1:364){
  for(i in 2:(364-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:366)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 2007", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[8664:9028, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 2007", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[8664:9028, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 2007", font.main = 4)

###2008
im <- as.matrix(ice_edge[9029:9394, 2:361])
Lnew <- im
L <- im

for(i in 1:365){
  for(i in 2:(365-1))
    for(j in 2:(360-1)) {
      Lnew[i, j] <- 0.25*(L[i-1,j]+L[i+1, j]+L[i,j-1]+L[i, j+1]) 
    }
  L <- Lnew 
}

dni <- (1:366)
azymuty <- (1:ncol(L))
image(dni,azymuty,L, col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki po transformacji Laplace'a - 2008", font.main = 4)

n <- as.matrix(wynik_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,n[9029:9394, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki wymodelowane - 2008", font.main = 4)

nn <- as.matrix(ice_edge_new)
dni <- (1:366)
azymuty <- (1:ncol(n))
image(dni,azymuty,nn[9029:9394, 1:361], col = rev(heat.colors(10)), axes = FALSE)
axis(1, at = seq(0, 365, by = 40))
axis(2, at = seq(0, 360, by = 40))
box()
title(main = "wyniki rzeczywiste - 2008", font.main = 4)
#########################################################################3


##########przyk³adowy model###############################################

polar.plot(wynik_new[1,],grid.col="gray",clockwise = TRUE, start = 90, 
           rp.type = "polygon", radial.lim = c(-90,-50),line.col="red") 


polar.plot(ice_edge_new[1,],grid.col="gray",clockwise = TRUE, start = 90, 
           rp.type = "polygon", radial.lim = c(-90,-50),line.col="blue", add=TRUE)

text(40,40,ice_edge[1,1])
legend(20, -40, legend=c("model matematyczny", "wartoœci rzeczywiste"),
       col=c("red", "blue"), lty=1:1, cex=0.8)


#animacja przedstawiaj¹ca zmianê w czasie rzeczywistego zasiêgu lodu morskiego 


#instlacja pakietu
#install.package("animation")
#za³adowanie pakietu
library(animation)
k=0
#blok do zapisu w animacji GIF o domyœlnych: interwale 1s i nazwie animation.gif
saveGIF({
  #pêtla po iteracjach (k)
  for (k in 1:30:3000)
  {
    
    polar.plot(wynik_new[k,],grid.col="gray",clockwise = TRUE, start = 90, 
               rp.type = "polygon", radial.lim = c(-90,-50),line.col="red") 
    
    
    polar.plot(ice_edge_new[k,],grid.col="gray",clockwise = TRUE, start = 90, 
               rp.type = "polygon", radial.lim = c(-90,-50),line.col="blue", add=TRUE)
    
    text(40,40,ice_edge[k,1])
    legend(20, -40, legend=c("model matematyczny", "wartoœci rzeczywiste"),
           col=c("red", "blue"), lty=1:1, cex=0.8)
    
  } #po k
}, movie.name = 'Antarktyda.gif' ,interval = 0.08) #SaveGIF

