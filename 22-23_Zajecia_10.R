library(tidyverse)

# dziś chciałbym zająć się z Państwem zagadnieniem podstaw modelowania danych; wypracujemy kody źródłowe pozwalające obliczyć i zwizualizować problem dopasowywania modeli do danych metodą najmniejszych kwadratów

# zacznijmy od wygenerowania sobie danych treningowych; nasz kod będzie bazował na pewnym założeniu co do relacji między zmienną X i Y

#=======================================
# ZADANIE 1

# Krok 1:
# zdefiniujmy wektor X jako 300 losowych wartości z rozkładu normalnego o średniej 56 i odchyleniu std. 3

set.seed(666)
x <- rnorm(300, 56, 3)

# Krok 2:
# zdefiniujmy wektor Yfun jako wartość funkcji liniowej 0.7*X +2; narysujmy histogramy dla X, Yfun oraz wykres rozrzutu wektorów X i Yfun (na szybko możemy użyć plot())

yfun <- 0.7*x+2
hist(x)
hist(yfun)
plot(x,yfun)

# Krok 3:
# dodajmy do Y "poprawkę" (to będzie nasz "błąd") losową z rozkładu normalnego o średniej 0 i odchyleniu standardowym 1; zachowajmy nowy wektor po nazwą Ylos; powtórzmy wykresy rozrzutu ale dla par wektorów: (1) X oraz Ylos (2) X oraz Yfun + Ylos

ylos <- yfun + rnorm(300, 0, 1)
plot(x,yfun)
plot(x,ylos)

# nasze wszystkie dotychczasowe wektory zachowamy w ramce danych, dodatkowo tworząc komunę Y będącą sumą Yfun i Ylos:

wektory <- data.frame(x, yfun, ylos)
vectors <- wektory%>%
  mutate(y = rowSums(wektory[2:3]))
data.frame(vectors)

# Na tym pracujemy

set.seed(100)
X <- rnorm(300, mean = 56, sd = 3)

Yfun <- 0.7*X+2

hist(X)
hist(Yfun)

plot(X,Yfun)

Ylos <- Yfun + rnorm(300, mean = 0, sd = 1)
plot(X,Ylos)

dane <- data.frame("X" = X, 
                   "Yfun" = Yfun, 
                   "Y" = Ylos )

# ========= koniec zadania =========

# ustalmy współczynnik kierunkowy i wyraz wolny przykładowej prostej jaką dopasujemy do naszych danych i obliczmy wartości takiej funkcji dla X-ów z naszej bazy danych:

a = 1.1
b = 1

Dane <- dane %>%
  mutate(Yhat = a * X + b)

#=======================================
# ZADANIE 2

# Krok 1:
# zmodyfikować nasz wykres rozrzutu, dodając punkty o współrzędnych X-Yhat; wyróżnijmy nowe punkty kolorem czerwonym (skoro mamy juz ramkę danych, to przechodzimy na ggplot()); zmniejszmy nieco punkty i dodajmy w warstwie czerwonej nieco przezroczystości

ggplot(Dane, aes(X, Yhat)) +
  geom_point(color="red")

# Krok 2:
# dodajmy do naszego zbioru danych wektory:
#   - Rozn: różnica między zmienną Y a Yhat
#   - RoznSq: kwadrat Rozn

Dane <- Dane %>%
  mutate(Rozn=Yhat-Y, RoznSa=Rozn^2)

# Krok 3:
# narysujmy wykresy rozrzutu: (1) Rozn versus X, (2) RoznSq versus X; do wykresu (1) dodajmy niebieską, przerywaną linię poziomą na poziomie y = 0 (sprawdź w help jakie estetyki obsługuje geometria geom_hline()

ggplot(Dane, aes(X, Rozn))+
  geom_point()+
  geom_hline(aes(0, color="blue", linetype=2))

# obliczmy też średnią kwadratów błędów przybliżenia:

___

fit <- lm(formula = Y ~ X, data = dane)
summary(fit)


# ========= koniec zadania =========

# wnioski i rekomendacje:
#   - czy nasza prosta wygląda na optymalną? Czy jesteśmy w stanie przesunąć ją aby zmniejszyć średni, kwadratowy błąd?
#   - to, do czego dąży się w regresji, to dobranie "łatwo wyrażalnej" funkcji, by błędy przybliżenia były maksymalnie losowe (chcemy "trend" naszego rozkładu przerzucić na funkcję modelującą, a resztki mają być już najbardziej losowe jak się da)

#===========================
# sprawdźmy, jak wyglądałoby przybliżenie naszych punktów inną funkcją (tym razem kwadratową)

# ZADANIE 3:

# Krok 1:
# dodajemy do naszej bazy danych z zadania 2 kolumnę (Yhat2), która będzie funkcją kwadratową naszej zmiennej X postaci

Yhat2 <- X^2 + 0.2*X

# Kroki 2 i 3 wykonaj zgodnie z instrukcją w zadaniu 2

___

# ========= koniec zadania =========

# ZADANIE 4:

# na ile pamiętamy testy statystyczne i dplyr?

# 4.1:
# przetestuj różnice średnich wymiarów prawo-lewo diamentów w grupach cięcia Ideal oraz Premium:
#   - odfiltruj ze zbioru dane wymiarów prawo-lewo dla cięcie Ideal i zachowaj w nowym wektorze danych
#   - odfiltruj ze zbioru dane wymiarów prawo-lewo dla cięcie Premium i zachowaj w nowym wektorze danych
#   - dla nowych wektorów narysuj historam (hist()); wykonaj test shapiro.wilka lub narysuj wykres kwantyl-kwantyl porównujący dane do rozkładu normalnego; określ, czy rozkłady są normalne
#   - niezależnie od wpowyższego testu wykonaj test na równośc wariancji a następnie test t ze stosowną poprawką na równość/nierówność wariancji w próbach

ideal <- diamonds%>%
  filter(cut == "Ideal")%>%
  select(x)%>%
  pull()

premium <- diamonds%>%
  filter(cut == "Premium")%>%
  select(x)%>%
  pull()

hist(premium)

hist(ideal)

idealny <- data_frame(ideal)
premium1 <- data_frame(premium)

ggplot(data=idealny)+
  geom_qq(aes(sample=ideal))

ggplot(data=premium1)+
  geom_qq(aes(sample=premium))

var.test(ideal, premium)

t.test(ideal, premium, var.equal = FALSE)

# 4.2:
# chcemy sprawdzić, czy w grupach koloru diamentu D, E i H proporcja diamentów z cięciem Premium jest taka sama jak z cięciem Good:
#   - oblicz ile w zbiorze danych diamonds jest diamentów o poprawnych wymiarach, w podziale na cięcie i kolor jakie są dla nas interesujące
#   - stwórz wektor liczebności grup kolorów jakie sa dla nas interesujące (przy zawężeniu do cięcia Premium i Good!)
#   - stwórz wektor liczebności diamentów z cięciem "Premium" w grupach kolorów jakie sa dla nas interesujące (Premium traktujemy tutaj jako "sukces")
#   - stworzone wektory przekaż do stosownego testu statystycznego i przeprowadź wnioskowanie

diamentyP <- diamonds%>%
  filter(cut=="Premium")%>%
  group_by(color)%>%
  select(color)%>%
  summarise(N=n())%>%
  filter(color=="D"|color=="E"|color=="H")%>%
  pull()

diamentyG <- diamonds%>%
  filter(cut=="Good")%>%
  group_by(color)%>%
  select(color)%>%
  summarise(N=n())%>%
  filter(color=="D"|color=="E"|color=="H")%>%
  pull()

n <- c(...)
x <- c(...)

prop.test(x = x, n = n)

