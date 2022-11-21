#==================
# tworzenie wykresów w ggplot

library(tidyverse)

# przyjrzyjmy się zbiorowi danych "diamonds"

str(diamonds)

# ponieważ nasz zbiór danych zwiera bardzo dużo obserwacji, wylosujemy mniejszy podzbiór: np. 5000 wierszy

ile_wierszy_mamy <- dim(diamonds)[1]

set.seed(400)

wiersze_do_wybrania <- sample(x = 1:ile_wierszy_mamy, size = 5000)

diam_samp <- diamonds[wiersze_do_wybrania,]

# stworzymy prosty wykres związku wymiaru po osi x diamentu oraz jego ceny

ggplot(data = diam_samp, mapping = aes(x = x, y = price)) +
  geom_point()

# stajemy przed dwoma wyzwaniami: punkty na siebie zachodzą oraz chcielibyśmy zobaczyć profil modelujący dane
# pierwsze rozwiążemy dodając do geometrii punktowej parametr "alpha" z wartością np. 0.3; drugi problem rozwiążemy dodagąc nową geometrię "smooth"

# ZADANIE 1.1
# dodaj do naszego wykresu parametr alpha z wartością 0.3 oraz geometrię wygładzania "smooth"; dodatkowo zmień parametr "shape": warto sprawdzić jakie są dostępne (Google -> "r ggplot shapes")

ggplot(data = diam_samp, mapping = aes(x = x, y = price)) +
  geom_point(shape=3, alpha = 0.3) +
  geom_smooth()

# ZADANIE 1.2
# dodaj do naszego wykresu parametr alpha z wartością 0.3 oraz geometrię wygładzania "smooth"; dodatkowo zmień parametr "size": wypróbujcie wartości zarówno większe jak i mniejsze niż jeden

ggplot(data = diam_samp, mapping = aes(x = x, y = price)) +
  geom_point(shape=3, size=0.2, alpha = 0.3) +
  geom_smooth()

ggplot(data = diam_samp, mapping = aes(x = x, y = price)) +
  geom_point(shape=3, size=2.5, alpha = 0.3) +
  geom_smooth()

# ZADANIE 1.3
# dodaj do naszego wykresu parametr alpha z wartością 0.3 oraz geometrię wygładzania "smooth"; dodatkowo zmień parametr "color": warto sprawdzić jakie są dostępne (Google -> "r colors")

ggplot(data = diam_samp, mapping = aes(x = x, y = price, color="red")) +
  geom_point(shape=3, size=0.2, alpha = 0.3) +
  geom_smooth()

#=============
# parametry color, size i alpha nie muszą być stałe: mogą stać się estetykami, którym przyporządkujemy zmienne
# punkty naszego wykresu rozrzutu pokolorujmy zgodnie z kategoriami zadanymi zmienną "color" w zbiorze diamonds
# dodatkowo pozbądźmy się wstążki wkoło profilu przybliżonego (opcja "se" w geom_smooth())

ggplot(data = diam_samp, mapping = aes(x = x, y = price, color=I("red"))) +
  geom_point(shape=3, size=0.2, alpha = 0.3) +
  geom_smooth(aes(x,carat))

# ZADANIE 2 - dla wszystkich
# zmodyfikujmy nasz wykres tak, aby:
#   - estetyka "size" była zadana zmienną "y"
#   - estetyka "shape" była zadana zmienną "color"
#   - estetyka "color" była zadana zmienną "clarity"
# wszystkie powyższe estetyki przypiszmy w elemencie bazowym wykresu (ggplot())
# nie zapominajmy o geometrii smooth!
# przygotujcie się do dyskusji na temat tego, czy ten wykres jest czytelny i jasny w interpretacji

ggplot(data = diam_samp, mapping = aes(x = x, y = price, size=y, shape=color, color=clarity)) +
  geom_point() +
  geom_smooth()

#LEPSZE

ggplot(data = diam_samp, mapping = aes(x = x, y = price, color=clarity)) +
  geom_point(aes(size=y, shape=color), alpha=0.2) +
  geom_smooth()

#=============
# wyprobujmy geometrię "boxplot" - zerknijmy na ściągawkę...

# ZADANIE 3.1
# przygotujcie wykres bazujący na danych diam_samp; chcemy zobaczyć wykres pudełkowy dla zmiennej price w podziale zadanym zmienną "clarity"; zadbajcie o to, aby pudełka były wypełnione kolorem czerwonym

ggplot(data=diam_samp, mapping=aes(x=clarity,y=price))+
  geom_boxplot(fill="red")

# ZADANIE 3.2
# przygotujcie wykres bazujący na danych diam_samp; chcemy zobaczyć wykres pudełkowy dla zmiennej price w podziale zadanym zmienną "color"; zadbajcie o to, aby pudełka "leżały na boku" - były w orientacji poziomej

ggplot(data=diam_samp, mapping=aes(x=color,y=price))+
  geom_boxplot()+
  coord_flip()

# ZADANIE 3.3
# przygotujcie wykres bazujący na danych diam_samp; chcemy zobaczyć wykres pudełkowy dla zmiennej price w podziale zadanym zmienną "cut"; prócz geometrii boxplot dodajcie geometrię wykresu rozrzutu; czy wykres jest czytelny?

ggplot(data=diam_samp, mapping=aes(x=cut,y=price))+
  geom_boxplot()+
  geom_jitter(shape=8, aes(color=clarity))

#=============
# wypróbujmy geomterię wykresu kwantyl-kwantyl
# zerknijmy na histogramy rozkładów zmiennych x i y:

ggplot(diam_samp, aes(x = x)) +
  geom_histogram()

ggplot(diam_samp, aes(x = y)) +
  geom_histogram()

# korzystając z własności dziedziczenia estetyk możemy narysować dwa histogramy na jednym obrazku:

ggplot(diam_samp, aes(x = x)) +
  geom_histogram() +
  geom_histogram(aes(x = y))

# nic nam to jednak nie daje! jakie jest potencjalne rozwiązanie?

# ZADANIE4 - dla wszystkich
# znaleźć nazwę geometrii wykresu kwanty-kwantyl w ggplot()
# narysować wykres kwanty-kwantyl dla zmiennych x i y zbioru diam_samp (uwaga! w ggplot wykres kwantyl-kwantyl domyślnie porównuje zadany rozkład z rozkładem normalnym; zatem tworzymy dwa wykresy - osobno dla zmiennej x i osobno dla zmiennej y; zwracamy też uwagę na nazwę estetyki, która przechowuje przekazywany rozkład empiryczny, który ma być porównany z rozkłądem normalnym)
# zamienić kolor punktów wykresu na niebieski
# zamienić rozmiar punktów wykresu na 5
# zamienić przezroczystość punktów wykresu na 0.8
# *zamienić etykiety osi x ("rozmiar diamentu w kierunku x") oraz osi y ("rozmiar diamentu w kierunku y")

ggplot(diam_samp, aes(sample=x, color=I("blue"), size=I(5), alpha=I(0.8)))+
  geom_qq()+
  geom_qq(aes(sample=y))

ggplot(diam_samp, aes(sample=x, color=I("blue"), size=I(5), alpha=I(0.8)))+
  geom_qq()+
  geom_qq(aes(sample=y))+
  labs(x="rozmiar diamentu w kierunku x", y="rozmiar diamentu w kierunku y")

