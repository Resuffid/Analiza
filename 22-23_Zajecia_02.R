###================
### WYKONYWANIE KODU

# wszystko co zaczyna sie od '#' to komentarz i nie da się tego odwołać; dodatkowo warto zwrócić uwagę na sposób łamiania linii przez RStudio

# wykonajmy kilka poleceń, wysyłając je na konsolę
# pojedynczo
2 + 2

# w bloku
2 * 7
8 + 90

# możemy też wybierać fragment linii do wysłania na konsolę
3 * 67 - 52

###------------------
### ćwiczenia

# wykonaj pojedynczo poniższe polecenia a potem wykonaj je "jednym ruchem"

25 * 65 + 98
1 + 2 + 3 + 4 * 
  5
((7 + 8) * 6 - 12) / 2 # komentarz  7*67
(504 - 87 + sqrt(16)) / (64 + 32 - 87 * abs(5 - 9^2))

# wykonuj poniższe polecenia "partiami", czyli wybieramy fragmenty, aby sprawdzić wartości cząstkowych kodów

25 * 65 + 98
1 + 2 + 3 + 4 * 5

# Wykonaj kawałkami kody, aby otrzymać ciąg działań prowadzący do finalnego wyniku, zgodnie z kolejnością wykonywania działań

((7 + 8) * 6 - 12) / 2

# oblicz najpierw sam mianownik i licznik, nie przepisując formuł

(504 - 87 + sqrt(16)) / (64 + 32 - 87 * abs(5 - 9^2))

###================
### PRZYPISYWANIE NAZW

# każdy obiekt w R można nazwać; niekiedy powinniśmy to robić, niekiedy nie

# plusy dodatnie:
# - mamy porządek w obiektach
# - możemy śledzić przepływ danych/obiektów a przez to łatwiej wyłapać błąd
# - uczymy się myślenia "obiektowego"

# plusy ujemne:
# - zbyt wiele nazw prowadzi do chaosu
# - czasem przy zbyt wielu obiektach polecenia stają się nieporęczne (w przypadku pewnych konstrukcji języka wręcz celowo opuszcza się etap nazwywania (dplyr))

# klasyczne przypisanie nazwy do obiektu
#==# c(), print()

wektor <- c(1,2,3)
print(wektor)
wektor

# utwórz wektor symulujący losowe 10 rzutów kostką

kostka <- c(2,4,6,2,5,1,2,1,4,6,4,3,5,5,1,2,2,2)

# utwórz wektor naprzemiennych 10 wartości 1 i 0

moneta <- c(1,0,1,0,1,0,1,0,1,0)

# wartości możemy też przypisywać znakiem =

# wykonać wszystko co poprzednio, tylko przy pomocy "=" i dodając w nazwach przyrostek "2"
# zwracamy uwagę na "puchnącą" listę obiektów w zakładce Environment po prawej na górze
# co więcej, trzeba mieć świadomość, że obiekty są nadpisywane:

wektor2 = c(1,2,3)

kostka2 = c(1,4,3,5,5,6,1,2,2,6,5,6,5,4,3,3,3)

moneta2 = c(1,0,1,0,1,0,1,0,1,0)

wektor <- c(1,2,3,4,5)
print(wektor)

# jednym z ważniejszych rodzajów przypisań jest ten w funkcjach; tutaj można użyć również obu przypisań, ale...

# przypatrzmy się funkcji "mean" (-> help)
#==# mean()
# istnieje bardzo subtelna różnica miedzy opratorem "=" i "<-"; czy umiesz rozpoznac roznice w zachowaniu R po uruchomieniu obu tych linii kodu?

mean(x = wektor)
mean(x <- wektor)

# obliczmy średnią dla naszego wektora rzutu kostką

mean(x = kostka)

# przypiszmy nazwy wynikom obu średnich: srednia1 i srednia2 odpowiednio dla pierwszego i drugiego wektora sumulujacego rzut kostka:

srednia1 <- mean(x = kostka)
srednia2 <- mean(x = kostka2)
srednia1
srednia2
# obliczmy średnią z owych średnich i nadajmy jej nazwę srednia_srednich

srednia_srednich <- mean(x = c(srednia1, srednia2))
srednia_srednich

# obliczmy średnią zbioru liczbowego składającego się z wartości obiektu obu symulacji rzutu kostką i nadajmy jej nazwe srednia_calosci; wypiszmy na ekran obie obliczone średnie

srednia_calosci <- mean(x = c(kostka, kostka2))
srednia_calosci

# dokładnie to samo dla odchylenia standardowego... jak wyjasnic roznice w wynikach?
#==# sd()

sd(x = srednia_srednich)
sd(x = srednia_calosci)

###================
### INDEKSOWANIE WEKTORÓW

# aby móc poruszać się dalej, musimy nabyć umiejętności wybierana elementów z wektora
# Wektor to "uporządkowana kolekcja elementów tego samego typu"; przy tej okazji załatwmy też podstawowe typy danych... :)

# wektor liczbowy
#==# typeof()

typeof(c(1,2,3))
typeof(c(1.1,2,3))

# wektor znakowy

typeof(c("a", "b"))
typeof(c(a, b))

# wektor logiczny

typeof(c(TRUE, FALSE, TRUE))
typeof(c(TRUE, FALSE, T))
typeof(c(T, F, T))
typeof(c("T", "F", "T"))
typeof(c("T", F, T))

typeof(c(T, "F", T))


typeof(c(F, "michal", F, T))

x <- c(F, "T", F, T)

# między typami można przechodzić
#==# as.numeric(), as.character(), as.logical()

# dla wektorów
x1 <- c(30,40,60)
x2 <- c(T, T, F, T)
x3 <- c("0", "1", "1", "0")
x4 <- c("1", F, F, 2)
# sprawdźmy typ i konwertujmy każdy z nich do pozostałych typów; wyjasnij skad wziely sie wyniki

as.numeric(x1)
as.character(x1)
as.logical(x1)

as.numeric(x2)


# czy da się tylko poleceniami typu "as." konwertować wektor x4 na wartości logiczne c(T, F, F, NA)? odpowiedz uzasadnij

as.logical(x4)

# wybieranie elementów wektora odbywa się najczęściej poprzez użycie nawiasów indeksowania; Indeksowanie możemy wykonać przy użyciu 
#  - jawnego wypisania elementów, które chcemy odfiltrować
#  - wektora logicznego tej samej długości
#  - przekazania do indeksu obiektu (wektora) zawierającego spis elementów lub wektor logiczny
# pamiętamy, że w R (inaczej niż w Pythonie) indeksowanie zaczyna się od 1 a nie 0!
#==# object[...]

x1[2]
x1[c(T, F, T)]
indeks <- c(3,4)
x4[indeks]

# odfiltrujmy/indeksujmy wektor x3, aby otrzymać wektor c(0,1,0)

indeks <- c(1,2,4)
x3[indeks]

grupa <- c(1,2,2,2,2,3,1,2,3,3,3,3,3,1,1,1,3,3,1,3,1,3,2,2,1,3,1,2,1,2,1,2,2,1,1,3,1,2,3,1,2,2,2,2,3,1,2,3,3,3,3,3,1,1,1,3,3,1,3,1,3,2,2,1,3,1,2,1,2,1,2,2,1,1,3,1,2,3,1,2,2,2,2,3,1,2,3,3,3,3,3,1,1,1,3,3,1,3,1,3,2,2,1,3,1,2,1,2,1,2,2,1,1,3,1,2,3,1,2,2,2,2,3,1,2,3,3,3,3,3,1,1,1,3,3,1,3,1,3,2,2,1,3,1,2,1,2,1,2,2,1,1,3,1,2,3)

etykiety <- c("ojciec", "dziecko", "matka")

czlonkowie <- etykiety[grupa]
czlonkowie

# odfiltrujmy wektor x1 by otrzymać wektor zawierający 5 kopii elementu 2. tego wektora

indeks <- c(2,2,2,2,2)
x1[indeks]

# co się stanie, gdy zaindeksujemy wektor x4 wektorem logicznym c(F,F,T)? A gdy indeks będzie wektorem c(T, F)?

x4[c(F,F,T)]
x4[c(T, F)]

###================
### GENEROWANIE LICZB (PSEUDO)LOSOWYCH

# idziemy w kierunku zmiennych losowych i ich rozkładów. Generatory liczb losowych są narzędziem koniecznym w pracy z zaawansowana statystyką; przyjrzyjmy się podstawom działania generatora liczb losowych w R 

# w największym skrócie, najważniejsze generatory liczb losowych to te, które doatarczają nam symulacji rozkładu U(0,1); każdy inny rozkład można otrzymać z pewnych działań wykonanych na tym rozkładzie

# typowy generator liczb pseudolosowych potrzebuje do swojego działania tzw. ziarna (seed); ziarno (s0) jest poddawane rekurencyjnemu działaniu funkcji generatora i w ten sposób tworzone są kolejne liczby pseudolosowe: x_{n+1} = f(x_n), gdzie n \in N oraz n_0 = s0

# generatory liczb pseudolosowych mają pewną "niepokojącą" cechę: mają "okres" (T), czyli maksymalną liczbę elementów jakie mogą wygenerować, aby ciąg zaczął się powtarzać, inaczej mówiąc x_{n+T} = x_{n}. W materiałach CRAN sprzed 11 lat okres dla generatora zaimplementowanego w R wynosi

(2^19937 - 1)

# "Matsumoto and Nishimura invents the first algorithm whose period (2^19937 − 1) exceeds the number of electron spin changes since the creation of the Universe (10^6000 against 10^120)"

# najważniejsze:
#  - generując liczby pseudolosowe możemy polegać na "losowości" generowanej przez R - pracowali nad tym najlepsi spece w dziedzinie
#  - MOŻEMY WSKAZYWAĆ ZIARNO :)

# wyprobujmy polecenie runif(); jakie to polecenie ma parametry podstawowe? jaki rozklad jest generowany przez to polecenie?

runif(5, min = 0, max = 45)

# wygeneruj 10 wartosci pseudolosowych z uzyciem runif()

runif(10)

# ustalenie ziarna odbywa sie poleceniem set.seed(); ustal ziarno i wygeneruj 5 wartosci pseudolosowych runif(); nastepnie wygeneruj kolejne 5 wartosci pseudolosych

set.seed(23)
runif(10)

# ustaw jeszcze raz ziarno na wczesniejsza wartosc i wygeneruj 3 wartosci pseudolosowe... potem kolejne 3 i na koniec 4; jak skomentujesz wyniki tego i poprzedniego eksperymentu?

set.seed(23)
runif(3)
runif(3)
runif(4)

# ustaw ziarno generatora liczb na 1011; wygeneruj 100 liczb runif() i zachowaj wynik pod nazwa "wygenerowane"

set.seed(1011)
wygenerowane <- runif(100)
wygenerowane

# wyrysuj histogram wylosowanych wartosci (polecenie hist() - sprawdź najpierw jakie to polecenie ma podstawowe parametry)

hist(wygenerowane)

# powtórz linie 219-225 dla licznosci proby 1.000, 10.000 i 100.000 losowych liczb

set.seed(1011)
wygenerowane1 <- runif(1000)
hist(wygenerowane1)

set.seed(1011)
wygenerowane2 <- runif(10000)
hist(wygenerowane2)

set.seed(1011)
wygenerowane3 <- runif(100000)
hist(wygenerowane3)


# podobny schemat jak dla runif() (linie kodu 203-229) zastosuj do generatorow:
#    rnorm()
#    rbinom()
#    rt() [dla chetnych]
#    rpois() [dla chetnych]

set.seed(1011)
normalny <- rnorm(1000)
hist(normalny)

set.seed(1011)
bi <- rbinom(1000, 10, 0.1)
hist(bi)

set.seed(1011)
tr <- rt(1000, 10000)
hist(tr)

set.seed(1011)
pois <- rpois(1000, 40)
hist(pois)