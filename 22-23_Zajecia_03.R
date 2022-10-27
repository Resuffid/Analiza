###================
### NARZĘDZIOWNIK

# utwórzmy trzy wektory, ktore bedziemy uzywac do testowania poleceń graficznych:

# wektor `x` powinien zawierac liczby od -5 do 5 w równych odstępach by odcinek podzielić na 100 równych części

x <- seq(from = -5, to = 5, by = ((5-(-5))/100))

# wektor `y` to wartosci funkcji f(x) = (x+3)^2*(x-1)^3*(x-4) obliczone na wczesniej stworzonym wektorze `x`

y <- ((x+3)^2*(x-1)^3*(x-4))

# wektor `z` zawierający (pseudo)losowe wartosci z rokladu jednostajnego; wektor powinien miec tę samą dlugosc co wektory `x` i `y`

set.seed(22)
z <- runif(101)

# wykresy rozrzutu: wyrysujmy wykres rozrzutu par wektorow x i z oraz x i y

plot(x,y)
plot(x,z)

# wykres liniowy: mamy mozliwosc polaczenia punktow wykresu rozrzutu w linię, korzystając z parametru `type`; wyrysujmy zatem wykres funkcji f(x)

plot(x, y,
     type="l",
     col="red",
     lwd=3)

# co, kiedy chcielibyśmy zwiększyć dokladnosc wykresu przez zageszczenie punktow na osi x?

# wyrysujmy, wzorujac sie na powyzszym kodzie, funkcje 1/(x-1)^2 na przedziale od -1 do 2

a <- seq(-1, 2, by=((2-(-1))/100))
b <- (1/(a-1)^2)
plot(a,b, type="l"
     )
max(b)

#       ASYMPTOTA

### wracamy do naszego wektora wartości losowych

# wyrysujmy histogram wartosci wektora `z`

hist(z)

# zmienmy liczbe slupkow na wieksza (który parametr będzie za to odpowiadał?):

hist(z, breaks=20)

# przy ilu slupkach kazda wartosc z naszego zbioru bedzie miala "swoj" slupek? czy wszystkie one musza byc tej samej wysokosci rownej 1?

hist(z, breaks=z, freq=T, right = F)

# LUB tyle ile wynosi różnica między dwoma najmniejszymi danymi w zbiorze. TO DZIAŁA TYLKO DLA DANYCH W KTÓRYCH WYNIKI NIE POWTARZAJĄ SIĘ

# Narysować histogram z danego wektora, żeby histogram był płaski

set.seed(123)
x <- sort(runif(1000, -5, 10))
x[100]
x[101]
(x[101]-x[100])/2 # Granica pierwszego przedziału i tak co 100 robimy ręcznie kolejne

# LUB

mniejsze <- c(quantile(x, probs = seq(0.1,0.9,0.1)))
wieksze <- c(quantile(x, probs = seq(0.101,0.901,0.1)))
breaki <- (wieksze+mniejsze)/2
as.numeric(breaki)
hist(x, breaks = breaki)

# dystrybuanta i gestosc rozkladu jednostajnego: rozklad teoretyczny versus empiryczny

# ustalmy ziarno na 123
# wylosujmy 1000 wartosci z rozkladu jednostajnego na przedziale od -1 do 1 i nazwisjmy je "rozkl_empir"

set.seed(123)
rozkl_empir <- runif(1000, min=-1, max=1)

# korzystając z "triku" sumowania wartosci logicznej, obliczmy jaka jest "szansa", że w naszym rozkladzie trafimy na liczbe mniejsza niz 0.5

wart <- (c(rozkl_empir) < 0.5)
probability <- sum(wart, na.rm = T)/length(rozkl_empir)
sum(rozkl_empir < 0.5, na.rm = T)/length(rozkl_empir)
# przy okazji obliczmy ile wynosi 1 kwartyl oraz 8 decyl (polecenie: quantile()) naszego wektora losowego

quantile(x = rozkl_empir, probs = c(0.15, 0.85))
quantile(x = rozkl_empir, probs = c(0.25, 0.80))

# Rozkład decylowy

quantile(x = rozkl_empir, probs = seq(0.1, 0.9, 0.1))

#Mediana
median(rozkl_empir)

# wykorzystajmy polecenie punif, qunif, dunif albo runif, aby obliczyć jaka jest szansa trafienia liczby mniejszej niż 0.5 w rozkladzie teoretycznym, jednostajnym, na przedziale -1 do 1 (wskazane by odwiedzić help)

punif(q=0.5, min = -1, max = 1)

# co kiedy zwiekszymy liczebnosc naszej proby empirycznej do 10.000? czy obliczone wartości z rozkładu empirycznego będa bardziej czy mniej przypominały wartości z rozkładu teoretycznego?

#Będą zbliżone - centralne twierdzenie graniczne :)

# narysujmy dystrybuante rozkladu jednostajnego
# stworzmy wektor x zawierajacy rowno rozlozone wartosci od -2 do 2 (niech bedzie ich ~ 100)

c <- seq(-2,2,(4/100))
d <- punif(c,-1,1)
plot(c,d,type="l")

#qunif - szukanie wartości poniżej której prawdopodobieństwo wylosowania tej i niższej liczby jest równy argumentowi p

qunif(p=0.12, -1,1)
punif(q=-0.76, -1,1)

# obliczmy wartosci dystrybuanty rozkladu U(-1,1) dla punktow z wektora x i narysujmy wykres liniowy dla niej

___

# obliczmy "recznie" prawdopodobieństwo wylosowania z naszej proby emoirycznej wartosci mnioejszych lub roznych wartosciom z wektora x (tutaj trzeba troche pokombinowac :)) wynik zdeponujmy w wektorze o wybranej nazwie

___

# narysujmy dystrybuante rozkladu empirycznego

___

# prócz rozkładu jednostajnego mamy dostęp do wielu innych rozkładów losowych... przyjrzyjmy się im...



