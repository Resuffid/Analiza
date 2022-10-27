### PRZYPOMNIENIE Z OSTATNICH ZAJĘĆ
# Kiedy chcemy stworzyć wykres funkcji musimy najpierw wygenerować wektor wartości x, dla których chcemy narysować funkcję. Możemy do tego użyć "trików", ale możemy posiłkowac się funckją seq() [sprawdź argumenty w help]

x <- seq(from = -3, to = 3, by = 0.05)

# następnie dla wartości wektora x obliczamy wartości naszej funkcji (niech będzie to dla przykładu funkcja kwadratowa f(s) = 1/20*s^2 - 1/20*s - 0.1)

y <- 1/20*x^2 - 1/20*x - 0.1

# rysujemy wykres przy pomocy plot()

plot(x = x, y = y)

# na koniec możemy zamienić typ wykresu na liniowy:

plot(x = x, y = y, type = "l")

### ZADANIE 1.1
# narysujmy na przedziale od -1 do 3 gęstość rozkladu jednostajnego, gdzie losujemy liczby z zakresu od 0 do 2

# KROK 1: przygotuj odpowiedni wektor dla osi x; punkty muszą być "gęsto"
# KROK 2: oblicz wartości funkcji gęstości rozkładu (d z odpowiednim przyrostkiem) dla wektora x
# KROK 3: narysuj wykres x versus y jako linię

a <- seq(-1,3,0.05)
b <- dunif(a,0,2)
plot(a,b,"l")

### ZADANIE 1.2
# narysujmy na przedziale od -4 do 4 gęstość rozkladu normalnego o średniej 0 i odchyleniu standardowym 0

# KROK 1: przygotuj odpowiedni wektor dla osi x; punkty muszą być "gęsto"
# KROK 2: oblicz wartości funkcji gęstości rozkładu (d z odpowiednim przyrostkiem) dla wektora x
# KROK 3: narysuj wykres x versus y jako linię

c <- seq(from=-4, to=4, by=0.05)
d <- dnorm(x=c, mean=0, sd=1)
plot(x=c, y=d, type="l")

### ZADANIE 1.3
# narysujmy na przedziale od -1 do 6 gęstość rozkladu binomialnego (size = 5, prob = 0.5) 

# KROK 1: przygotuj odpowiedni wektor dla osi x; punkty muszą być "gęsto"
# KROK 2: oblicz wartości funkcji gęstości rozkładu (d z odpowiednim przyrostkiem) dla wektora x
# KROK 3: narysuj wykres x versus y jako linię

e <- seq(from=-1, to=6, by=0.01)
f <- dbinom(x=e, size=5, prob=0.5)
plot(x=e, y=f, type="l")


#=======
# do stworzonych wykresów możemy dodawać kolejne wykresy w postaci linii
# aby to się powiodło, musimy pracować na tych samych wektorach dedykowanych osi x; wygenerujmy 

y2 <- 0.05*x + 0.2

plot(x = x, y = y, type = "l")
lines(x = x, y = y2) # możemy zmienić też kolor parametrem "color"

# możemy również wyrysowywać gęstości rozkładów empirycznych, nie tylko teoretycznych, jak dotychczas

set.seed(670)
wektor_losowy <- rnorm(n = 20, mean = 10, sd = 3)
plot(density(x = wektor_losowy))

wektor_losowy2 <- rnorm(n = 20, mean = 7, sd = 3)
lines(density(x = wektor_losowy2)) # warto dodać kolor

### ZADANIE 2.1
# wybierzcie parametr średniej i odchylenia std. dla rozkładu normalnego
# dla wybranych wartości stwórzcie serię wykresów gęstości (powiedzmy 5 gęstości na jednym obrazku), gdzie gęstości będą generowane dla coraz większych liczności próby

set.seed(120)
los1 <- rnorm(n=5, mean = 12, sd = 7)
los2 <- rnorm(n=10, mean = 12, sd = 7)
los3 <- rnorm(n=20, mean = 12, sd = 7)
los4 <- rnorm(n=40, mean = 12, sd = 7)
los5 <- rnorm(n=80, mean = 12, sd = 7)

plot(density(x = los1))
lines(density(x=los2), col = 'red')
lines(density(x=los3), col = 'blue')
lines(density(x=los4), col = 'green')
lines(density(x=los5), col = 'yellow')


# na co ma wpływ rosnąca (malejąca) liczba losowanych liczb?

### ZADANIE 2.2
# wybierzcie jeden parametr średniej oraz losujcie próby wielkości n = 1000
# dla wybranych wartości odchylenia std. dla rozkładu normalnego stwórzcie serię wykresów gęstości (powiedzmy 5 gęstości na jednym obrazku), gdzie gęstości będą generowane dla różnych wartości odchylenia

set.seed(120)
pos1 <- rnorm(n=1000, mean = 100, sd = 5)
pos2 <- rnorm(n=1000, mean = 100, sd = 10)
pos3 <- rnorm(n=1000, mean = 100, sd = 15)
pos4 <- rnorm(n=1000, mean = 100, sd = 20)
pos5 <- rnorm(n=1000, mean = 100, sd = 25)

plot(density(x = pos1))
lines(density(x=pos2), col = 'red')
lines(density(x=pos3), col = 'blue')
lines(density(x=pos4), col = 'green')
lines(density(x=pos5), col = 'yellow')

# na co ma wpływ odchylenie standardowe rozkładu normalnego?

### ZADANIE 2.3
# wybierzcie jeden parametr odchylenia standardowego oraz losujcie próby wielkości n = 1000
# dla wybranych wartości średniej dla rozkładu normalnego stwórzcie serię wykresów gęstości (powiedzmy 5 gęstości na jednym obrazku), gdzie gęstości będą generowane dla różnych wartości średnich

set.seed(120)
kos1 <- rnorm(n=1000, mean = 10, sd = 7)
kos2 <- rnorm(n=1000, mean = 15, sd = 7)
kos3 <- rnorm(n=1000, mean = 20, sd = 7)
kos4 <- rnorm(n=1000, mean = 25, sd = 7)
kos5 <- rnorm(n=1000, mean = 30, sd = 7)

plot(density(x = kos1))
lines(density(x=kos2), col = 'red')
lines(density(x=kos3), col = 'blue')
lines(density(x=kos4), col = 'green')
lines(density(x=kos5), col = 'yellow')

# na co ma wpływ rosnąca (malejąca) wartość średniej?

#==================
# spróbujmy teraz na jednym wykresie narysować gęstość teoretyczną wybranego rozkładu normalnego oraz gęstość rozkładu empirycznego wylosowanego z tego samego rozkładu (n = 200)

set.seed(11)
x1 <- seq(-4,4,0.001)
y1 <- dnorm(x1, 0.5,1.5)
plot(x = x1, y = y1, type = "l")
probka <- rnorm(2000, 0.5,1.5)
lines(density(probka))

# w jaki sposób moglibyśmy "policzyć" na ile rozkład teoretyczny jest zgodny z empirycznym na podstawie gęstości?

___

#==================
# rozkłady możemy również porównywać dzięki odwrotnej dystrybuancie

# wylosujmy dwie próbki z rozkładu normalnego (różniące się średnią i odchyleniem); policzmy kwartyle dla obu rozkładów i narysujmy wykres rozrzutu

set.seed(123)
probka1 <- rnorm(1000,0,2)
probka2 <- rnorm(500,0,2)

# co kiedy podobny wykres stworzymy dla centyli?

quantile(x = probka1, probs = c(0.25, 0.5, 0.75))

# wykonajmy to samo ćwiczenie, ale z jedną próbką z rozkładu normalnego a drugą z rozkładu jednostajnego

set.seed(666)
probka_n <- rnorm(1000, 0, 2)
probka_u <- runif(1000, -2, 2)
q1 <- quantile(probka_n, probs = seq(0.01,0.99,0.01))
q2 <- quantile(probka_u, probs = seq(0.01,0.99,0.01))
plot(q1,q2)

# OPCJONALNIE: dla przygotowanych wcześniej rozkładów epirycznych znajeźć pasujące parametry tych rozkładów (najpierw trzeba "poznać" rozkłady binomialny i Piossona)

___
___
___
___
___




