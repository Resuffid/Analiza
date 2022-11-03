#==================
# przypomnijmy: rozkłady możemy również porównywać dzięki odwrotnej dystrybuancie

# wylosujmy dwie próbki z rozkładu normalnego (różniące się średnią i odchyleniem); policzmy kwartyle dla obu rozkładów i narysujmy wykres rozrzutu

set.seed(677)
probka1 <- rnorm(n = 200, mean = 10, sd = 3)
probka2 <- rnorm(n = 300, mean = 12, sd = 4)

centyle1 <- quantile(x = probka1, probs = (1:99)/100)
centyle2 <- quantile(x = probka2, probs = (1:99)/100)

plot(x = centyle1, y = centyle2)

# co kiedy podobny wykres stworzymy dla centyli?

# ZADANIE 1.1
#   wylosujcie dwie próbki z tego samego rozkładu normalnego (ta sama średnia, to samo odchylenie i ta sama liczba losowanych liczb = 1000)
#   wyznaczcie centyle dla obu próbek
#   narysujcie wykres rozrzutu dla obu wektorów centyli

set.seed(677)
probka1_11 <- rnorm(n = 1000, mean = 26, sd = 5)
probka2_11 <- rnorm(n = 1000, mean = 26, sd = 5)
centyle1_11 <- quantile(x = probka1_11, probs = (1:99)/100)
centyle2_11 <- quantile(x = probka2_11, probs = (1:99)/100)
plot(x = centyle1_11, y = centyle2_11)

# ZADANIE 1.2
#   wylosujcie dwie próbki z rozkładu normalnego (różne średnie, różne odchylenia, ale ta sama liczba losowanych liczb = 1000)
#   wyznaczcie centyle dla obu próbek
#   narysujcie wykres rozrzutu dla obu wektorów centyli

set.seed(677)
probka1_12 <- rnorm(n = 1000, mean = 15, sd = 2)
probka2_12 <- rnorm(n = 1000, mean = 26, sd = 5)
centyle1_12 <- quantile(x = probka1_12, probs = (1:99)/100)
centyle2_12 <- quantile(x = probka2_12, probs = (1:99)/100)
plot(x = centyle1_12, y = centyle2_12)

# ZADANIE 1.3
#   wylosujcie dwie próbki: jedna z rozkładu normalnego a druga z rozkładu jednostajnego (parametry rozkładów dowolne wybrane; dla obu próbek ta sama liczba losowanych liczb = 1000)
#   wyznaczcie centyle dla obu próbek
#   narysujcie wykres rozrzutu dla obu wektorów centyli

set.seed(677)
probka1_13 <- rnorm(n = 1000, mean = 15, sd = 2)
probka2_13 <- runif(n = 1000)
centyle1_13 <- quantile(x = probka1_13, probs = (1:99)/100)
centyle2_13 <- quantile(x = probka2_13, probs = (1:99)/100)
plot(x = centyle1_13, y = centyle2_13)

#=================================================
# a teraz zajmijmy się testowaniem hipotez
# przypomnijmy:



#                               | H0 przyjęta w teście | H1 przyjęta w teście |
#------------------------------------------------------------------------------
# H0 w rzeczywistości prawdziwa |          OK          |    błąd I rodzaju    |
#------------------------------------------------------------------------------
# H1 w rzeczywistości prawdziwa |    błąd II rodzaju   |         OK           |
#------------------------------------------------------------------------------


# spróbujemy w oparciu o duże replikacje eksperymentu zdecydować, czy losowana próba 30 osób jest z rozkładu normalnego o średniej 100, czy nie jest (zakładamy, że odchylenie standardowe jest równe 15)

# HIPOTEZA ZEROWA: poziom cechy == 100; rozkład normalny z sd == 15
# HIPOTEZA ALTERNATYWNA: poziom cechy =/= 100; rozkład normalny z sd == 15

# Zakładając, że hipoteza zerowa jest prawdziwa sprawdźmy, jakie wartości przyjmuje nasza statystyka testowa (średnia arytmetyczna próbki)

set.seed(4044)

# Krok 1: losujemy jedną próbkę 30 osób i liczymy średnią (w jednej linijce)

probka <- rnorm(n = 30, mean = 104, sd = 15)

# Krok 2: wykonujemy replikację 5.000 razy powyższego kodu

set.seed(11)
srednie <- replicate(n = 5000, expr = mean(rnorm(n=30, mean = 100, sd=15)))
hist(srednie)

# Krok 3: sprawdzamy jakie są skrajne wartości rozkłądu średnich (2.5% z lewej i 2.5% z prawej strony)

quantile(x = srednie, probs = c(0.025, 0.975))

# Krok 4: korzystamy z funkcji ifelse() by dodać kolumnę z decyzją jaką podejmujemy

ifelse(srednie > quantile(x = srednie, probs = c(0.975)) | srednie < quantile(x = srednie, probs = c(0.025)), "obszar odrzucenia H0", "Obszar przyjęcia H0")

# ZADANIE 2.1
#   poprawcie przykładowy kod, aby otrzymać wektor decyzji dla próbek 10 osób oraz 15.000 replikacji

set.seed(11)
srednie1 <- replicate(n = 15000, expr = mean(rnorm(n=10, mean = 100, sd=15)))

quantile(x = srednie1, probs = c(0.025, 0.975))

ifelse(srednie > quantile(x = srednie1, probs = c(0.975)) | srednie < quantile(x = srednie1, probs = c(0.025)), "obszar odrzucenia H0", "Obszar przyjęcia H0")
table(ifelse(srednie1 > quantile(x = srednie1, probs = c(0.975)) | srednie1 < quantile(x = srednie1, probs = c(0.025)), "obszar odrzucenia H0", "Obszar przyjęcia H0"))

# ZADANIE 2.2
#   poprawcie przykładowy kod, aby otrzymać wektor decyzji dla próbek 50 osób oraz 12.500 replikacji

set.seed(11)
srednie2 <- replicate(n = 12500, expr = mean(rnorm(n=50, mean = 100, sd=15)))

quantile(x = srednie2, probs = c(0.025, 0.975))

ifelse(srednie2 > quantile(x = srednie2, probs = c(0.975)) | srednie2 < quantile(x = srednie2, probs = c(0.025)), "obszar odrzucenia H0", "Obszar przyjęcia H0")
table(ifelse(srednie2 > quantile(x = srednie2, probs = c(0.975)) | srednie2 < quantile(x = srednie2, probs = c(0.025)), "obszar odrzucenia H0", "Obszar przyjęcia H0"))

# ZADANIE 2.3
#   poprawcie przykładowy kod, aby otrzymać wektor decyzji dla próbek 100 osób oraz 10.000 replikacji

set.seed(11)
srednie3 <- replicate(n = 10000, expr = mean(rnorm(n=100, mean = 100, sd=15)))
hist(srednie3)

quantile(x = srednie3, probs = c(0.025, 0.975))

ifelse(srednie3 > quantile(x = srednie3, probs = c(0.975)) | srednie3 < quantile(x = srednie3, probs = c(0.025)), "obszar odrzucenia H0", "Obszar przyjęcia H0")
table(ifelse(srednie3 > quantile(x = srednie3, probs = c(0.975)) | srednie3 < quantile(x = srednie3, probs = c(0.025)), "obszar odrzucenia H0", "Obszar przyjęcia H0"))

#=================================================
# wiemy jaką decyzję mamy podjąć w zależności od wartości statystyki testu (kwantyle 2,5% oraz 97,5%)
# co gdybyśmy mieli poważne przesłanki by sądzić, że w testowanej populacji średnia jest równa np. 120? Moglibyśmy wtedy zobaczyć, na podstawie eksperymentu, jakie statystyki testu wychodzą gdy w populacji mamy średnią 120
# zreplikujmy zatem 5.000 razy eksperyment polegający na wylosowaniu 30 osób z rozkładu normalnego o średniej 120 i odchyleniu 15. Wyznaczmy średnie próbek

set.seed(11)
srednie4 <- replicate(n = 5000, expr = mean(rnorm(n=30, mean = 120, sd=15)))
hist(srednie4)

# dodajmy do naszego wektora kolumnę, w której podejmiemy decyzję ZGODNĄ ZE STATYSTYKAMI opartymi na hipotezie zerowej o średniej równej 100; sprawdźmy jak często popełnimy błąd II rodzaju!

ifelse(srednie4 > quantile(x = srednie, probs = c(0.975)) | srednie4 < quantile(x = srednie, probs = c(0.025)), "obszar odrzucenia H0", "Obszar przyjęcia H0")
table(ifelse(srednie4 > quantile(x = srednie, probs = c(0.975)) | srednie4 < quantile(x = srednie, probs = c(0.025)), "obszar odrzucenia H0", "Obszar przyjęcia H0"))

# ZADANIE 3.1
#   poprawcie przykładowy kod, aby oszacować szansę popełnienia błędu II rodzaju przy założeniu, że w hipotezie alternatywnej mamy średnią 110, dla próbek 10 osób oraz 15.000 replikacji

___

# ZADANIE 3.2
#   poprawcie przykładowy kod, aby oszacować szansę popełnienia błędu II rodzaju przy założeniu, że w hipotezie alternatywnej mamy średnią 110, dla próbek 50 osób oraz 12.500 replikacji

___

# ZADANIE 3.3
#   poprawcie przykładowy kod, aby oszacować szansę popełnienia błędu II rodzaju przy założeniu, że w hipotezie alternatywnej mamy średnią 110, dla próbek 100 osób oraz 10.000 replikacji

___



