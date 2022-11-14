#==================
# przypomnijmy: rozkłady możemy również porównywać dzięki odwrotnej dystrybuancie

# wylosujmy dwie próbki z rozkładu normalnego (różniące się średnią i odchyleniem); policzmy kwartyle dla obu rozkładów i narysujmy wykres rozrzutu

set.seed(677)
probka1 <- rnorm(n = 200, mean = 10, sd = 3)
probka2 <- rnorm(n = 300, mean = 12, sd = 4)

hist(probka1)
hist(probka2)

centyle1 <- quantile(x = probka1, probs = (1:99)/100)
centyle2 <- quantile(x = probka2, probs = (1:99)/100)

plot(x = centyle1, y = centyle2)

# co kiedy podobny wykres stworzymy dla centyli?

# ZADANIE 1.1
#   wylosujcie dwie próbki z tego samego rozkładu normalnego (ta sama średnia, to samo odchylenie i ta sama liczba losowanych liczb = 1000)
#   wyznaczcie centyle dla obu próbek
#   narysujcie wykres rozrzutu dla obu wektorów centyli

set.seed(677)
probka1 <- rnorm(n = 1000, mean = 10, sd = 4)
probka2 <- rnorm(n = 1000, mean = 10, sd = 4)

centyle1 <- quantile(x = probka1, probs = (1:99)/100)
centyle2 <- quantile(x = probka2, probs = (1:99)/100)

plot(x = centyle1, y = centyle2)

# ZADANIE 1.2
#   wylosujcie dwie próbki z rozkładu normalnego (różne średnie, różne odchylenia, ale ta sama liczba losowanych liczb = 1000)
#   wyznaczcie centyle dla obu próbek
#   narysujcie wykres rozrzutu dla obu wektorów centyli

set.seed(350)
probka3 <- rnorm(n = 1000, mean = 15, sd = 5)
probka4 <- rnorm(n = 1000, mean = 20, sd = 9)

plot(density(probka3))
lines(density(probka4))

centyle3 <- quantile(x = probka3, probs = (1:99)/100)
centyle4 <- quantile(x = probka4, probs = (1:99)/100)

plot(x = centyle3, y = centyle4)

# ZADANIE 1.3
#   wylosujcie dwie próbki: jedna z rozkładu normalnego a druga z rozkładu jednostajnego (parametry rozkładów dowolne wybrane; dla obu próbek ta sama liczba losowanych liczb = 1000)
#   wyznaczcie centyle dla obu próbek
#   narysujcie wykres rozrzutu dla obu wektorów centyli

set.seed(350)
probka7 <- rnorm(n = 1000, mean = 30, sd = 15)
probka8 <- runif(n = 1000, min = 20, max = 40)

centyle7 <- quantile(x = probka7, probs = (1:99)/100)
centyle8 <- quantile(x = probka8, probs = (1:99)/100)

plot(x = centyle7, y = centyle8)

hist(probka7)
hist(probka8)

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

# HIPOTEZA ZEROWA: średnia w grupie jest równa 100
# HIPOTEZA ALTERNATYWNA: średnia w grupie nie jest równa 100

# Zakładając, że hipoteza zerowa jest prawdziwa sprawdźmy, jakie wartości przyjmuje nasza statystyka testowa (średnia arytmetyczna próbki)

set.seed(4044)

# Krok 1: losujemy jedną próbkę 30 osób i liczymy średnią (w jednej linijce)

probka_badania <- rnorm(n = 30, mean = 102, sd = 15)
mean(probka_badania)

# Krok 2: wykonujemy replikację 5.000 razy powyższego kodu

x <- replicate(n = 5000, expr = rnorm(n = 30, mean = 100, sd = 15))
srednie <- colMeans(x)

hist(srednie)

# Krok 3: sprawdzamy jakie są skrajne wartości rozkłądu średnich (2.5% z lewej i 2.5% z prawej strony)

quantile(x = srednie, probs = c(0.025, 0.975))

# Krok 4: korzystamy z funkcji ifelse() by dodać kolumnę z decyzją jaką podejmujemy

decyzja <- ifelse(srednie > quantile(x = srednie, probs = c(0.975)) | srednie < quantile(x = srednie, probs = c(0.025)), "H1", "H0")

table(decyzja)/5000

# ZADANIE 2.1
#   poprawcie przykładowy kod, aby otrzymać wektor decyzji dla próbek 10 osób oraz 15.000 replikacji

x <- replicate(n = 15000, expr = rnorm(n = 10, mean = 100, sd = 15))
rnorm(n = 10, mean = 110, sd = 15)

srenie<- colMeans(x)
hist(srenie)

quantile(x = srenie, probs = c(0.025, 0.975))

# ZADANIE 2.2
#   poprawcie przykładowy kod, aby otrzymać wektor decyzji dla próbek 50 osób oraz 12.500 replikacji

x <- replicate(n = 12500, expr = rnorm(n = 50, mean = 100, sd = 15))
rnorm(n = 10, mean = 110, sd = 15)

srenie<- colMeans(x)
hist(srenie)

quantile(x = srenie, probs = c(0.025, 0.975))

# ZADANIE 2.3
#   poprawcie przykładowy kod, aby otrzymać wektor decyzji dla próbek 100 osób oraz 10.000 replikacji

x <- replicate(n = 100000, expr = rnorm(n = 1000, mean = 100, sd = 15))

srenie<- colMeans(x)
hist(srenie)

quantile(x = srenie, probs = c(0.025, 0.975))

#=================================================

# testując hipotezę H0: średnia w próbie 30 osób dla pewnej cechy jest równa 100 (przy czym wiemy, że rozkład cechy jest normalny i ma odchylenie standardowe równe 15), mogliśmy wybrać statystykę testową (średni poziom cechy w probie) oraz wyznaczyć dzięki generatorowi liczb losowych punkty odcięcia dla statystyki testowej, przy poziomie istotności 5%:

set.seed(10)
rnorm(n = 30, mean = 100, sd = 15)

set.seed(10)
mean(rnorm(n = 30, mean = 100, sd = 15))

set.seed(10)
srednie <- replicate(n = 5000, expr = mean(rnorm(n = 30, mean = 100, sd = 15)))
hist(srednie)

set.seed(10)
quantile(x = srednie, probs = c(0.025, 0.975))

# umieśćmy nasze wyniki srednie dla repliacji w ramce danych:

testowanie <- data.frame("replikacja" = 1:5000, "srednie" = srednie, 
                         "decyzja1" = ifelse(srednie > quantile(x = srednie, probs = c(0.975)) | srednie < quantile(x = srednie, probs = c(0.025)), "przyjmujemy H1", "przyjmujemy H0"))

# co gdybyśmy mieli poważne przesłanki by sądzić, że w testowanej populacji średnia jest równa np. 105? Moglibyśmy wtedy zobaczyć, na podstawie eksperymentu, jakie statystyki testu wychodzą gdy w populacji mamy średnią 105
# zreplikujmy zatem 5.000 razy eksperyment polegający na wylosowaniu 30 osób z rozkładu normalnego o średniej 105 i odchyleniu 15. Wyznaczmy średnie próbek

set.seed(10)
srednie2 <- replicate(n = 5000, expr = mean(rnorm(n = 30, mean = 105, sd = 15)))
hist(srednie2)

# możemy teraz prześledzić co się stanie, jeżeli będziemy podejmowali decyzje w oparciu o H0 o średniej 100, gdy dane faktycznie pochodzą z populacji ze średnią 105:

testowanie$decyzja2 = ifelse(srednie2 > quantile(x = srednie, probs = c(0.975)) | srednie2 < quantile(x = srednie, probs = c(0.025)), "przyjmujemy H1", "przyjmujemy H0")

table(testowanie$decyzja2)

