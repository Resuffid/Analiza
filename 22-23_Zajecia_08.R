#==================
# dplyr

library(tidyverse)

# zbiory danych

str(diamonds)
str(mtcars)
str(iris)

# zbiór HolzingerSwineford1939 z pakietu 'lavaan'

load(file = "./Dane/HS1939.Rda")

# zacznijmy od czegoś prostego

# ZADANIE 1: 
# ze zbioru mtcars odfiltrujmy samochody 8-cylindrowe; następnie obliczmy w podziale na grupy po liczbie biegów średnią mil, jakie może pokonać samochód na galonie benzyny

zad1 <- mtcars%>%
  filter(cyl == 8)%>%
  group_by(gear)%>%
  summarise(srednia_mil=mean(mpg))

# ZADANIE 2:
# w zbiorze iris stwórzmy nową zmienną będącą sumą kwadratów wszystkich długości zmierzonych dla danego kwiatka; odfiltrujmy tylko te kwiatki, dla których nowa zmienna jest większa lub równa od swojego Q1; stworzyć tabelkę podsumowującą ile takich kwiatków jest w podziale na gatunek

zad2 <- iris%>%
  mutate(suma_kwadrat=((Sepal.Length + Petal.Length)^2))%>%
  filter(suma_kwadrat>=quantile(x = suma_kwadrat, 0.25))%>%
  group_by(Species)%>%
  summarise(N=n())
  
# DYGRESJA 1:

# zobaczmy w jaki sposób możemy wykorzystać dplyr do narysowania wykresów gęstości dla wielu zmiennych jednocześnie

# zajmijmy się zbiorem iris; chcielibyśmy porównać (na poziomie gęstości) rozkłady wszystkich czterech zmiennych liczbowych

# SPOSÓB 1:

ggplot(iris, aes(x = Sepal.Length)) +
  geom_density()

ggplot(iris, aes(x = Sepal.Length)) +
  geom_density() +
  geom_density(aes(x = Petal.Length))

ggplot(iris, aes(x = Sepal.Length)) +
  geom_density() +
  geom_density(aes(x = Petal.Length), color = "red")

ggplot(iris, aes(x = Sepal.Length)) +
  geom_density() +
  geom_density(aes(x = Petal.Length), color = "red") +
  geom_density(aes(x = Sepal.Width), color = "blue") +
  geom_density(aes(x = Petal.Width), color = "green")

# SPOSÓB 2:

iris_long <- iris %>%
  pivot_longer(cols = 1:4, names_to = "Zmienna", values_to = "Wartosc")

ggplot(iris_long, aes(x = Wartosc, color=Zmienna)) +
  geom_density()

# DYGRESJA 2:

# przygotowanie danych do testów statystycznych
# test Shapiro-Wilka potrzebuje wektora wartości, zatem musimy wydobyć wektor z danych, np.: chcielibyśmy zbadać normalność rozkładu Sepal.Length dla gatunku "setosa"

iris %>%
  filter(Species == "setosa")

iris %>%
  filter(Species == "setosa") %>%
  select(Sepal.Length)

iris %>%
  filter(Species == "setosa") %>%
  select(Sepal.Length) %>%
  pull()

probka <- iris %>%
  filter(Species == "setosa") %>%
  select(Sepal.Length) %>%
  pull()

shapiro.test(x = probka)
hist(probka)

# DYGRESJA 3:

# porównanie próbek 2 próbek możemy przeprowadzić testem t Studenta
# potrzebujemy dwóch wektorów zawierających próbki
# w naszym przypadku 

probka1 <- iris %>%
  filter(Species == "setosa") %>%
  select(Sepal.Length) %>%
  pull()

probka2 <- iris %>%
  filter(Species == "versicolor") %>%
  select(Sepal.Length) %>%
  pull()

var.test(x = probka1, y = probka2)

t.test(x = probka1, y = probka2, var.equal = FALSE)

# ZADANIE:
# Pracujemy na zbiorze danych HolzingerSwineford1939.
#   1) dodać do zbioru danych zmienną będącą średnią arytmetyczną udzielonych odpowiedzi (zmienne x1:x9)
#   2) przygotować tabelę w podziale na płeć uczestników i szkołę, w której znajdą się następujące podsumowujące informacje: ilu uczestników badania było w danej grupie + średni poziom zmiennej wyniku diagnozy + odchylenie standardowe zmiennej wyniku diagnozy
#   3) odfiltrować z danych dwa wektory (probki) z wynikami diagnozy: wektor_1 = wyniki dla osób z płcią równą 1 i wektor_2 = wyniki dla osób z płcią równą 2
#   4) narysować (na osobnych rysunkach) dla obu odfiltrowanych wektorów wykresy qqnorm oraz histogramy
#   5) zweryfikować normalność obu rozkładów w wektor_1 i wektor_2 przy pomocy testy Shapiro-Wilka
#   6) zweryfikować równość wariancji w wektor_1 i wektor_2 przy pomocy odpowiedniego testu
#   7) zweryfikować równość średnich w grupach ze względu na płeć wyników diagnozy, używając testu t z odpowiednim parametrem wprowadzającym poprawkę na równe/różne wariancje

dane1 <- HSdane%>%
  mutate(wynik=rowMeans(HSdane[7:15]))

dane2 <- dane1%>%
  group_by(sex,school)%>%
  summarise(N=n(), M = mean(wynik), SD = sd(wynik))

wektor_1 <- dane1%>%
  filter(sex == 1)%>%
  select(wynik)%>%
  pull()

wektor_2 <- dane1%>%
  filter(sex == 2)%>%
  select(wynik)%>%
  pull()

hist(wektor_1)
hist(wektor_2)

shapiro.test(wektor_1)
shapiro.test(wektor_2)

var.test(wektor_1, wektor_2)

t.test(wektor_1, wektor_2, var.equal = TRUE)
