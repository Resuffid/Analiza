#==================
# dplyr

library(tidyverse)
library(formattable)

# zbiory danych

str(diamonds)
str(mtcars)
str(iris)

# przypomnienie:

diamonds %>%
  filter(x > 2.0) %>%
  select(price, color) %>%
  mutate(pricePLN = 4.3*price) %>%
  group_by(color) %>%
  summarise(M = mean(pricePLN), SD = sd(pricePLN)) %>%
  arrange(desc(M))

# rozgrzewka:

# ZADANIE 1.1
# Pogrupowac obserwacje zbioru diamonds po kolorze diamentu i policzmy średnie oraz odchylenia cen; w podsumowaniu dodajmy również maksymalną cenę i minimalną; wynik zapiszmy pad nazwą pods_1

pods_1 <- diamonds%>%
  group_by(color)%>%
  select(price, color) %>%
  summarise(srednia=mean(price), SD=sd(price), max=max(price), min=min(price))
  

# ZADANIE 1.2
# Pogrupowac obserwacje zbioru diamonds po rodzaju cięcia diamentu i policzmy średnie oraz odchylenia cen; w podsumowaniu dodajmy również maksymalną cenę i minimalną; wynik zapiszmy pad nazwą pods_2

pods_2 <- diamonds%>%
  group_by(cut)%>%
  select(price, cut) %>%
  summarise(srednia=mean(price), SD=sd(price), max=max(price), min=min(price))

# ZADANIE 1.3
# Pogrupowac obserwacje zbioru diamonds po przezroczystości diamentu i policzmy średnie oraz odchylenia cen; w podsumowaniu dodajmy również maksymalną cenę i minimalną; wynik zapiszmy pad nazwą pods_3

pods_3 <- diamonds%>%
  group_by(clarity)%>%
  select(price, clarity) %>%
  summarise(srednia=mean(price), SD=sd(price), max=max(price), min=min(price))

#======================
# zobaczmy jak wyglądają rozkłady wymiarów diamentów

ggplot(diamonds, aes(x = color, y = x)) +
  geom_boxplot()
ggplot(diamonds, aes(x = color, y = y)) +
  geom_boxplot()
ggplot(diamonds, aes(x = color, y = z)) +
  geom_boxplot()

summary(diamonds$price)

# ZADANIE 2.1
# Dodajmy do naszego kodu linię kodu, aby dodać nową kolumnę będącą iloczynem x, y i z; dalej odfiltrujmy wszystkie wiersze, których iloczyn wymiarów diamentu nie jest zerem; potem wykonajmy ponownie poprzednie obliczenia podsumowujące; wynik zapisujemy pod poprzednią nazwą

pods_21 <- diamonds%>%
  group_by(color)%>%
  mutate(iloczyn=(x*y*z))%>%
  filter(iloczyn!=0)%>%
  summarise(srednia=mean(price), SD=sd(price), max=max(price), min=min(price), wymiar=mean(iloczyn))

# ZADANIE 2.2
# Dodajmy do naszego kodu linię kodu, aby dodać nową kolumnę będącą iloczynem x, y i z; dalej odfiltrujmy wszystkie wiersze, których iloczyn wymiarów diamentu nie jest zerem; potem wykonajmy ponownie poprzednie obliczenia podsumowujące; wynik zapisujemy pod poprzednią nazwą

pods_22 <- diamonds%>%
  group_by(cut)%>%
  mutate(iloczyn=(x*y*z))%>%
  filter(iloczyn!=0)%>%
  summarise(srednia=mean(price), SD=sd(price), max=max(price), min=min(price), wymiar=mean(iloczyn))

# ZADANIE 2.3
# Dodajmy do naszego kodu linię kodu, aby dodać nową kolumnę będącą iloczynem x, y i z; dalej odfiltrujmy wszystkie wiersze, których iloczyn wymiarów diamentu nie jest zerem; potem wykonajmy ponownie poprzednie obliczenia podsumowujące; wynik zapisujemy pod poprzednią nazwą

pods_23 <- diamonds%>%
  group_by(clarity)%>%
  mutate(iloczyn=(x*y*z))%>%
  filter(iloczyn!=0)%>%
  summarise(srednia=mean(price), SD=sd(price), max=max(price), min=min(price), wymiar=mean(iloczyn))

#===========================
# gdy w podsumowaniu chcemy umieścić liczbę obserwacji w zbiorze/grupie, stosujemy funkcję n()

mtcars %>%
  group_by(cyl) %>%
  summarise(N = n())

# ZADANIE 3
# W zbiorze mtcars podsumować bazę danych tak, aby w podsumowaniu znalazły się następujące kolumny: zmienna grupująca gear + N zawierająca liczbę obesracji w grupie ze względu na gear + PROC zawierająca informację ile procent całego zbioru danych stanowi dana grupa ze względu na gear

zad_3 <- mtcars%>%
  group_by(gear)%>%
  summarise(N=n())%>%
  mutate(PROC=formattable::percent(N/sum(N)))

# lub bez paczki "formattable"

zad_3 <- mtcars%>%
  group_by(gear)%>%
  summarise(N=n())%>%
  mutate(PROC=(N/sum(N))*100)

przyklad <- mtcars%>%
  group_by(gear, am)

#===========================
# W zbiorze iris, dla każdego gatunku liczymy mediany każdej zmiennej liczbowej

zad_noname <- iris%>%
  group_by(Species)%>%
  summarise(MED_Slength=median(Sepal.Length),
            MED_Swidth=median(Sepal.Width),
            MED_Plength=median(Petal.Length),
            MED_Pwidth=median(Petal.Width))

# ZADANIE 4
#   a) odfiltrować ze zbioru iris tylko te obserwacje, które mają wszystkie wartości liczbowe powyżej mediany
#   b) odfiltrować ze zbioru iris tylko te obserwacje, które mają co najmniej jedną wartość liczbową powyżej mediany

zad_4a <- iris%>%
  group_by(Species)%>%
  filter(Sepal.Length>median(Sepal.Length) &
           Sepal.Width>median(Sepal.Width) &
           Petal.Length>median(Petal.Length) &
           Petal.Width>median(Petal.Width))

zad_4b <- iris%>%
  group_by(Species)%>%
  filter(Sepal.Length>median(Sepal.Length) |
           Sepal.Width>median(Sepal.Width) |
           Petal.Length>median(Petal.Length) |
           Petal.Width>median(Petal.Width))
