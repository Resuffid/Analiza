rm(list = ls())

# załadujmy plik treningowy "train.Rda"

load(file = "Dane/train.Rda")

# przyjrzyjmy się zawartości naszego zbioru danych:
#  - jakiego typu dane są w zbiorze?
#  - czy mamy braki danych?
#  - jakie są rozkłady zmiennych w zbiorze?

str(train)
sum(!complete.cases(train))
table(train$g)
hist(train$x1)
hist(train$x2)
hist(train$y)
shapiro.test(train$x1)

# przyjrzyjmy się rozrzutowi zmiennych x1 i y

library(tidyverse)

ggplot(train, aes(x1, y)) +
  geom_point()

# dodajmy trend liniowy

ggplot(train, aes(x1, y)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE)

# co się zmieni, kiedy to samo zrobimy w podziale na grupy względem "g"?

ggplot(train, aes(x1, g))+
  geom_jitter()

ggplot(train, aes(x1, y, color=g)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE)

# dopasujmy model liniowy

# y = a * x1 + b + <błąd>  ; opuszczamy czynnik błędu
# y ~ a * x1 + b           ; w modelach lm nie trzeba używać nazw parametrów
# y ~ x1 + 1               ; w modelach lm wyraz wolny dodawany jest automatycznie
# y ~ x1

lm(formula = y ~ x1, data = train)

# wynikiem jest prosta:
# y = 1.608 * x1 + 918.924

# jeżeli masz osobę z x1 = 84, jaki jest najlepszy "strzał/typ" na wartość zmiennej y
# najlepszym typem jest wartość:

1.608 * 84 + 918.924

# diagnostyka modelu:
fit.lm.y.x1 <- lm(formula = y ~ x1, data = train)

# elementy szczegółowe
fit.lm.y.x1[["coefficients"]]
fit.lm.y.x1[["residuals"]]
fit.lm.y.x1[["fitted.values"]]

# informacje ogólne:
fit.lm.y.x1
print(fit.lm.y.x1)
summary(fit.lm.y.x1)

# odfiltrujmy wartości dopasowane oraz błąd i dodajmy do naszego zbioru danych jako odpowiednio "dop1" i "res1"

train <- train %>%
  mutate(res1 = fit.lm.y.x1[["residuals"]],
         dop1 = fit.lm.y.x1[["fitted.values"]])

names(train)

# wyrysujmy rozkład błędu

ggplot(train, aes(x = res1)) + 
  geom_density()

# sprawdźmy, czy rozkład błędu jest bliski rozkładowi normalnemu:

shapiro.test(train$res1)

# wyrysujmy jako wykres liniowy linie trendu na podstawie zmiennej dop1 wraz z rozrzutem zmiennych x i y

ggplot(train, aes(x = x1, y = y)) + 
  geom_point() +
  geom_smooth(train, mapping=aes(x1, dop1))

ggplot(train, aes(x = x1, y = y)) + 
  geom_point() +
  geom_line(train, mapping=aes(x1, dop1))

# czy linia trendu jest ta sama co w przypadku "geom_smooth()"?

# obliczmy sami błędy aproksymacji i porównajmy z wartościami obliczonymi przez "lm"

# y = a * x1 + b + <błąd>
# <błąd> = y - a * x1 - b

fit.lm.y.x1$coefficients
train <- mutate(train, res2 = y - 1.608 * x1 - 918.924)

# jak możemy porównać zgodnośc dwóch zmiennych res1 i res2?

var.test(train$res1, train$res2)

# spróbujmy odtworzyć R^2

ggplot(train, aes(x = x1, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = I("red"))+
  geom_hline(yintercept = mean(train$y), size = I(1.5), colour = I("green"))

A = summarise(train, "blad_prosta" = sum(res1^2))
B = summarise(train, "blad_srednia" = sum((y - mean(y))^2))

# R^2 = (B-A)/B

###=====
# zadanie:zbadać model y ~ 1 + g
###=====

train$g

train %>%
  group_by(g) %>%
  summarise("M" = mean(y))

ggplot(train, aes(x = g, y = y)) +
  geom_point()

fit.lm.y.g <- lm(formula = y ~ 1 + g, data = train)
summary(fit.lm.y.g)

ggplot(train, aes(x = as.numeric(g)-1, y = y)) +
  geom_point() +
  geom_abline(intercept = 1025.025, slope = 11.230)

###=====
# zadanie:zbadać model y ~ x1 + x2
###=====

fit.lm.y.x1.x2 <- lm(formula = y ~ x1 + x2, data = train)
summary(fit.lm.y.x1.x2)
summary(fit.lm.y.x1)

# jak zapiszemy ten model wzorem? jak obliczyć używając współczynników wartość modelu dla wybranych wartości x1 i x2?

fit.lm.y.x1.x2.1 <- 1.7962*train$x1 + -4.9909*train$x2 + 1005.3099

###=====
# zadanie: zbadać model y ~ x1 + g
###=====

fit.lm.y.x1.g <- lm(formula = y ~ x1 + g, data = train)
summary(fit.lm.y.x1.g)

###=====
# zadanie: zbadać model y ~ x1 + x2 + g
###=====

fit.lm.y.x1.x2.g <- lm(formula = y ~ x1 + x2 + g, data = train)
summary(fit.lm.y.x1.x2.g)
summary(fit.lm.y.x1.x2)

###=====
# zadanie: zbadać model y ~ x1 + g + x1 * g
###=====

fit.lm.y.x1.g.x1g <- lm(formula = y ~ x1 + g + x1*g, data = train)
summary(fit.lm.y.x1.g.x1g)


# spróbujmy na innym zbiorze danych:

library(psych)
library(tidyverse)

load(file = "./Dane/Baza_z_etykietami.Rda")

str(dane)

# obliczmy wyniki badania kwestionariuszowego i dodajmy stosowne kolumny w bazie danych:
#   - Wynik_Kw1.1: średnia z odpowiedzi udzielonych w pytaniach 1-7 kwestionariusza 1
#   - Wynik_Kw1.2: średnia z odpowiedzi udzielonych w pytaniach 8-11 kwestionariusza 1
#   - Wynik_Kw2  : średnia z odpowiedzi udzielonych w pytaniach 1-12 kwestionariusza 2

dane <- dane%>%
  mutate(Wynik_Kw1.1=(Kw1_P1+Kw1_P2+Kw1_P3+Kw1_P4+Kw1_P5+Kw1_P6+Kw1_P7)/7, Wynik_Kw1.2=(Kw1_P8+Kw1_P9+Kw1_P10+Kw1_P11)/4, Wynik_Kw2=(Kw2_P1+Kw2_P2+Kw2_P3+Kw2_P4+Kw2_P5+Kw2_P6+Kw2_P7+Kw2_P8+Kw2_P9+Kw2_P10+Kw2_P11+Kw2_P12/12))

# UWAGA: dlaczego akurat tak? zobaczmy jak wygląda analiza rzetelności:

psych::alpha(x = dane %>% 
               dplyr::select(contains("Kw1")), 
             check.keys = TRUE)
psych::alpha(x = dane %>% 
               dplyr::select(contains("Kw1")) %>% 
               dplyr::select(1:7),
             check.keys = TRUE)
psych::alpha(x = dane %>% 
               dplyr::select(contains("Kw1")) %>% 
               dplyr::select(-c(1:7)), 
             check.keys = TRUE)
psych::alpha(x = dane %>% 
               dplyr::select(contains("Kw2")), 
             check.keys = TRUE)

# wyrysujmy rozrzut wyników parami:

___

ggplot(dane, aes(x = ___, y = ___)) +
  geom_point()

ggplot(dane, aes(x = ___, y = ___)) +
  geom_point(alpha = ___)

ggplot(dane, aes(x = ___, y = ___)) +
  geom_jitter(alpha = ___)

ggplot(dane, aes(x = ___, y = ___)) +
  geom_jitter(alpha = ___) +
  geom_smooth(method = ___)

ggplot(dane, aes(x = ___, y = ___, colour = ___)) +
  geom_jitter(alpha = ___) +
  geom_smooth(method = ___)

# zbadajmy "rokujące" modele

___
___
___
___
