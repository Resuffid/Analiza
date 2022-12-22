library(tidyverse)
library(broom)

# dopasujmy w końcu jakiś model liniowy

# pracujemy na zbiorze diamonds, mpg i iris

str(diamonds)
str(mpg)
str(iris)

# skupmy się na zmiennych ciągłych

# ZADANIE 1 (...na zapisywanie modeli w notacji R)
# dopasujcie następujące modele i zapiszcie wynik dopasowania w kolejnych obiektach o nazwach fit_lm_<numer modelu>
# 1) model tłumaczący zmienną displ zmienną cty
# 2) model dla zmiennej wyjaśnianej Sepal.Length i zmiennych wyjaśniających Petal.Length i Petal.Width
# 3) regresję zmiennej price względem zmiennych wymiaru prawo-lewo, głębi oraz wagi diamentu
# 4) modelu interakcji zmiennych depth oraz carat dla wyjaśnienia zmiennej price w zbiorze diamonds

fit_lm_1 <- lm(formula = displ ~ cty, data = mpg)
fit_lm_2 <- lm(formula = Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
fit_lm_3 <- lm(formula = price ~ x + carat + depth, data = diamonds)
fit_lm_4 <- lm(formula = price ~ depth*carat, data = diamonds)

summary(fit_lm_1)
summary(fit_lm_2)
summary(fit_lm_3)
summary(fit_lm_4)

# podsumujcie każdy z modeli i bądźcie gotowi do odpowiedzi na następujące pytania:
#   - czy model wnosi jakąś istotną (z punktu widzenia statystycznego) informację o zmiennje zależnej (poziom istotności = 0.01)?
#   - czy/które współczynniki regresji są istotne statystycznie? (poziom istotności = 0.01)
#   - jaka część wariancji zmiennej zależnej jest wyjaśniana przez model?

#====== wspólnie: przyjrzyjmy się wykresom diagnostycznym dla modelu pierwszego:

plot(fit_lm_1)

# warto też obejrzeć wykres rozrzutu:

ggplot(mpg, aes(x = cty, y = displ)) +
  geom_point(alpha = 0.4)
ggplot(mpg, aes(x = cty, y = displ)) +
  geom_jitter(alpha = 0.8)

# zadanie domowe: jak można zwizualizować dane w modelach z podpunktów 2-4?

# ZADANIE 2: (...na poprawienie modelu, gdy coś w nim nam nie gra)
# weźmy model z podpunktu 1 zadania 1. 
# krok 1: dopasujmy model, gdzie zmienna displ będzie tłumaczona przez stałą (wyraz wolny), zmienną cyl oraz kwadrat zmiennej cyl (pamiętaj: najlepiej stworzyć nowy zbiór danych zawierających zmienną bedącą kwadratem cyl); dopasowany model zapisz pod nazwą fit_lm_1_kw
# krok 2: sprawdź wykresy diagnostyczne dla nowego modelu
# krok 3: oblicz o ile razy polepszyła się ilość wyjaśnionej wariancji w modelu fit_lm_1_kw w porównaniu z modelem fit_lm_1
# krok 4*: wykorzystaj funkcję augment z pakietu broom by dodać do zbioru danych wartości dopasowane przez modela, a następnie narysuj wykres rozrzutu punktów w naszym zbiorze oraz puntków dopasowanych w modelu (najlepiej wyróżnij je kolorem)

mpg1 <- mpg%>%
  mutate(ctysqr = cty^2)
fit_lm_1_kw <- lm(formula = displ ~ cty+ctysqr, data = mpg1)

plot(fit_lm_1_kw)
plot(fit_lm_1)

summary(fit_lm_1)
summary(fit_lm_1_kw)
0.6918/0.6376

ggplot(augment(
  fit_lm_1_kw), aes(.fitted, displ)) +
  geom_jitter()
ggplot(mpg2, aes(ctysqr, displ)) +
  geom_point()


mpg2 <- augment(
  fit_lm_1_kw)

# inną metodą wyciągnięcia wartości dopasowanych jest funkcja predict(); pozawala ona jednak symulować wartości z modelu dla innych wartości zmiennych niezależnych niż w zbiorze oryginalnym

# ZADANIE 3: (na interpretację współczynników i metodę predict)
# przyjrzyjmy się modelowi 3 z zadania 1; 
# krok 1: używając metody predict() oblicz wartość generowaną przez nasz model dla wartości zmiennych niezależnych równych 0 (pamiętaj, że do metody predict musisz przekazac nie tylko obiekt z dopasowanym modelem ale i data.frame z nowymi wartościami zmiennych niezależnych); czy uzyskana wartość przypomina nam coś w dopasowanym modelu?
# krok 2: używając metody predict() wyznacz wartość zmiennej price dla następujących kombinacji zmiennych x, depth oraz carat:
#   - 0,0,1
#   - 0,1,0
#   - 1,0,0
#   - 1,1,1
#   - 2,0,0
# co przypominają uzyskane wartości w kontekście współczynników dopasowanego modelu?

dane <- data.frame("x" = c(0, 0, 0, 1, 1, 2),
                   "carat" = c(0,0,1,0,1,0),
                   "depth" = c(0,1,0,0,1,0))
predict(fit_lm_3, dane)

summary(fit_lm_3)

porownanie <- data.frame("predicts" = c(12015.528, 11862.165, 22639.824, 10778.398, 21249.330,  9541.267),
                         "wspolczynniki" = c(12015.528, 12015.528-153.363, 12015.528+10624.296, 12015.528-1237.131, 12015.528-1237.131+10624.296-153.363, 12015.528-1237.131-1237.131))

