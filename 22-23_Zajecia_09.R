library(tidyverse)

str(diamonds)
str(iris)
str(mtcars)

autka <- mtcars

tab01 <- mtcars%>%
  group_by(cyl, gear,vs)%>%
  summarise(M_mpg=(mean(mpg)))%>%
  ungroup()

tab02 <- diamonds%>%
  group_by(color)%>%
  summarise(Q1_x=quantile(x, 0.25), Q3_x=quantile(x,0.75), Q1_y=quantile(y,0.25), Q2_y=quantile(y,0.5))

tab02a <- diamonds%>%
  group_by(color)%>%
  summarise(Q1_x=quantile(x, 0.25), Q3_x=quantile(x,0.75), Q1_y=quantile(y,0.25), Q2_y=median(y))

tab_example <- mtcars %>%
  group_by(cyl, gear) %>%
  summarise(SD_wt = sd(wt)) %>%
  pivot_wider(id_cols = cyl, names_from = gear, values_from = SD_wt)

tab03 <- diamonds%>%
  group_by(cut, color)%>%
  summarise(M_price=mean(price))%>%
  pivot_wider(id_cols = cut, names_from = color, values_from = M_price)%>%
  ungroup()

plot04 <- ggplot(data=iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Species)) +
  geom_point() +
  labs(x = "długość liścia", y = "szerokość płatka", color = "gatunek")

plot05 <- ggplot(data=iris, mapping = aes(x=Sepal.Length, y = Sepal.Width)) + 
  geom_point(alpha=0.7, shape=14, size=4)

diamonds

ggplot(data=diamonds, mapping = aes(x=y, y=price, color=clarity)) +
  geom_jitter() +
  labs(x= "wymiar w kierunku przód-tył") +
  theme_bw()

diamonds_zad7 <- diamonds%>%
  filter(y!=0 &
  y<=(quantile(y,0.75)+1.5*(quantile(y,0.75)-quantile(y,0.25))))%>%
  ggplot( mapping = aes(x=y, y=price, color=clarity)) +
  geom_point() +
  labs(x= "wymiar w kierunku przód-tył") +
  theme_bw()
