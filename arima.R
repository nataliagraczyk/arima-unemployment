# projekt ostateczny ARIMA - stopa bezrobocia

library(RJDemetra)
library(readxl)
library(tidyverse)
library(Metrics)
library(fpp2)
library(xts)
library(urca)
library(lmtest)
library(fBasics)

################################################################
####       przygotowanie danych               ##################
################################################################

# zaladowanie danych
setwd("C:/Users/gracz/Dropbox/My PC (LAPTOP-QJJ2QSFO)/Desktop/PiS")
dane <- read_excel("stopa1990.xlsx")

# konwersja na date
dane$data <- as.yearmon(dane$data)

# mówimy R'owi, ze to jest zmienna szereg czasowy
y <- ts(dane$y,start=c(1990,1),frequency=12)

#procedura dostosowania sezonowego - odsezonowanie
y_rsafull <- tramoseats(y, spec = "RSAfull")

#wyniki odsezonowania
y_rsafull_sa <- y_rsafull[["final"]][["series"]][,2] #zmienna dostosowana sezonowo
dane$y_rsafull_sa <- y_rsafull_sa
View(dane)
# policzenie pierwszych róznic

dane$dy <- diff.xts(dane$y_rsafull_sa)

# i usuwamy obserwacje starsze niz 2004-01
dane <- dane[dane$data >= as.yearmon("Jan 2004"), ]

plot(dane$data, dane$y,  type = "l", main = "stopa bezrobocia")
plot(dane$data, dane$dy, type = "l", main = "pierwsze róznice stopy bezrobocia")

# wykres stopy bezrobocia + stopy odsezonowanej

ggplot(dane, aes(x=data)) + 
  geom_line(aes(y = y, colour = "y")) + 
  geom_line(aes(y = y_rsafull_sa, colour="y_rsafull_sa")) +
  theme_bw() +
  scale_color_discrete(name = "", labels=c('Stopa bezrobocia', 'Stopa bezrobocia dostosowana sezonowo')) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

################################################################
####       okreslenie wartosci d w ARIMA (p,d,q)     ###########
################################################################

# test DF w celu okreslenia stacjonarnosci szeregu
# okreslenie stopnia integracji, tj wartosci d

testdf(variable = dane$y, adf_order = 3)

# H0 (o niestacjonarnosci) nie moze byc odrzucone,
# a zatem powtarzamy test dla pierwszych róznic

testdf(variable = dane$dy, adf_order = 3)

# pierwsze róznice stacjonarne, a zatem stopa bezrobocia jest procesem I(1)
# d=1
# ARIMA (p,d,q) -> ARIMA (p,1,q)
  
################################################################
####       okreslenie wartosci p,q w ARIMA (p,d,q)     #########
################################################################

# przekonertujmy dane do obiektu typu xts
# (podobny do obiektu ts)

dane.xts <- xts(dane[,2:4], # kolumny z danymi
                dane$data)  # kolumny z data/czasem

View(dane.xts)
#### wszystkie arimy ###

arima011 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(0, 1, 1),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa_rsafull_sa (p,d,q)
)
arima011

arima012 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(0, 1, 2),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa_rsafull_sa (p,d,q)
)
arima012

arima013 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(0, 1, 3),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa_rsafull_sa (p,d,q)
)
arima013

arima014 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(0, 1, 4),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima014
####
arima111 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(1, 1, 1),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima111

arima112 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(1, 1, 2),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima112

arima113 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(1, 1, 3),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima113

arima114 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(1, 1, 4),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima114

####
arima211 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(2, 1, 1),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima211

arima212 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(2, 1, 2),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima212

arima213 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(2, 1, 3),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima213

arima214 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(2, 1, 4),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima214

####
arima311 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(3, 1, 1),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima311

arima312 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(3, 1, 2),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima312

arima313 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(3, 1, 3),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima313

arima314 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(3, 1, 4),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima314

####
arima411 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(4, 1, 1),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima411

arima412 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(4, 1, 2),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima412

arima413 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(4, 1, 3),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima413

arima414 <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                  order = c(4, 1, 4),xreg = 1:length(dane.xts$y_rsafull_sa)  # rzedy_rsafull_sa (p,d,q)
)
arima414

# wartosci AIC dla wszystkich ARIM

AIC(arima011, arima012, arima013, arima014,
    arima111,arima112,arima113,arima114,
    arima211,arima212,arima213,arima214,
    arima311,arima312,arima313,arima314,
    arima411,arima412,arima413,arima414)

# Arima (1,1,2) - ten wypada najlepiej


################################################################
####                 obliczenie prognoz                #########
################################################################

# https://www.youtube.com/watch?time_continue=784&v=dXWy5nleaSg&feature=emb_title

# zbudowanie modelu

fit_ARIMA <- arima(dane.xts$y_rsafull_sa,  # zmienna zalezna
                           order = c(1, 1, 2))# rzedy (p,d,q)


summary(fit_ARIMA)
checkresiduals(fit_ARIMA)

# prognoza

fcast <- forecast (fit_ARIMA, h=3)
print(summary(fcast))

# wykres prognozy
fit_ARIMA %>% forecast(h=3) %>% autoplot()

autoplot(forecast(fit_ARIMA,h=3)) +
  ggtitle("Prognoza stopy bezrobocia") +
  xlab("Czas") + ylab("Stopa bezrobocia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# zoom wykresu prognozy
autoplot(forecast(fit_ARIMA,h=3), xlim = c(18, 20), ylim = c(0, 10)) +
ggtitle("Prognoza stopy bezrobocia") +
xlab("Czas") + ylab("Stopa bezrobocia") +
theme_bw() +
theme(plot.title = element_text(hjust = 0.5))

##########################

