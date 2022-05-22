install.packages("sets", dependencies=T)

library(sets)

sets_options("universe", seq(1, 100, 1))

variaveis <- set(
  Frequencia = fuzzy_partition(varnames = c( MenDSemanas  = 30, MaiDSemanas = 60, Diario = 70, Continuo=90), radius=20, FUN = fuzzy_cone),
  SABA = fuzzy_partition(varnames = c(MenDSemanas= 20, MaiDSemanas = 30, Diario = 70, DuasxDia=90), sd = 10),
  DebitoExp = fuzzy_partition(varnames = c(CinqOiten = 20, TrintTCinqCin = 30,  MaisTrintT = 70),  sd = 10),
  Classificacao = fuzzy_partition(varnames = c(Moderada = 20, AgudaGrave=40  , RiscoVida = 60), sd=10)
)

regras <-
  set(
    fuzzy_rule( Frequencia %is% MenDSemanas && SABA %is% MenDSemanas && DebitoExp %is% CinqOiten, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MenDSemanas && SABA %is% MenDSemanas && DebitoExp %is% TrintTCinqCin, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MenDSemanas && SABA %is% MenDSemanas && DebitoExp %is% MaisTrintT, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MenDSemanas && SABA %is% MaiDSemanas && DebitoExp %is% MaisTrintT, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MaiDSemanas && SABA %is% MenDSemanas && DebitoExp %is% CinqOiten, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MaiDSemanas && SABA %is% MenDSemanas && DebitoExp %is% MaisTrintT, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MaiDSemanas && SABA %is% MaiDSemanas && DebitoExp %is% CinqOiten, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MaiDSemanas && SABA %is% MaiDSemanas && DebitoExp %is% TrintTCinqCin, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% Diario && SABA %is% Diario && DebitoExp %is% TrintTCinqCin, Classificacao %is% AgudaGrave ),
    fuzzy_rule( Frequencia %is% Diario && SABA %is% Diario && DebitoExp %is% CinqOiten, Classificacao %is% AgudaGrave ),
    fuzzy_rule( Frequencia %is% Diario && SABA %is% DuasxDia && DebitoExp %is% TrintTCinqCin, Classificacao %is% AgudaGrave ),
    fuzzy_rule( Frequencia %is% Diario && SABA %is% DuasxDia && DebitoExp %is% MaisTrintT, Classificacao %is% AgudaGrave ),
    fuzzy_rule( Frequencia %is% Continuo && SABA %is% Diario && DebitoExp %is% TrintTCinqCin, Classificacao %is% RiscoVida ),
    fuzzy_rule( Frequencia %is% Continuo && SABA %is% Diario && DebitoExp %is% MaisTrintT, Classificacao %is% RiscoVida ),
    fuzzy_rule( Frequencia %is% Continuo && SABA %is% DuasxDia && DebitoExp %is% TrintTCinqCin, Classificacao %is% RiscoVida ),
    fuzzy_rule( Frequencia %is% Continuo && SABA %is% DuasxDia && DebitoExp %is% MaisTrintT, Classificacao %is% RiscoVida )
  )

sistema <- fuzzy_system(variaveis, regras)
sistema
plot(sistema)

inferencia <- fuzzy_inference(sistema, list(Frequencia  = 80 , SABA = 70, DebitoExp = 80 ))
inferencia
plot(inferencia)

def = gset_defuzzify(inferencia, "centroid")

plot(sistema$variables$Classificacao)
lines(inferencia, col = "red", lwd=4)

sets_options("universe", NULL)
