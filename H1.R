#H1: Økt total forelesningsdeltakelse (fysisk + digitalt) gir bedre karakter.
library(metRology)
library(brms)

data = read.csv("Bok1.csv", sep = ";")

x = data$pOppmote
y = data$snitt

reg = lm(y~x)

SSe = deviance(reg)
SS0 = 0
v0 = -2
n = length(x)
v1 = v0 + n
SS1 = SS0 + SSe

SSx = sum((x - mean(x))^2)
s1 = sqrt(SS1/v1)
beta = coef(reg)["x"]

# H1: b>0, H0: b<=0

distr = pt.scaled(0, v1, beta, s1*sqrt(1/SSx))
distr

breg =  brm(
  formula = snitt ~ pOppmote,
  data = data,
  family = gaussian(),
  file = "brm_snitt_oppmote"
)

plot(conditional_effects(breg))