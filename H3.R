#H3: Antall timer jobbet uken før eksamen forklarer karakterer bedre enn oppmøte (skippertak).
library(metRology)
library(tidybayes)
library(ggplot2)
library(plotly)
library(dplyr)


data = read.csv("Bok1.csv", sep = ";")

x1 = data$pOppmote
x2 = data$snitt
y = data$timer

reg = lm(y~ x1 + x2)

SSe = deviance(reg)
SS0 = 0
v0 = -3 # n0-k-1
n = length(y)
v1 = v0 + n
SS1 = SS0 + SSe

SSx_1 = sum((x1 - mean(x1))^2)
SSx_2 = sum((x2 - mean(x2))^2)
s1 = sqrt(SS1/v1)
beta_1 = coef(reg)["x1"]
beta_2 = coef(reg)["x2"]



# H1: b_timer>b_oppmote theta=b_timer-b_oppmote theta>0, H0: theta<=0


satterthwaite = function(sigx, sigy, vx, vy){
  a = (sigx^2)/(vx+1)
  b = (sigy^2)/(vy+1)
  return (floor( (a+b)^2 / ((a^2)/vx + (b^2)/vy) ))
}

my = beta_1 - beta_2
sigma = sqrt( (s1*sqrt(1/SSx_1))^2 + (s1*sqrt(1/SSx_2))^2 )
nu = satterthwaite(
  sigx = s1*sqrt(1/SSx_1), 
  sigy = s1*sqrt(1/SSx_2), 
  vx = v1, 
  vy = v1
  )

distr = pt.scaled(0, nu, my, sigma)
distr

breg =  brm(
  formula = snitt ~ pOppmote + timer,
  data = data,
  family = gaussian(),
  file = "brm_snitt_oppmote_timer"
)



# Lag et grid av verdier for timer og oppmøte
newdata <- expand.grid(
  pOppmote = seq(min(data$pOppmote), max(data$pOppmote), length.out = 30),
  timer = seq(min(data$timer), max(data$timer), length.out = 30)
)

# Legg til prediksjoner med usikkerhet
preds <- add_fitted_draws(breg, newdata = newdata, re_formula = NA)

# Beregn gjennomsnitt og usikkerhet per punkt
summary_preds <- preds %>%
  group_by(pOppmote, timer) %>%
  summarise(
    mean = mean(.value),
    lower = quantile(.value, 0.025),
    upper = quantile(.value, 0.975)
  )

# Plot interaktivt 3D plan med usikkerhet som farge
plot_ly(summary_preds, x = ~pOppmote, y = ~timer, z = ~mean, 
        type = "mesh3d", intensity = ~upper - lower, colors = c("blue", "red")) %>%
  layout(scene = list(
    xaxis = list(title = "Oppmote"),
    yaxis = list(title = "Timer"),
    zaxis = list(title = "Karakter (snitt)")
  ))
