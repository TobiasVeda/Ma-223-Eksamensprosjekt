library(matlib)
library(scatterplot3d)
library(effects)
library(plotly)

# Innsamlet data
data = read.csv("Bok1.csv", sep = ";")

x1 = data$pOppmote
x2 = data$pFysisk
x3 = data$pDigital
x4 = data$timer

y1 = data$snitt
y2 = data$timer

# Beregn alle mulige regresjonslinjer
reg1 = lm(y1 ~ x1)
reg2 = lm(y1 ~ x2)
reg3 = lm(y1 ~ x3)
reg4 = lm(y1 ~ x4)
reg5 = lm(y2 ~ x1)
reg6 = lm(y2 ~ x2)
reg7 = lm(y2 ~ x3)
mreg1 = lm(y1 ~ x1 + x2)
mreg2 = lm(y1 ~ x1 + x3)
mreg3 = lm(y1 ~ x1 + x4)
mreg4 = lm(y1 ~ x2 + x3)
mreg5 = lm(y1 ~ x2 + x4)
mreg6 = lm(y1 ~ x3 + x4)
mreg7 = lm(y2 ~ x1 + x2)
mreg8 = lm(y2 ~ x1 + x3)
mreg9 = lm(y2 ~ x2 + x3)
mmreg1 = lm(y1 ~ x1 + x2 + x3)
mmreg2 = lm(y1 ~ x1 + x2 + x4)
mmreg3 = lm(y1 ~ x1 + x3 + x4)
mmreg4 = lm(y1 ~ x2 + x3 + x4)
mmreg5 = lm(y2 ~ x1 + x2 + x3)
m3reg1 = lm(y1 ~ x1 + x2 + x3 + x4)

regresjonsnavn = c("reg1","reg2","reg3","reg4","reg5","reg6","reg7","mreg1","mreg2","mreg3","mreg4","mreg5","mreg6","mreg7","mreg8","mreg9",
                   "mmreg1","mmreg2","mmreg3","mmreg4","mmreg5","m3reg1")
regresjon = list(reg1,reg2,reg3,reg4,reg5,reg6,reg7,mreg1,mreg2,mreg3,mreg4,mreg5,mreg6,mreg7,mreg8,mreg9,
                 mmreg1,mmreg2,mmreg3,mmreg4,mmreg5,m3reg1)



# Lag liste med alle modellnavn, total kvadratisk feil, standardfeil, forklaringsgrad, adjust forklaringsgrad
all = list()
for (i in 1:length(regresjon)) {
  
  frame = data.frame(
    name = regresjonsnavn[i],               # Navn til modellen
    SSe = deviance(regresjon[[i]]),         # Total kvadratisk feil til modellen
    se = sqrt(                              # standardfeil til modellen (fra før jeg lærte "summary" :| )
      deviance(regresjon[[i]])/
        (length(y1) - length(coef(regresjon[[i]])))
    ),
    R2 = summary(regresjon[[i]])$r.squared, # R^2 til modellen
    adjR2 = summary(regresjon[[i]])$adj.r.squared     # adjusted R^2 til modellen, justerer for multipple regresjon
  )
  all = append(all, list(frame))
}

# Finn modellene som kommer best ut
min_SSe   = all[[ which.min(sapply(all, function(x) x$SSe))   ]]
min_se    = all[[ which.min(sapply(all, function(x) x$se))    ]]
max_R2    = all[[ which.max(sapply(all, function(x) x$R2))    ]]
max_adjR2 = all[[ which.max(sapply(all, function(x) x$adjR2)) ]]


min_SSe
min_se
max_R2
max_R2


s3d <- scatterplot3d(x3, x4, y1,
                     pch = 19,
                     highlight.3d = TRUE,
                     angle = 50,
                     type = "h",
                     main = "3D-plot med regresjonsplan",
                     xlab = "x1 (pOppmote)",
                     ylab = "x2 (pFysisk)",
                     zlab = "y1 (snitt)")
s3d$plane3d(mreg6, col = "maroon")


plot_ly(data, 
        x = ~x2,  # pFysisk
        y = ~x3,  # pDigital
        z = ~x4,  # timer
        color = ~y1, 
        colors = colorRamp(c("blue", "green", "red")),
        type = "scatter3d", 
        mode = "markers") %>%
  layout(title = "3D Visualisering av mmreg4",
         scene = list(xaxis = list(title = "pFysisk"),
                      yaxis = list(title = "pDigital"),
                      zaxis = list(title = "Timer")))

plot(allEffects(mmreg4))