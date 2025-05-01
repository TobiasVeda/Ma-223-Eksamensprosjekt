library(matlib)
library(scatterplot3d)

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

# Beregn total kvadratisk feil og standardfeil for regresjonslinjene
SSe_regresjon = c()
se_regresjon = c()

for (i in regresjon) {
  SSe_regresjon = c(SSe_regresjon, deviance(i))
  se_regresjon = c(se_regresjon, sqrt(deviance(i)/(length(y1) - length(coef(i)))))
}


# Plot regresjonslinjene og punkter
plot(x1, y1, col="maroon")
abline(reg1, col="maroon")
points(x2, y1, col="green")
abline(reg2, col="green")
points(x3, y1, col="blue")
abline(reg3, col="blue")

plot(x1, y2, col="maroon")
abline(reg5, col="maroon")
points(x2, y2, col="green")
abline(reg6, col="green")
points(x3, y2, col="blue")
abline(reg7, col="blue")

s3d <- scatterplot3d(x1, x2, y1,
                     pch = 16,
                     highlight.3d = TRUE,
                     angle = 55,
                     type = "h",
                     main = "3D-plot med regresjonsplan",
                     xlab = "x1 (pOppmote)",
                     ylab = "x2 (pFysisk)",
                     zlab = "y1 (snitt)")
s3d$plane3d(mreg1)

