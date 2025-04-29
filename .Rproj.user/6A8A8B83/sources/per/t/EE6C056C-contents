library(matlib)

# Innsamlet data
nOppmote = c(8, 8, 3, 10, 7, 3, 10, 5, 8, 2)
nFysisk = c(3, 5, 2, 8, 4, 1, 6, 3, 7, 2)
nDigital = c(5, 3, 1, 2, 3, 2, 4, 2, 1, 0)
timer = c(6, 8, 5, 10, 7, 4, 9, 6, 12, 5)
snitt = c(4.5, 4.7, 3.9, 5, 4.2, 3.5, 4.8, 4.3, 4.9, 3.8)

x1 = nOppmote
x2 = nFysisk
x3 = nDigital
x4 = timer

y1 = snitt
y2 = timer




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

# Beregn total kvadratisk feil for regresjonslinjene
SSe_reg1 = deviance(reg1)
SSe_reg2 = deviance(reg2)
SSe_reg3 = deviance(reg3)
SSe_reg4 = deviance(reg4)
SSe_reg5 = deviance(reg5)
SSe_reg6 = deviance(reg6)
SSe_reg7 = deviance(reg7)
SSe_mreg1 = deviance(mreg1)
SSe_mreg2 = deviance(mreg2)
SSe_mreg3 = deviance(mreg3)
SSe_mreg4 = deviance(mreg4)
SSe_mreg5 = deviance(mreg5)
SSe_mreg6 = deviance(mreg6)
SSe_mreg7 = deviance(mreg7)
SSe_mreg8 = deviance(mreg8)
SSe_mreg9 = deviance(mreg9)
SSe_mmreg1 = deviance(mmreg1)
SSe_mmreg2 = deviance(mmreg2)
SSe_mmreg3 = deviance(mmreg3)
SSe_mmreg4 = deviance(mmreg4)
SSe_mmreg5 = deviance(mmreg5)
SSe_m3reg1 = deviance(m3reg1)

# Beregn standardfeil for regresjonslinjene
se_reg1 = sqrt(SSe_reg1/(length(y1) - 2))
se_reg2 = sqrt(SSe_reg2/(length(y1) - 2))
se_reg3 = sqrt(SSe_reg3 / (length(y1) - 2))
se_reg4 = sqrt(SSe_reg4 / (length(y1) - 2))
se_reg5 = sqrt(SSe_reg5 / (length(y1) - 2))
se_reg6 = sqrt(SSe_reg6 / (length(y1) - 2))
se_reg7 = sqrt(SSe_reg7 / (length(y1) - 2))
se_mreg1 = sqrt(SSe_mreg1 / (length(y1) - 3))
se_mreg2 = sqrt(SSe_mreg2 / (length(y1) - 3))
se_mreg3 = sqrt(SSe_mreg3 / (length(y1) - 3))
se_mreg4 = sqrt(SSe_mreg4 / (length(y1) - 3))
se_mreg5 = sqrt(SSe_mreg5 / (length(y1) - 3))
se_mreg6 = sqrt(SSe_mreg6 / (length(y1) - 3))
se_mreg7 = sqrt(SSe_mreg7 / (length(y2) - 3))
se_mreg8 = sqrt(SSe_mreg8 / (length(y2) - 3))
se_mreg9 = sqrt(SSe_mreg9 / (length(y2) - 3))
se_mmreg1 = sqrt(SSe_mmreg1 / (length(y1) - 4))
se_mmreg2 = sqrt(SSe_mmreg2 / (length(y1) - 4))
se_mmreg3 = sqrt(SSe_mmreg3 / (length(y1) - 4))
se_mmreg4 = sqrt(SSe_mmreg4 / (length(y1) - 4))
se_mmreg5 = sqrt(SSe_mmreg5 / (length(y2) - 4))
se_m3reg1 = sqrt(SSe_m3reg1 / (length(y1) - 5))


# Plot regresjonslinjene og punkter
plot(nOppmote, snitt, xlim=c(0,10), ylim=c(0,10), col="blue")
points(nFysisk, snitt, col="green")
points(nDigital, snitt, col="red")
abline(reg_snitt_nOppmote, col = "blue", lwd = 2)
abline(mreg_snitt_nOppmote_timer, col = "green", lwd = 2)
abline(mreg_snitt_nFysisk_nDigital, col = "red", lwd = 2)
abline(mreg_snitt_timer_nFysisk_nDigital, col = "maroon", lwd = 2)


