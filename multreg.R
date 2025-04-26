library(matlib)

# Innsamlet data
oppmote = c(8, 8, 3, 10, 7, 3, 10, 5, 8, 2)
fysisk = c(3, 5, 2, 8, 4, 1, 6, 3, 7, 2)
stream = c(5, 3, 1, 2, 3, 2, 4, 2, 1, 0)
timer = c(6, 8, 5, 10, 7, 4, 9, 6, 12, 5)
snitt = c(4.5, 4.7, 3.9, 5, 4.2, 3.5, 4.8, 4.3, 4.9, 3.8)

# Gjør data om til matrise
X_oppmote = cbind(1, oppmote)
X_fysisk = cbind(1, fysisk)
X_stream = cbind(1, stream)
# X_timer = cbind(1, timer)
X_mult = cbind(1, fysisk, stream)
y = matrix(snitt, nrow=10, ncol=1)

# Beregn a og b til regresjonslinje (hadde vært lettere med lm(y~x))
line_oppmote = inv(t(X_oppmote) %*% X_oppmote) %*% (t(X_oppmote) %*% y)
line_fysisk = inv(t(X_fysisk) %*% X_fysisk) %*% (t(X_fysisk) %*% y)
line_stream = inv(t(X_stream) %*% X_stream) %*% (t(X_stream) %*% y)
# line_timer = inv(t(X_timer) %*% X_timer) %*% (t(X_timer) %*% y)
line_mult = inv(t(X_mult) %*% X_mult) %*% (t(X_mult) %*% y)

# Beregn total kvadratisk feil for regresjonslinjene
SSe_oppmote = (t(y) - t(line_oppmote) %*% t(X_oppmote)) %*% y
SSe_fysisk = (t(y) - t(line_fysisk) %*% t(X_fysisk)) %*% y
SSe_stream = (t(y) - t(line_stream) %*% t(X_stream)) %*% y
SSe_mult = (t(y) - t(line_mult) %*% t(X_mult)) %*% y

# Beregn kvadratisk standardfeil for regresjonslinjene
se2_oppmote = SSe_oppmote/(length(oppmote) - 2)
se2_fysisk = SSe_fysisk/(length(fysisk) - 2)
se2_stream = SSe_stream/(length(stream) - 2)
#se2_mult = SSe_mult/((length(fysisk) + length(stream)) - 2)

# Plot regresjonslinjene og punkter
plot(oppmote, snitt, xlim=c(0,10), ylim=c(0,10), col="blue")
points(fysisk, snitt, col="green")
points(stream, snitt, col="red")
abline(a=line_oppmote[1], b=line_oppmote[2], col = "blue", lwd = 2)
abline(a=line_fysisk[1], b=line_fysisk[2], col = "green", lwd = 2)
abline(a=line_stream[1], b=line_stream[2], col = "red", lwd = 2)
abline(a=line_mult[1], b=line_mult[2], col = "maroon", lwd = 2)

# Print variabler
line_oppmote
SSe_oppmote
se2_oppmote

line_fysisk
SSe_fysisk
se2_fysisk

line_stream
SSe_stream
se2_stream

line_mult
SSe_mult
#se2_mult
