

library(polynom)

# your polynomial (coefficients in ascending powers of x order)

x11()
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(yl~densityweed, data=Test, subset = weed =="1", ylab="", xlab="", type="none", pch=16, cex=2, xaxt='n', yaxt='n', las=1)
x=seq(0,6,0.25)
weedL=(0.0+15*x)
lines(x,weedL, lwd=5, col=1)
legend("topleft", legend=c("A"), bty="n", cex=2)







par(mar=c(1,1,1,1))
p <- polynomial(c(10,3,-1))
plot(p,  xaxt='n', yaxt='n', lty=1, xaxt='n', ylab="", xlab="", lwd=5, col=1) 
legend("topleft", legend=c("B"), bty="n", cex=2) 


DRC=drm(yl~densityweed, subset=weed=="1", fct=l4 (fixed =c(NA,NA,NA,NA)),data=DMT) 
summary(DRC) 
par(mar=c(1,1,1,1))
plot(DRC, legend="", font.size=2, cex=2, axes=FALSE, type = "none", las=1, lwd=4, lty=1, yaxt='n', xaxt='n',
     xlab="", ylab ="")

legend("topleft", legend=c("C"), bty="n", cex=2)



# analysis for manuscript - TOTAL DRY MATTER:

#Load the workbook
Test=read.csv("dmtotaltest.csv")


#par(mfrow=c(3,1))
par(mar=c(1,1,1,1))
plot(yl~densityweed, data=Test, subset = weed =="1", ylab="", xlab="", type="none", pch=16, cex=2, xaxt='n', yaxt='n', las=1)

# adding lines
x=seq(0,6,0.25) 
weed1=(150*x)/(1+(150/90)*x) 


lines(x,weed1, lty=1, lwd=5, col=1)
legend("topleft", legend=c("D"), bty="n", cex=2)

