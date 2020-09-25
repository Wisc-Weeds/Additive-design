#Load the workbook
DMT=read.csv("dmtotal.csv")
names(DMT)
str(DMT)
DMT$weed=as.factor(DMT$weed)

par(mar=c(5,5,2,2), pty="s", mgp=c(4,1.5,0))

library(drc)
par(mar=c(5,6,2,2), pty="s", mgp=c(4,1.5,0))
DRC=drm(yl~densityweed, subset=weed=="1", fct=l4 (fixed =c(NA,NA,NA,NA)),data=DMT)
summary(DRC)
plot(DRC, legend="", font.size=2, cex=2, type = "none", las=1, lwd=4, lty=1, yaxt='n', xaxt='n',
     xlab="", ylab ="")




legend("bottomright", legend=c("C. benghalensis", "R. brasiliensis"), text.font = 3, col=c(1,2), pch= c(16,1), lty=c(1,3), lwd= c(5,5), bty="n", cex=2)

AIC(DRC)


Hei=read.csv("height.csv")
names(Hei)
str(Hei)

DRC1=drm(yl~densityweed, weed, fct=l4 (fixed =c(NA,NA,NA,NA)),data=Hei)
summary(DRC1)
plot(DRC1, legend="", font.size=2, cex=2, las=1, xlim=c(0,10), lwd=4, col=c(1,2), lty=c(1,2), pch=c(16,1),
     xlab=expression("Densidade (plantas vaso"^-1*")"), ylim=c(-10,110), ylab = "Redução biomassa total (%)", cex.axis=1.8, cex.lab=2)
legend("bottomright", legend=c("C. benghalensis", "R. brasiliensis"), text.font = 3, col=c(1,2), pch= c(16,1), lty=c(1,3), lwd= c(5,5), bty="n", cex=2)

AIC(DRC1)
