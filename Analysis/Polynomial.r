# analysis for manuscript - TOTAL DRY MATTER:
library(qpcR)
#Load the workbook
DMT=read.csv("dmshoot.csv")
head(DMT)
str(DMT)
DMT$yl=as.numeric(DMT$yl)


#Cousens model
#par(mfrow=c(3,1))
par(mar=c(5,6,2,2), pty="s", mgp=c(4,1.5,0))
tiff("plot2.tiff")
plot(yl~densityweed, data=DMT, subset = weed =="1", pch=16, cex=2, las=1, 
     xlab=expression("Weed density (plants pot"^-1*")"), ylim=c(-10,110), ylab = "Yield loss (%)", cex.axis=1.8, cex.lab=2)
lines(yl~densityweed, type="p",data=DMT, subset = weed =="2", col=2, cex=2, pch=15)
tiff("plot2.tiff")

### 1st model - Cousen's model 1985 - Rectangular hyperbola with 2 parameters

# fitting a general model - parameter for each treat
Cousens.model.1 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A[weed])*densityweed), alg="port", data=DMT, 
                      start=list(I=c(60,30), A=c(80,60)), trace=T)
summary(Cousens.model.1, correlation=T)
RMSE(Cousens.model.1, which = NULL)
AIC(Cousens.model.1)

# adding lines
x=seq(0,4,0.25)
weed1=(167.228*x)/(1+(167.228/108.058)*x)
weed2=(51.770*x)/(1+(51.770/118.074)*x)

lines(x,weed1, lty=1, lwd=5, col=1)
lines(x,weed2, lty=3, lwd=5, col=2)

comp=par(font=3)
legend("bottomright", legend=c("C. benghalensis", "R. brasiliensis"), text.font = 3, col=c(1,2), pch= c(16,15), lty=c(1,3), lwd= c(5,5), bty="n", cex=2)
#legend("topleft", legend=c("A"), text.font=1, col=1, bty="n", cex=3)

DRC

# now reducing the full model: using a single I and A for the two species
Cousens.model.2 = nls(yl ~ (I*densityweed)/(1+(I/A)*densityweed), data=DMT, alg="port",
                      start=list(I=40, A=80),  trace=T)
summary(Cousens.model.2, correlation=T)

anova(Cousens.model.2,Cousens.model.1)

confint(Cousens.model.2, level=0.95)
AIC(Cousens.model.2)
RMSE(Cousens.model.2, which = NULL)

mse <- mean(residuals(Cousens.model.1)^2/df.residual(Cousens.model.1))
rmse <- sqrt(mse)
rmse


# a single model is not enough to describe the yield loss curve for all treatments, indicating difference in competiton between weed


# now reducing the full model: using a single I but different A for the two species
Cousens.model.3 = nls(yl ~ (I*densityweed)/(1+(I/A[weed])*densityweed), data=DMT, alg="port",
                      start=list(I=60, A=c(80,60)),  trace=T)
summary(Cousens.model.3, correlation=T)

anova(Cousens.model.3,Cousens.model.1)
AIC(Cousens.model.3)
RMSE(Cousens.model.3, which = NULL)
# a model with a single I and different A is not enough to describe the yield loss curve for all treatments, indicating difference in initial competiton(I) between weeds


# now reducing the full model: using a single A but different I for the two species
Cousens.model.4 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A)*densityweed), data=DMT, alg="port", 
                      start=list(I=c(30,30), A=70), trace=T)
summary(Cousens.model.4)
anova(Cousens.model.4,Cousens.model.1)
AIC(Cousens.model.4)
RMSE(Cousens.model.4, which = NULL)

# a model with a single A and different I is enough to describe the yield loss curve for all treatments, indicating difference in initial competiton(I) but no difference in final competition (A) between weeds

AIC(Cousens.model.4)

AIC(Cousens.model.1, Cousens.model.2, Cousens.model.3, Cousens.model.4)
#THE BEST MODEL IS MODEL 4!!!
RMSE=(summary(Cousens.model.1)$sigma)
RMSE
RMSE1=(summary(Cousens.model.2)$sigma)
RMSE1
RMSE2=(summary(Cousens.model.3)$sigma)
RMSE2
RMSE3=(summary(Cousens.model.4)$sigma)
RMSE3

RMSE(weed2,  which = NULL)
#Residuals
plot(resid(Cousens.model.2)~yl, DMT)
plot(Cousens.model.2)



# adding lines
x=seq(0,4,0.25)
weed1=(235.384*x)/(1+(235.384/105.432)*x)
weed2=(39.085*x)/(1+(39.085/105.432)*x)

lines(x,weed1, lty=1, lwd=5, col=1)
lines(x,weed2, lty=3, lwd=5, col=2)

comp=par(font=3)
legend("bottomright", legend=c("C. benghalensis", "R. brasiliensis"), text.font = 3, col=c(1,2), pch= c(16,1), lty=c(1,3), lwd= c(5,5), bty="n", cex=2)
#legend("topleft", legend=c("A"), text.font=1, col=1, bty="n", cex=3)

AIC()

library(nlstools)

#Premissas 
Res_Model <- nlsResiduals(Cousens.model.1)
plot(Res_Model, which = 0)
# Histogram and qq-plot
plot(Res_Model, which = 5)

plot(Res_Model, which = 6)
# Tests
test.nlsResiduals(Res_Model)










#Polymonial model

par(mar=c(5,6,2,2), pty="s", mgp=c(4,1.5,0))
plot(yl~densityweed, data=DMT, subset = weed =="1", pch=16, cex=2, las=1, 
     xlab=expression("Weed density (plants pot"^-1*")"),  ylab="Yield loss (%)", ylim =c(-10,110), cex.axis=1.8, cex.lab=2)
lines(yl~densityweed, type="p",data=DMT, subset = weed =="2", col=2, cex=2, pch=1)


DMT.m1<- lm(yl ~ I(densityweed) + I(densityweed^2), subset=weed=="1", data=DMT)
summary(DMT.m1)
RMSEPOL=(summary(DMT.m1)$sigma)
RMSEPOL


x=seq(0,4,0.25)
weed1=5.229+65.193*x-11.036*x^2


lines(x,weed1, lty=1, lwd=5, col=1)

DMT.m2<- lm(yl ~ I(densityweed) + I(densityweed^2), subset=weed=="2", data=DMT)
summary(DMT.m2)

x=seq(0,4,0.25)
weed2=-1.171+38.268*x-6.161*x^2


lines(x,weed2, lty=3, lwd=5, col=2)
legend("bottomright", legend=c("C. benghalensis", "R. brasiliensis"), text.font = 3, col=c(1,2), pch= c(16,1), lty=c(1,3), lwd= c(5,5), bty="n", cex=2)
#legend("topleft", legend="B", text.font=1, col=1, bty="n", cex=3) 
AIC(DMT.m1)
AIC(DMT.m2)
RMSE(DMT.m1, which = NULL)
RMSE(DMT.m2, which = NULL)
citation()



mse <- mean(residuals(DMT.m1)^2/df.residual(DMT.m1))
rmse <- sqrt(mse)
rmse

mse <- mean(residuals(DMT.m2)^2/df.residual(DMT.m2))
rmse <- sqrt(mse)
rmse



Linear.1<- lm(yl ~ I(densityweed), subset=weed=="1", data=DMT)
summary(Linear.1)

plot(yl~densityweed, data=DMT, subset = weed =="1", pch=16, cex=2, las=1, 
     xlab=expression("Weed Density (plants pot"^-1*")"),  yaxt='n', ylab="", ylim =c(-10,110), cex.axis=1.8, cex.lab=2)
x=seq(0,4,0.25)
weedL=36.438+12.619*x
lines(x,weedL, lty=3, lwd=5, col=2)


library(drc)
par(mar=c(5,2,2,2), mgp=c(4,1.5,0))
DRC=drm(yl~densityweed, weed, fct=l4 (fixed =c(NA,NA,NA,NA)),data=DMT)
summary(DRC)

predict(DRC)

DRC1=drm(yl~densityweed, subset=weed=="1", fct=l4 (fixed =c(NA,NA,NA,NA)),data=DMT)
DRC2=drm(yl~densityweed, subset=weed=="2", fct=l4 (fixed =c(NA,NA,NA,NA)),data=DMT)
AIC(DRC)
AIC(DRC1)
AIC(DRC2)
RMSE(DRC, which = NULL)
RMSE(DRC1, which = NULL)
RMSE(DRC2, which = NULL)

mse <- mean(residuals(DRC)^2/df.residual(DRC))
rmse <- sqrt(mse)
rmse



plot(DRC, legend="", col=c(1,2), type="all", cex=2, pch=c(16,1), ylab="Yield loss (%)",
     xlab=expression("Weed density (plants pot"^-1*")"), lwd=3,
     ylim=c(-10,110), cex.axis=1.8, cex.lab=2)
axis(1, at=seq(1,4, by=1), cex.axis=2)

par(mar=c(5,6,2,2), pty="s", mgp=c(4,1.5,0))
plot(yl~densityweed, add=T, data=DMT, subset = weed =="1", pch=16, cex=2, las=1, 
     xlab=expression("Weed Density (plants pot"^-1*")"), ylim=c(-10,110), xlim=c(0,4), yaxt='n', ylab ="", cex.axis=1.8, cex.lab=2)
lines(yl~densityweed, type="p",data=DMT, subset = weed =="2", col=2, cex=2, pch=1)
plot(DRC, add=T,type="n", lwd=5, pch=c(16,1), col=c(1,2), cex=1, legend="")
legend("bottomright", legend=c("C. benghalensis", "R. brasiliensis"), text.font = 3, col=c(1,2), pch= c(16,1), lty=c(1,3), lwd= c(5,5), bty="n", cex=2)
#legend("topleft", legend=c("C"), col=1, bty="n", text.font = 1, cex=3)











library(stats)
EXP=nls(yl~ a * exp(-k*densityweed), subset = weed=="1", data=DMT,
               start=list(a=10, k=0.01), trace=T)
EXP1=nls(yl~ a * exp(-k*densityweed), subset = weed=="2", data=DMT,
        start=list(a=10, k=0.01), trace=T)

AIC(EXP)
AIC(EXP1)
summary(EXP)
plot(EXP)
abline(h=0)

x11()
par(mar=c(5,6,2,2), pty="s", mgp=c(4,1.5,0))
plot(yl~densityweed, data=DMT, subset = weed =="1", pch=16, font.size=2, cex=2, las=1, 
     xlab=expression("Weed Density (plants pot"^-1*")"), ylim=c(-10,110), ylab = "Biomass Reduction (%)", cex.axis=1.8, cex.lab=2)
lines(yl~densityweed, type="p",data=DMT, subset = weed =="2", col=2, cex=2, pch=1)

x=seq(0,4,0.25)
weed1=38.04*exp(-0.25*x)
lines(x,weed1, lty=1, lwd=5, col=1)


