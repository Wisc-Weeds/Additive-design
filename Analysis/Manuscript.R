
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################

# analysis for manuscript - TOTAL DRY MATTER:
install.packages(nls)
#Load the workbook
DMT=read.csv("dmshoot.csv")
names(DMT)
str(DMT)
head(DMT)

x11()
par(mfrow=c(3,1))
par(mar=c(5,5,2,2), mgp=c(4,1.5,0))


par(mar=c(5,5,2,2), pty="s", mgp=c(4,1.5,0))
plot(yl~densityweed, data=DMT, subset = weed =="1", pch=16, font.size=2, cex=2, las=1, 
     xlab=expression("Densidade (plantas vaso"^-1*")"), ylim=c(-10,110), ylab = "Redução biomassa total (%)", cex.axis=1.8, cex.lab=2)
lines(yl~densityweed, type="p",data=DMT, subset = weed =="2", col=2, cex=2, pch=1)


### 1st model - Cousen's model 1985 - Rectangular hyperbola with 2 parameters

# fitting a general model - parameter for each treat
Cousens.model.1 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A[weed])*densityweed), alg="port", data=DMT, 
                      start=list(I=c(60,30), A=c(80,60)), trace=T)
summary(Cousens.model.1, correlation=T)


# now reducing the full model: using a single I and A for the two species
Cousens.model.2 = nls(yl ~ (I*densityweed)/(1+(I/A)*densityweed), data=DMT, alg="port",
                      start=list(I=40, A=80),  trace=T)
summary(Cousens.model.2, correlation=T)

anova(Cousens.model.2,Cousens.model.1)

confint(Cousens.model.2, level=0.95)


# a single model is not enough to describe the yield loss curve for all treatments, indicating difference in competiton between weed


# now reducing the full model: using a single I but different A for the two species
Cousens.model.3 = nls(yl ~ (I*densityweed)/(1+(I/A[weed])*densityweed), data=DMT, alg="port",
                      start=list(I=60, A=c(80,60)),  trace=T)
summary(Cousens.model.3, correlation=T)

anova(Cousens.model.3,Cousens.model.1)

# a model with a single I and different A is not enough to describe the yield loss curve for all treatments, indicating difference in initial competiton(I) between weeds


# now reducing the full model: using a single A but different I for the two species
Cousens.model.4 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A)*densityweed), data=DMT, alg="port", 
                      start=list(I=c(30,30), A=70), trace=T)
summary(Cousens.model.4, correlation=T)
anova(Cousens.model.4,Cousens.model.1)


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


#Residuals
plot(resid(Cousens.model.2)~yl, DMT)
plot(Cousens.model.2)



# adding lines
x=seq(0,4,0.25)
weed1=(159.896*x)/(1+(159.896/109.732)*x)
weed2=(56.432*x)/(1+(56.432/109.732)*x)

lines(x,weed1, lty=1, lwd=5, col=1)
lines(x,weed2, lty=3, lwd=5, col=2)

comp=par(font=3)
legend("bottomright", legend=c("C. benghalensis", "R. brasiliensis"), text.font = 3, col=c(1,2), pch= c(16,1), lty=c(1,3), lwd= c(5,5), bty="n", cex=2)
par(comp) 
#legend("topleft", legend=c("Trapoeraba: y = [(209.294*x)/(1+(209.294/105.581)*x)]", "Poaia: y = [(28.539*x)/(1+(28.539/105.581)*x)]"), cex=1.0, bty="n")
# just to see if A and I were signif. different:
#summary(Cousens.model.1)
# adding lines
#x=seq(0,4,0.25)
#weed1=(197.04*x)/(1+(197.04/107.41)*x)
#lines(x,weed1, lty=1, lwd=2, col=1)
#weed2=(38.38*x)/(1+(38.38/ 77.97)*x)
#lines(x,weed2, lty=2, lwd=2, col=1)


#########################################################################################################################################################################

#Height

citation()

# analysis for manuscript - TOTAL DRY MATTER:

#Load the workbook
Height=read.csv("height.csv")
names(Height)
str(Height)


par(mar=c(5,5,2,2), pty="s", mgp=c(4,1.5,0))
plot(yl~densityweed, data=Height, subset = weed =="1", pch=16, font.size=2, cex=2, las=1, 
     xlab=expression("Densidade (plantas vaso"^-1*")"), ylim=c(-10,110), ylab = "Redução de altura (%)", cex.axis=1.8, cex.lab=2)
lines(yl~densityweed, type="p",data=Height, subset = weed =="2", col=2, cex=2, pch=1)


### 1st model - Cousen's model 1985 - Rectangular hyperbola with 2 parameters

# fitting a general model - parameter for each treat
Cousens.model.1 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A[weed])*densityweed), alg="port", data=Height, 
                      start=list(I=c(60,30), A=c(80,60)), trace=T)
summary(Cousens.model.1, correlation=T)


# now reducing the full model: using a single I and A for the two species
Cousens.model.2 = nls(yl ~ (I*densityweed)/(1+(I/A)*densityweed), data=Height, alg="port",
                      start=list(I=40, A=80),  trace=T)
summary(Cousens.model.2, correlation=T)

anova(Cousens.model.2,Cousens.model.1)

confint(Cousens.model.2, level=0.95)


# a single model is not enough to describe the yield loss curve for all treatments, indicating difference in competiton between weed


# now reducing the full model: using a single I but different A for the two species
Cousens.model.3 = nls(yl ~ (I*densityweed)/(1+(I/A[weed])*densityweed), data=Height, alg="port",
                      start=list(I=60, A=c(80,60)),  trace=T)
summary(Cousens.model.3, correlation=T)

anova(Cousens.model.3,Cousens.model.1)

# a model with a single I and different A is not enough to describe the yield loss curve for all treatments, indicating difference in initial competiton(I) between weeds


# now reducing the full model: using a single A but different I for the two species
Cousens.model.4 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A)*densityweed), data=Height, alg="port", 
                      start=list(I=c(30,30), A=70), trace=T)
summary(Cousens.model.4, correlation=T)
anova(Cousens.model.4,Cousens.model.1)


# a model with a single A and different I is enough to describe the yield loss curve for all treatments, indicating difference in initial competiton(I) but no difference in final competition (A) between weeds



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


#Residuals
plot(resid(Cousens.model.2)~yl, DMT)
plot(Cousens.model.2)



# adding lines
x=seq(0,4,0.25)
weed1=(71.011*x)/(1+(71.011/89.86)*x)
weed2=(32.137*x)/(1+(32.137/89.86)*x)

lines(x,weed1, lty=1, lwd=5, col=1)
lines(x,weed2, lty=3, lwd=5, col=2)

comp=par(font=3)
legend("bottomright", legend=c("C. benghalensis", "R. brasiliensis"), text.font = 3, col=c(1,2), pch= c(16,1), lty=c(1,3), lwd= c(5,5), bty="n", cex=2)
par(comp)

#########################################################################################################################################################################

#Area FOliar


# analysis for manuscript - Area Foliar:

#Load the workbook
LA=read.csv("leafarea.csv")
names(LA)
str(LA)


par(mar=c(5,5,2,2), pty="s", mgp=c(4,1.5,0))
plot(yl~densityweed, data=LA, subset = weed =="1", pch=16, font.size=2, cex=2, las=1, 
     xlab=expression("Densidade (plantas vaso"^-1*")"), ylim=c(-10,110), ylab = "Redução de área foliar (%)", cex.axis=1.8, cex.lab=2)
lines(yl~densityweed, type="p",data=LA, subset = weed =="2", col=2, cex=2, pch=1)


### 1st model - Cousen's model 1985 - Rectangular hyperbola with 2 parameters

# fitting a general model - parameter for each treat
Cousens.model.1 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A[weed])*densityweed), alg="port", data=LA, 
                      start=list(I=c(60,30), A=c(80,60)), trace=T)
summary(Cousens.model.1, correlation=T)


# now reducing the full model: using a single I and A for the two species
Cousens.model.2 = nls(yl ~ (I*densityweed)/(1+(I/A)*densityweed), data=LA, alg="port",
                      start=list(I=40, A=80),  trace=T)
summary(Cousens.model.2, correlation=T)

anova(Cousens.model.2,Cousens.model.1)

confint(Cousens.model.2, level=0.95)


# a single model is not enough to describe the yield loss curve for all treatments, indicating difference in competiton between weed


# now reducing the full model: using a single I but different A for the two species
Cousens.model.3 = nls(yl ~ (I*densityweed)/(1+(I/A[weed])*densityweed), data=LA, alg="port",
                      start=list(I=60, A=c(80,60)),  trace=T)
summary(Cousens.model.3, correlation=T)

anova(Cousens.model.3,Cousens.model.1)

# a model with a single I and different A is not enough to describe the yield loss curve for all treatments, indicating difference in initial competiton(I) between weeds


# now reducing the full model: using a single A but different I for the two species
Cousens.model.4 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A)*densityweed), data=LA, alg="port", 
                      start=list(I=c(30,30), A=70), trace=T)
summary(Cousens.model.4, correlation=T)
anova(Cousens.model.4,Cousens.model.1)


# a model with a single A and different I is enough to describe the yield loss curve for all treatments, indicating difference in initial competiton(I) but no difference in final competition (A) between weeds



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


#Residuals
plot(resid(Cousens.model.2)~yl, DMT)
plot(Cousens.model.2)



# adding lines
x=seq(0,4,0.25)
weed1=(130.755*x)/(1+(130.755/112.479)*x)
weed2=(36.381*x)/(1+(36.381/112.479)*x)

lines(x,weed1, lty=1, lwd=5, col=1)
lines(x,weed2, lty=3, lwd=5, col=2)

comp=par(font=3)
legend("bottomright", legend=c("C. benghalensis", "R. brasiliensis"), text.font = 3, col=c(1,2), pch= c(16,1), lty=c(1,3), lwd= c(5,5), bty="n", cex=2)




# analysis for manuscript - Altura:

#Load the workbook
Height=read.csv("height.csv")
names(Height)
str(Height)


par(mar=c(5,5,2,2), pty="s", mgp=c(4,1.5,0))
plot(yl~densityweed, data=Height, subset = weed =="1", pch=16, font.size=2, cex=2, las=1, 
     xlab=expression("Densidade (plantas vaso"^-1*")"), ylim=c(-10,110), ylab = "Redução de área foliar (%)", cex.axis=1.8, cex.lab=2)
lines(yl~densityweed, type="p",data=Height, subset = weed =="2", col=2, cex=2, pch=1)


### 1st model - Cousen's model 1985 - Rectangular hyperbola with 2 parameters

# fitting a general model - parameter for each treat
Cousens.model.1 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A[weed])*densityweed), alg="port", data=Height, 
                      start=list(I=c(60,30), A=c(80,60)), trace=T)
summary(Cousens.model.1, correlation=T)


# now reducing the full model: using a single I and A for the two species
Cousens.model.2 = nls(yl ~ (I*densityweed)/(1+(I/A)*densityweed), data=Height, alg="port",
                      start=list(I=40, A=80),  trace=T)
summary(Cousens.model.2, correlation=T)

anova(Cousens.model.2,Cousens.model.1)

confint(Cousens.model.2, level=0.95)


# a single model is not enough to describe the yield loss curve for all treatments, indicating difference in competiton between weed


# now reducing the full model: using a single I but different A for the two species
Cousens.model.3 = nls(yl ~ (I*densityweed)/(1+(I/A[weed])*densityweed), data=Height, alg="port",
                      start=list(I=60, A=c(80,60)),  trace=T)
summary(Cousens.model.3, correlation=T)

anova(Cousens.model.3,Cousens.model.1)

# a model with a single I and different A is not enough to describe the yield loss curve for all treatments, indicating difference in initial competiton(I) between weeds


# now reducing the full model: using a single A but different I for the two species
Cousens.model.4 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A)*densityweed), data=Height, alg="port", 
                      start=list(I=c(30,30), A=70), trace=T)
summary(Cousens.model.4, correlation=T)
anova(Cousens.model.4,Cousens.model.1)


# a model with a single A and different I is enough to describe the yield loss curve for all treatments, indicating difference in initial competiton(I) but no difference in final competition (A) between weeds



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


#Residuals
plot(resid(Cousens.model.2)~yl, DMT)
plot(Cousens.model.2)



# adding lines
x=seq(0,4,0.25)
weed1=(71.011*x)/(1+(71.011/89.86)*x)
weed2=(32.137*x)/(1+(32.137/89.86)*x)

lines(x,weed1, lty=1, lwd=5, col=1)
lines(x,weed2, lty=3, lwd=5, col=2)

comp=par(font=3)
legend("bottomright", legend=c("C. benghalensis", "R. brasiliensis"), text.font = 3, col=c(1,2), pch= c(16,1), lty=c(1,3), lwd= c(5,5), bty="n", cex=2)



cand.mods=list()
