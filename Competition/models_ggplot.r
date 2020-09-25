---
title: "Models"
author: "Maxwel C. Oliveira"
date: "October 26, 2016"
output: word_document
---
```{r setup, eacho=False, include = FALSE}
library(drc)
library(ggplot2)
library(stats)
```


This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r loadData, echo=FALSE, warning=FALSE}
Data=read.csv("dmtotal.csv")
str(Data)
names(Data)
Data$weed= as.factor(Data$weed) 
```

You can also embed plots, for example:

```{r, echo=FALSE}
ggplot(Data, aes(x=densityweed, y=yl)) + 
  geom_jitter(aes(color=weed, shape=weed))
```






```{r models, echo=FALSE, warning=FALSE}
Cousens.model.1 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A[weed])*densityweed), alg="port", data=Data, start=list(I=c(60,30), A=c(80,60)), trace=T)

summary(Cousens.model.1, correlation=T)
RMSE(Cousens.model.1, which = NULL)
AIC(Cousens.model.1)
```


```{r reduced model 1, echo=FALSE, warning=FALSE}
Cousens.model.2 = nls(yl ~ (I*densityweed)/(1+(I/A)*densityweed), data=Data, alg="port", start=list(I=40, A=80),  trace=T)

summary(Cousens.model.2, correlation=T)
AIC(Cousens.model.2)
RMSE(Cousens.model.2, which = NULL)
```

```{r testing hypothesis 1, echo=FALSE, warning=FALSE}
anova(Cousens.model.2,Cousens.model.1)
```

```{r reduced model 2, echo=FALSE, warning=FALSE}
Cousens.model.3 = nls(yl ~ (I*densityweed)/(1+(I/A[weed])*densityweed), data=Data, alg="port", start=list(I=60, A=c(80,60)),  trace=T)

summary(Cousens.model.3, correlation=T)
AIC(Cousens.model.3)
RMSE(Cousens.model.3, which = NULL)
```

```{r testing hypothesis 2, echo=FALSE, warning=FALSE}
anova(Cousens.model.3,Cousens.model.1)
```

```{r reduced model 3, echo=FALSE, warning=FALSE}
Cousens.model.4 = nls(yl ~ (I[weed]*densityweed)/(1+(I[weed]/A)*densityweed), data=Data, alg="port",start=list(I=c(30,30), A=70), trace=T)

summary(Cousens.model.4)
AIC(Cousens.model.4)
RMSE(Cousens.model.4, which = NULL)
```

```{r testing hypothesis 3, echo=FALSE, warning=FALSE}
anova(Cousens.model.4,Cousens.model.1)
```


```{r AIC comparison, echo=FALSE, warning=FALSE}
AIC(Cousens.model.1, Cousens.model.2, Cousens.model.3, Cousens.model.4)
```


```{r adding lines plots, echo=FALSE, warning=FALSE}
x=seq(0,4,0.25)
weed1=(159.896*x)/(1+(159.896/109.732)*x)
weed2=(56.432*x)/(1+(56.432/109.732)*x)

lines(x,weed1, lty=1, lwd=5, col=1)
lines(x,weed2, lty=3, lwd=5, col=2)
```





```{r, echo=FALSE}
qplot(densityweed, yl, data=Data, color=weed)
qplot(densityweed, yl, data=Data, shape=weed)
qplot(densityweed, yl, data=Data, alpha=weed)

qplot(densityweed, yl, data=Data, color=weed, geom = c("point", "smooth"))                         

```


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
