#YelpPREVA


#Yelp Prev Alpha

library(ROCR)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
library(googleVis)
library(plyr)





Yelp2<-read.csv("C:\\Python27\\Yelp Simulation2.5.csv")

length(Yelp2)

YELPMOTION<-read.csv("C:\\Python27\\YELPMOTION.csv")

Yelp2=as.data.frame(Yelp2)

summary(Yelp2)

YM <- glm( Response ~ RC+Star+dollar+ilove+oldschool+pushy+pool+affordable+Christ+stench+employees+humid+septic+Jesus+hell+dishes+thebest+highquality+adorable+fabulous+craving+favorite+excellent+service+recommend+professional+delicious+washhands+burnt+ache+pain+cigarette+asshole+awful+rotten+bathroom+toilet+puke+fuck+microwaved+shit+bitch+sucks+mold+mice+spider+exclaim+filthy+roach+DIRTY+Ifounda+clean+diarrhea+vomiting+foodpoisoning+dirty+truck+sick+stomach+hospital+fish+nausea+terrible+horrible, data=Yelp2, family=binomial )

head(Yelp2)

summary(YM)
#Yelp2<-na.exclude(Yelp2)


months=1:12

Yelp2$PRED=predict(YM, type="response")#

Yelp2$PREV=Yelp2$PRED>=0.35#States yes or no if restaurant is predicted risk

TESTPREV=Yelp2$PREV==TRUE & Yelp2$Date.13=='1/1/2013'
sum(TESTPREV)
Yelp2$PUBL=Yelp2$HCR<=80

mod=aggregate(Yelp2$PREV==TRUE,by=list((substr(Yelp2$Date.13,1,7))),sum) 
real=aggregate(Yelp2$PUBL==TRUE,by=list((substr(Yelp2$Date.13,1,7))),sum) 
mod
real


mod=as.data.frame(mod)
real=as.data.frame(real)

real=rename(real, c('Group.1'='Group.real','x'='real'))
mod=rename(mod, c('Group.1'='Group.mod', 'x'='model'))

MONTHS=c('1-13','2-13','3-13','4-13','5-13','6-13','7-13','8-13','9-13','10-13','11-13','12-13')


yv=cbind(mod,real,MONTHS)

yv=as.data.frame(yv)
yv

pchisq(yv$model,yv$real,lower.tail=FALSE)

library(rms.gof)

rms.pval(yv$model,yv$real,1000)
1 - pchisq(deviance(YM), df.residual(YM))

hosmerlem <- function (y, yhat, g = 10) {cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)),include.lowest = TRUE)
obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
chisq <- sum((obs - expect)^2 / expect)
P <- 1 - pchisq(chisq, g - 2)
c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)}

http://www.r-bloggers.com/veterinary-epidemiologic-research-glm-evaluating-logistic-regression-models-part-3/

predicted <- predict(YM)
library(ROCR)
library(ggplot2)
fpr <- unlist(slot(tprfpr, "x.values"))
roc <-data.frame(cbind(TPR, FPR))
ggplot(roc) + geom_line(aes(x = FPR, y = TPR)) + 
   geom_abline(intercept = 0, slope = 1, colour = "gray") + 
   ylab("Sensitivity") + 
   xlab("1 - Specificity")

plot(yv$model, main="Real and Predicted Prevalence of Health Code Violation",xlab='Months',ylab='Count of "High Risk" Restaurants',col='red')
points(yv$real,col="blue")
lines(months,yv$model, col='red')
lines(months,yv$real, col='blue')
legend("topright", c("Model High Risk Prevalence","SFDPH High Risk Counts"),lty=c(1,1),col=c("blue","red"))


Line2 <- gvisLineChart(yv, 'months', c('model','real'),
                       options=list(
                         series="[{targetAxisIndex: 0},
                                 {targetAxisIndex:1}]",
                         vAxes="[{title:'Predicted Health Code Violations'}, {title:'Reported Health Code Violations'}]"
				 ))
plot(Line2)


HCVMap <- gvisMap(Yelp2, "latlong" , "PREV", 
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
plot(HCVMap)


######NEED TO RESHAPE DATA

library(reshape)
YELPMOTION<-read.csv("C:\\Python27\\YELPMOTION2.csv")


head(YELPMOTION)

 YELPMOTION$Date.13 <- strptime(as.character(YELPMOTION$Date.13), "%m/%d/%Y")
 YELPMOTION$Date.13 <- as.date( YELPMOTION$Date.13)

YELPMOTION$DateF <- as.Date(YELPMOTION$DateF,
  format = "%m/%d/%Y")

YELPMOTION <- melt(YELPMOTION, id=c("ID","DateF"))
YelpM=melt(Yelp2,id=c("ID","DateF"))

M <- gvisMotionChart(YELPMOTION, idvar="ID", timevar="DateF",
                     options=list(width=700, height=600))
 
## Display the chart in your browser
plot(M)


##Now Check predictive power without looping ie simulated Using data with Stars data:
# Fitted probabilities:
#I decide on YM first
probs = fitted(YM)
length(probs)
Yelp2<-na.exclude(Yelp2)


# Proportion in sample with low HCR
mean(Yelp2$HCR<80,na.rm=TRUE)
# Use 0.264 as cutoff to calculate sensitivity and specificity:
# pred prob > 0.36 -> 1; <= 0.36 -> 0
preds = as.numeric(probs>0.2675)
length(preds)
length(Yelp2$Response)

table(Yelp2$Response,preds)
#look at sens spec
library(ROCR)
pred.obj = prediction(probs,Yelp2$Response)
plot(performance(pred.obj,"tpr","fpr"),main="ROC CURVE San Francisco Yelp Dataset 1ST HALF")
plot(performance(pred.obj,"ppv"),main="PPV San Francisco Yelp Dataset 1st HALF",ylim=c(0,1))
performance(pred.obj,"auc")
TPR=unlist(performance(pred.obj, "tpr"))
FPR=unlist(performance(pred.obj, "fpr"))


PPV=(performance(pred.obj,"ppv")@y.values)
mean(as.numeric(unlist(PPV)),na.rm=TRUE)






