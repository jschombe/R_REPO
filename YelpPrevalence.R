#Yelp Prevalence Plotting for year 2014
library(ROCR)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
library(googleVis)
library(plyr)

Yelp$Usefuness=as.numeric.factor(Yelp$Usefulness)
Yelp$latlong = Yelp$gpslat:Yelp$gpslong
Yelp$VMAP= Yelp$latlong
Yelp<-read.csv("C:\\Python27\\YELPPREV.csv")
head(Yelp)

Yelp$HCR=as.numeric.factor(Yelp$HCR)

Yelp<-na.omit(Yelp)

YM <- glm( ResponseP~ as.numeric.factor(Usefulness) + Star+Star*as.numeric.factor(Usefulness)+as.numeric.factor(dollar)+Star*as.numeric.factor(dollar)+Star*as.numeric.factor(RC)+as.numeric.factor(RC)+affordable+septic+hell+dishes+fabulous+favorite+excellent+service+recommend+professional+delicious+burnt+ache+pain+cigarette+asshole+awful+rotten+bathroom+toilet+microwaved+shit+bitch+sucks+mold+mice+spider+exclaim+filthy+roach+DIRTY+clean+diarrhea+vomit+food.poisoning+dirty+truck+sick+stomach+hospital+fish+nausea+terrible+horrible, data=Yelp, family=binomial,na.action = na.exclude )
summary(YM)

Yelp$PRED=predict(YM, type="response")#


Yelp$PREV=Yelp$PRED>=.35#States yes or no if restaurant is predicted risk
sum(Yelp$PREV)

Yelp$PREV


Yelp$PUBL=Yelp$HCR<=80

mod1=aggregate(Yelp$PREV==TRUE,by=list((substr(Yelp$Date,1,7))),sum) 
real1=aggregate(Yelp$PUBL==TRUE,by=list((substr(Yelp$Date,1,7))),sum) 

mod1
real1

mod1=as.data.frame(mod1)
real1=as.data.frame(real1)

mod1
real1

real1=rename(real1, c('Group.1'='Group.real','x'='real'))
mod1=rename(mod1, c('Group.1'='Group.mod', 'x'='model'))

real1
mod1

Months=1:24
months=13:24
MONTHS=c('1-14','2-14','3-14','4-14','5-14','6-14','7-14','8-14','9-14','10-14','11-14','12-14')
yv1=cbind(mod1,real1,MONTHS)

yv1=as.data.frame(yv1)
yv1


yv3=rbind(yv,yv1)
yv3
yv3=as.data.frame(yv3)
yv3
plot(yv1$model, main="Real and Predicted Prevalence of Health Code Violation",xlab='Months',ylab='Count of "High Risk" Restaurants',col='red')
points(yv1$real,col="bue")
lines(months,yv1$model, col='red')
lines(months,yv1$real, col='blue')
legend("topright", c("Model High Risk Prevalence","SFDPH High Risk Counts"),lty=c(1,1),col=c("blue","red"))

plot(yv3$model, main="Real and Predicted Prevalence of Health Code Violation",xlab='Months',ylab='Count of "High Risk" Restaurants',col='red')
points(yv3$real,col="blue")
lines(Months,yv3$model, col='red')
lines(Months,yv3$real, col='blue')
legend("topleft", c("Model High Risk Prevalence","SFDPH High Risk Counts"),lty=c(1,1),col=c("blue","red"))




Line2 <- gvisLineChart(yv1, 'months', c('model','real'),
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




Line3 <- gvisLineChart(yv3, 'MONTHS', c('model','real'),
                       options=list(
                         series="[{targetAxisIndex: 0},
                                 {targetAxisIndex:1}]",
                         vAxes="[{title:'Predicted Health Code Violations'}, {title:'Reported Health Code Violations'}]"
				 ))
plot(Line3)

http://127.0.0.1:17306/custom/googleVis/LineChartID79305523a38.html




mod1
plot(as.numeric.factor(mod1)~months)

months=1:12
points(mod1)
real

data(Yelp)
use(Yelp)                                       
aggregate.plot(x=Date, by=list(LHR = PUBL))
aggregate.plot(x=year, by=list(HOSPITAL = hospital, STAGE = stage), return = TRUE)
aggregate.plot(x=year, by=list(HOSPITAL = hospital, STAGE = stage), error="sd")
aggregate.plot(x=year, by=list(HOSPITAL = hospital, STAGE = stage), error="ci")



plot(mod$Group.1~mod$x)
plot(real)



dm <- read.table(text = Lines, header = TRUE)
Yelp$Date <- as.Date(Yelp$Dateinsp.14, "%m/%d/%Y")
plot(sum(ResponseP) ~ Date, Yelp, xaxt = "n", type = "l")
axis(1, Yelp$Date, format(Yelp$Date, "%b %d"), cex.axis = .7)

barplot(Yelp$PREV==TRUE ~ Date, Yelp)

plot(Yelp$PREV==TRUE ~Date, )
##Now Check predictive power without looping ie simulated Using data with Stars data:
# Fitted probabilities:
#I decide on YM first
probs = fitted(YM)
# Proportion in sample with low HCR
mean(Yelp$HCR<80,na.rm=TRUE)
# Use 0.264 as cutoff to calculate sensitivity and specificity:
# pred prob > 0.36 -> 1; <= 0.36 -> 0
preds = as.numeric(probs>0.2675)
table(Yelp$ResponseP,preds)
#look at sens spec
library(ROCR)
pred.obj = prediction(probs,Yelp$ResponseP)
plot(performance(pred.obj,"tpr","fpr"),main="ROC CURVE San Francisco Yelp Dataset 1ST HALF")
plot(performance(pred.obj,"ppv"),main="PPV San Francisco Yelp Dataset 1st HALF",ylim=c(0,1))
performance(pred.obj,"auc")
PPV=(performance(pred.obj,"ppv")@y.values)
mean(as.numeric(unlist(PPV)),na.rm=TRUE)




