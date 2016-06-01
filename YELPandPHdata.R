library(MASS)
as.number <- function(x) {as.numeric(levels(x))[x]}



YPH=read.csv("C:\\Users\\John\\Dropbox\\Yelp\\YPHI.csv")
YPH=read.csv("C:\\Documents and Settings\\jschombe\\My Documents\\Dropbox\\Yelp\\YPHI.csv")
YPH=read.csv("C:\\Users\\John\\Dropbox\\Yelp\\PHY.csv")
YPH=read.csv("C:\\Documents and Settings\\jschombe\\My Documents\\Dropbox\\Yelp\\PHY.csv")
plot(YPH$STARS, YPH$Phrate)
plot(YPH$vermin, YPH$Phrate)
plot(YPH$washhand, YPH$Phrate)

sum(YPH$NotWhite)
summary(YPH$NotWhite)
sum(YPH$Older)
summary(YPH$Older)
sum(YPH$HADFBI)
summary(YPH$HADFBI)
head(YPH)

# aggregate data frame  returning means
# for numeric variables

aggdata <-aggregate(YPH, by=list(YPH$HADFBI), 
  FUN=mean, na.rm=TRUE)
print(aggdata)
detach(mtcars)

# independent 2-group t-test
t.test(FBIY$STARS,FBIN$STARS) # where y1 and y2 are numeric
t.test(FBIYYW$STARS,FBINYW$STARS) # where y1 and y2 are numeric


t.test(FBIYYB$Phrate,FBINYB$Phrate) # where y1 and y2 are numeric
t.test(FBIYYW$Phrate,FBINYW$Phrate) # where y1 and y2 are numeric
t.test(WHITE$Phrate,MINOR$Phrate)
t.test(Male$Phrate,Female$Phrate)
t.test(FBIY$Phrate,FBIN$Phrate)
t.test(MILL$Phrate,NILL$Phrate)
t.test(OLD$Phrate,YOUNG$Phrate)
t.test(Male$Phrate,Female$Phrate)

t.test(FBIYYB$STARS,FBINYB$STARS) # where y1 and y2 are numeric
t.test(FBIYYW$STARS,FBINYW$STARS) # where y1 and y2 are numeric
t.test(FBIYYB$Reviews,FBINYB$Reviews) # where y1 and y2 are numeric
t.test(FBIYYW$Reviews,FBINYW$Reviews) # where y1 and y2 are numeric
t.test(WHITE$STARS,MINOR$STARS)
t.test(Male$STARS,Female$STARS)
t.test(FBIY$STARS,FBIN$STARS)
t.test(MILL$STARS,NILL$STARS)
t.test(OLD$STARS,YOUNG$STARS)
t.test(Male$STARS,Female$STARS)







YY=as.data.frame(subset(YPH, YPH$YELPYN=="Yes"))
YN=as.data.frame(subset(YPH, YPH$YELPYN=="No"))

t.test(YY$STARS,YN$STARS)
t.test(YY$Phrate,YN$Phrate)
t.test(YY$Reviews,YN$Reviews)
t.test(WHITE$Reviews,MINOR$Reviews)
t.test(Male$Reviews,Female$Reviews)
t.test(FBIY$Reviews,FBIN$Reviews)
t.test(MILL$Reviews,NILL$Reviews)
t.test(YOUNG$Reviews,OLD$Reviews)




chisq.test(mytable) 
  

hist(FBIYYB$Phrate)
hist(FBINYB$Phrate)

summary(FBIYYB)
summary(FBINYB)
summary(FBINYW)
summary(FBIYYW)







mytable <- table(YPH$Gender, YPH$Older, YPH$RACE) 
#DIFFERENCE in RACE
#DIFFERENCE in Older


ftable(mytable)

FBIY=as.data.frame(subset(YPH, YPH$HADFBI==TRUE))
FBIN=as.data.frame(subset(YPH, YPH$HADFBI==FALSE))

FBIYO=as.data.frame(subset(FBIY, FBIY$Older==TRUE))
FBIYY=as.data.frame(subset(FBIY, FBIY$Older==FALSE))
FBINO=as.data.frame(subset(FBIN, FBIN$HADFBI==TRUE))
FBINY=as.data.frame(subset(FBIN, FBIN$HADFBI==FALSE))

FBIYOW=as.data.frame(subset(FBIYO, FBIYO$RACE=='White'))
FBIYOB=as.data.frame(subset(FBIYO, FBIYO$RACE=='Minority'))
FBIYYW=as.data.frame(subset(FBIYY, FBIYY$RACE=='White'))
FBIYYB=as.data.frame(subset(FBIYY, FBIYY$RACE=='Minority'))
FBINOW=as.data.frame(subset(FBINO, FBINO$RACE=='White'))
FBINOB=as.data.frame(subset(FBINO, FBINO$RACE=='Minority'))
FBINYW=as.data.frame(subset(FBINY, FBINY$RACE=='White'))
FBINYB=as.data.frame(subset(FBINY, FBINY$RACE=='Minority'))

VERMINY=as.data.frame(subset(YPH, YPH$Vermin==TRUE))
VERMINN=as.data.frame(subset(YPH, YPH$Vermin==FALSE))

MILL=as.data.frame(subset(YPH, YPH$FBIO==TRUE))
NILL=as.data.frame(subset(YPH, YPH$FBIO==FALSE))

YOUNG=as.data.frame(subset(YPH, YPH$Older=='Old'))
OLD=as.data.frame(subset(YPH, YPH$Older=='Young'))

WHITE=as.data.frame(subset(YPH, YPH$RACE=='White'))

MINOR=as.data.frame(subset(YPH, YPH$RACE=='Minority'))

Male=as.data.frame(subset(YPH, YPH$Gender=='Male'))
Female=as.data.frame(subset(YPH, YPH$Gender=='Female'))

summary(YPH)
nrow(FBIYOW)
nrow(FBIYOB)
nrow(FBIYYW)
nrow(FBIYYB)
nrow(FBINOW)
nrow(FBINOB)
nrow(FBINYW)
nrow(FBINYB)











USERS=as.data.frame(subset(YPH, YPH$Useyelp=='1'))
NUSERS=as.data.frame(subset(YPH, YPH$Useyelp=='0'))

head(YPH)
head(USERS)
head(NUSERS)
nrow(USERS)
nrow(NUSERS)

S1=as.data.frame(subset(YPH, YPH$STARS=='1'))
S2=as.data.frame(subset(YPH, YPH$STARS=='2'))
S3=as.data.frame(subset(YPH, YPH$STARS=='3'))
S4=as.data.frame(subset(YPH, YPH$STARS=='4'))
S5=as.data.frame(subset(YPH, YPH$STARS=='5'))


S3$Phrate=na.exclude(S3$Phrate)

mean(S1$Phrate)
mean(S2$Phrate)
mean(S3$Phrate, na.rm=TRUE)
mean(S4$Phrate)
mean(S5$Phrate)


mean(S1$Phrate)

mt=table(YPH$STARS, YPH$YELPYN)

barplot(table(YPH$PHonYelp=='Yes', YPH$YELPYN==TRUE),beside=TRUE)
barplot(table(YPH$PHonYelp, YPH$Gender),beside=TRUE)
barplot(table(YPH$PHonYelp, YPH$RACE),beside=TRUE)
barplot(table(YPH$PHonYelp, YPH$Older),beside=TRUE)
barplot(table(YPH$HADFBI,YPH$PHonYelp),beside=TRUE)
barplot(table(YPH$HADFBI))
barplot(table(YPH$PHonYelp),col=c("blue","red"), main = "Respondents Reporting Knowledge of Public Health Data on Yelp.com")


counts <- table(S1$Phrate, S2$Phrate,S3$Phrate,S4$Phrate,S5$Phrate)
barplot(YPH, main="Car Distribution by Gears and VS",
  xlab="Number of Gears", col=c("red","orange", "green","blue","green","violet"),
  legend = rownames(counts), beside=TRUE)

plot(as.number(YPH$STARS),as.number(YPH$Phrate))

barplot(mean(as.number(S1$Phrate)),mean(as.number(S2$Phrate)),mean(as.number(S3$phrate)),mean(as.number(S4$phrate)),mean(as.number(S5$phrate)))

YTAB <- xtabs(~Age+Gender+RACE2, data=USERS)
NTAB <- xtabs(~Age+Gender+RACE2, data=NUSERS)
YTAB
NTAB
prop.table(YTAB, 1) # row percentages 
prop.table(YTAB, 2) # column percentages
prop.table(NTAB, 1) # row percentages 
prop.table(NTAB, 2) # column percentages
library(MASS)





plot(YPH$RACE)

library(mass)

plot(YPH$Gender)

plot(YPH$Age)

plot(YPH$effort)


head(YPH)

summary(YPH)

ncol(YPH)

nrow(YPH)

 YPH$Gender=factor(YPH$What.is.Your.Gender.)

head(YPH$Gender)

Male=sum(YPH$Gender=="Male")
Female=sum(YPH$Gender=="Female")
mytable <- table(iris$Species)
 lbls <- paste(names(YPH$Gender), "\n", YPH$Gender, sep="")
 pie(factor(YPH$Gender)) 

head(YPH$Gender)

slices <- c(Male,Female)
 lbls <- c("Male", "Female")
 pie(slices, labels = lbls, main="Pie Chart of Participation by Gender")


mod1<-glm(fivestar ~ Older+Gender+RACE+STARG2+as.numeric(FREQ)+as.numeric(Phrate)+as.numeric(Phrate),data=YPH, family=binomial,na.action = na.exclude )
summary(mod1)

mod1y<-glm(fivestar ~ Gender+ RACE+HADFBI+as.numeric(FREQ)+as.numeric(STARS)+as.numeric(Phrate)+as.numeric(Reviews),data=YOUNG, family=binomial,na.action = na.exclude )
summary(mod1y)
summary(YOUNG)

mod1o<-glm(fivestar ~ Gender+ RACE+HADFBI+as.numeric(FREQ)+as.numeric(STARS)+as.numeric(Phrate)+as.numeric(Reviews)+as.numeric(FREQ)*as.numeric(STARS),data=OLD, family=binomial,na.action = na.exclude )
summary(mod1o)
exp(confint(mod1o))


mod2<-glm(perfhealth ~ Hispanic+Older+Gender+ RACE+FBIHX+Chinese+as.numeric(FREQ)+as.numeric(STARS)+YELPYN+as.numeric(Phrate)+as.numeric(Reviews)+as.numeric(STARS)*as.numeric(FREQ),data=YPH, family=binomial,na.action = na.exclude )
summary(mod2)

mod2y<-glm(perfhealth ~ Hispanic+Gender+ RACE+FBIHX+Chinese+as.numeric(FREQ)+as.numeric(STARS)+YELPYN+as.numeric(Phrate),data=YOUNG, family=binomial,na.action = na.exclude )
summary(mod2y)



mod3<-glm(Violyn ~ Older+Gender+ RACE+HADFBI++as.numeric(FREQ)+as.numeric(STARS)+YELPYN+as.numeric(Phrate),data=YPH, family=binomial,na.action = na.exclude )
summary(mod3)

mod4=glm(HADFBI~ Chinese+FastFood+Mexican+Indian+Vietnamese+Thai+French+Italian+Older+Gender+ RACE+as.numeric(FREQ)+as.numeric(STARS)+YELPYN+as.numeric(Phrate)+as.numeric(Reviews),data=YPH, family=binomial,na.action = na.exclude )
summary(mod4)
exp(confint(mod4))
mod5=glm(FBIO~ Chinese+FastFood+Mexican+Indian+Vietnamese+Thai+French+Italian+Older+Gender+ RACE+as.numeric(FREQ)+as.numeric(STARS)+YELPYN+as.numeric(Phrate)+as.numeric(Reviews)+YELPYN*as.numeric(Phrate),data=YPH, family=binomial,na.action = na.exclude )
summary(mod5)
exp(confint(mod5))
mod6=glm(FBIO~ Chinese+FastFood+Mexican+Indian+Vietnamese+Thai+French+Italian+Younger+Gender+ RACE+as.numeric(FREQ)+as.numeric(STARS)+YELPYN+as.numeric(Phrate)+as.numeric(Reviews)+Younger*STARS,data=YPH, family=binomial,na.action = na.exclude )
summary(mod6)

summary(YPH$FBIO)
summary(YPH$FBIHX)


mod2<-glm(perfhealth ~ as.factor(Age)+Gender+ White+Asian+Black+Latino+as.factor(FT)+FBIHX+Chinese+Fast.Food+Italian+French+as.numeric(FREQ)+as.numeric(STARS)+Useyelp,data=YPH, family=binomial,na.action = na.exclude )
summary(mod2)
mod3<-glm(perfhealth ~ as.factor(Age)+as.factor(Gender)+ White+Asian+Black+Latino+as.factor(FT)+FBIHX+Chinese+Fast.Food+Italian+French+as.numeric(FREQ)+as.numeric(STARS)+Useyelp,data=YPH, family=binomial,na.action = na.exclude )
summary(mod2)

fbimod<-glm(fbi ~ as.factor(Age)+as.factor(Gender)+ White+Asian+Black+Latino+as.factor(FT)+Chinese+Fast.Food+Italian+French+as.numeric(FREQ)+Useyelp+as.factor(vermin)+as.factor(washhand)+as.factor(perfhealth)+as.number(fivestar)+as.number(STARS)+as.number(phrate),data=YPH, family=binomial,na.action = na.exclude )
summary(fbimod)
exp(confint(fbimod))

fbimod2<-glm(fbi ~ as.factor(Age)+as.factor(Gender)+ RACE2+as.factor(FT)+Chinese+Fast.Food+Italian+French+Japanese+Indian+Thai+Lebanese+Mexican+Vietnamese+Greek+as.numeric(FREQ)+Useyelp+Yelper+Yelps+Yelps*as.numeric(FREQ)+as.number(STARS)+as.number(phrate)+as.numeric(FREQ)+as.number(STARS)*RACE2,data=YPH, family=binomial,na.action = na.exclude)
summary(fbimod2)
exp(confint(fbimod2))


FBIMOD=glm(HADFBI~ Older+Gender+RACE+STARS+YELPYN+as.numeric(Phrate)+YELPERYN+as.numeric(Reviews),data=YPH, family=binomial,na.action=na.exclude)
summary(FBIMOD)
exp(confint(FBIMOD))

exp(confint(mod1))
exp(confint(mod2))


exp(confint(mod))
attach(YPH)
detach(YPH)
PLRY=subset(YPH, YPH$EFFORT>=1)
YPOLR <- polr(as.factor(EFFORT) ~as.factor(Age)+as.factor(Gender)+ White+Asian+Black+Latino+Useyelp+as.numeric(STARS), data = PLRY)
summary(YPOLR)
exp(confint(YPOLR))
#Test Parralel slopes assumption required by proportional odds logistic regression model

Y0=glm(I(as.numeric(EFFORT) >= 0) ~ as.factor(Age)+as.factor(Gender)+ White+Asian+Black+Latino+Useyelp+as.numeric(STARS), family="binomial", data = YPH)

Y1=glm(I(as.numeric(EFFORT) >= 1) ~ as.factor(Age)+as.factor(Gender)+ White+Asian+Black+Latino+Useyelp+as.numeric(STARS), family="binomial", data = YPH)

Y2=glm(I(as.numeric(EFFORT) >= 2) ~ as.factor(Age)+as.factor(Gender)+ White+Asian+Black+Latino+Useyelp+as.numeric(STARS), family="binomial", data = YPH)

Y3=glm(I(as.numeric(EFFORT) >= 3) ~ as.factor(Age)+as.factor(Gender)+ White+Asian+Black+Latino+Useyelp+as.numeric(STARS), family="binomial", data = YPH)

exp(confint(Y0))
exp(confint(Y1))
exp(confint(Y2))
exp(confint(Y3))

FBIm <- polr(as.factor(FBI) ~as.factor(Age)+as.factor(Gender)+ White+Asian+Black+Latino+Useyelp+as.numeric(STARS)+vermin+washhand+perfhealth+fivestar, data = YPH, na.action = na.exclude)
summary(FBIm)
exp(confint(FBIm))

levels(as.factor(YPH$FBI))