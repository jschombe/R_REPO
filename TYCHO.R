#TYcho

library(googleVis)
library(plyr)
library(reshape)

as.number <- function(x) {as.numeric(levels(x))[x]}



 # Convenience interface to gvisMotionChart that allows to set default columns
    myMotionChart = function(df,idvar=colnames(df)[1],timevar=colnames(df)[2],xvar=colnames(df)[3],yvar=colnames(df)[4], colorvar=colnames(df)[5], sizevar = colnames(df)[6],...) {
        library(googleVis)

        # Generate a constant variable as column for time if not provided
        # Unfortunately the motion plot still shows 1900...
        if (is.null(timevar)) {
            .TIME.VAR = rep(0,NROW(df))
            df = cbind(df,.TIME.VAR)
            timevar=".TIME.VAR"
        }

        # Transform booleans into 0 and 1 since otherwise an error will be thrown
        for (i in  1:NCOL(df)) {
            if (is.logical(df [,i])[1])
                df[,i] = df[,i]*1
        }

        # Rearrange columns in order to have the desired default values for
        # xvar, yvar, colorvar and sizevar
        firstcols = c(idvar,timevar,xvar,yvar,colorvar,sizevar)
        colorder = c(firstcols, setdiff(colnames(df),firstcols))
        df = df[,colorder]

        gvisMotionChart(df,idvar=idvar,timevar=timevar,...)
    }


tycho <- read.table("C:\\Python27\\ProjectTycho.txt",sep="\t", header=TRUE)
TPOP=read.csv("C:\\Python27\\TYCOPOP.csv", header=TRUE)
head(TPOP)
head(tycho)
MPOP <- melt(TPOP, id=c("state"))
head(MPOP)
MPOP$YEAR <- substr(MPOP$variable,2,5)
tycho$YEAR <- substr(tycho$to_date, 0, 4)

is.character(MPOP$YEAR)
is.character(tycho$YEAR)

tychoP<-as.data.frame( merge(tycho,MPOP,by=c("state","YEAR")))
head(tychoP)
nrow(tychoP)


tychoC=as.data.frame(subset(tychoP, tychoP$event=="CASES"))
nrow(tychoC)
tychoD=as.data.frame(subset(tychoP, tychoP$event=="DEATHS"))
nrow(tychoD)
head(tychoD)

tychoD$YEAR <- as.Date(tychoD$YEAR,format = "%Y")
tychoD$YEAR <- as.Date(as.character(tychoD$YEAR))

head(tychoD)

DISE <- ddply(tychoD, c("disease","YEAR"), summarise, Deaths = sum(number), POP=sum(value))
nrow(DISE)
summary(DISE)
head(DISE)
DISE$rats=DISE$Deaths/DISE$POP
Dis <- gvisMotionChart(DISE, idvar="disease", timevar="YEAR",options=list(width=700, height=600))
plot(Dis)



tychoC$YEAR <- as.Date(tychoC$YEAR,format = "%Y")
tychoC$YEAR <- as.Date(as.character(tychoC$YEAR))

head(tychoC)

DISEC <- ddply(tychoC, c("disease","YEAR","state"), summarise, Cases = sum(number), POP=sum(value))
nrow(DISEC)
summary(DISEC)
head(DISEC)
DISEC$rates=DISEC$Cases/DISEC$POP
DISEC=as.data.frame(DISEC)
write.csv(DISEC, file = "c:\\Python27\\TYCHOC2.csv")
write.csv(DISEC, file = "c:\\Python27\\TYCHOC3.csv")


DISEC=read.csv("c:\\Python27\\TYCHOC2.csv")
rm(tycho)
rm(tychoC)
rm(tychoD)

DISEC$YEAR <- as.Date(DISEC$YEAR,format = "%Y")
DISEC$YEAR <- as.Date(as.character(DISEC$YEAR))
DISEC$CONCAT=paste(DISEC$disease, DISEC$state)



head(tycho)
Diss <- gvisMotionChart(DISEC, idvar="CONCAT", timevar="YEAR",options=list(width=700, height=600))
plot(Diss)
print(Diss, 'chart')

head(tycho)
tycho$Date <- as.Date(tycho$to_date,format = "%Y-%m-%d")
tycho$Date <- as.Date(as.character(tycho$Date))


tycho$YEAR= format(tycho$Date, format="%Y")



whoop=subset(tycho, tycho$disease=="WHOOPING COUGH [PERTUSSIS]")
summary(whoop$Date)
head(whoop)
nrow(whoop)
whoop$Date2 <- substr(whoop$Date,1,7)
whoopC=as.data.frame(subset(whoop, whoop$event=="CASES"))
whoopD=as.data.frame(subset(whoop, whoop$event=="DEATHS"))
whoopT<-as.data.frame( merge(whoopC,whoopD,by=c("Date")))
nrow(whoopT)
head(whoopT)
whoopT$rats=whoopT$number.y/whoopT$number.x
head(whoopT)
WHOOP=as.data.frame(cbind(whoopT$id.x,as.character(whoopT$Date),whoopT$number.x,whoopT$number.y,whoopT$rats,as.character(whoopT$state.x),as.character(whoopT$loc.x)))
head(WHOOP)
WHOOP$DATE<-as.Date(as.character(WHOOP$V2),format = "%Y-%m-%d")
nrow(WHOOP)
woop <- ddply(WHOOP, c("DATE","V6"), summarise, sumx = sum(as.number(V3)), sumy= sum(as.number(V4)), mrate=mean(as.number(V5)))
head(woop)
nrow(woop)
woop$date2= format(woop$DATE, format="%m-%d-%Y")
woop$date2=as.Date(as.character(woop$date2))
woop$YEAR= format(woop$DATE, format="%Y")
summary(woop$DATE)


M <- gvisMotionChart(woop, idvar="V6", timevar="DATE",options=list(width=700, height=600))
plot(M)

TYCHO=subset(tycho, tycho$Date>1940-01-01)
head(TYCHO)
TYCHO$country <- NULL
TYCHO$indictor <- NULL
TYCHO$from_date<- NULL

#TYCHO$epi_week<- NULL
#TYCHO$event<- NULL
TYCHO$pdf_id<- NULL
TYCHO$subcategory_new<- NULL
TYCHO$Date<- NULL
TYCHO$pub_date<- NULL
TYCHO$to_date<- NULL
TYCHO$indicator<-NULL

TYCHO2=subset(TYCHO, TYCHO$event=='CASES')
nrow(TYCHO2)


ncol(TYCHO)
ncol(tycho)
head(TYCHO)

TYCHO$Numb=TYCHO$number*1



is.numeric(TYCHO$epi_week)
nrow(TYCHO)

library(plyr)

# Run the functions length, mean, and sd on the value of "change" for each group, 
# broken down by sex + condition
TYCO <- ddply(TYCHO, c("disease", "epi_week"), summarise, Nsum = sum(Numb))
head(TYCHO)

TYCHO$Date2 <- substr(TYCHO$Date,1,7)

#need to merge Deaths and CASES
TYCHOC=as.data.frame(subset(TYCHO, TYCHO$event=="CASES"))
TYCHOD=as.data.frame(subset(TYCHO, TYCHO$event=="DEATHS"))
TYCHOT<-as.data.frame( merge(TYCHOC,TYCHOD,by=c("Date2","disease")))
head(TYCHOT)
nrow(TYCHOT)
head(TYCHOD)
head(TYCHOC)
nrow(TYCHOC)
summary(TYCHOC)
nrow(TYCHOD)
nrow(TYCHOT)
head(TYCHOT)
summary(TYCHOT)
length(TYCHOT$epi_week.x)

TYCHOT$ratio=TYCHOT$number.x/TYCHOT$number.y
TYCHOT2=as.data.frame(cbind(as.character(TYCHOT$disease),as.numeric(TYCHOT$epi_week.x),TYCHOT$Numb.x,TYCHOT$Numb.y))
head(TYCHOT2)
library(reshape)
TYCHOT2 <- rename(TYCHOT2, c(V1="disease",V2="Date",V3="x",V4="y"))
TYCHOT2$Date=as.numeric(TYCHOT2$Date)
head(tycho$Date)numb
nrow(tycho)
tinycho=sample(tycho, 100, replace = FALSE, prob = NULL)
TYCHO$NDATE=as.numeric(TYCHO$Date)
tychoMOTION <- melt(TYCHO, id=c("loc","epi_week","Date2"))
head(tychoMOTION)
ncol(tychoMOTION)
nrow(tychoMOTION)
TYCO <- ddply(TYCHOT, c("loc.x","Date.y"), summarise, Nsumx = sum(Numb.x), Nsumy=sum(Numb.y, Nrat=sum(ratio)))
head(TYCO)
nrow(TYCO)
TYCO$rats=TYCO$Nsumx/TYCO$Nsumy
TYCHOT$DATE <- as.Date(TYCHOT$Date.y,format = "%Y-%m-%d")
head(TYCHOT2$DATE)
is.numeric(TYCHOT$Date.y)
is.character(TYCHOT$Date.y)

unique(TYCHOT$Date.y)

M <- gvisMotionChart(TYCHOC, idvar="id.x", timevar="Date.y",options=list(width=700, height=600))
 
## Display the chart in your browser
plot(M)
