## This code and the related data are open-source under the BSD 2-clause license.
## See http://www.tldrlegal.com/l/BSD2 for details
## Michael Ash, Thomas Herndon, and Robert Pollin April-May 2013
## Replicate and extend Reinhart and Rogoff (2010) using data from RR working spreadsheet

library(plyr)
library(ggplot2)
library(gridExtra)
library(car)
library(foreign)
library(xlsx)
library(reshape2)
options(scipen=10000)
options(digits=4)
pdf(width=10)

rm(list = ls())

## Read the individual country data (dumped from RR.xls with Save All to CSV)
Australia     <- read.csv("RR - Australia.csv") 
Austria       <- read.csv("RR - Austria.csv")   
Belgium       <- read.csv("RR - Belgium.csv")   
Canada        <- read.csv("RR - Canada.csv")    
Denmark       <- read.csv("RR - Denmark.csv")   
Finland       <- read.csv("RR - Finland.csv")   
France        <- read.csv("RR - France.csv")    
Germany       <- read.csv("RR - Germany.csv")   
Greece        <- read.csv("RR - Greece.csv")    
Ireland       <- read.csv("RR - Ireland.csv")   
Italy         <- read.csv("RR - Italy.csv")     
Japan         <- read.csv("RR - Japan.csv")     
Netherlands   <- read.csv("RR - Netherlands.csv")
NewZealand    <- read.csv("RR - New Zealand.csv")
Norway        <- read.csv("RR - Norway.csv")
Portugal      <- read.csv("RR - Portugal.csv")
Spain         <- read.csv("RR - Spain.csv")
Sweden        <- read.csv("RR - Sweden.csv")
UK            <- read.csv("RR - UK.csv")
US            <- read.csv("RR - US.csv")

## Stack the data
RR <- merge(Australia,Austria,all=TRUE)
RR <- merge(RR,Belgium    ,all=TRUE)
RR <- merge(RR,Canada     ,all=TRUE)
RR <- merge(RR,Denmark    ,all=TRUE)
RR <- merge(RR,Finland    ,all=TRUE)
RR <- merge(RR,France     ,all=TRUE)
RR <- merge(RR,Germany    ,all=TRUE)
RR <- merge(RR,Greece     ,all=TRUE)
RR <- merge(RR,Ireland    ,all=TRUE)
RR <- merge(RR,Italy      ,all=TRUE)
RR <- merge(RR,Japan      ,all=TRUE)
RR <- merge(RR,Netherlands,all=TRUE)
RR <- merge(RR,NewZealand ,all=TRUE)
RR <- merge(RR,Norway     ,all=TRUE)
RR <- merge(RR,Portugal   ,all=TRUE)
RR <- merge(RR,Spain      ,all=TRUE)
RR <- merge(RR,Sweden     ,all=TRUE)
RR <- merge(RR,UK         ,all=TRUE)
RR <- merge(RR,US         ,all=TRUE)

with(RR,table(Country))

## Convert public debt/GDP
RR$debtgdp <- RR$debtgdp

write.dta(RR,"RR-basic.dta")

## Follow rules in RR working spreadsheet for calculating public debt/GDP for each country-year
RR <- within(RR, debtgdp <- ifelse(Country=="Australia",ifelse(Year<=1948,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Austria",ifelse(Year<=1979,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Belgium",ifelse(Year<=1979,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Canada",ifelse(Year<=1948,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Denmark",ifelse(Year<=1949,100*Debt1/GDP1,100*Debt1/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Finland",ifelse(Year<=1977,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="France",ifelse(Year<=1948, 100*Debt1 / GDP1, ifelse(Year<=1977, 100*Debt1 / GDP2,100*Debt2/GDP2)),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Germany",ifelse(Year<=1950,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Greece",ifelse((Year>=1884 & Year<=1913) | (Year>=1919 & Year<=1939) | (Year>=1970 & Year<=1992),100*Debt/GDP1, ifelse(Year==2009,100,debtgdp)),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Ireland",ifelse(Year<=2010,100*Debt/GDP2,debtgdp),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Italy",ifelse(Year<=1913,100*Debt/GDP1,ifelse(Year<=1946,100*Debt/GNI,ifelse(Year<=1998,100*Debt/GDP1,100*Debt/GDP2))),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Japan",ifelse(Year<=1953,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Netherlands",ifelse(Year<=1956,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="New Zealand",ifelse(Year<=1947,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Norway",ifelse(Year<=1948,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Portugal",ifelse(Year<=1999,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Spain",ifelse(Year<=1957,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Sweden",ifelse(Year<=1949,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="UK" , 100*Debt/GDP, debtgdp ))
RR <- within(RR, debtgdp <- ifelse(Country=="US" , 100*Debt/GDP, debtgdp ))

RR$RGDP <- as.numeric(RR$RGDP)
RR$RGDP1 <- as.numeric(RR$RGDP1)
RR$RGDP2 <- as.numeric(RR$RGDP2)

## Calculate real GDP growth using rules per RR spreadsheet 
lg<-function(x)c(NA,x[1:(length(x)-1)])
RR <- ddply( RR, ~Country, transform, lRGDP=lg(RGDP), lRGDP1=lg(RGDP1), lRGDP2=lg(RGDP2)  )
RR <- within(RR, dRGDP <- ifelse( !is.na(dRGDP), dRGDP,
                                   ifelse( !is.na( RGDP / lRGDP - 1 ), 100*(RGDP / lRGDP - 1) ,
                                          ifelse( !is.na( RGDP2 / lRGDP2 - 1 ), 100*(RGDP2 / lRGDP2 - 1) ,
                                                 ifelse( !is.na( RGDP1 / lRGDP1 - 1 ), 100*(RGDP1 / lRGDP1 - 1),dRGDP )))))

write.dta(RR,"RR-200-processed.dta")

## Cut to postwar analysis
RR <- subset(RR,Year>=1946 & Year<=2009)

## Italy uses another data series through 1946 and is excluded from GITD postwar until 1951
RR <- subset(RR, !(Year<1951 & Country=="Italy"))

## Potential data years 1946-2009
avail.data <- ddply(RR, ~Country, summarize,
                    count.year.GDP=sum(!is.na(dRGDP)),count.year.debt=sum(!is.na(debtgdp)), count.year.both=sum(!is.na(dRGDP) & !is.na(debtgdp)))
avail.data[order(avail.data[,"count.year.both"]),]

write.dta(RR,"RR-processed.dta")
## Slow
RR$dgcat.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,Inf))
RR$dgcat <- factor(RR$dgcat.lm, labels = c("0-30%","30-60%","60-90%","Above 90%"),ordered=TRUE)
write.xlsx2(subset(RR,TRUE,select=c(Country,Year,debtgdp,dgcat,dRGDP) ),"RR-keycolumns.xlsx",row.names=FALSE)


## Limit to actually available data
RR <- subset(RR,  !is.na(dRGDP) & !is.na(debtgdp))

## Actually available data years 1946-2009
avail.data <- ddply(RR, ~Country, summarize, min.year=min(Year), count.year=sum(!is.na(dRGDP) & !is.na(debtgdp)))
avail.data[order(avail.data[,"min.year"]),]

with(RR,table(Year))
with(RR,table(Country))

## Create RR public debt/GDP categories
RR$dgcat.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,Inf))
RR$dgcat <- factor(RR$dgcat.lm, labels = c("0-30%","30-60%","60-90%","Above 90%"),ordered=TRUE)

## Create expanded public debt/GDP categories
RR$dgcat2.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,120,Inf))
RR$dgcat2 <- factor(RR$dgcat2.lm, labels = c("0-30%","30-60%","60-90%","90-120%","Above 120%"),ordered=TRUE)

## Regression analysis of categories
summary(dgcat.lm <- lm(dRGDP ~ dgcat.lm, data=RR))
summary(dgcat2.lm <- lm(dRGDP ~ dgcat2.lm, data=RR))
linearHypothesis(dgcat2.lm,
                 verbose=TRUE,
                 paste( c("dgcat2.lm(30,60]=dgcat2.lm(60,90]", "dgcat2.lm(30,60]=dgcat2.lm(90,120]", "dgcat2.lm(30,60]=dgcat2.lm(120,Inf]") ))
linearHypothesis(dgcat2.lm, verbose=TRUE, paste( c("dgcat2.lm(30,60]=dgcat2.lm(60,90]", "dgcat2.lm(30,60]=dgcat2.lm(90,120]")))
linearHypothesis(dgcat2.lm, verbose=TRUE, paste( c("dgcat2.lm(30,60]=dgcat2.lm(60,90]")))
linearHypothesis(dgcat2.lm, verbose=TRUE, paste( c("dgcat2.lm(60,90]=dgcat2.lm(90,120]")))
linearHypothesis(dgcat2.lm, verbose=TRUE, paste( c("dgcat2.lm(30,60]=dgcat2.lm(90,120]")))

linearHypothesis(dgcat2.lm, verbose=TRUE, paste( c("(Intercept) + dgcat2.lm(90,120] = 3") ))


## Country-Year average by debtgdp ("correct weights")
## Table 3 Corrected
(RR.correct.sd <- with(RR, tapply( dRGDP, dgcat, sd, na.rm=TRUE )))
(RR.correct.mean <- with(RR, tapply( dRGDP, dgcat, mean, na.rm=TRUE )))
(RR.correct.N <- with(RR, tapply( dRGDP, dgcat, length )))
(RR.correct.se <- RR.correct.sd / sqrt(RR.correct.N))

RR.correct.mean.df <- data.frame(RR.correct.mean, dgcat=names(RR.correct.mean) )
## Averaged Country averages by debtgdp ("equal weights")
(RR.equalwt.mean <- with(RR, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))
(RR.equalwt.sd <- apply(RR.equalwt.mean,2,sd,na.rm=TRUE))
(RR.equalwt.N <- apply(!is.na(RR.equalwt.mean), 2, sum))
(RR.equalwt.se <- RR.equalwt.sd / sqrt(RR.equalwt.N))


## NYT Appendix input to Table 1 Line 3
(RR.equalwt.median <- with(RR, tapply( dRGDP, list(Country,dgcat), median, na.rm=TRUE )))
## Table 3 Country equal weighting
summary(RR.equalwt.mean)

## Country-Year average by debtgdp ("correct weights") expanded categories
(RR.correct.mean.2 <- with(RR, tapply( dRGDP, dgcat2, mean, na.rm=TRUE )))
RR.correct.mean.2.df <- data.frame(RR.correct.mean.2, dgcat=names(RR.correct.mean.2) )
## Averaged Country averages by debtgdp ("equal weights")
(RR.ex.equalwt.mean <- with(RR, tapply( dRGDP, list(Country,dgcat2), mean, na.rm=TRUE )))
summary(RR.ex.equalwt.mean)


## Selective treatment of early years
RR.selective <- subset(RR,
                       !((Year<1950 & Country=="New Zealand") | (Year<1951 & Country=="Australia") | (Year<1951 & Country=="Canada") ))
(RR.selective.mean <- with(RR.selective, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))
(RR.selective.sd <- apply(RR.selective.mean,2,sd,na.rm=TRUE))
(RR.selective.N <- apply(!is.na(RR.selective.mean), 2, sum))
(RR.selective.sd) / sqrt(RR.selective.N)

(RR.selective.median <- with(RR.selective, tapply( dRGDP, list(Country,dgcat), median, na.rm=TRUE )))
## Equal weights
## Table 3 Weights,Exclusion
summary(RR.selective.mean)
## Correct weights
## Table 3 Selective years exclusion
with(RR.selective, tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

## Spreadsheet error 
RR.spreadsheet <- subset(RR, ! Country %in% c("Australia","Austria","Belgium","Canada","Denmark") )
(RR.spreadsheet.mean <- with(RR.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))
(RR.spreadsheet.median <- with(RR.spreadsheet, tapply( dRGDP, list(Country,dgcat), median, na.rm=TRUE )))
## Table 3 Spreadsheet, Weights
summary(RR.spreadsheet.mean)
## Table 3 Spreadsheet error
with(RR.spreadsheet, tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

## Selective treatment of early years and spreadsheet error
RR.selective.spreadsheet <- subset(RR.selective, ! Country %in% c("Australia","Austria","Belgium","Canada","Denmark") )
RR.selective.spreadsheet.mean <- with(RR.selective.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE ))
(RR.selective.spreadsheet.eqweight.median <- with(RR.selective.spreadsheet, tapply( dRGDP, list(Country,dgcat), median, na.rm=TRUE )))
## Equal weights
## Table 3 Weights,Exclusion,Spreadsheet Error
summary(RR.selective.spreadsheet.mean)
apply(RR.selective.spreadsheet.mean,2,mean,na.rm=TRUE)
(RR.selective.spreadsheet.sd <- apply(RR.selective.spreadsheet.mean,2,sd,na.rm=TRUE))
(RR.selective.spreadsheet.N <- apply(!is.na(RR.selective.spreadsheet.mean), 2, sum))
RR.selective.spreadsheet.sd / sqrt(RR.selective.spreadsheet.N)


## Correct weights
## Table 3 Exclusion,Spreadsheet Error
with(RR.selective.spreadsheet, tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

## Actually available data years 1946-2009 with selective exclusion and spreadsheet error
avail.data <- ddply(RR.selective.spreadsheet, ~Country, summarize, min.year=min(Year), count.year=sum(!is.na(dRGDP) & !is.na(debtgdp)))
avail.data[order(avail.data[,"min.year"]),]

## And New Zealand transcription error
## selective.spreadsheet.transcription <- with(RR.selective.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE ))
RR.selective.spreadsheet.mean.transcription <- RR.selective.spreadsheet.mean
RR.selective.spreadsheet.mean.transcription["New Zealand",4] <- -7.9
summary(RR.selective.spreadsheet.mean.transcription)
## Table 3 Weights,Exclusion,Spreadsheet Error,Transcription
(RR.published.mean <- apply(RR.selective.spreadsheet.mean.transcription,2,mean,na.rm=TRUE))
RR.published.mean.df <- data.frame(RR.published.mean , dgcat=names(RR.published.mean) )


## Medians
## NYT Appendix Table 1 Line 4
(RR.correct.median <- with(RR, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
(RR.correct.selective.median <- with(RR.selective, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
(RR.correct.spreadsheet.median <- with(RR.spreadsheet, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
## NYT Appendix Table 1 Line 3 (Use Median line)
(RR.eqweight.median <- summary(RR.equalwt.median))
summary(RR.spreadsheet.median)
## NYT Appendix Table 1 Line 2 (Dataset is "RR.selective" because it EXCLUDES early years but spreadsheet is corrected)
summary(RR.selective.median)
(RR.correct.ex.median <- with(RR, tapply( dRGDP, dgcat2, median, na.rm=TRUE )))
(RR.selective.spreadsheet.mean.median <- with(RR.selective.spreadsheet, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
(RR.published.median <- apply(RR.selective.spreadsheet.eqweight.median,2,median,na.rm=TRUE))


## Counts of years
with(RR, table(Country,dgcat))
(year.totals <- apply(with(RR,table( Country,dgcat)),2,sum))
prop.table ( with(RR, table(Country,dgcat)),2 )

with(RR.selective,table( Country,dgcat))
(year.totals <- apply(with(RR.selective,table( Country,dgcat)),2,sum))
prop.table ( with(RR.selective, table(Country,dgcat)),2 )

with(RR.selective.spreadsheet,table( Country,dgcat))
(years.total <- apply(with(RR.selective.spreadsheet,table( Country,dgcat)),2,sum))
prop.table (with(RR.selective.spreadsheet,table( Country,dgcat)), 2)



## Categorical scatterplot
RR.newzealand.1951 <- subset(RR.selective.spreadsheet,Country=="New Zealand" & Year==1951)
RR.newzealand.1951$Key <- as.character(RR.newzealand.1951$Country)

specials <- rbind(melt(RR.published.mean.df,variable.name="Key",value.name="dRGDP"),
                  melt(RR.correct.mean.df,variable.name="Key",value.name="dRGDP"),
                  subset(RR.newzealand.1951,select=c(Key,dgcat,dRGDP)))
specials$Key <- as.character(specials$Key)
specials <- mutate(specials,
                   shape=as.integer(ifelse(Key=="RR.published.mean",5,ifelse(Key=="RR.correct.mean",16,0))),
                   Key=ifelse(Key=="RR.published.mean","RR average real GDP growth",
                       ifelse(Key=="RR.correct.mean","Correct average real GDP growth", Key)),
                   size=4,
                   color="black")
RR.gr <- rbind(data.frame(Key="Country-year real GDP growth",shape=3,size=2,color="darkgray",(subset(RR,select=c(dgcat,dRGDP)))),specials)
RR.gr$Key <- as.character(RR.gr$Key)



m <- ggplot(RR.gr, aes(x=dgcat,y=dRGDP,color=color,shape=shape,size=size)) +
    scale_shape_identity() + scale_size_identity() + scale_color_identity() +
    geom_point() + ylab("Real GDP Growth") + xlab("Public Debt/GDP Category") + theme_bw()

m <- m + geom_text(RR.published.mean.df, mapping=aes(x=dgcat,y=RR.published.mean,shape=1,label=round(RR.published.mean,1)),
                   hjust=-0.8,size=4,color='darkgray')
m <- m + geom_text(RR.correct.mean.df,  mapping=aes(x=dgcat,y=RR.correct.mean,shape=1,label=round(RR.correct.mean,1)),
                   hjust=1.8,size=4,color='darkgray')
m <- m + geom_text(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP,shape=1,label=paste(round(dRGDP,1))),
                   hjust=-0.8,size=4,color='darkgray')
m <- m + geom_text(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP,shape=1,label=paste("NZ",Year)),
                   hjust=1.4,size=4,color='darkgray')
## print(m)

d=data.frame(p=c(5,0,16,3),
    c=c("black","black","black","darkgray"),
    x=c(1.1,1.5,1.1,1.5),
    y=c(1,1,2,2),
    l=c("RR average real GDP growth",
        "New Zealand 1951 real GDP growth",
        "Correct average real GDP growth",        
        "Individual country-year real GDP growth"))
leg <- ggplot() + scale_shape_identity() + scale_color_identity() +
    scale_y_continuous(name="",labels=NULL,breaks=NULL,limits=c(0.7,2.3)) +
    scale_x_continuous(name="",labels=NULL,breaks=NULL,limits=c(0.7,2.3)) +
    geom_point(data=d, mapping=aes(x=x, y=y, shape=p, color=c), size=3) + 
    geom_text(data=d, mapping=aes(x=x+0.02, y=y, label=l, hjust=0), size=3) + theme_bw() + theme(panel.border=element_blank())
grid.arrange(m,leg,nrow=2,heights=c(6,1))


## n <- ggplot(RR, aes(x=dgcat,y=dRGDP)) + geom_point(shape=3,color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Category")
## n <- n + geom_point(specials, mapping=aes(x=dgcat,y=dRGDP,shape=shape), size=4) + theme_bw() +
##     scale_shape_identity() ## +  theme(legend.position = "bottom") 
## print(n)
## ## n <- n + geom_point(RR.published.mean.df, mapping=aes(x=dgcat,y=RR.published.mean), shape=5,  size=5 ) 
## n <- n + geom_text(RR.published.mean.df, mapping=aes(x=dgcat,y=RR.published.mean,label=round(RR.published.mean,1)),hjust=-0.8,size=3,color='darkgray')
## ## n <- n + geom_point(RR.correct.mean.df,  mapping=aes(x=dgcat,y=RR.correct.mean,label=RR.correct.mean), shape=16, size=4 )  + theme_bw()
## n <- n + geom_text(RR.correct.mean.df,  mapping=aes(x=dgcat,y=RR.correct.mean,label=round(RR.correct.mean,1)), hjust=1.8,size=3,color='darkgray')
## ## n <- n + geom_point(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP), shape=0, size=3 )
## n <- n + geom_text(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP,label=paste(round(dRGDP,1))), hjust=-0.8,size=3,color='darkgray')
## n <- n + geom_text(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP,label=paste("NZ",Year)), hjust=1.4,size=3,color='darkgray')
## print(n)

## http://4dpiecharts.com/tag/dataviz/
## https://www.stat.auckland.ac.nz/~paul/R/CM/AdobeSym.html
## http://www.adobe.com/products/postscript/pdfs/PLRM.pdf

## g <- tableGrob(matrix(expression(
##     paste(symbol("\267")), paste(symbol("\340")), 
##     paste(" Correct average real GDP growth"),paste(" RR average real GDP growth"),
##     paste(symbol("\053")),paste("[",phantom(i),"]"),
##     paste(" Country-Year real GDP growth"),
##     paste(" New Zealand 1951") ), nrow=2,ncol=4),parse=TRUE)

## grid.arrange(n,g,nrow=2,heights=c(5,1))


## ## Create legend for categorical scatterplot
## plot(3,10,pch=0,ylim=c(0,70),xlim=c(0,5.5))
## text(3.2,10,"New Zealand 1951",adj=0)
## points(0,15,pch=16)
## text(0.2,15,"Correct average real GDP growth",adj=0)
## points(0,10,pch=5,cex=1.5)
## text(0.2,10,"RR average real GDP growth",adj=0)
## points(3,15,pch=3,col='darkgray')
## text(3.2,15,"Country-Year real GDP growth",adj=0)

## Categorical scatterplot for expanded categories
o <- ggplot(RR, aes(x=dgcat2,y=dRGDP)) + geom_point(shape=3,color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Category")
o <- o + geom_point(RR.correct.mean.2.df,  mapping=aes(x=dgcat,y=RR.correct.mean.2), shape=16, size=4 )  + theme_bw()
o <- o + geom_text(RR.correct.mean.2.df, mapping=aes(x=dgcat,y=RR.correct.mean.2,label=round(RR.correct.mean.2,1)), hjust=1.7, size=4,color='darkgray') + theme_bw()
print(o)

## Scatterplot (continuous)
library(mgcv)
RR.gam <- gam(dRGDP ~ s(debtgdp, bs="cs"),data=RR)

## Cross-validation technique for loess parameters
## http://stats.stackexchange.com/questions/2002/how-do-i-decide-what-span-to-use-in-loess-regression-in-r
m <- ggplot(RR, aes(x=debtgdp,y=dRGDP))
m1 <- m + geom_vline(xintercept=90,color='lightgray',size=1.5)
m1 <- m1 + geom_point(color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Ratio") + scale_x_continuous(breaks=seq(0,240,30)) + theme_bw()
## m1 <- m1 + geom_smooth(method='loess',span=1.0,color='black') + geom_smooth(method='loess',span=0.2,color='black')
m1 <- m1 + geom_smooth(method=gam, color='black',formula= y ~ s(x, bs = "cs"))
## m1 <- m1 + geom_smooth(method='auto', color='black')
print(m1)

## Categorical scatterplot later years
RR2000 <- subset(RR, Year>=2000)
(RR.later.mean <- with(RR2000, tapply( dRGDP, dgcat, mean, na.rm=TRUE )))
RR.later.mean.df <- data.frame(RR.later.mean, dgcat=names(RR.later.mean) )

L <- ggplot(RR2000, aes(x=dgcat,y=dRGDP)) + geom_point(shape=3,color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Category") + theme_bw()
## L <- L + geom_text(mapping=aes(label=paste(Country,Year) ), size=2, hjust=-0.2,color='darkgray') 
L <- L + geom_point(RR.later.mean.df,  mapping=aes(x=dgcat,y=RR.later.mean,label=RR.later.mean), shape=16, size=4 )  + theme_bw()
L <- L + geom_text(RR.later.mean.df,  mapping=aes(x=dgcat,y=RR.later.mean,label=round(RR.later.mean,1)), hjust=1.7,size=4,color='darkgray')
## L <- L + coord_cartesian(ylim=c(-12, 30)) 
print(L)


## Scatterplot closeup
## pdf("closeup.pdf",height=4,width=7)
m2 <- m + geom_point(color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Ratio") + scale_x_continuous(breaks=seq(0,240,30)) + theme_bw() +  geom_vline(xintercept=90,color='lightgray',size=1.5)
## m2 <- m2 + geom_smooth(method='loess',span=0.75,color='black') + geom_smooth(method='loess',span=0.4,color='black') 
## m2 <- m2 + geom_smooth(method='auto',color='black')
m2 <- m2 + geom_smooth(method=gam, color='black',formula= y ~ s(x, bs = "cs"))
m2 <- m2 + coord_cartesian(ylim=c(0, 7),xlim=c(0,150)) + scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7)) + theme_bw()
print(m2)


## Get the range for which the lower bound and upper bound include 3 percent
subset(ggplot_build(m1)$data[[3]],  (ymin<3.1 & ymin>2.9) | (ymax<3.1 & ymax>2.9 ))

subset(RR,
       Country %in% c("Australia","Belgium","Canada","Greece","Ireland","Italy","Japan","New Zealand","UK","US"),
       select=c(Country,Year,dgcat,debtgdp,dRGDP))

subset(RR,
       debtgdp>90,
       select=c(Country,Year,dgcat,debtgdp,dRGDP))

## Look at the public debt / GDP series
## p <- ggplot(RR, aes(x=Year,y=debtgdp,color=Country)) + geom_point() +  facet_grid(. ~ Country) + opts(legend.position="bottom")
## print(p)


## Average growth by debtgdp for more recent samples
## country-year weights
with(subset(RR, Year>=1950), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))
## country weights
apply(with(subset(RR, Year>=1950), tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )),2,mean,na.rm=TRUE)

## country-year weights
with(subset(RR, Year>=1960), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))
## country weights
apply(with(subset(RR, Year>=1960), tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )),2,mean,na.rm=TRUE)

## country-year weights
with(subset(RR, Year>=1970), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))
## country weights
apply(with(subset(RR, Year>=1970), tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )),2,mean,na.rm=TRUE)

## country-year weights
with(subset(RR, Year>=1980), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))
## country weights
apply(with(subset(RR, Year>=1980), tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )),2,mean,na.rm=TRUE)

## country-year weights
with(subset(RR, Year>=1990), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))
## country weights
apply(with(subset(RR, Year>=1990), tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )),2,mean,na.rm=TRUE)

## Post-2000
## NYT Appendix Table 4 Line 1
(mean2000 <- with(RR2000, tapply( dRGDP, dgcat, mean, na.rm=TRUE )))
(sd2000 <- with(RR2000, tapply( dRGDP, dgcat, sd, na.rm=TRUE )))
(length2000 <- with(RR2000, tapply( dRGDP, dgcat, length )))

## NYT Appendix Table 4 Line 1 Standard errors
sd2000 / sqrt(length2000)

## country-year weights
(RR2000.equalwt.mean <- with(RR2000, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))

with(RR2000, table(Country,dgcat))

## Regression analysis of categories
summary(dgcat.lm <- lm(dRGDP ~ dgcat.lm, data=RR2000))

## country weights
apply(RR2000.equalwt.mean,2,mean,na.rm=TRUE)
apply(RR2000.equalwt.mean,2,sd,na.rm=TRUE)
apply(with(RR2000,table( Country,dgcat)),2,sum)



## Median analysis of two periods ## Mean has problems because of very high value for Belgium in 1947
## country-year weights
with(subset(RR, Year<1980), tapply( dRGDP, dgcat, median, na.rm=TRUE ))
## country weights  ## Beware ridiculous value for Belgian GDP growth in 1947
apply(with(subset(RR, Year<1980), tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )),2,median,na.rm=TRUE)

## country-year weights
with(subset(RR, Year>=1980), tapply( dRGDP, dgcat, median, na.rm=TRUE ))
## country weights
apply(with(subset(RR, Year>=1980), tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )),2,median,na.rm=TRUE)

## country-year weights
with(RR, tapply( dRGDP, dgcat, median, na.rm=TRUE ))
## country weights
apply(with(RR, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )),2,median,na.rm=TRUE)

## Median analysis of two periods ## Mean has problems because of very high value for Belgium in 1947
## 1955-1980 has only Britain in the highest public debt category!!
## country-year weights
with(subset(RR, Year>=1955 & Year<1980), tapply( dRGDP, dgcat, median, na.rm=TRUE ))
## country weights  ## Beware ridiculous value for Belgian GDP growth in 1947
apply(with(subset(RR, Year>=1955 & Year<1980), tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )),2,median,na.rm=TRUE)


write.xlsx2(subset(RR,Year>=1990 & debtgdp>90,select=c(Country,Year,debtgdp,dgcat,dRGDP)),"RR-post1990.xlsx",row.names=FALSE)


subset(RR,dRGDP>10,select=c(Country,Year,dRGDP,dgcat,debtgdp))
subset(RR,dRGDP< -7,select=c(Country,Year,dRGDP,dgcat,debtgdp))

subset(RR,Year==1914,select=c(Country,Year,dRGDP,dgcat,debtgdp))
subset(RR,Year>=1914 & Year<=1918,select=c(Country,Year,dRGDP,dgcat,debtgdp))
subset(RR,Year>=1939 & Year<=1945,select=c(Country,Year,dRGDP,dgcat,debtgdp))


## rm(list = ls())

