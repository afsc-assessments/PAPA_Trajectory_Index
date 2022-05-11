#--get PTI time series up to YEAR
YEAR<-2020;
source("getPTI_asDataframe.R");
dfrPTI<-getPTI_asDataframe(YEAR);
dfrPTI$lat_z<-(dfrPTI$lat-mean(dfrPTI$lat))/sd(dfrPTI$lat);
rng<-range(dfrPTI$year);

#define winter months
winter<-c("JAN","FEB","MAR");

#--get and process PDO dataset
dfrPDO<-read.csv("envTS_PDO.csv",stringsAsFactors=FALSE);
#----calculate average winter (JFM) PDO
mnPDO<-apply(dfrPDO[,winter],1,mean);
dfrPDO$winter<-mnPDO;
names(dfrPDO)<-tolower(names(dfrPDO));
#----update inclusive year range
rng<-c(max(rng[1],min(dfrPDO$year)),min(rng[2],max(dfrPDO$year)));

#--get and process AO dataset
dfrAO<-read.csv("envTS_ArcticOscillation.csv",stringsAsFactors=FALSE);
#----calculate average winter (JFM) AO
dfrAO<-dfrAO[dfrAO$month<=3,];
dfrAO$month<-winter[dfrAO$month];
dfrAO<-reshape2::dcast(dfrAO,year~month,value.var="value");
mnAO<-apply(dfrAO[,winter],1,mean);
dfrAO$winter<-mnAO;
names(dfrAO)<-tolower(names(dfrAO));
#----update inclusive year range
rng_AO<-c(max(rng[1],min(dfrAO$year)),min(rng[2],max(dfrAO$year)));

#--plot time series
library(ggplot2);
tmp<-dfrPTI[,c("year","lat_z")]; names(tmp)[2]<-"winter";
tmp<-rbind(cbind(tmp,index="PTI"),
           cbind(dfrPDO[,c("year","winter")],index="PDO"),
           cbind(dfrAO[,c("year","winter")],index="AO"));
p <- ggplot(tmp,mapping=aes_string(x="year",y="winter",colour="index"));
p <- p + geom_line() + geom_smooth();
p <- p + labs(x="year",y="value")
print(p);

#--
idxPTI<-(rng[1]<=dfrPTI$year)&(dfrPTI$year<=rng[2]);
idxPDO<-(rng[1]<=dfrPDO$year)&(dfrPDO$year<=rng[2]);
cor(dfrPTI$lat_z[idxPTI],dfrPDO$winter[idxPDO]);

#--
idxPTI<-(rng_AO[1]<=dfrPTI$year)&(dfrPTI$year<=rng_AO[2]);
idxPDO<-(rng_AO[1]<=dfrPDO$year)&(dfrPDO$year<=rng_AO[2]);
idxAO <-(rng_AO[1]<=dfrAO$year) &(dfrAO$year <=rng_AO[2]);
cor_PTI_AO <-cor(dfrPTI$lat_z[idxPTI],dfrAO$winter[idxAO]);
cor_PTI_PDO<-cor(dfrPTI$lat_z[idxPTI],dfrPDO$winter[idxPDO]);
cor_PDO_AO <-cor(dfrPDO$winter[idxPDO],dfrAO$winter[idxAO]);

