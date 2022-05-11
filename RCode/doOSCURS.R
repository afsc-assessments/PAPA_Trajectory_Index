
library(rOSCURS);
stLLs<-readr::read_csv("OSCURS_StartLocation.csv");

#--run the OSCURS model
fnBase="PTI_";
path="./2018";
randNum<-round(runif(1,1,10000));
res1<-runOSCURS(fnBase=fnBase,
                path=path,
                nDays=90,           #number of days to track
                stYrs=1991:2013,    #years for releases
                stMDs=list(DEC=1),  #months and days for releases
                stLLs=stLLs,
                randNum=randNum,
                test=FALSE,
                verbose=TRUE);

#--convert OSCURS output to list with data.frame and sf tibble with a WGS84 lat/lon crs.
lst1<-convertOSCURStoTbl(fnBase=file.path(path,fnBase),
                        stYrs=1991:2018,   #years for releases
                        stMDs=list(DEC=1), #months and days for releases
                        stLLs=stLLs,
                        verbose=FALSE)

#--plot map with tracks
lst2<-plotPTI(tracks=lst1$tracks,alpha=0.8,showMap=TRUE);
#map<-plotOSCURS(tracks=lst$tracks,stLLs=stLLs,alpha=0.5,showMap=TRUE);

#--save end points for Excel
write.csv(sf::st_drop_geometry(lst2$endLocs),file="OSCURS.EndLocations.csv");

#--save tracks shapefile for ArcGIS
tmaptools::write_shape(lst2$tracks,file="OSCURS.Tracks.shp");

#--save objects
save(stLLs,fnBase,path,lst1,lst2,file="outputOSCURS.RData")

