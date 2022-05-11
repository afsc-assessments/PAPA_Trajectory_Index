
library(rOSCURS);
stLLs<-readr::read_csv("./2019/PTI_StartLocation.csv");

#--run the OSCURS model
#----first valid year for website data is Dec 1, 1967.
fnBase="PTI_";
path="./Trajectories";
if (FALSE){
  randNum<-round(runif(1,1,10000));
  res1<-runOSCURS(fnBase=fnBase,
                  path=path,
                  nDays=90,           #number of days to track
                  stYrs=1967:1990,    #years for releases
                  stMDs=list(DEC=1),  #months and days for releases
                  stLLs=stLLs,
                  randNum=randNum,
                  test=FALSE,
                  verbose=TRUE);
}

#--convert OSCURS output to list with data.frame and sf tibble with a WGS84 lat/lon crs.
#----NOTE: stYr (start year) indicates year in which drifter is RELEASED.
#----Results pertain to Winter, stYr+1 and are labelled as such.
lst1<-convertOSCURStoTbl(fnBase=file.path(path,fnBase),
                        stYrs=1967:2018,   #years for releases
                        stMDs=list(DEC=1), #months and days for releases
                        stLLs=stLLs,
                        verbose=FALSE)

#--save end points for Excel
tmp<-lst1$tracks;
tmp$lonStart <- -1*tmp$lonStart;#switch to degrees west
tmp$lonEnd   <- -1*tmp$lonEnd;  #switch to degrees west
wtsGIS::writeTableToCSV(tmp,file="PTI.EndLocations.1968-2019.csv");
rm(tmp);

#--get basemap
strCRS<-tmaptools::get_proj4(3338,output="character")
basemap<-createBaseTMap(layer.land=wtsGIS::getPackagedLayer("Alaska"),
                        layer.bathym=wtsGIS::getPackagedLayer("ShelfBathymetry"),
                        alpha.bathym=0.2,
                        strCRS.finl=strCRS);
#--plot map with all tracks
tmp<-lst1$tracks;
tmp$year<-as.numeric(substr(tmp$dayEnd,start=1,4));
palette<-rev(wtsUtilities::createColorPalette(name="jet",n=11))[1:10];
lst2<-plotPTI(tracks=tmp,
              style="grey",
              basemap=basemap,
              color="year",
              alpha=0.5,
              palette=palette,
              showMap=TRUE);
#--plot map with tracks from last 10 years
tmp1<-tmp[tmp$year>2009,];
lst3<-plotPTI(tracks=tmp1,
              style="grey",
              basemap=basemap,
              color="year",
              palette=palette,
              alpha=1.0,
              showMap=TRUE);
rm(tmp,lst3);

#--save tracks shapefile for ArcGIS
wtsGIS::writeTableToShapefile(lst2$tracks,file="PTI.Tracks.1968-2019.shp");

#--save objects
save(stLLs,fnBase,path,lst1,lst2,file="PTI.1968-2019.RData")

