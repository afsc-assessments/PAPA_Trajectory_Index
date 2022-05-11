
library(rOSCURS);
YEAR<-2020;
stLLs<-tibble::tibble(LATITUDE=50,LONGITUDE=-145);

#--run the OSCURS model
#----first valid year for website data is Dec 1, 1967.
fnBase="PTI_";
path="./TrajectoriesWeekly";
if (FALSE){
  randNum<-round(runif(1,1,10000));
  res1<-runOSCURS(fnBase=fnBase,
                  path=path,
                  nDays=90,                     #number of days to track
                  stYrs=YEAR,                   #years for releases
                  stMDs=list(JAN=c(1,8,15,22),
                             FEB=c(1,8,15,22),
                             MAR=c(1,8,15,22)), #months and days for releases
                  stLLs=stLLs,
                  randNum=randNum,
                  test=FALSE,
                  verbose=TRUE);
}

#--convert OSCURS output to list with data.frame and sf tibble with a WGS84 lat/lon crs.
#----NOTE: stYr (start year) indicates year in which drifter is RELEASED.
#----Results pertain to Winter, stYr+1 and are labelled as such.
lst1<-convertOSCURStoTbl(fnBase=file.path(path,fnBase),
                        stYrs=YEAR,                   #years for releases
                        stMDs=list(JAN=c(1,8,15,22),
                                   FEB=c(1,8,15,22),
                                   MAR=c(1,8,15,22)), #months and days for releases
                        stLLs=stLLs,
                        verbose=FALSE)

#--get basemap
strCRS<-tmaptools::get_proj4(3338,output="character")
basemap<-wtsGIS::createBaseTMap(
                        layer.land=wtsGIS::getPackagedLayer("Alaska"),
                        layer.bathym=wtsGIS::getPackagedLayer("ShelfBathymetry"),
                        alpha.bathym=0.2,
                        colors.land="grey50",
                        colors.bg="grey85",
                        strCRS.finl=strCRS);

#--plot map with all tracks
tmp     <-lst1$tracks;
ds      <-as.Date(tmp$dayStart)
#tmp$year<-as.numeric(format(ds,"%Y"));
tmp$doy <-as.character(julian(tmp$dayStart,origin=as.Date(paste0(YEAR,"-01-01"))));#--day-of-year
palette<-rev(wtsUtilities::createColorPalette(name="jet",n=14))[1:13];
lst2<-plotPTI(tracks=tmp,
              trackID_col="doy",
              emphasizeID=NULL,
              text_col="doy",
              basemap=basemap,
              color_lines="blue",
              color_text="black",
              alpha_lines=0.5,
              alpha_text=1.0,
              palette=palette,
              showMap=TRUE,
              verbose=TRUE);
png(filename="mapWeekly.png",width=775,height=473);
  print(lst2$map);
dev.off();

