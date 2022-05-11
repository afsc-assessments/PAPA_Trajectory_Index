require(magrittr);
require(rOSCURS);

YEAR<-2022;
stLLs<-tibble::tibble(LATITUDE=50,LONGITUDE=-145);#--PAPA Ocean Station location

dirCurr = file.path(".",YEAR);
if (!dir.exists(dirCurr)) dir.create(dirCurr)

#--run the OSCURS model
#----first valid year for website data is Dec 1, 1967.
fnBase="PTI_";
path="./Trajectories";
if (FALSE){
  #--run the model and save output to ./Trajectories
  randNum<-round(runif(1,1,10000));
  res1<-runOSCURS(fnBase=fnBase,
                  path=path,
                  nDays=90,           #number of days to track
                  stYrs=YEAR-1,       #years for releases
                  stMDs=list(DEC=1),  #months and days for releases
                  stLLs=stLLs,
                  randNum=randNum,
                  test=FALSE,
                  verbose=TRUE);
}

if (FALSE){
  #--convert OSCURS output to list with data.frame and sf tibble with a WGS84 lat/lon crs.
  #----NOTE: stYr (start year) indicates year in which drifter is RELEASED.
  #----Results pertain to Winter, stYr+1 and are labelled as such.
  lst1<-convertOSCURStoTbl(fnBase=file.path(path,fnBase),
                          stYrs=1967:(YEAR-1),   #years for releases
                          stMDs=list(DEC=1),     #months and days for releases
                          stLLs=stLLs,
                          verbose=FALSE)
  
  #--save end points for Excel
  tmp<-lst1$tracks;
  tmp$lonStart <- -1*tmp$lonStart;#switch to degrees west
  tmp$lonEnd   <- -1*tmp$lonEnd;  #switch to degrees west
  wtsGIS::writeTableToCSV(tmp,file=file.path(dirCurr,paste0("PTI.EndLocations.1968-",YEAR,".csv")));
  dfrPTI<-read.csv(file="PTI_EndLocations1902-1967.csv",stringsAsFactors=FALSE);
  dfrPTI<-rbind(dfrPTI,data.frame(year=as.numeric(format(as.Date(tmp$dayEnd),"%Y")),lat=tmp$latEnd,lon=tmp$lonEnd));
  wtsUtilities::saveObj(dfrPTI,file.path(dirCurr,paste0("dfrPTI_1902to",YEAR,".RData")));
  rm(tmp);
} else {
  dfrPTI = wtsUtilities::getObj(file.path(dirCurr,paste0("dfrPTI_1902to",YEAR,".RData")));
}

#--calculate smooth PTI using a gam
mdl.lat<-mgcv::gam(data=dfrPTI,formula=lat~s(year,bs="tp",k=50));
prd.lat<-predict(mdl.lat,se.fit=TRUE);
dfrPTI$prd_lat<-prd.lat$fit;     #--gam-predicted latitude
dfrPTI$se_lat<-prd.lat$se.fit;   #--gam-predicted se
#--calculate mean PTI
dfrPTI$mn_lat<-mean(dfrPTI$lat); #--long-term mean 
#--calculate 5-year running average PTI
avg<-2; krnl<-kernel("daniell",avg); #--defines symmetric 5-point running average
dfrPTI$krn_lat<-c(rep(NA,avg),stats::kernapply(dfrPTI$lat,krnl),rep(NA,avg));

#--plot PTI with smooth
require(ggplot2);
clrs = c("L-T-M"="green","PTI"="black","5-Yr Running"="red");
p <- ggplot(dfrPTI,mapping=aes(x=year,y=lat,colour="PTI")) +
      geom_line(mapping=aes(y=mn_lat,colour="L-T-M"),size=1.25) +      #--long-term mean 
      geom_line(linetype=2) + geom_point() +                          #--annual PTI
# p <- p + geom_ribbon(mapping=aes(ymin=prd_lat-se_lat,ymax=prd_lat+se_lat),fill="green",alpha=0.3)
# p <- p + geom_line(mapping=aes_string(y="prd_lat"),colour="green",size=2,alpha=0.5);
# p <- p + geom_smooth(method="gam",color="red");
#      geom_smooth(method="loess",color="yellow") +
      geom_line(mapping=aes(y=krn_lat,colour="5-Yr Running"),size=1.25,alpha=0.8) + #--running average PTI
      scale_x_continuous(breaks=seq(1900,2050,10),limits=c(1900,YEAR+1),expand=c(0,0)) +
      scale_y_continuous(breaks=seq(46,60,2)) +
      scale_colour_manual(name="type",values=clrs,breaks=names(clrs)) +
      labs(x="",y="Latitude (N)") +
      theme(axis.title       = element_text(size=12,face="bold"),
            axis.title.x     = element_blank(),
            axis.text.x      = element_text(size=11,angle=90,face="bold"),
            axis.text.y      = element_text(size=11,angle= 0,face="bold"),
            axis.line        = element_line(colour="black"),
            legend.position  = "top",
            legend.title     = element_blank(),
            legend.text      = element_text(size=12,face="bold"),
            panel.background = element_rect(fill="light blue"),
            panel.grid.major = element_line(colour="black",size=0.25),
            panel.grid.minor = element_blank(),
            panel.border     = element_rect(colour="black",fill=NA))
print(p);
ggsave(filename=file.path(dirCurr,"fig_2_PTI.png"),width=6,height=4,units="in")

#--get basemap
bbx = wtsGIS::getBBox(c(-160,46,-125,61));
#strCRS<-tmaptools::get_proj4(3338,output="character")
strCRS<-wtsGIS::get_crs(4326);
shp = "~/Work/Programming/R/GitPackages/ACLIM2/Data/in/Map_layers/shp_files/global/natural_earth_vector/10m_physical/ne_10m_land.shp";
sf_shp = wtsGIS::readShapefile(shp);
sf_lnd = sf_shp %>% sf::st_crop(bbx);
# basemap<-wtsGIS::tmap_CreateBasemap(
#                         layer.land=wtsGIS::getPackagedLayer("Alaska"),
#                         layer.bathym=wtsGIS::getPackagedLayer("ShelfBathymetry"),
#                         bbox=wtsGIS::getStandardBBox("CGOA"),
#                         alpha.bathym=0.2,
#                         colors.land="grey50",
#                         colors.bg="grey85",
#                         final.crs=strCRS);
basemap<-wtsGIS::tmap_CreateBasemap(
                        layer.land=sf_lnd,
                        layer.bathym=wtsGIS::getPackagedLayer("ShelfBathymetry"),
                        bbox=bbx,
                        alpha.bathym=0.2,
                        colors.land="grey50",
                        colors.bg="grey85",
                        final.crs=strCRS);
#--plot map with all tracks
tmp<-lst1$tracks;
tmp$year<-as.numeric(substr(tmp$dayEnd,start=1,4));
palette<-rev(wtsUtilities::createColorPalette(name="jet",n=11))[1:10];
lst2<-plotPTI(tracks=tmp,
              basemap=basemap,
              bounding_box=bbx,
              color_lines="year",
              color_text="year",
              alpha_lines=0.5,
              palette=palette,
              showMap=TRUE,
              verbose=TRUE);
png(filename=file.path(dirCurr,"mapAll.png"),width=775,height=473);#--was 775 x 473 pixels
  print(lst2$map);
dev.off();

#--plot map with tracks from last 10 years
tmp1<-tmp[tmp$year>(YEAR-10),];
lst3<-plotPTI(tracks=tmp1,
              basemap=basemap,
              bounding_box=bbx,
              color_lines="year",
              color_text="year",
              alpha_lines=1.0,
              palette=palette,
              showMap=TRUE);
png(filename=file.path(dirCurr,"fig_1_mapRecent.png"),,width=775,height=473);#--was 775 x 473 pixels
 print(lst3$map);
dev.off();
rm(tmp,lst3);

#--save tracks shapefile for ArcGIS
shp = file.path(dirCurr,paste0("PTI.Tracks.1968-",YEAR,".shp"));
wtsGIS::writeToShapefile(lst2$tracks,file=shp);

#--save objects
save(stLLs,fnBase,path,lst1,lst2,file=file.path(dirCurr,paste0("PTI.1968-",YEAR,".RData")));

