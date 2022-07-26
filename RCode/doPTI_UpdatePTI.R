require(ggplot2);
require(magrittr);
require(rOSCURS);

#-NOTE: all paths should be relative to the project root folder

YEAR<-2022;
stLLs<-tibble::tibble(LATITUDE=50,LONGITUDE=-145);#--PAPA Ocean Station location

dirCurr = file.path(".","CurrentResults");
if (!dir.exists(dirCurr)) dir.create(dirCurr)
fnBase="PTI_";
path="./Trajectories";

#--run the OSCURS model
#----first valid year for website data is Dec 1, 1967.
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
  dfrPTI = readr::read_csv(file=file.path("./RCode","PTI_EndLocations1902-1967.csv"));
  dfrPTI = dplyr::bind_rows(dfrPTI,data.frame(year=as.numeric(format(as.Date(tmp$dayEnd),"%Y")),lat=tmp$latEnd,lon=tmp$lonEnd));
  trks   = lst1$tracks;
  wtsUtilities::saveObj(lst1$tracks,file.path(dirCurr,paste0("rda_sfTrks_1968to",YEAR,".RData")));
  wtsUtilities::saveObj(dfrPTI,file.path(dirCurr,paste0("rda_dfrPTI_1902to",YEAR,".RData")));
  rm(tmp,lst1);
} else {
  trks   = wtsUtilities::getObj(file.path(dirCurr,paste0("rda_sfTrks_1968to",YEAR,".RData")));
  dfrPTI = wtsUtilities::getObj(file.path(dirCurr,paste0("rda_dfrPTI_1902to",YEAR,".RData")));
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

#--plot PTI with smooth (colours from NMFS palette)
clrs = c("L-T-M"="#0093D0","PTI"="#4C9C2E","5-Yr Running"="#D65F00");
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
            panel.background = element_rect(fill="white"),
            panel.grid.major = element_line(colour="white",size=0.25),
            panel.grid.minor = element_blank(),
            panel.border     = element_rect(colour="black",fill=NA))
print(p);
ggsave(filename=file.path(dirCurr,"fig_2_PTI.png"),width=6,height=4,units="in",dpi=600)

#--plot tracks
#----get basemap
bbx = wtsGIS::getBBox(c(-160,46,-125,61.5));
#strCRS<-tmaptools::get_proj4(3338,output="character")
strCRS<-wtsGIS::get_crs(4326);
shp = "~/Work/Programming/R/GitPackages/ACLIM2/Data/in/Map_layers/shp_files/global/natural_earth_vector/10m_physical/ne_10m_land.shp";
sf_shp = wtsGIS::readShapefile(shp);#--in WGS84
sf_lnd = sf_shp %>% sf::st_crop(bbx);
sf_bth = wtsGIS::getPackagedLayer("ShelfBathymetry") %>% sf::st_transform(strCRS) %>% sf::st_crop(bbx);
bmls = wtsGIS::gg_CreateBasemapLayers(layer.land=sf_lnd,
                                      layer.bathym=sf_bth,
                                      bbox=bbx,
                                      alpha.bathym=0.2,
                                      colors.land="grey50",
                                      colors.bg="white",
                                      final.crs=strCRS
                                      )

#----plot maps with tracks
trks$year<-substr(trks$dayEnd,start=1,4);
trks_rcnt = trks %>% dplyr::filter(year==as.character(YEAR));
#------function to get final location from each track
#--------to provide coordinates to place track text label
get_last<-function(sfc){
  # message("starting get_last")
  # message(print(sfc));
  n = length(sfc);
  # message("n = ",n)
  dfr = tibble::tibble(x = vector(mode="numeric",length=n),
                       y = vector(mode="numeric",length=n));
  for (i in 1:n){
    # message("i = ",i)
    sfg = sfc[[i]];
    # message("nrow(sfg) = ",nrow(sfg))
    pt = sfg[nrow(sfg),];
    # message("pt = ",pt);
    dfr$x[i] = pt[1];
    dfr$y[i] = pt[2];
  }
  # message("dfr = ",print(dfr))
  sfc_pt = sfheaders::sfc_point(dfr,x="x",y="y");
  sf::st_crs(sfc_pt) = sf::st_crs(sfc);
  # message(print(sfc_pt))
  # message("finished get_last\n\n")
  return(sfc_pt)
}
#res = get_last(trks$geometry)

#----plot map of all tracks
map_all = ggplot(trks %>% dplyr::filter(year!=as.character(YEAR)),
                 aes(colour=year,label=year)) + 
            geom_sf(data=sf_lnd,inherit.aes=FALSE) + 
            geom_sf(data=sf_bth,inherit.aes=FALSE) +
            geom_sf(alpha=0.5) + geom_sf_text(alpha=0.5,fun.geometry=get_last) +
            geom_sf(data=trks_rcnt,colour="black",size=2) +
            geom_sf_text(data=trks_rcnt,colour="black",fun.geometry=get_last,nudge_x=1.6,size=6,fontface="bold") +
            bmls$map_scale + 
            bmls$theme + 
            theme(panel.background=element_rect(colour="black",fill="white"),
                  panel.border=element_rect(colour="black",fill=NA),
                  legend.position="none");
print(map_all);
ggsave(filename=file.path(dirCurr,"mapAll.png"),plot=map_all,width=8,height=6,units="in");#--was 775 x 473 pixels

#----plot map of all tracks
map_all = ggplot(trks, aes(colour=year,label=year)) + 
            geom_sf(data=sf_lnd,inherit.aes=FALSE) + 
            geom_sf(data=sf_bth,inherit.aes=FALSE) +
            #--tracks before YEAR-10
            geom_sf(     data=trks %>% dplyr::filter(as.numeric(year)<YEAR-10),colour="grey",alpha=0.3) + 
            geom_sf_text(data=trks %>% dplyr::filter(as.numeric(year)<YEAR-10),colour="grey",alpha=0.3,fun.geometry=get_last) +
            #--tracks YEAR-10<year<YEAR-1
            geom_sf(     data=trks %>% dplyr::filter(dplyr::between(as.numeric(year),YEAR-10,YEAR-1)),alpha=1.0) +
            geom_sf_text(data=trks %>% dplyr::filter(dplyr::between(as.numeric(year),YEAR-10,YEAR-1)),alpha=1.0,fun.geometry=get_last) +
            #--current year track
            geom_sf(     data=trks_rcnt,colour="black",size=2) +
            geom_sf_text(data=trks_rcnt,colour="black",size=6,fun.geometry=get_last,nudge_x=1.6,fontface="bold") +
            bmls$map_scale + 
            bmls$theme + 
            theme(panel.background=element_rect(colour="black",fill="white"),
                  panel.border=element_rect(colour="black",fill=NA),
                  legend.position="none");
print(map_all);
ggsave(filename=file.path(dirCurr,"fig_1_mapAll.png"),plot=map_all,width=8,height=6,units="in");#--was 775 x 473 pixels

#--plot map of tracks from last 10 years
map_rcnt = ggplot(trks %>% dplyr::filter(dplyr::between(as.numeric(year),YEAR-10,YEAR)),
                 aes(colour=year,label=year)) + 
            geom_sf(data=sf_lnd,inherit.aes=FALSE) + 
            geom_sf(data=sf_bth,inherit.aes=FALSE) +
            geom_sf(alpha=0.5) + geom_sf_text(alpha=1.0,fun.geometry=get_last,fontface="bold") +
            geom_sf(data=trks_rcnt,colour="black",size=2) +
            geom_sf_text(data=trks_rcnt,colour="black",fun.geometry=get_last,nudge_x=1.6,size=6,fontface="bold") +
            bmls$map_scale + 
            bmls$theme + 
            theme(panel.background=element_rect(colour="black",fill="white"),
                  panel.border=element_rect(colour="black",fill=NA),
                  legend.position="none");
print(map_rcnt);
ggsave(filename=file.path(dirCurr,"fig_1_mapRecents.png"),plot=map_rcnt,width=8,height=6,units="in");#--was 775 x 473 pixels

