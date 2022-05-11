getPTI_asDataframe<-function(YEAR){
  tmp<-read.csv(file=file.path(".",YEAR,paste0("PTI.EndLocations.1968-",YEAR,".csv")),stringsAsFactors=FALSE);
  dfrPTI<-read.csv(file="PTI_EndLocations1902-1967.csv");
  dfrPTI<-rbind(dfrPTI,data.frame(year=as.numeric(format(as.Date(tmp$dayEnd),"%Y")),lat=tmp$latEnd,lon=tmp$lonEnd));
  return(dfrPTI);
}
