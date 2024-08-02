#--extract GOA recruitment time series
dirPrj = rstudioapi::getActiveProject();
fn = file.path("Data","Assessment_TimeSeries_Data.csv");

#--read descriptions, find columns with recruitment information
qtys = names(readr::read_csv(fn,col_names=TRUE,name_repair="minimal",show_col_types=FALSE,n_max=1,skip=6));
rec_cols = qtys |> stringr::str_detect("^Rec");
cdx      = which(rec_cols);#--column indices 
rec_typs = qtys[rec_cols];

#--read stock names from first row
stock_cols = names(readr::read_csv(fn,col_names=TRUE,name_repair="minimal",show_col_types=FALSE,n_max=1,skip=0))[rec_cols];
stocks = stock_cols |> stringr::str_split_i(" \\- ",1);
areas  = stock_cols |> stringr::str_split_i(" \\- ",2);
stock_cols |> stringr::str_extract(".+?(?= \\- )"); #--look ahead: match all characters (except newlines) up to  " - "
stock_cols |> stringr::str_extract("(?<= \\- ).+"); #--look behind: match all characters (except new line) after " - "

#--get assessment year
asmt_yrs = as.numeric(names(readr::read_csv(fn,col_names=TRUE,name_repair="minimal",show_col_types=FALSE,n_max=1,skip=3))[rec_cols]);

#--create info dataframe
dfrInfo = tibble::tibble(stock=stocks,area=areas,asessment=asmt_yrs,quantity=rec_typs)

#--extract recruitment time series
dfrRec = readr::read_csv(fn,col_names=FALSE,name_repair="minimal",show_col_types=FALSE,n_max=Inf,skip=8)[,c(2,cdx)];
names(dfrRec) = c("year",stocks);
dfrRec = dfrRec |> 
            tidyr::pivot_longer(cols=tidyselect::all_of(stocks),
                                names_to="stock",values_to="recruits") |> 
            dplyr::filter(!is.na(recruits)) |> 
            dplyr::arrange(stock,year);

#--get PTI
dfrPTI = wtsUtilities::getObj(file.path(dirPrj,"ResultsCurrent","rda_dfrPTI_1902to2024.RData"));

#--look at cross-correlations
source(file.path(dirPrj,"Data","r_cc.test.R"));
source(file.path(dirPrj,"Data","r_plotcorr.R"));
source(file.path(dirPrj,"Data","r_plotstat.R"));
require(ggplot2);

##--correlations with PTI latitude----
lstLat = list();
for (stock_ in stocks){
  #--testing: stock_ = stocks[1];
  tmpRec = dfrRec |> dplyr::filter(stock==stock_);
  tmpPTI = dfrPTI |> dplyr::filter(year %in% tmpRec$year);
  lstLat[[stock_]] = cc.test(tmpRec$recruits,tmpPTI$lat,max.lag=5,var.names=c(stock_,"PTI"));
}

for (stock_ in stocks){
  print(lstLat[[stock_]]$plots$corr)
}

#--correlations with PTI longitude
lstLon = list();
for (stock_ in stocks){
  #--testing: stock_ = stocks[1];
  tmpRec = dfrRec |> dplyr::filter(stock==stock_);
  tmpPTI = dfrPTI |> dplyr::filter(year %in% tmpRec$year);
  lstLon[[stock_]] = cc.test(tmpRec$recruits,tmpPTI$lon,max.lag=5,var.names=c(stock_,"PTI (lon)"));
}

for (stock_ in stocks){
  print(lstLon[[stock_]]$plots$corr)
}

#--Winter PDO----
dfrPDO = readr::read_csv(file.path(dirPrj,"Data/data_Monthly_PDO.csv"),skip=1) |> 
           tidyr::pivot_longer(cols=!1) |> 
           dplyr::filter(value<99) |> 
           dplyr::mutate(lead1=dplyr::lead(value,n=1),
                         lead2=dplyr::lead(value,n=2),
                         avg3mo=(value+lead1+lead2)/3,year=Year+1) |> 
           dplyr::filter(name=="Dec") |> 
           dplyr::select(year,value=avg3mo);
tmpPDO = dfrPDO |> dplyr::filter(year %in% dfrPTI$year);
lstPDO = cc.test(dfrPTI$lat,tmpPDO$value,max.lag=5,var.names=c("PTI","Winter PDO"));
print(lstPDO$plots$corr);

#--Winter NPGO----
dfrNPGO = readr::read_csv(file.path(dirPrj,"Data/data_Monthly_NPGO.csv"),skip=24) |> 
           dplyr::mutate(lead1=dplyr::lead(NPGO,n=1),
                         lead2=dplyr::lead(NPGO,n=2),
                         avg3mo=(NPGO+lead1+lead2)/3,
                         year=YEAR+1) |> 
           dplyr::filter(MONTH==12,!is.na(avg3mo)) |> 
           dplyr::select(year,value=avg3mo);
tmpNPGO = dfrNPGO |> dplyr::filter(year %in% dfrPTI$year);
tmpPTI  = dfrPTI  |> dplyr::filter(year %in% tmpNPGO$year);
lstNPGO = cc.test(tmpPTI$lat,tmpNPGO$value,max.lag=5,var.names=c("PTI","Winter NPGO"));
print(lstNPGO$plots$corr);

#--Winter AO----
dfrAO = readr::read_csv(file.path(dirPrj,"Data/data_Monthly_AO.csv"),skip=1) |> 
           dplyr::mutate(lead1=dplyr::lead(AO,n=1),
                         lead2=dplyr::lead(AO,n=2),
                         avg3mo=(AO+lead1+lead2)/3,
                         year=year+1) |> 
           dplyr::filter(month==12,!is.na(avg3mo)) |> 
           dplyr::select(year,value=avg3mo);
tmpAO  = dfrAO |> dplyr::filter(year %in% dfrPTI$year);
tmpPTI = dfrPTI  |> dplyr::filter(year %in% tmpAO$year);
lstAO = cc.test(tmpPTI$lat,tmpAO$value,max.lag=5,var.names=c("PTI","Winter AO"));
print(lstAO$plots$corr);




