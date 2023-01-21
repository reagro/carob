# R script for "carob"

## ISSUES
###reference file missing: doi_10.7910_DVN_LTS278

library(carobiner)

carob_script <- function(path) {

"Description
Title: Replication Data for: Response of Maize to blended fertilizer  
Abstract: Maize grain and bio-mass yield were increased by application of different rates of blended fertilizers (2020-12-02)"

## Process 
uri <- "doi:10.7910/DVN/LTS278"
dataset_id <- carobiner::simple_uri(uri)
group <- "fertilizer"
dset <- data.frame(
  dataset_id = dataset_id,
  group=group,
  uri=uri,
  publication=NA,
  carob_contributor="Siyabusa Mkuhlani",
  experiment_type="fertilizer",
  has_weather=FALSE,
  has_management=FALSE
)

## treatment level data 
ff  <- carobiner::get_data(uri, path, group)

## read the json for version, license, terms of use  
js <- carobiner::get_metadata(dataset_id, path, major=1, minor=0, group)
dset$license <- carobiner::get_license(js)[1]

## the AFSIS data 
f <- ff[basename(ff) == "AGP II 2017.18 RAW DATA.xlsx"] 
d <- suppressMessages(as.data.frame(readxl::read_excel(f)))

##Skip early rows(Descriptive rows)
dd <- suppressMessages(as.data.frame(readxl::read_excel(f)[,-c(1:11)]))

#transfer columns
dd$country <- "Ethiopia"

#Transfer locations & year
dd$region <- NA
dd$region <- replace(dd$region,1:11,"Limmu Sekka")
dd$region <- replace(dd$region,12:27,"Omo Nada")
dd$year <- NA
dd$year <- replace(dd$year,1:11,"2016/17")
dd$year <- replace(dd$year,12:27,"2017/18")

#Dissagregate the data frame
de <- dd[c(1:10),c(1:4,9:11)] #limmu sekka 2017/18
de$year[de$year=='2016/17'] <- '2017/18'
de <- de[-1,]
#names(de)
colnames(de) <- c("Rep","Trt","GY kg/ha","BM kg/ha","country","region","year")

df <- dd[c(14:22),c(1:4,9:11)] #Omo Nada 2017/18
colnames(df) <- c("Rep","Trt","GY kg/ha","BM kg/ha","country","region","year")
dg <- dd[c(2:10),c(5:11)] #Limmu Sekka 2016/17
colnames(dg) <- c("Rep","Trt","GY kg/ha","BM kg/ha","country","region","year")

dh <- dd[c(14:22),c(5:11)] #Omo Nada 2016/17
dh$year[de$year=='2017/18'] <- '2016/17'
colnames(dh) <- c("Rep","Trt","GY kg/ha","BM kg/ha","country","region","year")

##BindRows
dv  <- rbind(dh,dg,df,de)
colnames(dv) <- c("rep","treatment","yield","biomass_total","country","region","start_date")

#####Change Treatment
dv$treatment[dv$treatment=='A'] <- 'Ctrl/FP'
dv$treatment[dv$treatment=='B'] <- 'Cal. P & rec. N'
dv$treatment[dv$treatment=='C'] <- '92kg/ha N & 30kg/ha P'

##Correct date
dv$start_date[dv$start_date=='2016/17'] <- '2016'
dv$start_date[dv$start_date=='2017/18'] <- '2017'
dv$end_date <- NA
dv$end_date <- replace(dv$end_date,1:18,"2017")
dv$end_date <- replace(dv$end_date,19:36,"2018")

#Format data set
dv$dataset_id <- "doi_10.7910_DVN_LTS278"
dv$crop <- 'maize'
dv$on_farm <- TRUE
dv$is_survey <- FALSE
dv$trial_id <- 'Blendedfert'

##Re-Order
dv <- dv[,-1]
dv <- dv[c("dataset_id","trial_id","country","region","start_date","end_date","crop","treatment","yield","biomass_total","on_farm","is_survey")]

carobiner::write_files(dset, dv, path, dataset_id, group)

}

