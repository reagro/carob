# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:

  On-farm demonstration plots were set in Zambia to demonstrate the 
  effects of conservation agriculture (CA) technologies as compared 
  to the traditional farmers practice (ploughing with a mouldboard plough). 
  The CA treatments included basins (BA), ripping (RI) and direct seeding 
  with a direct seeder (DS) and direct seeding with a jab planter (JP). 
  Also superimposed to the treatments are rotations and intercropping of maize with 
  a grain legume (either soyabean or cowpea) and these are compared 
  with continuous maize planting. The study is carried out in various
  communities of Zambia. Thus, the data set presents yields for maize 
  and the legumes from these sites over 9 seasons (2006-2015). (2016-12-08)

"
  
  uri <- "https://hdl.handle.net/11529/10825"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "crop_cuts"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    publication=NA,
    data_institutions = "CIMMYT",
    carob_contributor="Cedric Ngakou",
    experiment_type="NA",
    has_weather=FALSE,
    has_soil=FALSE,
    has_management=FALSE
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=1)
  dset$license <- carobiner::get_license(js)
  
  f <- ff[basename(ff) == "Summary Zambia On-farm Demonstration 2006-2015.xls"]
  
  #read the data
  
  r <- readxl::read_excel(f,sheet = 1) |> as.data.frame()
  r1 <- readxl::read_excel(f,sheet = 2) |> as.data.frame()
  
  
  ## process file(s)
  
  d1<-r[,c(2,3,4,18,19,21,22,20)]
  # name columns with standard names 
  colnames(d1)<-c("season","adm1","site","treatment","crop","residue_yield","yield","plant_density")
  d1$crop<-"maize"
  d2<-r1[,c(2,3,4,13,14,16,17,15)]
  colnames(d2)<-c("season","adm1","site","treatment","crop","residue_yield","yield","plant_density")
  
  # combine d1 and d2
  d<-rbind(d1,d2)
  
  # add columns
  d$country <- "Zambia"
  d$dataset_id <- dataset_id
  d$trial_id<-paste0(d$dataset_id,"-",d$adm1)
  d$start_date <-"2006"
  d$end_date <-"2015"
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  
  # fix name treatment name
  
  p <- carobiner::fix_name(d$treatment)
  p<-gsub("Direct seeder","DS",p)
  p<-gsub("direct seeder","DS",p)
  p<-gsub("Direct","DS",p)
  p<-gsub("Control plot","control",p)
  #p<-gsub("DS, soybean-maize rotation","DS, maize-soybean rotation",p)
  #p<-gsub("DS, cowpea-maize rotation","DS, maize-cowpea rotation",p)
  
  d$treatment<- p 
  
  # fix crop name
  e <- carobiner::fix_name(d$crop,"lower")
  d$crop<- e 
  d$crop[d$crop=="cowpeas"]<- "cowpea"
  #add inter crop crop rotation column 
 
  d$intercrops<-ifelse(d$treatment=="DS, maize/cowpea int" ,"cowpea",
                          ifelse(d$treatment=="DS, maize-cowpea rotation","soybean ",
                                 ifelse(d$treatment=="Ripper, maize-soybean rotation","cowpea",
                                        ifelse(d$treatment=="DS, maize-soybean rotation","soybean",
                                               ifelse(d$treatment=="DS, cowpea-maize rotation","soybean",
                                                      ifelse(d$treatment=="Ripper, soybean-maize rotation","cowpea",
                                                             ifelse(d$treatment=="DS, soybean-maize rotation","cowpea","no crop")))))))
  d$crop_rotation<- ifelse(d$intercrops=="cowpea","soybean",
                           ifelse(d$intercrops=="no crop","no crop","cowpea"))
                                   
  
                                                             
  # add longitude and  latitude
  d$latitude[d$adm1=="Monze"] <--16.2759563
  d$longitude[d$adm1=="Monze"] <-27.4763925
  d$latitude[d$adm1=="Kabwe"] <--14.4571147
  d$longitude[d$adm1=="Kabwe"] <-28.3992336
  d$latitude[d$adm1=="Chipata"] <--13.7478246
  d$longitude[d$adm1=="Chipata"] <-32.6324569
  d$latitude[d$adm1=="Chibombo"] <--14.8569383
  d$longitude[d$adm1=="Chibombo"] <-27.6530228
  d$latitude[d$adm1=="Lundazi"] <--12.4137188
  d$longitude[d$adm1=="Lundazi"] <-33.3487457
  d$latitude[d$adm1=="Katete"] <--14.060241
  d$longitude[d$adm1=="Katete"] <-32.04272
  
    # data type 
    d$season<-as.character(d$season)
    d$yield<-(as.double(d$yield))
    # all scripts must end like this
    carobiner::write_files(dset, d, path, dataset_id, group)
}



