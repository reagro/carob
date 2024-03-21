# R script for "carob"

## ISSUES
# ....
path <-setwd (".")
carob_script <- function(path) {
  
  "
	Description:

    The dataset contains the result of a series of experiments 
    conducted in Morocco between 2014 and 2015, about land and water productivity.
    The overall objective of this study is the sustainable increase of wheat
    yield in dry areas of WANA. The purpose is the development of options that
    improve the adaptation of wheat to high temperature and drought.
    
    Crop: durum wheat
    Crop system: Mono-cropping system, rainfed, supplemental irrigation
    
    Related Publication:
    [Mohammed Karrou, Khalid Daoui, Rachid Razouk, Mohamed Boutfirass, 
    Abdeljabar Bahri. (22/12/2015). Land and Water Productivity: 
    Technical report. doi: 20.500.11766/4516]
    
"
  uri <- "hdl:20.500.11766.1/FK2/XOSFYG"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "wheat_trials"
  
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="CGIAR Research Program on Dryland Systems led by ICARDA",
    uri=uri,
    publication="20.500.11766/4516",
    data_citation = 'Karrou, Mohammed; Daoui, Khalid; Razouk, Rachid; Boutfirass, Mohamed; Bahri, Abdeljabar, 2015, "Experimental results on land and water productivity in rainfed areas in Morocco", https://hdl.handle.net/20.500.11766.1/FK2/XOSFYG',
    data_institutions = "ICARDA",
    carob_contributor="Samar Attaher",
    carob_date="2023-03-15",
    data_type="experiment"
    
    )
  
  ## download and read data 
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id,path,group,major=5,minor = 0)
  dset$license <- carobiner::get_license (js)
  dset$title <- carobiner::get_title(js)   #issue: returns error: 'get_title' is not an exported object from 'namespace:carobiner'
  dset$authors <- carobiner::get_authors(js) #issue: returns error: 'get_authors' is not an exported object from 'namespace:carobiner'
  dset$description <- carobiner::get_description(js) #issue: returns error:'get_description' is not an exported object from 'namespace:carobiner'
  
  #read the data file
  f <- ff[basename(ff) == "Grain_Yield.csv"] 
  # read the dataset
  r <- data.frame(read.csv(f, sep=";"))
 
  #Istoped here#######################
  
  #change the columns names, and new dataframe include the significant columns that describe the experemint
  d1 <- carobiner::change_names(r, c("Water_Regime", "Planting_Date","Bloc", "Variety","Yield"), 
                                c("treatment", "variety_type","rep","variety_code","yield"))
  
  
#populating data for the new added columns 
d1$country<-"Morocco"
d1$adm1<- "Béni Mellal-Khénifra Region"
d1$adm2<- "Fquih Ben Salah Province"
d1$adm3<- "Afourar"
d1$site<- "Afourer experiment station"
d1$latitude<- 32.26191493053088
d1$longitude<- -6.534853791058046
d1$crop<-"durum wheat"
d1$seed_amount<-160
d1$rain<-378
d1$P_fertilizer<-200
d1$N_fertilizer<-60
  
d1$treatment [d1$treatment=="Irrigated"] <- "supplemental irrigation"

#varieties names:
d1$variety [d1$variety_code=="V1"] <- "Karim"
d1$variety [d1$variety_code=="V2"] <- "Louiza"
d1$variety [d1$variety_code=="V3"] <- "Nassira"
d1$variety [d1$variety_code=="V4"] <- "PM9"

#planting_date  
d1$planting_date [d1$variety_type=="Early"] <- "2014-11-11"
d1$planting_date [d1$variety_type=="Late"] <- "2014-12-24"

#irrigation_amount & irrigation_number
i <- (d1$treatment=="supplemental irrigation"& d1$variety_type=="Early")
d1$irrigation_amount[i] <- 160
d1$irrigation_number[i] <- 3

i <- (d1$treatment=="supplemental irrigation"& d1$variety_type=="Late")
d1$irrigation_amount[i] <- 275
d1$irrigation_number[i] <- 5

i <- (d1$treatment=="Rainfed")
d1$irrigation_amount[i] <- 0
d1$irrigation_number[i] <- 0

# yield...Unit correction from quintal/ha to kg/ha
d1$yield <- d1$yield * 100


d2 <- d1[,c("country","adm1","adm2","adm3","site","latitude","longitude","crop","treatment","variety","variety_code","variety_type","rep",
            "seed_amount","planting_date","rain", "irrigation_amount","irrigation_number","P_fertilizer",
            "N_fertilizer", "yield","Lodging")]

carobiner::write_files (dset, d2, path=path)
  
}
