# R script for "carob"

## ISSUES
#Not showing license, showing question mark

#remotes::install_github("reagro/carobiner")
library(carobiner)

carob_script <- function(path) {
  
  "Description
Title:Responses of upland NERICA rice varieties to nitrogen and plant density
Abstract: Improved varieties, nitrogen fertilizer, and plant spacing have been identified for increasing upland rice productivity. However, these factors have not been adequately investigated on interspecific rice, New Rice for Africa (NERICA). Different levels of nitrogen (0, 30, 60 and 120 kg/ha) and plant spacing (dibbling: 30 6 30 cm, 20 6 20 cm, and drilling: 25 6 5 cm) on the growth and yield of three interspecific rice varieties and a check variety were evaluated on Terre de barre soils. Rainfall in both years was unevenly distribution which caused drought in both years. Across both years, rice yield was significantly depressed with 60N and 120N by 53-81%, compared with other N levels. NERICA4 with 30N gave the highest panicles density and harvest index, and the best yield (1.2 Mg/ha). Wide spacing of 20 6 20 cm or 30 6 30 cm with four plants/stand was optimum for the NERICA. Drilling rice at 25 6 5 cm with one plant/stand depressed yield. Results showed that in smallholder upland ecosystems prone to unpredictable drought, wide spacing and low N can be recommended for production of NERICA"
  
  ## Process 
 
  uri <- "doi:10.7910/DVN/JWUEGN"
  dataset_id <- agro::get_simple_URI(uri)
  group <- "fertilizer"
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,
    carob_contributor="Siyabusa",
    experiment_type="fertilizer",
    has_weather=FALSE,
    has_management=FALSE
  )
  
  ## treatment level data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ## read the json for version, license, terms of use  
  js <- carobiner::get_metadata(dataset_id, path, major=1, minor=0, group)
  dset$license <- "CC0 1.0 Universal"
  
  ## the AFSIS data 
  f <- ff[basename(ff) == "Responses of upland NERICA rice varieties to nitrogen and plant density.xlsx"]
  d <- suppressMessages(as.data.frame(readxl::read_excel(f)))
  
  ##Skip early rows-Descriptive rows)
  d <- suppressMessages(as.data.frame(readxl::read_excel(f)[-c(1:21),]))
  
  ##Convert First Row to Header
  names(d)<-d[1,]
  e<-d[-1,]

  e <- e[,c("Rep","N","D","V","Straw dry weight kg/ha","Adjusted 14% grains dry weight kg/ha")]
  colnames(e)<-c('rep','N_fertilizer','spacing','variety','biomass_leaves','yield')

  e[e=="NA"] <- NA

  e$dataset_id <- "doi_10.7910_DVN_JWUEGN"
  e$trial_id <- "NERICA"
  e$country<- "Benin"
  e$on_farm<-TRUE
  e$is_survey<-FALSE
  e$crop<-"rice"
  e$start_date <- "2006-05-01"
  e$end_date <-"2006-10-30"
  
  #Replace values in a data frame
 e["N_fertilizer"][e["N_fertilizer"]=="0"]<-"0"
 e["N_fertilizer"][e["N_fertilizer"]=="1"]<-"30"
 e["N_fertilizer"][e["N_fertilizer"]=="2"]<-"60"
 e["N_fertilizer"][e["N_fertilizer"]=="3"]<-"120" 
 
 e$plant_spacing[e$spacing == "1"]<-"30"
 e$plant_spacing[e$spacing == "1"]<-"30"
 e$row_spacing[e$spacing == "1"]<-"30"
 e$plant_spacing[e$spacing == "2"]<-"20"
 e$row_spacing[e$spacing == "2"]<-"20"
 e$plant_spacing[e$spacing == "3"]<-"25"
 e$row_spacing[e$spacing == "3"]<-"25"

 
 e["variety"][e["variety"]=="1"]<-"WAB 450-I-B-P-38-HB (NERICA 1)"
 e["variety"][e["variety"]=="2"]<-"WAB 450-I-B-P-20-HB (NERICA 2)"
 e["variety"][e["variety"]=="3"]<-"WAB 450-I-B-P-91-HB (NERICA 4)"
 e["variety"][e["variety"]=="4"]<-"v4 - WAB 56-50"

 ##re-order
 e<-e[, c("rep","dataset_id","trial_id","country","on_farm","is_survey","start_date","end_date","crop","N_fertilizer","variety","yield","biomass_leaves","plant_spacing","row_spacing")]       
 
 carobiner::write_files(dset, e, path, dataset_id, group)
}


