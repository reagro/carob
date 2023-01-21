# R script for "carob"

carob_script <- function(path) {
  
  "Description
Title: Response of Groundnut to plant density and phosphorous application in the sudan savanna zone of Minjibir, Nigeria. 
Abstract: Despite the recent release of several improved varieties of groundnut in Nigeria the productivities have not increase significantly due to lack of commensurate recommendation in agronomic practices. Two groundnut varieties were evaluated for their response to different plant density and phosphorus application in two locations in the Sudan Savanna zone of Nigeria in 2012 and 2013. The groundnut were planted at density of 44444, 66667, and 133333 hills ha-1 with average of two plants per hill. Phosphorus was applied at rate of 0 or 20 kg P ha-1 . P fertilizer application increased pod and haulm yields by 26% and 16% respectively in Minjibir. It increased pod and haulm yields by 62% and 27% respectively in Wudil. Pod and haulm yields, harvest index, revenue, profit and cost benefit ratio increased with increasing plant density. Samnut-24 produced pod yields that were significantly higher than Samnut-22 across treatments. Pod yields at density of 133,333 hills ha-1 was 31% higher than at 66667 and 40% than at 44,444 hills ha-1. Application of fertilizer increased profit by 22% and 49% in Minjibir and Wudil respectively. Planting at density of 133,333 hill ha-1 increased profit by 19% and 27% over 66,667 and 444444 hill ha-1 respectively in Minjibir, while it increase profit by 9% in Wudil. Cultivation of Samnut-24 at high density with 
  phosphorus application will make groundnut production a more profitable venture in Sudan Savanna zone of Minjibir, Nigeria."
  
  ## Process 
 
  uri <- "doi:10.21421/D2/5O93XX"
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
    has_management=TRUE
  )
  
  ## treatment level data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ## read the json for version, license, terms of use  
  js <- carobiner::get_metadata(dataset_id, path, major=1, minor=0, group)
  dset$license <- carobiner::get_license(js) 
  
  ## the AFSIS data 
  f <- ff[basename(ff) == "Data file of Groundnut to plant density and phosphorous application in Wudil 2012-13.xlsx"]
  d <- carobiner::read.excel(f)
  
  names(d)
  e<-d[,c(1,2,4,5,6,7,14,15)]
  names(e)
  colnames(e)<-c('season','location','rep','variety_type','treatment','spacing','yield','residue_yield')
  
  e$country<- "Nigeria"
  e$crop<-"groundnut"
  e$dataset_id <- dataset_id
  e$trial_id <- paste0("seed_nutr_gnut_", e$location)
  e$adm1<-'Kano'
  e$on_farm<-FALSE
  e$is_survey<-FALSE
  e$P_fertilizer[e$treatment=='F1']<-'0'
  e$P_fertilizer[e$treatment=='F2']<-'20'
  names(e)
  e<-e[c("dataset_id","country", "adm1", 'location', "trial_id", "season","on_farm", "is_survey", "rep", "crop", "variety_type","yield", "residue_yield")]  
 #Still need to decode the treatment. Which is a level of fertilizer
 carobiner::write_files(dset, e, path, dataset_id, group)
}


