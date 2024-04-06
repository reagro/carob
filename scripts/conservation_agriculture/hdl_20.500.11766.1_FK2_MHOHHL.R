# R script for "carob"
#issues:
#Sugesseted variables to be added to terms (under :variables_crop.csv)"
#["accession_number", type:character, unit:No] 
#["Pedigree_name",type:character, unit:No] 
#["days_to_flowering", type: integer, unit:days]
#["days_to_maturity", type: integer, unit:days]
#adding the suffix "_intercrop" after variables names to represent the intcroping expemints paramters] 

path <-setwd (".") 
carob_script <- function(path) {

"
  The dataset contains the description and results of a field experiment performed under 
  the project “Designing InnoVative plant teams for Ecosystem Resilience and agricultural 
  Sustainability (DIVERSify)” in the Lebanese Agricultural Research Institute station 
  in Kfardan Lebanon in 2018. 
  The trial includes 40 faba bean varieties and 2  wheat varieties. 
  2 replications and 5 treatments: 
  1.	Faba bean monoculture
  2.	Wheat sole (first variety)
  3.	wheat sole (second variety)
  4.	mixture of Faba bean/Durum Wheat (first variety)
  5.	mixture of Faba bean/Durum Wheat (second variety). 

  The personnel working on this experiment consists of the principal investigator 
  Dr Fouad Maalouf (ICARDA-Terbol) and the trial responsible Lynn Abou Khater 
  (ICARDA-Terbol). 
  Data date range: 20180201-20180630
  Crops: Faba bean - Durum Wheat
"
  
  uri <- "hdl:20.500.11766.1/FK2/MHOHHL"
  dataset_id <- carobiner::simple_uri(uri)
  
  group <- "conservation_agriculture"
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=0)
  
  dset <- data.frame(
    #carobiner::extract_metadata(js, uri, group=group),  ## Issue: returns error: 'extract_metadata' is not an exported object from 'namespace:carobiner'
    project="DIVERSify",
    publication="hdl:20.500.11766.1/FK2/MHOHHL",
    data_citation = "Maalouf, Fouad; Abou-Khater, Lynn, 2020, DIVERSify field experiment results in Kafardan 2018, https://hdl.handle.net/20.500.11766.1/FK2/MHOHHL, MELDATA, V3",
    data_institutions = "ICARDA",
    carob_contributor="Samar Attaher",
    carob_date="2023-04-05",
    data_type="experiment"
  )
  
  f1 <- ff[basename(ff) == "01_Plot_Information.csv"] 	
  r1 <- read.csv(f1, sep=";")
  
  f2 <- ff[basename(ff) == "02_Plot_Level_Data.csv"] 	
  r2 <- read.csv(f2, sep=";")
  
  d1 <- data.frame(
    rep=r1$Rep,
    country = "Lebanon",
    adm1 = "Baalbek-Hermel Governorate",
    adm2 = "Baalbek",
    adm3 = "Kfar Dan",
    site = "Lebanese Agricultural Research Institute station",
    latitude = 34.00713889,
    longitude = 36.04647222,
    elevation= 1080,
    record_id=r1$PlotCode,
    treatment= paste(r1$CropCombination, r1$Diversity, sep = "_"),
    crop = r1$CropSpeciesCommonName,
    variety=r1$CropVariety,
    planting_date= "2018-02-01",  
    harvest_date= "2018-06-30",
    rain = 194.4,
    irrigation_number= 2,
    irrigation_amount= 60,
    herbicide_product= "Pendimethalin",
    insecticide_product="Imidacloprid and Lambdacyhalothrin"
    
    
  ) 
  
  d1$crop <-tolower(d1$crop)
  d1$intercrops[r1$NumberOfPlantSpecies==1] <- NA
  d1$intercrops[r1$NumberOfPlantSpecies==2] <- "durum wheat"
  
  
  
  #Pick the needed variables from r2
  d2 <- data.frame(
    record_id=r2$PlotCode,
    accession_number_main_crop= r2$Faba_Bean_PN, #the variable name does not exist in the Carob terms
    Pedigree_name_main_crop=r2$Faba_Bean_Pedigree, #the variable name does not exist in the Carob terms
    days_to_flowering_main_crop= r2$DFLRFB, #the variable name does not exist in the Carob terms
    days_to_flowering_intercrop= r2$DFLRWT, #the variable name does not exist in the Carob terms
    days_to_maturity_main_crop= r2$DMATFB , #the variable name does not exist in the Carob terms
    days_to_maturity_intercrop= r2$DMATWT, #the variable name does not exist in the Carob terms
    plant_height= r2$FBPLHT,
    plant_height_intercrop= r2$WTPLHT, #the variable name does not exist in the Carob terms
    yield_part="grain",
    yield= r2$FBGY,   #kg/ha
    yield_intercrop= r2$WTGY, # kg/ha -the variable name does not exist in the Carob terms
    number_of_pests_on_plants= r2$Pest #the variable name does not exist in the Carob terms
  ) 
  
  
#merge d1 and d2  to create the final data set
d <- merge(d1, d2, by="record_id", all=TRUE)
  
carobiner::write_files (path, dset, d) 
  #carobiner::write_files (dset, d, path=path)
}
