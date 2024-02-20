# R script for "carob"

## ISSUES
# Can't seem to understand why the: "Error in simple_uri(pubs)"


carob_script <- function(path) {
  
  
  "Description:
  Performance trials (N=52) in two zones (West Shewa and Jimma) in Ethiopia. Trials comprise four nutrient management treatments, namely control withzero fertilizer ; and three fertilizer recommendations to achieve the same  target yield based on regional fertilizer recommendation, a Nutrient Expert      (IPNI software) based recommendation and a soil-test NE based recommendation. Trials were conducted on-farm with four plots per farm. Observations  include biomass and grain yields, as well as pre-sowing pH, nitrogen and phosphorus levels. Some N & K data are missing."

   uri <- "hdl:11529/11012"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="T Balemi; M Kebede; T Abera; G Hailu; J Rurinda; G Gurumu, 2017, TAMASA Ethiopia. Performance trial dataset for validating maize nutrient management recommendations, 2016 season., https://hdl.handle.net/11529/11012, CIMMYT Research Data & Software Repository Network, V2",
    publication= "NA",
    data_institutions = "CIMMYT",
    data_type="experiment", 
    carob_contributor="Njogu Mary",
    carob_date="2023-02-20",
    revised_by="NA"
  )
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  f <- ff[basename(ff) == "TAMASA_ET_PT_2016F.xlsx"]
  r <- readxl::read_excel(f,sheet = 5) 
  
  ## use a subset
  
  d <- data.frame(country= "Ethiopia", region= r$Region, zone= r$Zone, districts= r$Districts,
                  location= r$Location, elevation= r$Altitude, treatments= r$Treatments, crop= "maize", yield_part= "grain"  )
  
  d$ yield<- r$ "Actual Yield (t/ha)"
  d$ pl_st <- r$ "Crop Stand count"
  d$ moist <- r$ "Grain MC(%)"
  d$dataset_id<- dataset_id
  d$ grain_weight<- r$`Kernel yield       (kg/ha)`
  d$region <- "Oromia"
  d$ biomass_weight<- r$`Total Biomass Weight (kg)`
  d$soil_pH <- r$pH
  d$soil_N <- r$`TN               (%)`
  d$soil_P_available <- r$`Av. P (ppm)`
  

  # get lat and lon coordinates using carobiner geocode function
  h <- carobiner::geocode (country=d$country, adm1=d$region, location =d$location)$df
  d$lon <- h$lon
  d$lat <- h$lat
  
  # Check records with missing lon values
  o <- which(is.na(d$lon)== TRUE)
  # Exclude records identified with missing lon
  d <- d[-o,]
  
  carobiner::write_files(dset, d, path=path)
}





 
  