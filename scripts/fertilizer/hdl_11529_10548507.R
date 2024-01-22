

## ISSUES
# efyrouwa: can the NPK fertilizers be comprised of the basal and top dressing? 



carob_script <- function(path) { 
  
  "
  The objective of the Landscape Diagnostic Survey (LDS) for wheat is to bridge the existing data-gap around current 
  production practices of wheat, and also to help in evidence-based planning. The LDS is designed in a way that 
  data is collected from randomly selected farmers spread uniformly within a KVK (government extension system) 
  domain/district. Data has been collected from farmers largest wheat plot for winter season of 2018. Survey questionnaire
  captures all production practices applied by farmers from land preparation to harvesting, including detailed sections on
  fertilizer use, weed control and irrigation application. Data is captured through electronically enabled Open Data Kit
  (ODK) tool on mobile phone or tablet. (2019-12-31)
"
  
  uri <- "hdl:11529/10548507"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="Landscape Diagnostic Survey (LDS)",
    uri=uri,
    data_citation="Ajay, Anurag; Craufurd, Peter; Sharma, Sachin; Ranjan, Harshit; Poudel, Gokul; Malik, RK; Singh, Balwinder; Singh, AK; Samaddar, Arindam; Rai, Ashok; Keil, Alwin; McDonald, Andrew, 2020, Landscape diagnostic survey data of wheat production practices and yield of 2018 from eastern India, https://hdl.handle.net/11529/10548507, CIMMYT Research Data & Software Repository Network, V1, UNF:6:ACX3w1PnF4Otyf++Z6mO3g== [fileUNF]",
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="survey", 
    carob_contributor="Effie Ochineg'",
    carob_date="2024-01-22"
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- "CIMMYT"
  
  
  f <- ff[basename(ff) == "CSISA_IND_LDS_Whe_2018_Data.csv"]
  r <- read.csv(f)
 
  
  ## use a subset
  d <- r[, c(309,6,10,11,12,13,14,25,56,77,281,51,69,304,305,182,183,187,190,192,193,194,195,196,197,35,38,41,289)]
   
  colnames(d) <- c("trial_id","date","country","adm1","adm2","adm3","site","crop","previous_crop","planting_date","harvest_date","plot_area_acres","variety","latitude","longitude",
               "DAP_plot","NPK_plot","Urea_plot","NPKS_plot","MOP_plot","SSP_plot","TSP_plot","ZnSO4_plot","gypsum_plot","boron_plot", "total_biomass1", "total_biomass2", "total_biomass3","yield")
  
  d$country[d$country == "8"] <- "India"
  d$crop <- "wheat"
  d$yield_part <- "grain"
  d$is_survey <- TRUE
  d$dataset_id <- dataset_id
  
  # efyrouwa: previous crop has other and fallow
  d$previous_crop <- carobiner::fix_name(d$previous_crop, "lower")   
  d$previous_crop <- carobiner::replace_values(d$previous_crop, c("bajra","jowar","greenmanure","greengram","pulses","mungbean"), c("pearl millet","sorghum","green manure","mung bean","pulse","mung bean"))
  
  # a function to deal with the diversity of dates
  convert_to_ymd <- function(date_str) {
    formats <- c("%d/%m/%Y", "%d-%m-%y", "%d %b, %Y", "%d-%b-%y", "%d-%b-%Y")
    for (format in formats) {
      date <- as.Date(date_str, format = format, tryFormats = format)
      if (!is.na(date)) {
        return(format(date, "%Y-%m-%d"))
      }
    }
    return(date_str)
  }
  
  d$planting_date <- sapply(d$planting_date, convert_to_ymd)
  d$harvest_date <- sapply(d$harvest_date, convert_to_ymd)
  d$date <- sapply(d$date, convert_to_ymd) # efyrouwa:some dates came as numbers 
  
  
  #standardize fertilizers 
  d$plot_area_ha <- d$plot_area_acres / 0.4 #  to convert from acres to ha
  
  # to get the fertilizer/ha
 
  fert <- c("DAP","NPK","Urea","NPKS","MOP","SSP","TSP","ZnSO4","gypsum","boron")
    
  for (i in fert) {
    d[[paste0(i, "_ha")]] <- d[[paste0(i, "_plot")]] / d$plot_area_ha
  }
  
  
 #efyrouwa: NPK percentages gotten from the R script that was published with the data
 d$N_fertilizer <- d$Urea_ha*0.46 + d$DAP_ha*.18 + d$NPK_ha*.12   
 d$P_fertilizer <- d$DAP_ha*.46 + d$NPK_ha*.32 + d$TSP_ha*.48 + d$SSP_ha*.16
 d$K_fertilizer <- d$MOP_ha*.60 + d$NPK_ha*.16 
 d$S_fertilizer <- d$ZnSO4_ha*.15 + d$gypsum_ha*19 # percentages according to carob fertilizer values
 
 d$N_fertilizer <-  ifelse(is.na(d$N_fertilizer), 0,d$N_fertilizer)
 d$P_fertilizer <- ifelse(is.na(d$P_fertilizer), 0,d$P_fertilizer)
 d$K_fertilizer <- ifelse(is.na(d$K_fertilizer), 0,d$K_fertilizer)
 
 
 d$yield <- d$yield * 1000 # to convert to kg/ha
 d$dmy_total_av <- (d$total_biomass1 + d$total_biomass2 + d$total_biomass3)/3 # average biomass per 4*4 quadrants
 d$dmy_total <-  d$dmy_total_av /16 * 1000
  
 
 d <- d[, c("dataset_id","trial_id","date","country","adm1","adm2","adm3","site","crop","previous_crop","planting_date","harvest_date","variety","latitude","longitude",
            "yield","yield_part","is_survey","N_fertilizer","P_fertilizer","K_fertilizer","S_fertilizer","dmy_total")]
 
 
    # all scripts must end like this
    carobiner::write_files(dset, d, path=path)

 }


