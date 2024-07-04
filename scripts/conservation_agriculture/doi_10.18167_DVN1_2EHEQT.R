# R script for "carob"


carob_script <- function(path) {
  
  "Data on rainfed rice production and management were collected for cropping seasons 2006–2007, 2007–2008, 2008–2009 and 2009–2010 around five villages of the BV-Lac programme on lowland and hillside farmer's fields under conservation agriculture in the Lake Alaotra region of Madagascar,  resulting in 3803 site x management x soil x season combinations. (2020-10-07)"
  
  uri <-  "doi:10.18167/DVN1/2EHEQT"
  group <- "conservation_agriculture" 
  ff <- carobiner::get_data(uri, path, group)
  
  dset <- data.frame(
    carobiner::read_metadata(uri, path, group, major=1, minor=2),
    publication= "doi:10.1017/S0014479714000155",
    treatment_vars= "OM_used; OM_amount",
    data_institute = "CIRAD",
    carob_contributor="Cedric Ngakou",
    carob_date="2023-10-18",
    data_type="experiment",
    project="‘Doctorant du Sud’ programme and the ABACO project (DCI-FOOD 2010/230-178)",
    revised_by = "Siyabusa Mkuhlani", 
    revision_date = "2024-07-05"
  )
  
  r <- readxl::read_excel(ff[basename(ff)=="2006-2010_database_bvlac_bruelle_v01.20201009.xlsx"],sheet=1) |> as.data.frame()
  
  d <- data.frame(
    location = r$village,
    season = r$crop_season,
    soil_type = r$soil,
    tillage = r$tillage_system,
    planting_date = as.character(r$sow_date),
    OM_amount = r$manure,
    N_fertilizer = r$nitrogen,
    
    #Assessment shows N, as 11%. Most common rice fert with 11% N in madagascar is
    #11:22:16 (NPK). https://agritrop.cirad.fr/539499/1/document_539499.pdf
    P_fertilizer = 9.59/100 * (r$npk), 
    K_fertilizer = 13.28/100 * (r$npk),  
    yield = r$yield,
    rain = r$rain_year
  )
  
  d$trial_id <- as.character(as.integer(as.factor(paste0(d$location, d$season)))) 
  d$country <- "Madagascar"
  d$crop <- "rice"
  d$yield_part <- "grain" 
  d$on_farm <- TRUE
  d$irrigated <- TRUE
  d$inoculated <- FALSE
  d$is_survey <- FALSE
  d$fertilizer_type <- "urea"
  d$OM_type <- "horse manure"
  d$OM_used <- TRUE
  
  d$herbicide_used <-  r$glypho == "Y"
  d$herbicide_product <- "none"
  d$herbicide_product[d$herbicide_used] <- "glyphosate"
  
  names(d)[names(d) == "tillage"] <- "treatment"
  d$treatment <- ifelse(d$treatment=="till", "tillage",
                        ifelse(d$treatment=="ca", "conservation agriculture", d$treatment))
  
  geo <- data.frame(location=c("Antsahamamy", "Ambohimiarina", "Ambohitsilaozana", "Ambongabe", "Ampitatsimo"),
                    latitude=c(-18.9185, -21.3561, -17.7014, -17.7067, -18.6729),
                    longitude=c(47.5592 , 47.5680 , 48.4657 , 48.1885 , 47.4564))
  
  d <- merge(d, geo, by="location", all.x=TRUE)
  
  d$season[grepl("Y06_07",d$season)] <- "2006-2007"
  d$season[grepl("Y08_09",d$season)] <- "2008-2009"
  d$season[grepl("Y07_08",d$season)] <- "2007-2008"
  d$season[grepl("Y09_10",d$season)] <- "2009-2010"
  
  
  # d <- unique(d) #why needed?
  #Not very sure, what you mean.
  
  carobiner::write_files(dset, d, path=path)
  
}

