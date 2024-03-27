# R script for "carob"

# ## ISSUES 
# ....


carob_script <- function(path) {
  
  "Description:
  This dataset is a result of a study that was carried out in nine on-farm sites of Central and Southern Malawi to understand and compare the effects of different cropping systems (conservation agriculture (CA) and conventional) on soil physical and chemical parameters and long-term maize productivity. Six experiments were established in each target community. Each experiment had three treatments at one farm and was treated as a replicate, plot sizes were 0.1 ha per treatment. The treatments were as follows:
1. Conventional control plot consisting of the traditional ridge and furrow land preparation planted with continuous monocrop maize (CPM). The residues were managed using methods commonly practiced in each extension planning area; i.e., the residues were incorporated into the ridges. Continuous monocrop maize was planted on the ridges.
2. CA plot with continuous monocrop maize (CAM) planted into the previous years’ ridges (where they still existed) or directly into the plot without previous ridge formation. Crop residues from the previous years’ harvests were retained as a surface mulch. Maize seeds were planted as sole crops in no-till methods using a pointed stick (dibble stick).
3. CA plot with maize intercropped with a legume [cowpea or pigeon pea or groundnut. Both crops were planted with the dibble stick into the previous years’ ridges (where they still existed) or directly into the plot without further ridging. Crop residues were retained as surface mulch as in treatment 2.
      "
  
  uri <- "doi:10.7910/DVN/QLJUY7"
  group <- "conservation_agriculture"
  ff <- carobiner::get_data(uri, path, group)
  ## dataset level data 
  dset <- data.frame(
  	carobiner::read_metadata(uri, path, group, major=1, minor=2),
    project=NA,
    publication= "doi:10.1017/S0014479715000265",
    data_institutions = "CIMMYT",
    data_type="experiment",
    carob_contributor="Fredy Chimire",
    carob_date="2024-1-11"
  )
  
  
  
  
  
  f <- ff[basename(ff) == "AR_MAL_CIMMYT_CAmother_onfarm_2020.csv"]
  
  # Select sheet with revised data from the excel file 
  r <- read.csv(f)
  
  d <- data.frame(harvest_date=r$Harvest.Year,variety=r$Variety,plant_density=r$Plantpopulation,adm2=r$District,location=r$Village,treatment=r$Treat,dmy_total = r$Biomassyield, yield = r$Grain.yield)
  
  # for first dataset
  
  d$country<- "Malawi"
  d$crop <- "Maize"
  d$is_survey <- FALSE
  d$yield_part <- "grain"
  d$fertilizer_type <- "NPS"
  # source: dictionary_AR_MAL_CIMMYT_CAmother_onfarm_2020.csv
  d$N_fertilizer <- 23 
  d$P_fertilizer <- 21
  d$K_fertilizer <- 0
  d$S_fertilizer <- 4
  d$rep <- r$Plot.No.
  
  # Gps was found at district level since the villages are not available at map
  # https://www.google.com/maps/search/balaka+malawi++lemu+gps+coordinates/@-14.9319518,34.9463051,17z?entry=ttu

	d$location <- gsub("machinga", "Machinga", d$location)
	d$crop <- tolower(d$crop)
  	d$harvest_date <- as.character(d$harvest_date)

	geo <- list("Balaka" = c(-14.9318, 34.9511),
              "Dowa"   = c(-13.6279, 33.9329),
              "Machinga" = c(-15.1775, 35.2963),
              "Nkhotakota" = c(-12.7037, 34.2570),
              "Salima" = c(-13.7114, 34.4461),
              "Zomba" = c(-15.3737, 35.3194)) 

    geo <- t(as.data.frame(geo))
	colnames(geo) <- c("latitude", "longitude") 
	d <- merge(d, geo, by.x="location", by.y=0, all.x=TRUE)

                           
	carobiner::write_files(dset, d, path=path)
}




