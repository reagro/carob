# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:

    Many soils on smallholder farms in Malawi have poor soil organic matter content. This results in poor maize productivity when sufficient mineral fertilizers are not added. Building soil organic matter requires improving both cereal and legume crops primary productivity through mineral fertilizers, and retaining the associated crop residues on the cropped lands. These residues decompose to provide mineral N to crops grown in sequence, as well as being an important source for SOM capitalization. Residues of legumes crops have a narrow C/N ration and are hypothesized to improve N cycling and benefit the rotational crop, whereas residues of maize, which have a wide C/N ratio, promote immobilization. While this knowledge is widely known, what is not clear is the interaction between crop residue quality, quantity and soil water management on maize productivity and mineral N dynamics. The data will address the following: 1. Does incorporating soil water enhancing technologies increase/reduce the immobilization potential of maize residues? 2. What is the effect of varying the quantity of the crop residues incorporated (both maize and legumes) on mineral N dynamics, soil water content and maize productivity; 3. For farmers with limited fertilizer use (50% NP), how detrimental is use of maize residues (X0, X1, X2), with or without water conservation measures; 4. What is the fertilizer substitution value of different quantity residues generated from a groundnut/pigeonpea doubled up system?

	This data is for the residue generation phase

"
  
  uri <- "doi:10.7910/DVN/1A6WMD"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "conservation_agriculture"
  # dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="International Institute of Tropical Agriculture (IITA), 2023, Assessing the Effect of Residue Quantity and Quality, and Water Conservation on Maize Productivity and Nitrogen Dynamics on Smallholder Farms in Malawi, https://doi.org/10.7910/DVN/1A6WMD, Harvard Dataverse, V1, UNF:6:3Chcr949v7E4fbqU8ULAsQ== [fileUNF]",
    publication= "doi.org/10.1016/j.fcr.2021.108225",
    data_institutions = "IITA,IFPRI",
    data_type="on-farm experiment", 
    carob_contributor="Mitchelle Njukuya",
    carob_date="2024-01-09"
  )
  
  ## download and read data 
  
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
  dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
  
  
  f <- ff[basename(ff) == "NewDesign_gnut yields.csv"]
  
  r <- read.csv(f)
  ## process file(s)
  
  d <- r
  message("error: 'District' is adm2; EPA could be location with village being 'site'")

  d <- carobiner::change_names(d,c("District","EPA","Village","Treatment","Year.of.establishment","Crop.harvested","Groundnut.variety","Rep","stover.dry.weight..kg.ha.","Grain.weight..kg.ha.","Total.biomass..kg.ha.")
                  ,c("location","adm1","adm2","treatment","year","crop","variety","rep","dym_residue","yield","dym_total"))
				  
					
				  
  d$treatment <- carobiner::replace_values(d$treatment,"Groundnut/pigeonpea","groundnut/pigeonpea")
  d$crop <- carobiner::replace_values(d$crop,"Groundnut","groundnut")
  d$dataset_id <- dataset_id
  d$on_farm <- TRUE
  d$is_experiment <- TRUE
  d$trial_id <- d$Treatment.number
  d$country <- "Malawi"
  d1 <- unique(d[,c("country","location")])
  d2 <- data.frame(
		country = "Malawi", 
		location = c("Dedza","Machinga"),
		longitude = c(34.3197, 35.6026),
		latitude = c(-14.2374, -14.9027))
		
  d <- merge(d,d2,by=c("country","location"),all.x=TRUE)
  
  #planting dates sourced from publication
  d$planting_date <- as.character(2017)
  d$N_fertilizer <- 34.5
  d$yield_part <- "seed"
  d$plant_density <- "164 000"
  
   # do not do this:
  d <- d[,c("year","dataset_id","on_farm","is_experiment","country","location","adm1","adm2","longitude","latitude","planting_date","treatment","rep","crop","variety","N_fertilizer","plant_density","dym_residue","dym_total","yield","yield_part")]
   
    # all scripts must end like this
    carobiner::write_files(dset, d, path=path)
}

