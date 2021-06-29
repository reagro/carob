# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:
    These data were produced with co-funding provided by the AgMIP project, and funds received via CGIAR CRP WLE (2013)

"

	uri <- "doi:10.7910/DVN/LJPW4O"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="",
	   contributor="fava",
	   experiment_type="fertilizer",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group) # WHICH VERSION OF THE DATSET IS DOWNLOADED???
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=5) 
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "9a Yield data.xlsx"]

	d <- as.data.frame(readxl::read_excel(f))
	d$country <- "Uganda"
	d$region <- ""
	d$adm1 <- "Wakiso"
	d$adm2 <- "Jinja"
	d$adm3 <- ifelse(d$Site == "Kawanda", "Nabweru", "Busukuma")
	d$trial_id <- paste0(dataset_id, '-', d$Site)
	d$latitude <- ifelse(d$Site == "Kawanda", 0.4172778, 0.5256090)
	d$longitude <- ifelse(d$Site == "Kawanda", 32.5355326, 32.6136960)
	d$start_date <- as.numeric("2013") # Correct date format (to date)
	d$end_date <- as.numeric("2014")  # Correct date format (to date)
	d$on_farm <- "no"
	d$is_survey <- "no"
	d$crop <- "maize"
	
	# Merge with measured biomass ("2a Dry matter measurements.xlsx")
	biomass <- as.data.frame(readxl::read_excel(ff[basename(ff) == "2a Dry matter measurements.xlsx"]))

### RH: season "1" and "2" is not informative 
###  
	biomass$Season <- ifelse(biomass$`Days after planting (dap)` == 30, 1, 2)
  # biomass <- biomass[order(biomass[,"Site"], biomass[,"Season"], biomass[,"Block"], biomass[,"Treatment"], biomass[,"Plot"]), ]
  biomass <- biomass[order(biomass[,"Site"], biomass[,"Season"], biomass[,"Block"], biomass[,"Treatment"]), ]
  # biomass1 <- aggregate(biomass$`Dry weight with roots (g)`,by=list(biomass$Site,biomass$Season,biomass$Block,biomass$Treatment,biomass$Plot),data=biomass,FUN=mean)

## RH instead of
#  biomass1 <- aggregate(biomass$`Dry weight with roots (g)`,by=list(biomass$Site,biomass$Season,biomass$Block,biomass$Treatment),data=biomass,FUN=mean)
#	colnames(biomass1) <- colnames(biomass)[c(1,8,3,4,7)]
## do this: 
	biomass1 <- aggregate(biomass[, "Dry weight with roots (g)", drop=FALSE], 
						  biomass[, c("Site", "Season", "Block", "Treatment")], FUN=mean)
	d1 <- merge(d, biomass1, by = intersect(names(d), names(biomass1)), all.x = TRUE)
	d1$grain_weight <- d$`Grain yield (kg/plot -5.625m2)`*1000
	d1$Plot <- biomass$Plot
	
	###########
	# Missing information on amount of manure applied.
	###########
	# # Merge with Manure applied ("1a Cattle manure lab analysis.xlsx")
	# OM <- as.data.frame(readxl::read_excel(ff[basename(ff) == "1a Cattle manure lab analysis.xlsx"], skip = 5))
	# d1$OM_used <- "yes"
	# d1$OM_type <- "manure"
	# d1$OM_N <- OM$N/1000
	# d1$OM_P <- OM$P/1000
	# d1$OM_K <- OM$K/1000

	# Merge with Soil data ("8a Soil lab data.xlsx")
	soil <- as.data.frame(readxl::read_excel(ff[basename(ff) == "8a Soil lab data.xlsx"], skip = 12))
	soil$Block <- sub("^\\D*(\\d+).*$", "\\1",  soil$`Client's ref`)
	soil$Plot <- sub('.*(?=.{2}$)', '', soil$`Client's ref`, perl=T)
	soil1 <- soil[,c(1,4:17)]
	# Consider only the first 30 cm
	soil1 <- soil1[soil1$`Depth (CM)` == "0-15" | soil1$`Depth (CM)` == "15-30", ]
	soil1$`P (ppm)` <- as.numeric(soil1$`P (ppm)`)
	soil1$Block <- as.numeric(soil1$Block)
	soil1$Plot <- as.numeric(soil1$Plot)
	soil1$`N (%)` <- soil1$`N (%)`*10
	soil1$`P (ppm)` <- soil1$`P (ppm)`/1000
	soil1$`K (ppm)` <- soil1$`K (ppm)`/1000
	soil2 <- aggregate(soil1[, c(3,5,6,9,10,11)], list(Site = soil1$Site, Block = soil1$Block, Plot = soil1$Plot), mean, na.rm = TRUE)
	soil2 <- soil2[order(soil2[,"Site"], soil2[,"Block"], soil2[,"Plot"]), ]
	d2 <- merge(d1, soil2, by = intersect(names(d1), names(soil2)), all.x = TRUE)

## RH: yield is the grain yield (for cereals)
## 	d2$yield <- (d2$`Grain yield (kg/plot -5.625m2)` + d2$`Stover yield (kg/plot - 5.625m2)`)* (100/5.625)
	
## RH
	d2$yield <- d2$`Grain yield (kg/plot -5.625m2)` * (100/5.625)
	d2$residue_yield <- d2$`Stover yield (kg/plot - 5.625m2)` * (100/5.625)

	
	# process file(s)
	d <- carobiner::change_names(d2,
	                             c("Site","Dry weight with roots (g)", "pH", "N (%)", "K (ppm)", "P (ppm)", "Sand (%)", "Clay (%)"),
	                             c("site","biomass_total", "soil_pH", "soil_N", "soil_K", "soil_P_total", "soil_sand", "soil_clay"))
	d <- d[,c(1,8,9,11:29)]
	d$trial_id <- paste0(d$trial_id, "-", ifelse(d$site == "Kawanda", seq(1,length(which(d$site == "Kawanda"))), seq(1,length(which(d$site == "Namulonge")))))
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
