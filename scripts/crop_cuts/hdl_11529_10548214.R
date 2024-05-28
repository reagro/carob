# R script for "carob"

## ISSUES
# Issues with the Fertilizer type


carob_script <- function(path) {

"
Crop cut survey in 2015 conducted by EIAR and CIMMYT. Replicated crop cuts of 16m2 
in farmers fields along with additional data on nutrient use and variety, and soil 
sample. There are two linked files for early and mid-season data. (2015)
"

#### Identifiers
	uri <- "hdl:11529/10548214"
	group <- "crop_cuts"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=2, minor=1),
		data_institute = "TAMASA",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication = NA,
		project = NA,
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = "experiment",
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "variety ;	fertlizer_type", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-05-23"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "ET_BAko agronomy data.xlsx"]
	r <- carobiner::read.excel(f,sheet ="Raw_Data")
	f1 <- ff[basename(ff) == "ET_BAko agronomy data_2015.xls"]
	r0<-carobiner::read.excel(f1,sheet ="Raw_Data")

## process file(s)

## select the variables of interest and assign them to the correct name
	d1 <- data.frame(
		latitude=r$Latitude,
		longitude=r$Longitude,
		yield = r$`Average yield kg/ha or (Q1+Q2)/2`,
		variety=r$`Type of Variety`,
		land_prep_method=r$`Land Preparation Method`,
		elevation=r$Altitude,
		crop_rotation=r$`Crop Rotation with`,
		intercrops=r$`Intercropping with legume`,
		previous_crop=r$`Previous/precursor crop`,
		soil_pH=r$pH,
		soil_Ca=r$`Ca (mg kg-1)`,
		soil_K=r$`K (mg kg-1)`,
		soil_Na=r$`Na (mg kg-1)`,
		soil_Mg=r$`Mg (mg kg-1)`,
		soil_B=r$`Boron (mg kg-1)`,
		soil_Mn=r$`Mn (mg kg-1)`,
		soil_Fe=r$`Fe (mg kg-1)`,
		soil_Zn=r$`Zn (mg kg-1)`, 
		soil_Al=r$`Al (mg kg-1)`,
		soil_C=r$`Carbon (%)`,
		fertlizer_type=r$`Type of Inorganic Fertilizer`,
		adm3 =r$`Name of the Village`
		
		
		
		
		# etc
	)

	d0$planting_date <- as.character(as.Date( r$`Planting Date` ))
	d0$harvest_date  <- "2015"
	
	
#Fixing names

		#d1$fertilizer_type <- gsub("Urea,DAP|DAP-UREA|UREA & DAP|Urea, DAP|DAP,Urea|Urea ,DAP\r\n\r\n\r\n\r\n|DAP .,urea|DAP,UREA|DAP\r\n,UREA|UREA, DAP|DAP, Urea", "urea,DAP", d1$fertilizer_type)
	
	d1$crop_rotation[d1$crop_rotation=="Pepper"]<-"pepper"
	d1$crop_rotation[d1$crop_rotation=="Pepper, Teff, Other"]<-"pepper;teff;none"
	d1$crop_rotation[d1$crop_rotation=="Teff"]<-"teff"
	d1$crop_rotation[d1$crop_rotation=="Beans, Teff"]<-"kidney bean;teff"
	d1$crop_rotation[d1$crop_rotation=="Pepper, Teff"]<-"pepper;teff"
	d1$crop_rotation[d1$crop_rotation=="Vegetables"]<-"none"
	  
	
	d1$previous_crop[d1$previous_crop=="Pepper"]<-"pepper"
	d1$previous_crop[d1$previous_crop=="Other"]<-"none"
	d1$previous_crop[d1$previous_crop=="Teff"]<-"teff"
	d1$previous_crop[d1$previous_crop=="Vegetables"]<-"none"
	d1$previous_crop[d1$previous_crop=="Beans"]<-"kidney bean"
	
	d1$land_prep_method[d1$land_prep_method=="Ploughing"]<-"ploughing"
	
	
	
	
	
	d0 <- data.frame(
	  latitude=r0$Latitude,
	  longitude=r0$Longitude,
	  yield = r0$Average.yield.kg.ha.or..Q1.Q2..2,
	  variety=r0$Type.of.Variety,
	  land_prep_method=r0$Land.Preparation.Method,
	  elevation=r0$Altitude,
	  crop_rotation=r0$Crop.Rotation.with,
	  intercrops=r0$Intercropping.with.legume,
	  previous_crop=r0$Previous.precursor.crop,
	  soil_pH=r0$pH,
	  soil_Ca=r0$Ca..mg.kg.1.,
	  soil_K=r0$K..mg.kg.1.,
	  soil_Na=r0$Na..mg.kg.1.,
	  soil_Mg=r0$Mg..mg.kg.1.,
	  soil_B=r0$Boron..mg.kg.1.,
	  soil_Mn=r0$Mn..mg.kg.1.,
	  soil_Fe=r0$Fe..mg.kg.1.,
	  soil_Zn=r0$Zn..mg.kg.1., 
	  soil_Al=r0$Al..mg.kg.1.,
	  soil_C=r0$Carbon....,
	  fertlizer_type=r0$Type.of.Inorganic.Fertilizer,
	  adm3 = r0$Name.of.the.Village)
	
	#fixing terms
	
	d0$crop_rotation[d0$crop_rotation=="Pepper"]<-"pepper"
	d0$crop_rotation[d0$crop_rotation=="Pepper, Teff, Other"]<-"pepper;teff;none"
	d0$crop_rotation[d0$crop_rotation=="Teff"]<-"teff"
	d0$crop_rotation[d0$crop_rotation=="Beans, Teff"]<-"kidney bean;teff"
	d0$crop_rotation[d0$crop_rotation=="Pepper, Teff"]<-"pepper;teff"
	d0$crop_rotation[d0$crop_rotation=="Vegetables"]<-"none"
	
	d0$previous_crop[d0$previous_crop=="Pepper"]<-"pepper"
	d0$previous_crop[d0$previous_crop=="Other"]<-"none"
	d0$previous_crop[d0$previous_crop=="Teff"]<-"teff"
	d0$previous_crop[d0$previous_crop=="Vegetables"]<-"none"
	d0$previous_crop[d0$previous_crop=="Beans"]<-"kidney bean"
	
	
	d0$land_prep_method[d0$land_prep_method=="Ploughing"]<-"ploughing"
	
	
	d0$planting_date <- as.character(as.Date( r0$Planting.Date  ))
	d0$harvest_date  <- "2015"
	  
	d <- carobiner::bindr(d0, d1)
## (TRUE/FALSE)
	d$on_farm <- TRUE


##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Ethiopia" 
	d$site <- "Oromia"
	d$adm1 <- "West Showa"
	


##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"
	


	#what plant part does yield refer to?
	d$yield_part <- "grain"
	d$trial_id <- "1"
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

