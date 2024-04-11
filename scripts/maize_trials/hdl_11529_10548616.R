# R script for "carob"

## ISSUES
# 1. some of the plants were recorded 0


carob_script <- function(path) {

"
[Maize (Zea mays L.), the staple crop of Mexico, is often produced by 
smallholder farmers on sloping terrains. Historically, little agronomic 
research has been performed under the conditions of these farmers to support
them in the sustainable intensification of their production systems. We set
up trials at two locations in the state of Oaxaca to evaluate conservation 
agriculture and agroforestry in collaboration with local farmers. This dataset 
includes the data taken between 2015 and 2019 in the Santa Maria Teopoxco 
research platform and between 2014 and 2019 in the Tamazulapam del Espiritu
Santo research platform, both located in the Mexican state of Oaxaca in the
Pacífico Sur innovation hub. In both platforms the local conventional system
was compared with 4 improved treatments. Improved treatments included zero 
tillage, residue management, liming, crop rotation and fertilization in 
different combinations. In both platforms these treatments were evaluated
within a MIAF (Milpa Intercalada con Arboles Frutales) agroforestry 
arrangement. The MIAF system was implemented at both sites by planting rows of
trees along the contour lines with a distance between rows of 10.6 m and 
between trees of 1 m in Tamazulapam (900 trees ha-1) and 2 m in Teopoxco (450
trees ha-1). Maize was sown on 60% of the area and 40% was under fruit trees.
The database contains agronomic data (fertilization, density, tillage,
residue management, plant spacing), yield data (grain yield of maize, squash
and bean, fresh yield of peas, avocado and peach), plant development data
(sowing, emergence, flowering, maturity and harvest, plant height, lodging)
and economic data (production costs, gross margin and net profit).
(2021-09-27)]
"

#### Identifiers
	uri <- "hdl:11529/10548616"
	group <- "maize_trials"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institutions = "CIMMYT",
		publication=NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Blessing Dzuda",
		carob_date="2024-04-09"
	)
	
##### PROCESS data records
	
# read data 
 maize_mexico<-function(fname, sheetname, id)
 {	
 f <- ff[basename(ff) == fname]
 r <- carobiner::read.excel(f, sheet= sheetname)
 ## process file(s)
 
 ## use a subset
 #in the original dataset,some of the harvest dates were put as 0 and now replacing with NA
 r$Harvest_date[r$Harvest_date==0]<-NA
 ifelse(is.character(r$Sowing_date),r$Sowing_date<-as.Date(as.numeric(r2$Sowing_date),origin="1900-01-01"),r$Sowing_date<-r$Sowing_date)
 ifelse(is.character(r$Harvest_date),r$Harvest_date<-as.numeric(r$Harvest_date),r$Harvest_date<-r$Harvest_date)
 
 
 d <- data.frame(crop="maize", 
                 country="Mexico",
                 site=r$Name,
                 adm1=r$State,
                 adm2=r$Municipality,
                 latitude=r$Latitude,
                 longitude=r$Longitude,
                 elevation=r$Altitude,
                 planting_date=as.character(r$Sowing_date),
                 rep=as.integer(r$Num_Rep),
                 yield=r$Yield_dry,
                 plant_height=as.numeric(r$Height),
                 moist=r$Moist,
                 slper=r$Lodging,
                 harvest_date=as.character(as.Date(r$Harvest_date,origin ="1900-01-01")),
                 flowering=as.numeric(r$Flower_days),
                 maturity=as.numeric(r$Mat_days),
                 grain_weight=r$Thousand,
                 crop_price=as.numeric(r$Price)/1000,
                 N_fertilizer=as.numeric(r$Fert_N),
                 P_fertilizer=as.numeric(r$Fert_P)/2.29,
                 K_fertilizer=as.numeric(r$Fert_K),
                 row_spacing=as.numeric(r$Row_dist),
                 variety=r$Variety,
                 previous_crop_residue=r$Res_perc,
                 land_prep_method=r$Till,
                 crop_rotation=r$Crop_rotation,
                 treatment=as.character(r$Num_Trat),
                 season=r$Cycle,
                 plant_density=as.numeric(r$Plants_m2)*10000,
                 yield_part="grain",
                 trial_id=id)
 d$plant_height[d$plant_height==0]<-NA
 d$plant_height[d$plant_height=="-"]<-NA
 
 d$land_prep_method<- carobiner::replace_values(d$land_prep_method,"Conventional tillage","conventional")
 #no standard carob term for zero tillage
 d$crop_rotation<- gsub("/|-",",",d$crop_rotation)
 # dashes are different from commas as they are used for intercropping
 d$crop_rotation<- tolower(d$crop_rotation)
 d$crop_rotation<- gsub("frjiol","kidney bean",d$crop_rotation)
 d$crop_rotation<- gsub("frjiol","kidney bean",d$crop_rotation)
 
 d$crop_rotation<- gsub("chicaro","pea",d$crop_rotation)
 #in the original dataset there was chicaro, which has been replaced with pea
 
 
 #### about the data #####
 ## (TRUE/FALSE)
 d$on_farm <- FALSE
 d$is_survey <- FALSE
 d$irrigated <- FALSE
 d$striga_trial<- FALSE
 d$borer_trial<- FALSE
 d$striga_infected<- FALSE
 return(d)
 }
 
 d1 <- maize_mexico("DAT-Oaxaca-MIAF.xlsx", "Teopoxco", 1)
 d2 <- maize_mexico("DAT-Oaxaca-MIAF.xlsx", "Tamazulapam", 2)
d<-rbind(d1,d2)
## process file(s)

## use a subset
	#in the original dataset,some of the harvest dates were put as 0 and now replacing with NA
	r$Harvest_date[r$Harvest_date==0]<-NA
	
	
	d <- data.frame(crop="maize", 
					        country="Mexico",
					        site=r$Name,
					        adm1=r$State,
					        adm2=r$Municipality,
					        latitude=r$Latitude,
					        longitude=r$Longitude,
					        elevation=r$Altitude,
					        planting_date=as.character(r$Sowing_date),
					        rep=as.integer(r$Num_Rep),
					        yield=r$Yield_dry,
					        plant_height=as.numeric(r$Height),
					        moist=r$Moist,
					        slper=r$Lodging,
					        harvest_date=as.character(as.Date(r$Harvest_date,origin ="1900-01-01")),
					        flowering=as.numeric(r$Flower_days),
					        maturity=as.numeric(r$Mat_days),
					        grain_weight=r$Thousand,
					        crop_price=r$Price/1000,
					        N_fertilizer=as.numeric(r$Fert_N),
					        P_fertilizer=as.numeric(r$Fert_P)/2.29,
					        K_fertilizer=as.numeric(r$Fert_K),
					        row_spacing=as.numeric(r$Row_dist),
					        variety=r$Variety,
					        previous_crop_residue=r$Res_perc,
					        land_prep_method=r$Till,
					        crop_rotation=r$Crop_rotation,
					        treatment=as.character(r$Num_Trat),
					        season=r$Cycle,
					        plant_density=as.numeric(r$Plants_m2)*10000,
					        yield_part="grain",
					        trial_id="1")
	d$plant_height[d$plant_height==0]<-NA
	d$plant_height[d$plant_height=="-"]<-NA
	
	d$land_prep_method<- carobiner::replace_values(d$land_prep_method,"Conventional tillage","conventional")
	#no standard carob term for zero tillage
	d$crop_rotation<- gsub("/|-",",",d$crop_rotation)
	# dashes are different from commas as they are used for intercropping
	d$crop_rotation<- tolower(d$crop_rotation)
	d$crop_rotation<- gsub("frjiol","kidney bean",d$crop_rotation)
	d$crop_rotation<- gsub("chicaro","pea",d$crop_rotation)
	#in the original dataset there was chicaro, which has been replaced with pea
	
	
	
#### about the data #####
## (TRUE/FALSE)
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$striga_trial<- FALSE
	d$borer_trial<- FALSE
	d$striga_infected<- FALSE

# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

# carob_script(path)

