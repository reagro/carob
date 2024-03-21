# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"This dataset was obtained from maize Crop cut survey conducted in 2015 by 
EIAR and CIMMYT. Replicated crop cuts of 16m2 in farmers fields along with addition
data on nutrient use and variety, and soil sample (0-20, 20-50 cm). Note that not all 
soil samples have been analysed yet.

"
#### Identifiers
	uri <- "hdl:11529/11016"
	group <- "crop_cuts"

# the script filename should be paste0(dataset_id, ".R")
	dataset_id <- carobiner::simple_uri(uri)

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group=group,dataset_id=dataset_id),
		data_citation="Peter Craufurd, 2017, TAMASA Ethiopia. Yield, soil and agronomy data 
		from farmers’ maize fields collected by EIAR, 2015 season, 
		https://hdl.handle.net/11529/11016, CIMMYT Research Data & 
		Software Repository Network, V1",
		data_institutions = "CIMMYT",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		project=NA,
		data_type= "on-farm experiment",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-03-19"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "TAMASA_ET_CC_2015F.xlsx"]
	r <- carobiner::read.excel(f,sheet= "Revised_data")
	# or  r <- carobiner::read.excel(f)

## process file(s)

## use a subset
	d <- data.frame(variety=r$`Name of variety`, 
	                soil_C=r$`Carbon (%)`,soil_pH=r$pH,soil_Al=r$`Al (mg/kg)`,soil_Ca=r$`Ca  (mg/kg)` ,
	                soil_S=r$`S  (mg/kg)`,soil_Mn=r$`Mn  (mg/kg)`,soil_P_total=r$`P  (mg/kg)`,soil_Zn=r$`Zn  (mg/kg)`,
	                soil_K=r$`K  (mg/kg)`,soil_Na=r$`Na  (mg/kg)`, soil_Fe=r$`Fe  (mg/kg)`,
	                soil_sample_top= "0-20",latitude=9.005401,longitude=38.763611,trial_id="1",yield_part="grain")

	
	r$`Quadrant(1)-Grain yield kg/ha` <-as.numeric(r$`Quadrant(1)-Grain yield kg/ha`)
	r$`Quadrant(2)-Grain yield kg/ha`<-as.numeric(r$`Quadrant(2)-Grain yield kg/ha`)
	r$`Quadrant(3)-Grain yield kg/ha`<-as.numeric(r$`Quadrant(3)-Grain yield kg/ha`)
	
	r$quadrant1<- r$`Quadrant(1)-Grain yield kg/ha`
	r$quadrant2<-r$`Quadrant(2)-Grain yield kg/ha`
	r$quadrant3<-r$`Quadrant(3)-Grain yield kg/ha`
	
	d$yield<- rowMeans(r[,c("quadrant1","quadrant2" ,"quadrant3" )], na.rm = T)
	d$crop<-"maize"
	d$dataset_id <- dataset_id
	d$on_farm <-TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
## the treatment code	
	
##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Ethiopia"

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(  "2015-10-01"  ))
	d$harvest_date  <- as.character(as.Date(  "2015-12-31"  ))
	d$soil_sample_top<-as.numeric(d$soil_sample_top)

# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

