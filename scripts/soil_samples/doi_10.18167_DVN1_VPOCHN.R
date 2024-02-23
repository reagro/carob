# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [These are the raw data of the paper: Mulch application as the overarching factor explaining increase in soil organic carbon stocks under conservation agriculture in two 8-year-old experiments in Zimbabwe” authored by Armwell Shumba, Regis Chikowo, Christian Thierfelder, Marc Corbeels, Johan Six, Rémi Cardinael and submitted for publication in a peer-reviewed journal.]

"

	uri <- "doi:10.18167/DVN1/VPOCHN"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "soil_samples"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Shumba, Armwell; Chikowo, Regis; Thierfelder, Christian; Corbeels, Marc; Six, Johan; Cardinael, Rémi, 2023, Data for Mulch application as the overarching factor explaining increase in soil organic carbon stocks under conservation agriculture in two 8-year-old experiments in Zimbabwe, https://doi.org/10.18167/DVN1 /VPOCHN , CIRAD Dataverse, V2",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIRAD, University of Zimbabwe, CIMMYT, ETH ZURICH",
		# e.g. "on-farm experiment", "survey", "compilation"
   		data_type="experiment", 
		carob_contributor="Hope Takudzwa Mazungunye",
		# date of first submission to carob
		carob_date="2024-02-15",
		# name(s) of others who made significant improvements
		revised_by=NA
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)[1]


	f <- ff[basename(ff) == "Shumba_et_al_Raw_data_SOC_paper_vf.xlsx"]

	#r <- read.csv(f)
	r <- readxl::read_excel(f, sheet = "Soil_profile_BD_1m") |> as.data.frame()
	r1 <- readxl::read_excel(f, sheet = "SOC_conc_stocks") |> as.data.frame()
	r2 <- readxl::read_excel(f, sheet = "SOC_change_accum_rates") |> as.data.frame()
	r3 <- readxl::read_excel(f, sheet = "Seasonal_OC_inputs") |> as.data.frame()

	
## process file(s)

	d <- data.frame(treatment=r$Treatment, site=r$Site, soil_sample_top=r$depth_cm, rep=r$Rep, soil_bd= r$BD_g_cm3)
d1 <- data.frame(treatment=r1$Treatment, site=r1$Site, rep=r1$Rep, soil_sample_top=r1$Depth_cm, soil_SOC=r1$SOC_conc_mg_g_1, soil_bd=r$BD_g_cm3)
d2 <- data.frame(treatment=r2$Treatment, site=r2$Site, rep=r2$Rep, soil_sample_top=r2$Depth_cm)
d3 <- data.frame(treatment=r3$Treatment, site=r3$Site, rep=r3$Rep, yield=r3$`Grain (kg/ha)`, dmy_roots=r3$Root_biomass_kg_ha1, crop=r3$Crop)

##Fixing treatment names 
d$site<- carobiner::replace_values(d$site,c("DTC","UZF"),c("Domboshava Training Center","University of Zimbabwe farm"))
d1$site<- carobiner::replace_values(d1$site,c("DTC","UZ"),c("Domboshava Training Center","University of Zimbabwe farm"))
d2$site<- carobiner::replace_values(d2$site,c("DTC","UZ"),c("Domboshava Training Center","University of Zimbabwe farm"))
d3$site<- carobiner::replace_values(d3$site,c("DTC","UZF"),c("Domboshava Training Center","University of Zimbabwe farm"))

#converting soil_SOC units to percentage 
d1$soil_SOC[d1$soil_SOC=="NA"] <- NA
d1$soil_SOC <- as.numeric(d1$soil_SOC)
d1$soil_SOC <- d1$soil_SOC*0.1

##Adding variables
d$dmy_roots <- NA 
d$crop <- NA
d$yield <- NA 
d$soil_SOC <- NA 
d1$dmy_roots <- NA 
d1$crop <- NA 
d1$yield <- NA
d2$dmy_roots <- NA 
d2$crop <- NA 
d2$yield <- NA
d2$soil_SOC <- NA
d2$soil_bd <- NA 
d3$soil_sample_top <- NA 
d3$soil_SOC <- NA 
d3$soil_bd <- NA 

d <- d[,c("treatment", "site","soil_sample_top","rep","soil_bd", "dmy_roots","crop","yield","soil_SOC")]
d1 <- d1[,c("treatment", "site","soil_sample_top","rep","soil_bd", "dmy_roots","crop","yield","soil_SOC")]
d2 <- d2[,c("treatment", "site","soil_sample_top","rep","soil_bd", "dmy_roots","crop","yield","soil_SOC")]
d3 <- d3[,c("treatment", "site","soil_sample_top","rep","soil_bd", "dmy_roots","crop","yield","soil_SOC")]
dd <- carobiner::bindr(d, d1, d2, d3)

##location details 
d0<- data.frame(site=c("Domboshava Training Center", "University of Zimbabwe farm" ), longitude=c(31.1000, 31.0569), latitude=c(-17.4833, -17.7829))

#merging data frames 
d4 <- merge(dd, d0, by = "site", all.x = TRUE)
#### about the data #####
## (TRUE/FALSE)

	d4$dataset_id <- dataset_id
	d4$on_farm <- TRUE
	d4$is_survey <- FALSE
	d4$is_experiment <- TRUE
	d4$irrigated <- FALSE
	d4$country <- "Zimbabwe"
	d4$trial_id<- paste0(d4$dataset_id, "_",d4$rep) 
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path) 
}



