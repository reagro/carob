#################################################################################
# Source: Gardian
# Project name: Grain yield and other agronomic traits of international maize trials-Camerron 1989-2002
# Link: http://gardian.bigdata.cgiar.org/dataset.php?id=2037#!/
# Description: This is an international study that contains data on yield and other Agronomic
# traits of maize including borer and striga attacks on maize in Africa. 
# The study was carried out by the International Institute of Tropical Agriculture between 1989
# and 2015 in over thirty African countries. This dataset contains output of the research for Cameroon.

# Extra material: cs-58-6-2399.pdf
#################################################################################

carob_script <- function(path) {

	uri <- "doi:10.25502/20180716/0907/MA"
	dataset_id <- agro::get_simple_URI(uri)
		
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   uri=uri,
	   publication="",
	   contributor="Camila Bonilla",
	   experiment_type="fertilizer;striga",
	   has_weather=FALSE,
	   has_management=FALSE
	)

	## download and read data 
	ff  <- carobiner::get_data(uri, path)
	TRUE
}

todo <- function() {


	d$dataset_id <- dataset_id

	path <- 'Gardian/Grain_yield_other_agronomic_traits_international_maize_trials/1_Gardian_Grain_yield_internationa_maize_trials'

	df <- read.csv(paste0(path,'/international_maize_trial_cameroon_striga.csv'))[,c(2,3,6,7,9,10,12,13)]
	colnames(df) <- c('crop_variety','treatment_code','country','town_village','longitude','latitude','year',
					  'yield_kg_ha')
	df$year[df$year==22] <- 2002
	df$year[df$year==24] <- 2004
	df$year[df$year==25] <- 2005

	df <- df[df$yield_kg_ha>500,]
	df <- aggregate(.~town_village+year,data=df,median)

	df$country <- 'Cameroon'
	df$crop_variety <- 'mean of varieties'
	df$treatment_code <- 'breeding trial'
	df$yield_kg_ha <- round(df$yield_kg_ha)

	df$kg_N_ha <- 30
	df$kg_P_ha <- 30
	df$kg_K_ha <- 30

	df$comments <- 'data from genetic/environmental stress trials. Fertilizer management was taken from a paper'

	df <- df[df$town_village!='MAYO-GALKE' & df$town_village!='NFONTA', ]

	############
	# Format
	############
	df$institute <- 'IITA'
	df$plot_no <- NA
	df$replication <- NA
	df$block <- NA
	df$crop_type <- 'Maize'
	df$density <- NA
	df$soil_type <- NA
	df$sand<- NA
	df$clay<- NA
	df$soc <- NA
	df$pH <- NA
	df$avail_P <- NA
	df$ReadMe_file <- 'ReadMe_Gardian.xlsx'
	df$source <- 'Gardian-GrainYield'
	df$link <- 'http://gardian.bigdata.cgiar.org/dataset.php?id=2037#!/'
	df$folder_name <- 'Gardian/Grain_yield_other_agronomic_traits_international_maize_trials/1_Gardian_Grain_yield_internationa_maize_trials/'
	df$file_name <- 'international_maize_trial_cameroon_striga.csv'
	df$extra_file <- 'cs-58-6-2399.pdf'
	df$r_script <- '1_prepare_data/Gardian/Grain_yield_other_agronomic_traits/0_Gardian_Striga_Grain_yield/1_Gardian_Striga_Grain_yield.R'
	df$other_nutrients <- NA
	df$kg_otherNutrient_ha <- NA
	df$rotation_system <- NA
	df$district <- NA
	df$ fertilizer_type <- NA
	df$nut_response_eval <- 'NPK'

	# Map points
	afr <- shapefile('../../fertilizer_prices/data/shp_countries/africa_shp/Africa_2.shp')
	par(mar=c(1,1,1,1))
	plot(afr,main='AgTrials-AfSIS')
	points(df$longitude,df$latitude,col='red',pch=20,cex=1.2)

	df$depth_soil_sample <- NA
	df$sand_unit <- NA
	df$clay_unit <- NA
	df$avail_P_unit <- NA
	df$soc_unit <- NA
	df$trial_type <- 'on_station'
	df$start_year <- NA
	df$end_year <- NA
	df$start_month <- NA
	df$end_month <- NA

	# Reorder
	df_1Gardian_Grain_striga_yield <- df[,c('trial_type','year','start_month','start_year','end_month','end_year','country',
											'district','town_village','longitude','latitude','treatment_code','block','replication',
											'plot_no','rotation_system','crop_type','crop_variety','fertilizer_type',
											'other_nutrients','nut_response_eval','kg_N_ha','kg_P_ha','kg_K_ha','kg_otherNutrient_ha',
											'yield_kg_ha','density','soil_type', 'depth_soil_sample',
											'pH','sand','sand_unit','clay','clay_unit','soc','soc_unit','avail_P','avail_P_unit',
											'comments','institute','source','link',
											'folder_name','file_name','r_script','extra_file')]

	write.csv(df_1Gardian_Grain_striga_yield,'../results/prepare_data/Gardian/Grain_yield/individual/1_Gardian_Striga_Grain_yield.csv')
	
}