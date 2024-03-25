# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:
Nitrogen (N) is an essential nutrient for sorghum growth and development but often becomes limiting due to low availability and loss. The effects of N fertilization on water use efficiency (WUE) and physiological and yield traits of sorghum were investigated in two locations over two cropping seasons (2014 and 2015) in the Sudan Savanna zone of Nigeria. Three sorghum varieties were evaluated under six (6) N-levels (0, 20, 40, 60, 80, and 100 kg ha−1) at a constant phosphorus and potassium level of 30 kg ha−1. Results showed that N increased grain yield by 35–64% at the Bayero University Kano (BUK) and 23–78% at Minjibir. The highest mean grain yield in the N-fertilizer treatments (2709 kg ha−1 and 1852 kg ha−1 at BUK and Minjibir, resp.) was recorded at 80 kg N ha−1. ICSV400 produced the highest mean grain yields (2677 kg ha−1 and 1848 kg ha−1 at BUK and Minjibir, resp.). Significant differences were observed among the N-levels as well as among the sorghum varieties for estimated water use efficiency (WUE).To review the nutrient needs, especially N of some of selected sorghum varieties and their water use efficiency on marginal land	

"

	uri <- "doi:10.21421/D2/FATVHT"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	## dataset level data 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		publication="doi:10.1155/2018/7676058",
		carob_contributor="Eduardo Garcia Bendito",
		carob_date="2021-06-29",
		data_type="experiment",
		data_institutions="ICRISAT",
		project=NA		   
 	)



	f <- ff[basename(ff) == "Data file of Sorghum N trial Kano Nigeria.xlsx"]

	d <- as.data.frame(readxl::read_excel(f))
	
	d$country <- "Nigeria"
	d$adm1 <- "Kano"
	d$adm2 <- ifelse(d$Location == "BUK", "Gezawa", "Minjibir")
	d$location <- d$Location
	d$trial_id <- paste0(dataset_id, '-', d$Location)
	d$latitude <- ifelse(d$Location == "BUK", 8.5922, 8.5978)
	d$longitude <- ifelse(d$Location == "BUK", 12.0034, 12.1733)
	# As reported in the associated publication:
	d$planting_date <- ifelse(d$Location == "BUK" & d$Year == 2014, "2014-07-19",
                    ifelse(d$Location == "BUK" & d$Year == 2015, "2014-07-20",
                    ifelse(d$Location == "Minjibir" & d$Year == 2014, "2014-07-07", "2014-07-04"))) 
						  
						  
	d$harvest_date <- as.character(as.Date(d$planting_date) + d$`Days to Maturity`)
	d$planting_date <- as.character(d$planting_date)
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	
	d$treatment <- paste0("N", d$Nitrogen, "-P30-K30")

	d$rep <- as.integer(d$`Replication umber`)
	d$crop <- "sorghum"
	d$yield_part <- "grain"
	
	d$variety <- d$Sorghum
## RH	d$yield <- d$`Grain yield` + d$`Stalk yield`
	d$yield <- d$`Grain yield`
## assuming that "Stalk yield" also includes leaves	
	d$residue_yield <- d$`Stalk yield`
	d$grain_weight <- d$GW_1000grnM_g
# Not reported in the associated publication	
	d$fertilizer_type <- "unknown" 
# As reported in the associated publication	
	d$N_splits <- 2L
	d$N_fertilizer <- d$Nitrogen

# As reported in the associated publication. Converting P2O5 to P-elemental
	d$P_fertilizer <- 30/2.29 
# As reported in the associated publication Converting K2O to K-elemental	
	d$K_fertilizer <- 30/1.2051 
	d$OM_used <- FALSE
# As reported in the associated publication	
	d$plant_spacing <- 30 
	d$row_spacing <- 75 

	d <- d[,c(18:43)]
	d$dataset_id <- dataset_id
	d$yield_part <- "grain"

	carobiner::write_files(dset, d, path=path)

}
