
###########################################################################################################
# N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among 
# African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition
# and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable,
# long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain
# legumes through effective production technologies including inoculants and fertilizers adapted to local settings.
# A strong national expertise in grain legume production and N2-fixation research and development will be the legacy
# of the project.
# The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other 
# countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
###########################################################################################################

# Notes
# Note1: Where trial_id is labelled as "" it is replaced with the preceding value.


carob_script <- function(path){

	uri <- "doi.org/10.25502/EZQV-ZZ19"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"

#dataset level data

	dset <- data.frame(
		dataset_id = dataset_id,
		group = group,
		project="N2Africa",
		uri = uri,
		publication = 'doi.org/10.1016/j.agee.2017.08.015',
		data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa dianostic trial - Nigeria, 2014 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/EZQV-ZZ19",
		carob_contributor = "Andrew Sila",
		data_type = "on farm experiment",
		data_institutions="IITA"
	)

p_year <- 2014 #planting year
h_year <- 2014 # harvest year

## download and read data 
	ff <- carobiner::get_data(uri,path,group)
	js <- carobiner::get_metadata(dataset_id, path, group, major = 1, minor = 0)
	dset$license <- carobiner::get_license(js) 
	
	# read the experiment data table
	f0 <- ff[basename(ff) == "experiment.csv"]
	r2 <- read.csv(f0)
	
	f1 <- ff[basename(ff) == "general.csv"]
	d1 <- read.csv(f1)
	
	
	f5 <- ff[basename(ff) == "production.csv"]
	d5 <- read.csv(f5)
	

  #assemble everything for crop 1 
	##efyrouwa: unable to pull for "width_of_harvested_plot_crop_1_plot_1","pod_weight_unshelled_grain_groundnut_crop_1_plot_2.kg" 
	##because they are structured differently, will come to it later
	
	oldnames <- c("name_treatment_X", "description_treatment_X","width_of_harvested_plot_crop_1_plot_X.m","pod_weight_unshelled_grain_groundnut_crop_1_plot_2.kg","width_of_harvested_plot_crop_1_plot_X","depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_X.m","number_of_rows_in_plot_crop_1_plot_X","grain_weight_kg_shelled_grain_crop_1_plot_X.kg","pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_X.kg","above_ground_biomass_weight_husks_stover_res_crop_1_plot_X.kg") 
	newnames <- c("treatment","description","width","length","row_count","grain_weight","pod_weight","residue_yield")	

	olnm1 <- gsub("pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_X.kg","pod_weight_unshelled_grain_groundnut_crop_1_plot_2.kg", oldnames)
	olnm2 <- gsub("width_of_harvested_plot_crop_1_plot_X.m","width_of_harvested_plot_crop_1_plot_1",oldnames)
	
	lst <- lapply(1:10, function(i) {
	  if (i == 2) {
	    di <- r2[, gsub("X", i, olnm1)]
	  } else if (i ==1) {
	    di <- r2[, gsub("X", i, olnm2)]
	  } else {
	    di <- r2[, gsub("X", i, oldnames)]
	  }
	  colnames(di) <- newnames
	  di
	})
	
	dd <- do.call(rbind, lst)
	dd$trial_id <- r2$farm_id
	dd$crop <- carobiner::replace_values(r2$experimental_treatments_crop_1, c("","Groundnut","Soya bean","soya bean","Cowpea","Maize","SOYABEANS","SOYBEAN","COWPEA","SOYA BEAN","G per NUT"),c(NA,"groundnut","soybean","soybean","cowpea","maize","soybean","soybean","cowpea","soybean","groundnut"))
	dd$variety <- carobiner::fix_name(r2$experimental_treatments_variety_crop_1)
	dd$trial_id <- r2$farm_id

	
	# assembling everything for crop 2
	oldnames <- c("name_treatment_X", "description_treatment_X","width_of_harvested_plot_crop_2_plot_X.m","depth_of_harvested_plot_perpen_dicular_to_rows_crop_2_plot_X.m","number_of_rows_in_plot_crop_2_plot_X","grain_weight_kg_shelled_grain_crop_2_plot_X.kg","pod_weight_kg_unshelled_grain_groundnut_crop_2_plot_X.kg","above_ground_biomass_weight_husks_stover_res_crop_2_plot_X.kg")
	newnames <- c("treatment","description","width","length","row_count","grain_weight","pod_weight","residue_yield")
	
	lst <- list()
	for (i in 1:8) {
	  inms <- gsub("X", i, oldnames)
	  ri <- r2[, inms] 
	  colnames(ri) <- newnames
	  lst[[i]] <- ri
	}	
	
	ddd <- do.call(rbind, lst)
	ddd$trial_id <- r2$farm_id
	ddd$crop <- carobiner::replace_values(r2$experimental_treatments_crop_2,c("","Groundnut","Soya bean","Farmer local variety","Maize","soya bean","Cowpea","SOYBEAN","MAIZE","G per NUT","SOYABEANS"),c(NA,"groundnut","soybean","local","maize","soybean","cowpea","soybean","maize","groundnut","soybean"))         
	ddd$variety <- carobiner::fix_name(r2$experimental_treatments_variety_crop_2)
	ddd$trial_id <- r2$farm_id

	
	d0 <- carobiner::bindr(dd,ddd)
	
  #filling in the inoculated 
	d0$treatment <- gsub("\\s*([+-])\\s*", "\\1", d0$treatment) # removing spaces between + or - 
	d0$description <- gsub("\\s*([+-])\\s*I\\s*", "\\1I", d0$description)# removing spaces between + or - 
	d0$innoculated <- ifelse(grepl("\\+i", d0$treatment), TRUE, FALSE)
	
	
	#filling in the varieties
	i <- grepl("samnut \\d{2}", d0$treatment) | grepl("evdt 2009", d0$treatment)| grepl("tgx\\d{4}-\\d[a-zA-Z]", d0$treatment)|grepl("tgx\\d{4}", d0$treatment)|grepl("^it \\S+", d0$treatment)
	t <- is.na(d0$variety)
	d0$variety[i & t] <- gsub(".*(samnut \\d{2}|evdt 2009|tgx\\d{4}-\\d[a-zA-Z]|tgx\\d{4}|^it \\S+).*", "\\1", d0$treatment[i & t])
	
	
	#fill in the crops
	d0$crop[grepl("[+-]i", d0$treatment)|grepl("[+-]I", d0$description) & is.na(d0$crop)] <- "soybean"
	d0$crop[(grepl("^it\\s*\\S*|cowpea|^uam\\s*\\S*", d0$treatment) | grepl("^it\\s*\\S*", d0$description) | grepl("(?i)^uam", d0$description)) & is.na(d0$crop)] <- "cowpea"
	d0$crop[grepl("\\s*samnut \\d{2}", d0$treatment) & is.na(d0$crop)] <- "groundnut"
	d0$crop[(grepl("evdt\\s*\\S+|cereal|maize|evdt", d0$treatment)) & is.na(d0$crop)] <- "maize"
	
	
	#removing all the rows with no entries 
	i <- c("treatment","width","length","row_count","grain_weight","pod_weight")
	d0 <- d0[apply(d0[i], 1, function(row) any(!is.na(row) & row != "")), ]
	
	# efyrouwa: what should be used to calculate yield?, grain_weight or pod_weight?, 
	##  I used grain_weight, in cases there's no grain_weight, I used pod_weight
	d0$width <- as.numeric(d0$width)
	d0$yield <- 10000 / (d0$width*d0$length) * ifelse(is.na(d0$grain_weight), d0$grain_weight, d0$pod_weight)
	
	# consolidating info for fertilizers 1 for all the 8 plots
	oldnames <- c("name_treatment_X", "description_treatment_X","experimental_treatments_fertilizer_1","fert_1_kg_plot_plot_X.kg_per_plot")
	newnames <- c("treatment","description","fert_type","fert_amount_kg_per_plot")
	
	lst <- list()
	for (i in 1:8) {
	  inms <- gsub("X", i, oldnames)
	  ri <- r2[, inms] 
	  colnames(ri) <- newnames
	  lst[[i]] <- ri
	}	
	
	dd1 <- do.call(rbind, lst)
	dd1$trial_id <- d0$farm_id
	
	
	# consolidating info for fertilizers 2 for all the 8 plots
	oldnames <- c("name_treatment_X", "description_treatment_X","experimental_treatments_fertilizer_2","fert_2_kg_plot_plot_X.kg_per_plot")
	newnames <- c("treatment","description","fert_type","fert_amount_kg_per_plot")
	
	lst <- list()
	for (i in 1:8) {
	  inms <- gsub("X", i, oldnames)
	  ri <- r2[, inms] 
	  colnames(ri) <- newnames
	  lst[[i]] <- ri
	}	
	
	dd2 <- do.call(rbind, lst)
	dd2$trial_id <- d0$farm_id
	
	  
	# consolidating info for fertilizers 3 for all the 8 plots
	oldnames <- c("name_treatment_X", "description_treatment_X","experimental_treatments_fertilizer_3","fert_3_kg_plot_plot_X.kg_per_plot")
	newnames <- c("treatment","description","fert_type","fert_amount_kg_per_plot")
	
	lst <- list()
	for (i in 1:8) {
	  inms <- gsub("X", i, oldnames)
	  ri <- r2[, inms] 
	  colnames(ri) <- newnames
	  lst[[i]] <- ri
	}	
	
	dd3 <- do.call(rbind, lst)
	dd3$trial_id <- d0$farm_id
	
	# Combine data frames
	ddd1 <- rbind(dd1, dd2,dd3)
	
	#efyrouwa: is this correct? remove rows with no info fertilizer info
	i <- c("fert_type", "fert_amount_kg_per_plot")
	ddd1 <- ddd1[apply(ddd1[i], 1, function(row) any(!is.na(row) & row != "")), ]
	
	ddd1$N_fertilizer <- 0
	ddd1$K_fertilizer <- 0

 
	##efyrouwa: points of action
	## 1) ensure crop 1 info is working properly
	## 2) merge the fertilizer datasets with the crop datasets
	## 3) process the remaining two datasets
	

	# all scripts should end like this

	carobiner::write_files(dset, d, path=path)
}
