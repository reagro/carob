# R script for "carob"

## ISSUES
# ....
##RH: duplicate. This file was also done by Cedric

carob_script <- function(path) {

"Description:

    This study contains spring wheat yield data (1st, 2nd, and 3rd WYCYTs and 1st, 2nd, 3rd and 4th SATYNs) from 136 international environments that were used to evaluate the predictive ability of different models in diverse environments by modeling G×E using the pedigree-derived additive relationship matrix (A matrix).
"

	uri <- "hdl:11529/10831"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation=NA,
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMMYT",
   		data_type="experiment", 
		carob_contributor="Shumirai Manzvera" 
		carob_date="2023-09-09",
	)

## download and read data 
	f <- "C:/carob/data/raw/wheat_trials/hdl_11529_10831"
  path <- "C:/carob"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)


  library("readxl")
	r <- read_xlsx(f)
	r <- readxl::read_excel(f) |> as.data.frame()
  d <- X3WYCYT
	
## process file(s)

## use a subset
	d <- carobiner::change_names(r, from, to)

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
## the treatment code	
####Script For X3WYCYT ###

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- d$code


## each site must have corresponding longitude and latitude
## see carobiner::geocode
	
	#longitude
	d$longitude[d$country=="Croatia O"] <-  15.2
	d$longitude[d$country=="Egypt G"] <- 31.233334 
	d$longitude[d$country=="Egypt N"] <- 31.233334
	d$longitude[d$country=="Egypt S"] <- 31.233334
	d$longitude[d$country=="Egypt Si"] <- 31.233334
	d$longitude[d$country=="Egypt SK"] <- 31.233334
	d$longitude[d$country=="India K"] <- 78.96288
	d$longitude[d$country=="India L"] <- 78.96288
	d$longitude[d$country=="India U"] <- 78.96288
	d$longitude[d$country=="Mex CM"] <- 102.5528
	d$longitude[d$country=="Nepal B"] <- 84.124008
	d$longitude[d$country=="Pak B"] <- 69.3451
	d$longitude[d$country=="Pak f"] <- 69.3451
	d$longitude[d$country=="Pak I"] <- 69.3451
	d$longitude[d$country=="Pak N"] <- 69.3451
	d$longitude[d$country=="Romania I"] <- 24.966760000000022
	
##LATITUDE
	
	d$latitude[d$country=="Croatia O"] <-  45.1
	d$latitude[d$country=="Egypt G"] <- 30.033333  
	d$latitude[d$country=="Egypt N"] <- 30.033333
	d$latitude[d$country=="Egypt S"] <- 30.033333
	d$latitude[d$country=="Egypt Si"] <- 30.033333
	d$latitude[d$country=="Egypt SK"] <- 30.033333
	d$latitude[d$country=="India K"] <- 20.593684
	d$latitude[d$country=="India L"] <- 20.593684
	d$latitude[d$country=="India U"] <- 20.593684
	d$latitude[d$country=="Mex CM"] <- 23.6345
	d$latitude[d$country=="Nepal B"] <- 28.394857
	d$latitude[d$country=="Pak B"] <- 30.3753
	d$latitude[d$country=="Pak f"] <- 30.3753
	d$latitude[d$country=="Pak I"] <- 30.3753
	d$latitude[d$country=="Pak N"] <- 30.3753
	d$latitude[d$country=="Romania I"] <- 45.943161
	


##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "wheat"

 
##### Yield #####

	d$yield <- d$YLD 

	#####replication###
	d$rep <- d$Rep
	
	##script for X4SATYN
	d2 <- X4SATYN
	
	d2$dataset_id <- dataset_id
	d2$on_farm <- TRUE
	d2$is_survey <- FALSE
	d2$is_experiment <- TRUE
	d2$irrigated <- FALSE
	
	d2$country <- d2$code
	d2$crop <- "wheat"
	d2$yield <- d2$YLD
	d2$rep <- d2$Rep
	 
	
	
	d2$longitude[d2$country=="BGLD D"] <- 90.399452  
	d2$longitude[d2$country=="BGLD R"] <- 90.399452
	d2$longitude[d2$country=="India D"] <- 78.96288
	d2$longitude[d2$country=="India DR"] <- 78.96288
	d2$longitude[d2$country=="India K"] <- 78.96288
	d2$longitude[d2$country=="India L"] <- 78.96288
	d2$longitude[d2$country=="India U"] <- 78.96288
	d2$longitude[d2$country=="India V"] <- 78.96288
	d2$longitude[d2$country=="Iran D"] <- 53.6880
	d2$longitude[d2$country=="Iran DZ"] <- 53.6880
	d2$longitude[d2$country=="Iran S"] <- 53.6880
	d2$longitude[d2$country=="India Z"] <- 78.96288
	d2$longitude[d2$country=="Mex B"] <- 102.5528
	d2$longitude[d2$country=="Mex BC"] <- 102.5528
	d2$longitude[d2$country=="Mex H"] <- 102.5528
	d2$longitude[d2$country=="Mex HD"] <- 102.5528
	d2$longitude[d2$country=="Mex JAL"] <- 102.5528
	d2$longitude[d2$country=="Mex SON"] <- 102.5528
	d2$longitude[d2$country=="Nepal B"] <- 84.124008
	d2$longitude[d2$country=="Pak f"] <- 69.3451
	d2$longitude[d2$country=="Pak I"] <- 69.3451

	###latitude
	
	d2$latitude[d2$country=="BGLD D"] <- 23.777176   
	d2$latitude[d2$country=="BGLD R"] <- 23.777176 
	d2$latitude[d2$country=="India K"] <- 20.593684
	d2$latitude[d2$country=="India D"] <- 20.593684
	d2$latitude[d2$country=="India DR"] <- 20.593684
	d2$latitude[d2$country=="India L"] <- 20.593684
	d2$latitude[d2$country=="India U"] <- 20.593684
	d2$latitude[d2$country=="India V"] <- 20.593684
	d2$latitude[d2$country=="Iran D"] <- 32.4279
	d2$latitude[d2$country=="Iran DZ"] <- 32.4279
	d2$latitude[d2$country=="Iran S"] <- 32.4279
	d2$latitude[d2$country=="Iran Z"] <- 32.4279
	d2$latitude[d2$country=="Mex B"] <- 23.6345
	d2$latitude[d2$country=="Mex BC"] <- 23.6345
	d2$latitude[d2$country=="Mex H"] <- 23.6345
	d2$latitude[d2$country=="Mex HD"] <- 23.6345
	d2$latitude[d2$country=="Mex JAL"] <- 23.6345
	d2$latitude[d2$country=="Mex SON"] <- 23.6345
	d2$latitude[d2$country=="Nepal B"] <- 28.394857
	d2$latitude[d2$country=="Pak B"] <- 30.3753 
	d2$latitude[d2$country=="Pak f"] <- 30.3753
	d2$latitude[d2$country=="Pak I"] <- 30.3753
	
	
	##Sript for X3SATYN 
	d3 <- X3SATYN 
	
	d3$dataset_id <- dataset_id
	d3$on_farm <- TRUE
	d3$is_survey <- FALSE
	d3$is_experiment <- TRUE
	d3$irrigated <- FALSE
	
	d3$country <- d3$code
	d3$crop <- "wheat"
	d3$yield <- d3$YLD
	d3$rep <- d3$Rep
	
	d3$longitude[d3$country=="BGLD D"] <- 90.399452  
	d3$longitude[d3$country=="BGLD R"] <- 90.399452
	d3$longitude[d3$country=="Egypt A"] <- 31.233334   
	d3$longitude[d3$country=="India D"] <- 78.96288
	d3$longitude[d3$country=="India H"] <- 78.96288
	d3$longitude[d3$country=="India I"] <- 78.96288
	d3$longitude[d3$country=="India K"] <- 78.96288
	d3$longitude[d3$country=="India L"] <- 78.96288
	d3$longitude[d3$country=="India U"] <- 78.96288
	d3$longitude[d3$country=="India V"] <- 78.96288
	d3$longitude[d3$country=="Iran D"] <- 53.6880
	d3$longitude[d3$country=="Iran DZ"] <- 53.6880
	d3$longitude[d3$country=="Iran K"] <- 53.6880
	d3$longitude[d3$country=="Iran Z"] <- 53.6880
	d3$longitude[d3$country=="Mex B"] <- 102.5528
	d3$longitude[d3$country=="Mex BC"] <- 102.5528
	d3$longitude[d3$country=="Mex D"] <- 102.5528
	d3$longitude[d3$country=="Mex JAL"] <- 102.5528
	d3$longitude[d3$country=="Mex SIN"] <- 102.5528
	d3$longitude[d3$country=="Mex SON"] <- 102.5528
	d3$longitude[d3$country=="Nepal B"] <- 84.124008
	d3$longitude[d3$country=="Pak P"] <- 69.3451 
	d3$longitude[d3$country=="Pak f"] <- 69.3451
	d3$longitude[d3$country=="Pak I"] <- 69.3451
	d3$longitude[d3$country=="Pak R"] <- 69.3451

	##latitude 
	
	
	d3$latitude[d3$country=="BGLD D"] <- 23.777176   
	d3$latitude[d3$country=="BGLD R"] <- 23.777176
	d3$latitude[d3$country=="Egypt A"] <- 30.033333
	d3$latitude[d3$country=="India D"] <- 20.593684
	d3$latitude[d3$country=="India H"] <- 20.593684
	d3$latitude[d3$country=="India I"] <- 20.593684
	d3$latitude[d3$country=="India K"] <- 20.593684
	d3$latitude[d3$country=="India L"] <- 20.593684
	d3$latitude[d3$country=="India U"] <- 20.593684
	d3$latitude[d3$country=="India V"] <- 20.593684
	d3$latitude[d3$country=="Iran D"] <- 32.4279
	d3$latitude[d3$country=="Iran DZ"] <- 32.4279
	d3$latitude[d3$country=="Iran K"] <- 32.4279
	d3$latitude[d3$country=="Iran Z"] <- 32.4279
	d3$latitude[d3$country=="Mex B"] <- 23.6345
	d3$latitude[d3$country=="Mex BC"] <- 23.6345
	d3$latitude[d3$country=="Mex D"] <- 23.6345
	d3$latitude[d3$country=="Mex SIN"] <- 23.6345
	d3$latitude[d3$country=="Mex JAL"] <- 23.6345
	d3$latitude[d3$country=="Mex SON"] <- 23.6345
	d3$latitude[d3$country=="Nepal B"] <- 28.394857
	d3$latitude[d3$country=="Pak B"] <- 30.3753 
	d3$latitude[d3$country=="Pak f"] <- 30.3753
	d3$latitude[d3$country=="Pak I"] <- 30.3753
	d3$latitude[d3$country=="Pak P"] <- 30.3753
	d3$latitude[d3$country=="Pak R"] <- 30.3753
	
	
	
	 
	#script for X2WYCYT
	
	d4 <- X2WYCYT
	d4$dataset_id <- dataset_id
	d4$on_farm <- TRUE
	d4$is_survey <- FALSE
	d4$is_experiment <- TRUE
	d4$irrigated <- FALSE
	
	d4$country <- d4$code
	d4$crop <- "wheat"
	d4$yield <- d4$YLD
	d4$rep <- d4$Rep
	
	d4$longitude[d4$country=="BGLD D"] <- 90.399452  
	d4$longitude[d4$country=="BGLD J"] <- 90.399452 
	d4$longitude[d4$country=="BGLD R"] <- 90.399452
	d4$longitude[d4$country=="China L"] <- 104.195397 
	d4$longitude[d4$country=="Egypt A"] <- 31.2333342
	d4$longitude[d4$country=="India D"] <- 78.96288
	d4$longitude[d4$country=="India H"] <- 78.96288
	d4$longitude[d4$country=="India I"] <- 78.96288
	d4$longitude[d4$country=="India K"] <- 78.96288
	d4$longitude[d4$country=="India L"] <- 78.96288
	d4$longitude[d4$country=="India U"] <- 78.96288
	d4$longitude[d4$country=="India V"] <- 78.96288
	d4$longitude[d4$country=="Iran D"] <- 53.6880
	d4$longitude[d4$country=="Iran S"] <- 53.6880
	d4$longitude[d4$country=="Iran SP"] <- 53.6880
	d4$longitude[d4$country=="Iran Z"] <- 53.6880
	d4$longitude[d4$country=="Mex CM"] <- 102.5528
	d4$longitude[d4$country=="Mex BC"] <- 102.5528
	d4$longitude[d4$country=="Mex JAL"] <- 102.5528
	d4$longitude[d4$country=="Mex SIN"] <- 102.5528
	d4$longitude[d4$country=="Mex SON"] <- 102.5528
	d4$longitude[d4$country=="Nepal B"] <- 84.124008
	d4$longitude[d4$country=="Pak P"] <- 69.3451 
	d4$longitude[d4$country=="Pak f"] <- 69.3451
	d4$longitude[d4$country=="Pak I"] <- 69.3451

	##latitude
	d4$latitude[d4$country=="BGLD D"] <- 23.777176   
	d4$latitude[d4$country=="BGLD J"] <- 23.777176
	d4$latitude[d4$country=="BGLD R"] <- 23.777176 
	d4$latitude[d4$country=="China L"] <- 35.86166
	d4$latitude[d4$country=="Egypt A"] <- 30.033333
	d4$latitude[d4$country=="India D"] <- 20.593684
	d4$latitude[d4$country=="India H"] <- 20.593684
	d4$latitude[d4$country=="India I"] <- 20.593684
	d4$latitude[d4$country=="India K"] <- 20.593684
	d4$latitude[d4$country=="India L"] <- 20.593684
	d4$latitude[d4$country=="India U"] <- 20.593684
	d4$latitude[d4$country=="India V"] <- 20.593684
	d4$latitude[d4$country=="Iran D"] <- 32.4279
	d4$latitude[d4$country=="Iran S"] <- 32.4279
	d4$latitude[d4$country=="Iran SP"] <- 32.4279
	d4$latitude[d4$country=="Iran Z"] <- 32.4279
	d4$latitude[d4$country=="Mex BC"] <- 23.6345
	d4$latitude[d4$country=="Mex CM"] <- 23.6345
	d4$latitude[d4$country=="Mex SIN"] <- 23.6345
	d4$latitude[d4$country=="Mex JAL"] <- 23.6345
	d4$latitude[d4$country=="Mex SON"] <- 23.6345
	d4$latitude[d4$country=="Nepal B"] <- 28.394857
	d4$latitude[d4$country=="Pak B"] <- 30.3753 
	d4$latitude[d4$country=="Pak f"] <- 30.3753
	d4$latitude[d4$country=="Pak I"] <- 30.3753
	d4$latitude[d4$country=="Pak P"] <- 30.3753

	
	
	# script for X2SATYN
	d5 <- X2SATYN
	d5$dataset_id <- dataset_id
	d5$on_farm <- TRUE
	d5$is_survey <- FALSE
	d5$is_experiment <- TRUE
	d5$irrigated <- FALSE
	
	##Longitude
	
	d5$longitude[d5$country=="BGLD J"] <- 90.399452  
	d5$longitude[d5$country=="Egypt S"] <- 31.233334
	d5$longitude[d5$country=="India D"] <- 78.96288
	d5$longitude[d5$country=="India H"] <- 78.96288
	d5$longitude[d5$country=="India I"] <- 78.96288
	d5$longitude[d5$country=="India K"] <- 78.96288
	d5$longitude[d5$country=="India L"] <- 78.96288
	d5$longitude[d5$country=="India U"] <- 78.96288
	d5$longitude[d5$country=="India V"] <- 78.96288
	d5$longitude[d5$country=="Iran S"] <- 53.6880
	d5$longitude[d5$country=="Iran SC"] <- 53.6880
	d5$longitude[d5$country=="Iran K"] <- 53.6880
	d5$longitude[d5$country=="Iran Z"] <- 53.6880
	d5$longitude[d5$country=="Mex CM"] <- 102.5528
	d5$longitude[d5$country=="Mex D"] <- 102.5528
	d5$longitude[d5$country=="Mex H"] <- 102.5528
	d5$longitude[d5$country=="Mex HD"] <- 102.5528
	d5$longitude[d5$country=="Nepal B"] <- 84.124008
	d5$longitude[d5$country=="Pak B"] <- 69.3451 
	d5$longitude[d5$country=="Pak f"] <- 69.3451
	d5$longitude[d5$country=="Pak I"] <- 69.3451
	
	
	##latitude 
	
	
	d5$latitude[d5$country=="BGLD J"] <- 23.777176   
	d5$latitude[d5$country=="Egypt S"] <- 30.033333
	d5$latitude[d5$country=="India D"] <- 20.593684
	d5$latitude[d5$country=="India H"] <- 20.593684
	d5$latitude[d5$country=="India I"] <- 20.593684
	d5$latitude[d5$country=="India K"] <- 20.593684
	d5$latitude[d5$country=="India L"] <- 20.593684
	d5$latitude[d5$country=="India U"] <- 20.593684
	d5$latitude[d5$country=="India V"] <- 20.593684
	d5$latitude[d5$country=="Iran S"] <- 32.4279
	d5$latitude[d5$country=="Iran SC"] <- 32.4279
	d5$latitude[d5$country=="Mex CM"] <- 23.6345
	d5$latitude[d5$country=="Mex D"] <- 23.6345
	d5$latitude[d5$country=="Mex H"] <- 23.6345
	d5$latitude[d5$country=="Mex HD"] <- 23.6345
	d5$latitude[d5$country=="Nepal B"] <- 28.394857
	d5$latitude[d5$country=="Pak B"] <- 30.3753 
	d5$latitude[d5$country=="Pak f"] <- 30.3753
	d5$latitude[d5$country=="Pak I"] <- 30.3753
	
	
	
	
	
	d5$country <- d5$code
	d5$crop <- "wheat"
	d5$yield <- d5$YLD
	d5$rep <- d5$Rep
	
	d6 <- X1WYCYT
	d6$dataset_id <- dataset_id
	d6$on_farm <- TRUE
	d6$is_survey <- FALSE
	d6$is_experiment <- TRUE
	d6$irrigated <- FALSE
	
	d6$country <- d6$code
	d6$crop <- "wheat"
	d6$yield <- d6$YLD
	d6$rep <- d6$Rep
	
	## 
	d6$longitude[d6$country=="BGLD J"] <- 90.399452 
	d6$longitude[d6$country=="BGLD R"] <- 90.399452
	d4$longitude[d4$country=="China L"] <- 103.000 
	d6$longitude[d6$country=="India D"] <- 78.96288
	d6$longitude[d6$country=="India H"] <- 78.96288
	d6$longitude[d6$country=="India I"] <- 78.96288
	d6$longitude[d6$country=="India K"] <- 78.96288
	d6$longitude[d6$country=="India L"] <- 78.96288
	d6$longitude[d6$country=="India U"] <- 78.96288
	d6$longitude[d6$country=="Iran SA"] <- 53.6880
	d6$longitude[d6$country=="Mex CM"] <- 102.5528
	d6$longitude[d6$country=="Nepal B"] <- 84.124008
	d6$longitude[d6$country=="Pak N"] <- 69.3451 
	d6$longitude[d6$country=="Pak f"] <- 69.3451
	d6$longitude[d6$country=="Pak I"] <- 69.3451
	d6$longitude[d6$country=="SA B"] <- 45.079162
	
	#latitude 
	
	 
	d6$latitude[d6$country=="BGLD J"] <- 23.777176
	d6$latitude[d6$country=="BGLD R"] <- 23.777176 
	d6$latitude[d6$country=="India D"] <- 20.593684
	d6$latitude[d6$country=="India H"] <- 20.593684
	d6$latitude[d6$country=="India I"] <- 20.593684
	d6$latitude[d6$country=="India K"] <- 20.593684
	d6$latitude[d6$country=="India L"] <- 20.593684
	d6$latitude[d6$country=="India U"] <- 20.593684
	d6$latitude[d6$country=="Iran SA"] <- 32.4279
	d6$latitude[d6$country=="Mex CM"] <- 23.6345
	d6$latitude[d6$country=="Nepal B"] <- 28.394857
	d6$latitude[d6$country=="Pak f"] <- 30.3753
	d6$latitude[d6$country=="Pak I"] <- 30.3753
	d6$latitude[d6$country=="Pak N"] <- 30.3753
	d6$latitude[d6$country=="SA B"] <- 23.885942
	
	
	
	
	#script for X1SATYN
	d7 <- X1SATYN
	d7$dataset_id <- dataset_id
	d7$on_farm <- TRUE
	d7$is_survey <- FALSE
	d7$is_experiment <- TRUE
	d7$irrigated <- FALSE
	
	d7$country <- d7$code
	d7$crop <- "wheat"
	d7$yield <- d7$YLD
	d7$rep <- d7$Rep
	
	
	d7$longitude[d7$country=="BGLD J"] <- 90.399452   
	d7$longitude[d7$country=="BGLD J3"] <- 90.399452
	d7$longitude[d7$country=="India D"] <- 78.96288
	d7$longitude[d7$country=="India K"] <- 78.96288
	d7$longitude[d7$country=="India L"] <- 78.96288
	d7$longitude[d7$country=="India V"] <- 78.96288
	d7$longitude[d7$country=="Mex CM"] <- 102.5528
	d7$longitude[d7$country=="Nepal B"] <- 84.124008
	
	#latitude 
	
	##latitude 
	
	
	d7$latitude[d7$country=="BGLD J"] <- 23.777176   
	d7$latitude[d7$country=="BGLD J3"] <- 23.777176 
	d7$latitude[d7$country=="India D"] <- 20.593684
	d7$latitude[d7$country=="India K"] <- 20.593684
	d7$latitude[d7$country=="India L"] <- 20.593684
	d7$latitude[d7$country=="India V"] <- 20.593684
	d7$latitude[d7$country=="Mex CM"] <- 23.6345
	d7$latitude[d7$country=="Nepal B"] <- 28.394857
	
	
	
	
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

