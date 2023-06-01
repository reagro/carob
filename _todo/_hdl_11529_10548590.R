# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    [copy the abstract from the repo]

"

	uri <- "hdl:11529/10548590"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "pests"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project=NA,
	   uri=uri,
	   ## if there is a paper, include the paper's doi here
	   ## also add a RIS file in references folder (with matching doi)
	   publication= "",
	   data_institutions = "",
	   carob_contributor="Your name",
	   
	   ## something like randomized control...
	   experiment_type="___",
	   has_weather=FALSE,
	   has_soil=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=4, minor=0)
	dset$license <- carobiner::get_license(js)

	raw.data <- ff[basename(ff) == "52ND IDYN_RawData.xls"]
	loc.data <- ff[basename(ff) == "52ND IDYN_Loc_data.xls"]
	env.data <- ff[basename(ff) == "52ND IDYN_EnvData.xls"]
	gen.data <- ff[basename(ff) == "52ND IDYN_Genotypes_Data.xls"]
	
	## Read data referenced by the above pathnames
	d <- read.table(raw.data, comment.char="", sep="\t", header=TRUE)
	loc <- read.table(loc.data, sep = "\t", header=TRUE)
	env <- read.csv(env.data, sep = "\t")
	gen <- read.csv(gen.data, sep = "\t")
	colnames(gen) <- c()
	
	## process file(s)
	
	# Sub-setting relevant columns and reformatting dataset to "wide" for easier handling
	d$country <- tools::toTitleCase(tolower(as.character(d$Country)))
	d$location <- gsub("\\ -.*","",d$Loc_desc)
	d$site <- merge(d,loc, by = c("Loc_no"))[,"Loc..Description"]
	d$trial_id <- d$Trial.name
	d <- d[,c("country", "location", "site", "trial_id", "Loc_no", "Rep", "Sub_block", "Plot", "Gen_name", "Trait.name", "Value")]
	d <- reshape(d, idvar = c("country", "location", "site", "trial_id", "Loc_no", "Rep", "Sub_block", "Plot", "Gen_name"), timevar = "Trait.name", direction = "wide")
	colnames(d)[10:35] <- gsub(".*Value.", "", colnames(d)[10:35])

#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	# d$irrigated <- FALSE
## the treatment code	
	# d$treatment <- NA


# ##### Location #####
# ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
# ## you can use carobiner::fix_name()
	loc$latitude <- loc$Lat_degress + loc$Lat_minutes / 60 
	loc$longitude <- loc$Long_degress + loc$Long_minutes / 60 
	loc$longitude <- ifelse(loc$Longitud == "W", -loc$longitude, loc$longitude)
	d <- merge(d, loc[, c("Loc_no", "longitude", "latitude", "Altitude")], by ="Loc_no", all.x = T)

##### Crop #####
## normalize variety names
	d$crop <- "wheat"
	d$variety <- d$Gen_name

# ##### Time #####
# ## time can be year (four characters), year-month (7 characters) or date (10 characters).
# ## use 	as.character(as.Date()) for dates to assure the correct format.
	m <- merge(d,env, by = c("Loc_no"), all.x = TRUE)[,c("Loc_no", "Rep", "Sub_block", "Plot", "Gen_name", "Trait.name","Value")]
	dd <- reshape(m, idvar = c("Loc_no", "Rep", "Sub_block", "Plot", "Gen_name"), timevar = "Trait.name", direction = "wide")
	colnames(dd)[6:75] <- gsub(".*Value.", "", colnames(dd)[6:75])
	# Re-join d <-> dd
	ddd <- merge(d,dd, by = c("Loc_no", "Rep", "Sub_block", "Plot", "Gen_name"), all.x = TRUE)
	ddd$start_date <- as.character(as.Date(ddd$SOWING_DATE, "%b %d %Y"))
	ddd$end_date  <- as.character(as.Date(ddd$HARVEST_FINISHING_DATE, "%b %d %Y"))

# ##### Fertilizers #####
# ## note that we use P and K, not P2O5 and K2O
# ## P <- P2O5 / 2.29
# ## K <- K2O / 1.2051
#    d$P_fertilizer <- 
#    d$K_fertilizer <-
#    d$N_fertilizer <- 
# ## normalize names 
#    d$fertlizer_type <- 
#    d$inoculated <- 
   

##### in general, add comments to your script if computations are
##### based in information gleaned from metadata, publication, 
##### or not immediately obvious for other reasons

##### Yield #####

	ddd$yield <- as.numeric(ddd$GRAIN_YIELD)*1000

	# ddd$fertilizer <- as.factor(ifelse(ddd$FERTILIZER_APPLIED == "YES", "YES", "NO"))
	ddd$disease <- NA
	ddd[which(!is.na(ddd$LEAF_RUST) | ddd$LEAF_RUST != "-" | !is.na(ddd$STEM_RUST) | ddd$STEM_RUST != "-" | !is.na(ddd$STRIPE_RUST_ON_LEAF) | ddd$STRIPE_RUST_ON_LEAF != "-"), c("disease")] <- "rust"
	ddd[which(!is.na(ddd$SEPTORIA_SPECIES) | ddd$SEPTORIA_SPECIES != "-"), c("disease")] <- "septoria"
	ddd[which(!is.na(ddd$FUSARIUM_SPP) | ddd$FUSARIUM_SPP != "-"), c("disease")] <- "fusarium"
	ddd[which(!is.na(ddd$POWDERY_MILDEW) | ddd$POWDERY_MILDEW != "-"), c("disease")] <- "mildew"
	ddd[which(!is.na(ddd$OTHER_UNIDENTIFIED_FOLIAR_BLIGHTS) | ddd$OTHER_UNIDENTIFIED_FOLIAR_BLIGHTS != "-"), c("disease")] <- "blight"
	
	
	pd <- ddd[, c("dataset_id","longitude","latitude","start_date", "end_date","crop","disease","yield")]
	pd <- pd[!is.na(test$yield),]
	pd <- pd[!duplicated(pd),]
	pd$disease <- as.factor(ifelse(is.na(pd$disease), "FALSE", pd$disease))
	# boxplot(yield ~ disease, pd)
	
	
	# model1 <- lm(yield ~ disease, data = dis) 
	# summary(model1)$coefficients
	
# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
}

## now test your function in a clean environment 
# path <- _____
# carob_script(path)

