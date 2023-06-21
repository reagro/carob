# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:B3C0 is the initial cycle of recombination of group B3- Population B. This population has been derived from population A. R genes have been eliminated from this group and horizontal resistance has been retained. Currently, these clones are being used on the variety selection in developing countries. This group of clones were planted in a randomized complete block design (RCBD) with 2 replicates at Comas, located at 2400 to 2788 masl in Junin situated in the Central mountain ranges in Peru. The trials were established at Comas due to the high disease pressure of late blight present in the area and used to screen selected potato genotypes for resistance to this disease. 
"
#Metadata for Carob
	uri <- "doi:10.21223/P3/TNNRYW"
	dataset_id <- carobiner::simple_uri(uri)
	#dataset_id <- agro::get_simple_uri(uri)
	group <- "lateblight"
		## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project=NA,
	   uri=uri,
	   ## if there is a paper, include the paper's doi here
	   ## also add a RIS file in references folder (with matching doi)
	   publication= "",
	   data_institutions = "Centro Internacional de la Papa",
	   carob_contributor="Henry Juarez",
	   
	   ## something like randomized control...
	   experiment_type="___",
	   has_weather=FALSE,
	   has_soil=FALSE,
	   has_management=FALSE
	)

## download and read data 
  path <- ("C:/Users/HJUAREZ/OneDrive - CGIAR/OneCGIAR_EIA/Carob")

	ff  <- carobiner::get_data(uri, path, group)
  ## Major and minor version are important. This case is 1.3
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=3)
	js
	dset$license <- carobiner::get_license(js)
	dset$license

##Read data 
	library(readxl)
	f <- ff[basename(ff) == "PTLB200112_VIENA_B3C0COM02-01.xls"]
		#r <- read.csv(f)
	sheets <- excel_sheets(f)
	d <- readxl::read_excel(path = f, sheet = "Fieldbook") 
	
	g <- ff[basename(ff) == "PTLB200412_VIENA_B3C0COM05-01.xls"]
	#r <- read.csv(f)
	sheets <- excel_sheets(g)
	e <- readxl::read_excel(path = g, sheet = "Fieldbook", range ="A1:P87" ) 
	
	x <- carobiner::bindr(d, e)
	
	
	# Convertir a data.frame:
	x <- as.data.frame(x)
	head(x)
	
	
	
## process file(s)

## use a subset
##	d <- carobiner::change_names(r, from, to)

	
#### about the data #####
## (TRUE/FALSE)

	x$dataset_id <- dataset_id
	x$on_farm <- TRUE
	x$is_survey <-  FALSE
	x$irrigated <- FALSE
## the treatment code	
	d$treatment <- "Late blight pressure to screen  potato genotypes for resistance to this disease"


##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	x$country <- "Peru"
	x$site <- "Viena"
	x$adm1 <- "Junin"
	x$adm2 <- "Concepcion"
	x$adm3 <- "Mariscal Castilla"
	x$elevation <- 2415
## each site must have corresponding longitude and latitude
	x$longitude <- -11.5237
	x$latitude <- -75.1314
	x$rep <- d$REP

##### Crop #####
## normalize variety names
	x$crop <- "potato"
	x$variety <- d$INSTN
	

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format. Date in "yyyy-mm-dd", "yyyy-mm", or "yyyy" format
	drow<-nrow(d)
	erow = nrow(e)
	x= as.data.frame(x)
	start_date = c()
	end_date =c()
	start_date[1:drow] <- rep(as.character(as.Date("2001-12-10")), drow)
	start_date[(drow+1):(drow+erow)] <- as.character(as.Date("2004-12-10"))
	
	end_date [1:drow] <- rep(as.character(as.Date("2002-04-02")), drow)
	end_date[(drow+1):(drow+erow)] <- as.character(as.Date("2005-10-15"))
	
	x <- cbind(x, start_date, end_date)
	
	
##### in general, add comments to your script if computations are
##### based in information gleaned from metadata, publication, 
##### or not immediately obvious for other reasons

##### Yield #####

	x$yield <- d$TTYNA
	x$disease <- "Phytophthora infestans"

##### Pest&Diseases #####	For this dataset, there are 5 evaluations of LB over time as percentage (0-100)`)
		d$date_foliage_affected_01 <- as.character(as.Date("2002-02-03"))
		d$foliage_affected_01 <- d$LB1
		d$date_foliage_affected_02 <- as.character(as.Date("2002-02-13"))
		d$foliage_affected_02 <- d$LB2
		d$date_foliage_affected_03 <- as.character(as.Date("2002-02-21"))
		d$foliage_affected_03 <- d$LB3
		d$date_foliage_affected_04 <- as.character(as.Date("2002-02-28"))
		d$foliage_affected_04 <- d$LB4
		d$date_foliage_affected_05 <- as.character(as.Date("2002-03-07"))
		d$foliage_affected_05 <- d$LB5
	
# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
}
# The group is not part of a list of accepted group!!! This is great

# Luego, carga la biblioteca.
library(writexl)

# Escribe el dataframe en un archivo Excel.
write_xlsx(x, d,  "C:/Users/HJUAREZ/OneDrive - CGIAR/OneCGIAR_EIA/Carob/data/raw/lateblight/doi_10.21223_P3_TNNRYW/mi_archivo.xlsx")


## now test your function in a clean environment 
# path <- _____
# carob_script(path)

