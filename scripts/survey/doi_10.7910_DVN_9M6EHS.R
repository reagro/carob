## ISSUES


carob_script <- function(path) { 
  
  "
The Rural Household Multiple Indicator Survey (RHoMIS) is a standardized farm household survey approach which collects information on 753 variables covering household demographics, farm area, crops grown and their production, livestock holdings and their production, agricultural product use and variables underlying standard socio-economic and food security indicators like the Poverty Probability Index, the Household Food Insecurity Access Scale and dietary diversity. These variables are used to quantify more than 40 different aggregate indicators on farm household characteristics, welfare, productivity and economic performance. Between 2015 and the beginning of 2018, the survey instrument has been applied in 21 countries in Central America, sub-Saharan Africa and Asia. The data presented here cover the raw data, the indicator calculation code and the resulting indicator values, and can be used to quantify on- and off-farm pathways to food security, diverse diets and reduced poverty of rural smallholder farm households. (2019-10-31)"

	uri <- "doi:10.7910/DVN/9M6EHS"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "survey"

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=0)

	## dataset level data 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		project="RHoMIS",
		data_citation="-, RHoMIS, 2019. The Rural Household Multiple Indicator Survey (RHoMIS) data of 13,310 farm households in 21 countries. https://doi.org/10.7910/DVN/9M6EHS, Harvard Dataverse, V3, UNF:6:ZnVw+OfivEtIKcZWC9iwgw== [fileUNF]",
		publication= "doi:10.1038/s41597-020-0388-8",
		data_institutions = "ILRI",
		data_type="survey", 
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-29"
	)
  
  
	f <- ff[basename(ff) == "RHoMIS_Full_Data.csv"]
	r <- read.csv(f)
	
	# farmland
	# farmland_owned
	# farmland_rentedin
	# farmland_rentedout
	# cropland_used   # currently used, excluding fallow
    # cropland_total  # all cropland, including fallow
	# cropland_used_owned
    # cropland_total
	# communal_land
	# permanent_grassland
	# 


	
	d <- r[, c("GPS_LON", "GPS_LAT", "GPS_LON", "land_irrigated", "land_ownership", "land_slope", "land_tenure", "landcultivated", "landowned", "landrentin", "landrentout")]



#	d <- data.frame(
#		country = r$COUNTRY,
#		longitude = r$GPS_LON,
#		latitude = r$GPS_LAT,
#		elevation = r$GPS_ALT,
#		adm1 = r$region
#	)
		
	return(TRUE)	
}

