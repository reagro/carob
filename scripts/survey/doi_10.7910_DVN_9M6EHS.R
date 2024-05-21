## ISSUES

carob_script <- function(path) { 
  
"The Rural Household Multiple Indicator Survey (RHoMIS) is a standardized farm household survey approach which collects information on 753 variables covering household demographics, farm area, crops grown and their production, livestock holdings and their production, agricultural product use and variables underlying standard socio-economic and food security indicators like the Poverty Probability Index, the Household Food Insecurity Access Scale and dietary diversity. These variables are used to quantify more than 40 different aggregate indicators on farm household characteristics, welfare, productivity and economic performance. Between 2015 and the beginning of 2018, the survey instrument has been applied in 21 countries in Central America, sub-Saharan Africa and Asia. The data presented here cover the raw data, the indicator calculation code and the resulting indicator values, and can be used to quantify on- and off-farm pathways to food security, diverse diets and reduced poverty of rural smallholder farm households. (2019-10-31)"

	uri <- "doi:10.7910/DVN/9M6EHS"
	group <- "survey"

	ff <- carobiner::get_data(uri, path, group)


	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=3, minor=0),
		project="RHoMIS",
		publication= "doi:10.1038/s41597-020-0388-8",
		data_institute = "ILRI",
		data_type="survey", 
		treatment_vars = "none",
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-29"
	)
  
  
	f <- ff[basename(ff) == "RHoMIS_Full_Data.csv"]
	r <- read.csv(f, encoding="latin1", na.strings=c("NA", "", "na"))

	d <- data.frame(
		trial_id=as.character(NA),
		planting_date=as.character(NA),
		longitude = r$GPS_LON,
		latitude = r$GPS_LAT,
		elevation = r$GPS_ALT,
		adm1 = carobiner::fix_name(r$region, case="title"),
		location = r$sublocation,
		date = as.character(r$YEAR),	
		farmland_owned = r$landowned,
		farmland_rentedin = r$landrentin,
		farmland_rentedout= r$landrentout,
		cropland_used = r$landcultivated,
		land_irrigated = r$land_irrigated,
		land_ownedby = r$land_ownership,
		land_tenure = r$land_tenure
	)
	
	cdf <- data.frame(
		code= c("TZ", "GT", "SV", "HN", "ML", "BF", "MW", "KE", "IN", "KH", "VN", "LA", "ET", "CD", "ZM", "GH", "UG", "CR", "BI", "PE", "NI"),
		country = c("Tanzania", "Guatemala", "El Salvador", "Honduras", "Mali", "Burkina Faso", "Malawi", "Kenya", "India", "Cambodia", "Vietnam", "Laos", "Ethiopia", "Democratic Republic of the Congo", "Zambia", "Ghana", "Uganda", "Costa Rica", "Burundi", "Peru", "Nicaragua")
	)

	d$country <- cdf$country[match(r$ID_COUNTRY, cdf$code)]
	carobiner::write_files(dset, d, path=path)
}

