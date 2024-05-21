# R script for "carob"


carob_script <- function(path) {

"

    [We investigated the effect of bed width on grain yield under irrigated and rainfed conditions, for crops grown on permanent beds, where the top of the raised beds is not tilled. The study included nine sites in Central Mexico, where wide and narrow permanent beds were compared at the same site for at least three consecutive crop cycles. Six trials were selected under rainfed conditions in the states of Queretaro, Guanajuato, Michoacan, and State of Mexico, and three more with irrigation, which were located in Guanajuato and Queretaro. The data were collected in different periods, from 2007 to 2019. The database contains yield data for maize (Zea mays L.), wheat (Trititcum aestivum L.) and barley (Hordeum vulgare L.) planted on wide and narrow permanent beds.]

"
	uri <- "hdl:11529/10548609"
	group <- "conservation_agriculture"

	ff <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		#data_citation="Verhulst, Nele; Saldivia Tejeda, Abel; Guan, Taiyu; Fonteyne, Simon, 2021, Yield of maize, wheat and barley planted on wide and narrow permanent beds, under irrigated and rainfed conditions in Mexico, https://hdl.handle.net/11529/10548609, CIMMYT Research Data & Software Repository Network, V1, UNF:6:bMwiJ4W0pL9LG8olmN3meA== [fileUNF]",
		data_institute = "CIMMYT",
		publication=NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Blessing Dzuda",
		carob_date="2024-03-14"
	)
	
	f <- ff[basename(ff) == "DAT-BedWidth-2021-07.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Data")

	d <- data.frame(
		crop = tolower(r$Crop),
		country = "Mexico",
		adm1 = r$State,
		adm2 = r$Municipality,
		site = r$Site_experiment,
		planting_date = as.character(r$Year),
		yield_part = "grain",
		yield = r$Yield_moist * 1000,
		rep = as.integer(r$Num_Rep),
		irrigated = r$Water_Regime
	)
	
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	  
#	  g <-unique(d[,c("country","adm2")])
#	  g1<-carobiner::geocode(country = g$country,location = g$adm2,service = "nominatim")
	gg <- data.frame(
	    adm2 = c("Cadereyta", "Indaparapeo", "San Juan del Rio I", 
	                 "Apaseo el Alto", "Irapuato", "San Juan del Rio III",
	                 "Texcoco", "Penjamo"),
	    longitude=c(-99.666, -100.9429, -99.9879, -100.5922, -101.3885, 
	                -99.9643, -98.8554, -101.8405),
	    latitude=c(20.7912, 19.7602, 20.3756, 20.4326, 20.6749, 
	               20.428, 19.4756, 20.4055))
	
	d <- merge(d, gg, by="adm2", all.x=TRUE)
	d$trial_id <- as.character(as.factor(d$adm2))
	  
	carobiner::write_files(dset, d, path=path)
}

