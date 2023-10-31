

carob_script <- function(path) {
   
   "
	Description:
	Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community.
	Likewise, farmer-participatory validation trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.

"
   
   uri <-  "hdl.handle.net/11529/10548008"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "fertilizer" 
   ## dataset level data 
   dset <- data.frame(
      dataset_id = dataset_id,
      group=group,
      uri=uri,
      publication= NA,# 
      data_citation ="athala, Mahesh K.; Tiwari, Thakur P.; Islam, Saiful; Ghosh, Anup K.; Islam, Rashadul; Anwar, Mazharul; Molla, Samim H.; Akhter-Ul-Alam, Md., 2018, 6.2- Rabi (winter) crops-all nodes- Validation trials -Rangpur-Bangladesh,
      https://hdl.handle.net/11529/10548008, CIMMYT Research Data & Software Repository Network, V2",
      
      data_institutions = "CIMMYT",
      carob_contributor="Cedric Ngakou",
      carob_date="2023-10-18",
      data_type="experiment",
      project=NA 
   )
   
   ## download and read data 
   ff <- carobiner::get_data(uri, path, group)
   js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
   dset$license <- carobiner::get_license(js)
   
   bn <- basename(ff)
   
   # read and process files

   name <- c("Season","Crop","Variety","District","Latitude","Longitude","Site.Location.Node","Date.of.sowing..mm.dd.yr.","Date.of.harvesting","Dose.of.fertilizer..N.P.K.S.Zn..kg.ha..33.deci","Tillage","Grain.yield..t.ha.","Biomass..t.ha.")
   
   Newname <- c("season","crop","variety","adm1","latitude","longitude","site","planting_date","harvest_date","treatment","tillage","yield","biomass_total")
   
   lst <- list()
   for (i in 2:7) {
      
      r <- readxl::read_excel(ff[bn=="Rabi 2016-17-validation trials-all nodes-Rangpur.xlsx"],sheet=i) |> as.data.frame()
      names(r) <- make.names(names(r))
      names(r) <- gsub("Date.of.sowing..mm.dd.yryr.","Date.of.sowing..mm.dd.yr.",names(r))
      names(r) <- gsub("Jute.fibre.yield..t.ha.","Grain.yield..t.ha.",names(r)) 
      ri <- r[,name]
      colnames(ri) <- Newname
      lst[[i]] <- ri
   }	
   
   # append all the data
   d <- do.call(rbind, lst)
   
   d$yield <- d$yield*1000 ## in kg/ha
   d$biomass_total <- d$biomass_total*1000 ## in kg/ha
   ## add columns
   d$country <- "Bangladesh"
   d$dataset_id <- dataset_id
   d$trial_id <- paste(d$adm1,d$dataset_id,sep = "-")
   d$yield_part <- "grain" 
   d$yield_part[d$crop=="mustard"] <- "seed" ## not sure it can also be leaves
   d$on_farm <- TRUE
   d$irrigated <- TRUE
   d$inoculated <- FALSE
   d$is_survey <- FALSE

  ### add fertilizer
   d$N_fertilizer <- 0  
   d$P_fertilizer <- 0
   d$K_fertilizer <- 0
   i <- grepl("20-15-10-10",d$treatment)
   d$N_fertilizer[i] <- 20 
   d$P_fertilizer[i] <- 15/2.29
   d$K_fertilizer[i] <- 10/1.2051
   
   i <- grepl("30-15-10-10-1",d$treatment)
   d$N_fertilizer[i] <- 30 
   d$P_fertilizer[i] <- 15/2.29
   d$K_fertilizer[i] <- 10/1.2051
   
   i <- grepl("60-25-30-20-1",d$treatment)
   d$N_fertilizer[i] <- 60 
   d$P_fertilizer[i] <- 25/2.29
   d$K_fertilizer[i] <- 30/1.2051
   
   
   i <- grepl("20-15-10",d$treatment)
   d$N_fertilizer[i] <- 20 
   d$P_fertilizer[i] <- 15/2.29
   d$K_fertilizer[i] <- 10/1.2051
   
   i <- grepl("30-15-10",d$treatment)
   d$N_fertilizer[i] <- 30 
   d$P_fertilizer[i] <- 15/2.29
   d$K_fertilizer[i] <- 10/1.2051
   
   i <- grepl("30-20-15",d$treatment)
   d$N_fertilizer[i] <- 30 
   d$P_fertilizer[i] <- 20/2.29
   d$K_fertilizer[i] <- 15/1.2051
   ## fix crop name
   p <- carobiner::fix_name(d$crop,"lower")
   d$crop <- p
   # fix long and lat
   d$latitude <- ifelse(grepl("Rangpur",d$adm1),25.6376135,25.6260712)
   d$longitude <- ifelse(grepl("Rangpur",d$adm1),89.0826381,88.6346228)
   
   #data type
   d$harvest_date <- as.character(d$harvest_date)
   d$planting_date <- as.character(d$planting_date)
   
   # all scripts must end like this
   carobiner::write_files(dset, d, path=path)
   
}

