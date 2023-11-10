# R script for "carob"

## ISSUES
# ....
## review by cedric Ngakou

carob_script <- function(path) {

"Description:
Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory validation trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers
"

	uri <- "hdl:11529/10548008"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Gathala, Mahesh K.; Tiwari, Thakur P.; Islam, Saiful; Ghosh, Anup K.; Islam, Rashadul; Anwar, Mazharul; Molla, Samim H.; Akhter-Ul-Alam, Md., 2018, '6.2- Rabi (winter) crops-all nodes- Validation trials -Rangpur-Bangladesh', https://hdl.handle.net/11529/10548008, CIMMYT Research Data & Software Repository Network, V2",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMMYT",
   		data_type="on-farm experiment",
		carob_contributor="Michelle Njukuya",
		carob_date="2023-09-05"
	)

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
	
	bn <- basename(ff)
	##CN
	## Since the first file in the list is structured differently from others, I think it may be good to process sparely.
    
	# process validation trials-all files
    	##sheet names
      l<- c("Rabi-Maize","Rabi-Mustard","Rabi-Wheat","Kharif I-Maize","Boro rice")
      lst <- list()
      
	   for (i in 1:length(l) ){ 
	      
	   r0 <- carobiner::read.excel(ff[bn=="Rabi 2016-17-validation trials-all nodes-Rangpur.xlsx"],sheet =l[i])
	   names(r0)<- make.names(names(r0),unique = TRUE)
	   names(r0)<- gsub("Date.of.sowing..mm.dd.yryr.","Date.of.sowing..mm.dd.yr.",names(r0))
	   d <- r0[,c("Season","Crop","Variety","Tillage","District","Longitude","Latitude","Site.Location.Node","Date.of.sowing..mm.dd.yr.","Dose.of.fertilizer..N.P.K.S.Zn..kg.ha..33.deci","Date.of.harvesting","Straw.yield..t.ha.","Grain.yield..t.ha." ,"Biomass..t.ha.")]
	   colnames(d)<- c("season","crop","variety","tillage","adm1","longitude","latitude","location","planting_date","treatment","harvest_date","residue_yield","yield","biomass_total")
	   
	   d$yield<- d$yield*1000 # in kg/ha
	   d$residue_yield<- d$residue_yield*1000 # in kg/ha
	   d$biomass_total<- d$biomass_total*1000 # in kg/ha
	   d$crop<- carobiner::fix_name(d$crop,"lower")
	   d$trial_id<- paste(d$adm1,d$variety,sep = "_")
	   ## data type
	   d$planting_date <- as.character(d$planting_date)
	   d$harvest_date <- as.character(d$harvest_date)
	   d$row_spacing<- as.numeric(NA) 
	   d$soil_type<- as.character(NA)
	   lst[[i]] <- d
	   }
	 # append all the treatment data
   dd <- do.call(rbind, lst)
## add fertilizer
   fert<- data.frame(treatment=c("60-25-30-20-1","20-15-10-10","30-15-10-10-1","30-15-10","30-20-15"),
                     N_fertilizer=c(60,20,30,30,30),
                     P_fertilizer=c(25,15,15,15,20),
                     K_fertilizer=c(30,10,10,10,15),
                     S_fertilizer=c(20,10,10,0,0),
                     Zn_fertilizer=c(1,0,1,0,0))
dd<- merge(dd,fert,by="treatment",all.x = TRUE)
dd$gypsum<- 0
dd$fertilizer_type<- paste("NPK","ZnSO4",sep = "; ")
# fix crop name 
##fix lon and lat
dd$k<- dd$latitude
j<- grepl(25,dd$longitude)
 dd$latitude[j]<- dd$longitude[j]
 dd$longitude[j]<- dd$k[j]
 dd$k<- NULL


#### process files with DS&BP-all
ff<- ff[grepl("DS&BP-all",ff)]
##CN
## the structure of these files is the same as in # hdl_11529_10548007.R, the same function is used with a slight modification.  
proc <- function(f) {
   r1 <- carobiner::read.excel.hdr(f, sheet ="2 - Site information", skip=5, hdr=3)
   d1 <- data.frame(
      season=r1$Season, location=r1$Node, 
      trial_id=r1$Site.No.Unique.farmer.ID,
      treatment=r1$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
      soil_type=r1$Soil.texture.sand.silt.clay.etc)
   
   r2 <- carobiner::read.excel.hdr(f, sheet ="4- Stand counts & Phenology", skip=4, hdr=2)
   d2 <- data.frame(
      treatment=r2$Tmnt, trial_id=r2$Site.No, 
      variety=r2$Variety,
      row_spacing=r2$Row.spacing.cm,
      crop=tolower(r2$Types.of.Trial),
      planting_date=as.character(r2$Date.of.seeding.dd.mm.yy))
   if (!is.null(r2$Datw.of.harvest.dd.mm.yy)) {
      d2$harvest_date<- as.character(r2$Datw.of.harvest.dd.mm.yy)
   } else { 
      d2$harvest_date<- as.character(r2$Date.of.harvest.dd.mm.yy)
   }
      
   
   r3 <- carobiner::read.excel.hdr(f, sheet ="6 - Fertilizer amounts ", skip=4, hdr=2)
   
   d3 <- data.frame(
      treatment=r3$Tmnt, trial_id=r3$Site.No,
      N_fertilizer=r3$N.kg.ha, 
      P_fertilizer=r3$P2O5.kg.ha / 2.29, #!
      gypsum=r3$Gypsum.Kg.ha,
      Zn_fertilizer=r3$ZnSO4.kg.ha) 
   
   if (!is.null(r3$K2O.kg.ha)) {
      d3$K_fertilizer <- r3$K2O.kg.ha / 1.2051
   } else { # assuming this is also K2O
      d3$K_fertilizer=r3$K.kg.ha / 1.2051
   }
   
   d3$fertilizer_type <- apply(r3[, grep("Application_Product.used", names(r3))], 1, 
                               \(i) paste(unique(i), collapse="; "))
   
   
   
   r4 <- carobiner::read.excel.hdr(f, sheet ="14 - Grain Harvest ", skip=4, hdr=2)	
   colnames(r4) <- gsub("Calculation_", "", colnames(r4))
   d4 <- data.frame(
      treatment=r4$Tmnt, trial_id=r4$Site.No, 
      yield=r4$Grain.yield.t.ha * 1000,
      residue_yield=r4$Straw.yield.t.ha * 1000,
      biomass_total=r4$Biomass.t.ha * 1000)
   
   ## merge all 
   dd <- merge(d1, d2, by=c("treatment", "trial_id"), all.x=TRUE)
   dd <- merge(dd, d3, by=c("treatment", "trial_id"), all.x=TRUE)
   dd <- merge(dd, d4, by=c("treatment", "trial_id"), all.x=TRUE)
   dd
}
d <- lapply(ff, proc)
d<- do.call(rbind, d)
## fix fertilizer_type
d$fertilizer_type <- gsub("MOP", "KCl", d$fertilizer_type)
d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
d$fertilizer_type <- gsub("Gypsum", "gypsum", d$fertilizer_type)
d$fertilizer_type <- gsub("NA", "unknown", d$fertilizer_type)
#fix location names
p<- carobiner::fix_name(d$location)
p<- gsub("kolkondo","Kolkondo",p)
d$location<- p
d$adm1<- "Rangpur"
d$tillage <- d$treatment
d$S_fertilizer<- 0
geo= data.frame(location=c("Kolkondo", "Lakkhhitari", "Mohanpur" , "Lakkhatari", "Borodargha", "Durgapur","Lakkhiitari","Mohonpur" ), 
                latitude=c(25.63761,25.6376135,24.5474589,25.6376135,25.48355,25.55458,25.6376135,23.7750467), 
                longitude=c(89.08264,89.0826381,88.6475568,89.0826381,89.28012,89.29208,89.0826381,90.3606871))

d <- merge(d, geo, by="location", all.x = TRUE) 

### append both data d and dd

df<- rbind(d,dd)

##add columns
df$dataset_id<- dataset_id
df$on_farm <- TRUE
df$is_survey <- FALSE
df$irrigated <- TRUE
df$country<- "Bangladesh"
df$yield_part <- "grain"
df$yield_part[df$crop=="Mustard"]<- "stems"



	  	# all scripts must end like this
	carobiner::write_files(dset, df, path=path)
}
