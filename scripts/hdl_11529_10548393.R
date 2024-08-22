# R script for "carob"



carob_script <- function(path) {

"
Replicated crop-cuts from farmers' fields at harvest at multiple locations in Tanzania (180 to 2400 m), and Soil analysis from 0-20 cm and 20-50 cm depths from crop-cut fields in Southern Highlands, Eastern and Northern Zones of Tanzania in 2015.
"


	uri <- "hdl:11529/10548393"
	group <- "agronomy"

 
	ff  <- carobiner::get_data(uri, path, group)


	meta <- data.frame(
		
		carobiner::read_metadata(uri, path, group, major=2, minor=0),
		data_institute = "CIMMYT",
		
		publication = NA,
		project = NA,

		data_type = "experiment",
	
		treatment_vars = "longitude;latitude;adm2",
		response_vars = "yield", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-08-22",
		notes = NA
	)
	
 

	f <- ff[basename(ff) == "TAMASA_TZ_CC_Soil_2015 (Blurred).xlsx"]
	f1<- ff[basename(ff) == "TAMASA_TZ_CC_Yield_2015 (Blurred).xlsx"]
  r <- carobiner::read.excel(f,sheet ="Data")
  r1 <- carobiner::read.excel(f1,sheet ="Data")
  r <- head(r, -8)
  r1 <- head(r1, -7)
  


	d1 <- data.frame(
		country = r$Country,
		adm1=r$Region,
		adm2=r$District,
		adm3=r$Ward,
		adm4=r$Village,
		trial_id=r$SSID,
		soil_C=r$C,
		soil_pH=r$pH,
		soil_Al=r$Al,
		soil_Ca= r$Ca,
		soil_P_Mehlich= r$P,
		soil_S = r$S,
		soil_Mn = r$Mn,
		soil_Zn= r$Zn,
		soil_K = r$K,
		soil_Na = r$Na,
		soil_Mg = r$Mg,
		soil_Fe = r$Fe,
		soil_B= r$B,
		soil_N =r$N,
		soil_EC =r$EC.S,
		cob_density=NA,
		fwy_total=NA,
		dmy_total=NA,
		yield=NA,
		Date= r$Date)

	
	i <- grep('^4', d1$Date)
	as.Date(as.numeric(d1$Date[i]), origin = '1900-01-01')
	dd<- as.character(as.Date(as.numeric(d1$Date[i]), origin = '1900-01-01'))
	d1$Date[i]<-dd
	d1$Date[-i]
	dp<- as.character(as.Date(d1$Date[-i], format = '%d/%m/%Y'))
	d1$Date[-i]<-dp
	
	
	d2 <- data.frame(
		
	  country = r1$Country,
		adm1=r1$Region,
		adm2=r1$District,
		adm3=r1$Ward,
		adm4=r1$Village,
		trial_id=r1$SSID,
		soil_C=NA,
		soil_pH=NA,
    soil_Al=NA,
		soil_Ca=NA,       
		soil_P_Mehlich=NA,
		soil_S=NA,      
    soil_Mn=NA,       
		soil_Zn=NA,      
		soil_K=NA,
		soil_Na=NA,      
    soil_Mg= NA,
		soil_Fe = NA,
		soil_B= NA,
		soil_N = NA,    
    soil_EC= NA,
		cob_density = r1$`Number of Cobs`,
		fwy_total = r1$`FWt of Cobs_all (kg)`,
		dmy_total = r1$`Grain dry weight (kg/25m2 @12.5%)`,
		yield= r1$`Grain yield (kg/ha@12.5%)`,
		Date=r1$`sampling Date`)
	
		Date<-	as.Date(as.numeric(d1$Date), origin = '1900-01-01')

	  d<-rbind(d1,d2)
	  
   
   q <-unique(d[,c("country","adm2")])
   
   geocode <- data.frame (adm2 = c("Kilolo", "Mufindi", "Ileje", "Mbeya Rural", "Makete", "Njombe", 
                                       "Sumbawanga", "Namtumbo", "Mbozi", "Momba", "Handeni", 
                                        "Kilindi", "Lushoto", "Muheza", "Tanga"), 
                          longitude = c(36.3134, 35.2783, 33.3101, 33.2403, 34.1295, 34.7702, 31.6232, 
                                        36.1902, 32.9516, 32.3014, 38.4074, 37.6133, 38.448, 38.7731, 38.2922),
                          latitude = c(-7.7859, -8.5742, -9.3921,-9.2648, -9.2747, -9.6026, -7.9524, -10.6729, -8.9742, 
                                       -8.7905, -5.4948, -5.5042, -4.4953, -5.1815, -5.0665))
    
    d <- merge(d, geocode, by="adm2", all.x = TRUE)
    
    d$longitude[d$adm2 =="Iringa Rural"]<- 35.699120
    d$latitude[d$adm2 =="Iringa Rural"]<- -7.773094
   
    d$longitude[d$adm2 =="Ludewa"]<- 34.68012119999999
    d$latitude[d$adm2 =="Ludewa"]<- -10.1130361
          
          
     d$longitude[d$adm2 =="Wangingombe"]<- 34.5485200
     d$latitude[d$adm2 =="Wangingombe"]<- -9.0205400
    
     
     d$longitude[d$adm2 =="Songea Rural"]<- 35.655785
     d$latitude[d$adm2 =="Songea Rural"]<- -10.676803
    
     d$longitude[d$adm2 =="Songea Urban"]<- 35.6205
     d$latitude[d$adm2 =="Songea Urban"]<- -10.6570
     
     d$longitude[d$adm2 =="Korogwe"]<- 38.4192
     d$latitude[d$adm2 =="Korogwe"]<- -5.0827
            
     d$longitude[d$adm2 =="Arumeru"]<- 36.837243
     d$latitude[d$adm2 =="Arumeru"]<- -3.359298
     
     d$longitude[d$adm2 =="Karatu"]<- 35.65811200
     d$latitude[d$adm2 =="Karatu"]<- -3.33860200
     
     d$longitude[d$adm2 =="Moshi Rural"]<- 37.340382
     d$latitude[d$adm2 =="Moshi Rural"]<- -3.334883
     
     d$longitude[d$adm2 =="Babati"]<- 35.75
     d$latitude[d$adm2 =="Babati"]<- -4.21667
     
     d$longitude[d$adm2 =="Hai"]<- 37.16873390
     d$latitude[d$adm2 =="Hai"]<- -3.14705320
     
     d$longitude[d$adm2 =="Siha"]<- 37.35066580
     d$latitude[d$adm2 =="Siha"]<- -3.06646480
     
	  
	   
	   
     d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	d$geo_from_source <- FALSE



	d$planting_date <- as.character(as.Date( 2015  ))
	
   

  d$crop <- "maize"
	d$yield_part <- "grain"
	

# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

