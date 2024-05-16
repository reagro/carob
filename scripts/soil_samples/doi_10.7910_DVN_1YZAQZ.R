# R script for "carob"

## only processing the soil samples. The omission trials are extracted from different datasets 

carob_script <- function(path) {
  
"This was Task 1.2. “Soil survey to characterize two sentinel sites” as part of Work Package 1 “Identification of the key biophysical production constraints to crops and livestock at farm and landscape levels” The aim of this task was to conduct an assessment of soil and land health in the Babati district in Tanzania covering the action villages of the USAID- AfricaRISING project. The Land Degradation Surveillance Framework (LDSF) was employed to conduct a systematic biophysical baseline of key land and soil health metrics, which was to be combined with agronomic survey data collected as part of the wider project. The LDSF is designed to provide a biophysical baseline at landscape level, and a monitoring and evaluation framework for assessing processes of land degradation and the effectiveness of rehabilitation measures (recovery) over time. Babati Agricultural District Office and Tanzania National Agricultural Research System as well as Sokoine University of Agriculture (SUA) staff were involved in two one-week trainings in the LDSF methodology at the Long and Matufa LDSF sites. Those trained include: Majid Suleiman (District Agricultural Officer), Edgar Wakurwa (District Agronomist), Mbwambo (Extension Officer), Prosper Massawe from Selian Agricultural Research Institute and Boniface Massawe, from SUA. George Sayula of SARI also helped coordinate the field efforts. Local farmers were also involved in the surveys and farmer knowledge on management practices and land use history was incorporated into the LDSF database. (2015-09-10)"
  
	uri <- "doi:10.7910/DVN/1YZAQZ"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=2),
		data_institutions = "CIAT",
		publication= "doi:10.1007/s1070",
		project="Africa Rising",
		data_type= "survey",
		carob_contributor= "Andrew Sila",
		carob_date="2024-05-16"
	)
  
	f1 <- ff[basename(ff) == "02. Mafuta_Crop Nutrition Results for dataverse.xlsx"]
	r1 <- carobiner::read.excel(f1)
	f2 <- ff[basename(ff) == "03. Long Crop Nutrition Results for dataverse.xlsx"]
	r2 <- carobiner::read.excel(f2)
	f3 <- ff[basename(ff) == "04. Matufa LDPSA and CN for dataverse.xlsx"]
	r3 <- carobiner::read.excel(f3)
	f4 <- ff[basename(ff) == "05. Long LDPSA and CN for dataverse.xlsx"]
	r4 <- carobiner::read.excel(f4)
	f5 <- ff[basename(ff) == "07. Matufa LDSF data raw for dataverse.csv"]
	r5 <- read.csv(f5)
	f6 <- ff[basename(ff) == "08. Long_LDSF_data raw for dataverse .csv"]
	r6 <- read.csv(f6)
	
	r2 <- r2[-c(1:8),]
r2names1<- as.vector(t(r2[1,]))
r2names1[is.na(r2names1)==TRUE] <- as.vector(t(r2[2,1:10]))
colnames(r2) <- r2names1
r2 <- r2[-c(1:2),]
# drop CIAT Lab ID1 column
r2 <- r2[,-1]
colnames(r1) <- colnames(r2)

#row bind r3 and r4 to get CN and PSA data
# drop CIAT Lab ID1 column
r4 <- r4[,c('CIAT Lab ID1', 'Selian_SSN', 'ICRAF_SSN', 'Clay (%)', 'Silt (%)', 'Sand (%)', 'Carbon Content (%)', 'Nitrogen Content (%)')]
r3 <- r3[,c('CIAT lab ID', 'Selian_SSN', 'ICRAF_SSN', 'Clay (%)', 'Silt (%)', 'Sand (%)', 'Carbon Content (%)', 'Nitrogen Content (%)')]

colnames(r3) <- colnames(r4)

cnls<- rbind(r1,r2)
cnpsa <- rbind(r3,r4)
r <- merge(cnls, cnpsa, by.x = "Selian SSN", by.y = 'Selian_SSN')
r$Site[r$Site=="Mafuta"] <- "Matufa"
# create a new column for trial id
r$trial_id <- paste0(r$Site, '.', r$Cluster, '.', r$Plot)

# Get lat and lon from r5 and r6
ldsf <- rbind(r5[,c('Site','Cluster', 'Plot', 'Latitude', 'Longitude')], r6[,c('Site','Cluster', 'Plot', 'Latitude', 'Longitude')])
# create a new column for trial id
ldsf$trial_id <- paste0(ldsf$Site, '.', ldsf$Cluster, '.', ldsf$Plot)

r <- merge(r, ldsf[,c('trial_id', 'Latitude', 'Longitude')])

r <- data.frame(r)
	d <- data.frame(
		trial_id = r$trial_id,
		soil_pH = as.numeric(r$pH),
		soil_EC = as.numeric(r$EC.S.)/1000,
		soil_Al = as.numeric(r$Al),
		soil_B = as.numeric(r$B),
		soil_Ca = as.numeric(r$Ca),
		soil_Cu = as.numeric(r$Cu),
		soil_Fe = as.numeric(r$Fe),
		soil_K = as.numeric(r$K),
		soil_Mg = as.numeric(r$Mg),
		soil_Mn = as.numeric(r$Mn),
		soil_Na = as.numeric(r$Na),
		soil_P_total = as.numeric(r$P),
		soil_S = as.numeric(r$S),
		soil_Zn = as.numeric(r$Zn),
		soil_Ex_acidity = as.numeric(r$Hp),
		soil_PSI = as.numeric(r$PSI),
		soil_clay = as.numeric(r$Clay....),
		soil_silt = as.numeric(r$Silt....),
		soil_sand = as.numeric(r$Sand....),
		soil_C = r$Carbon.Content....,
		soil_N = r$Nitrogen.Content....,
		country = r$Country,
		location = r$Site,
		longitude = r$Longitude,
		latitude = r$Latitude
	)
	# d$soil_PSI <- ifelse(d$soil_PSI < 0, 0,d$soil_PSI)
	#  
	# d$latitude[d$latitude == 0] <- NA
	# d$longitude[d$longitude == 0] <- NA
	# d$longitude[d$location == "Finkolo"] <- -5.5113
	# d$latitude[d$location == "Finkolo"] <- 11.2692
	# 

	##miss <- unique(d[is.na(d$latitude), c("country", "location")])
	##geo <- carobiner::geocode(miss$country, miss$location)
	##geo$put

    carobiner::write_files(path, dset, d)
}
