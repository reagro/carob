

carob_script <- function(path){
  
  "
Title: Impact of NPK fertilization on upland rice yield, Nicaragua
  
Description: This dataset contains information of experiments carried out upland rice in two regions of Nicaragua (Caribbean and Pacific Region), as well as a compilation of soils data from different regions in Nicaragua collected during 2019 in seed banks of rice and beans. The experiments were designed to explore the effects of N, P and K in the yield of upland rice. The experiments were carried out on farmer’s field during the 2019 production cycle, the dataset contains yield and aerial biomass of the experiments.


" 
  uri <- "https://doi.org/10.7910/DVN/H0HUSY"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  ## dataset level data 
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication= NA,
    data_citation = "Siles, Pablo; Tellez, Orlando; Peng, Yuan-Ching; Zeledón, Yasser, 2020, Impact of NPK fertilization on upland rice yield, Nicaragua, https://doi.org/10.7910/DVN/H0HUSY",
    data_institutions = "CIAT",
    carob_contributor="Jean-Martial Johnson",
    experiment_type="fertilizer",
    has_weather=FALSE,
    has_management=TRUE)
  
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
  dset$license <- carobiner::get_license(js) 
  
  # processing Rice Data - Caribbean.tab
  f1 <- ff[basename(ff) == "03. Rice Data - Caribbean.xlsx"]
  d1 <- data.frame(readxl::read_xlsx(f1))
  d1$country <- 'Nicaragua'#
#  d1$region <- 'Latin America'#
  d1$adm1 <- d1$Departamento #
  d1$adm1[grep("Region Autonoma de la Costa Caribe Sur",d1$Departamento)] <- 'RAAN'
  d1$adm2 <- d1$Municipio #
  d1$adm3 <- d1$Comunidad #
  d1$longitude <- ifelse(d1$Comunidad == "Montivideo", -84.609,
                         ifelse(d1$Comunidad == "El Panchon", -83.863,
                                ifelse(d1$Comunidad == "La Tortuga", -84.469, -84.312)))
  d1$latitude <- ifelse(d1$Comunidad == "Montivideo", 11.808,
                        ifelse(d1$Comunidad == "El Panchon", 12.323,
                               ifelse(d1$Comunidad == "La Tortuga", 11.999, 12.170)))
  d1$crop <- 'rice' #
  d1$treatment <- d1$ttos #
  d1$N_fertilizer <- d1$N #
  d1$P_fertilizer <- d1$P #
  d1$K_fertilizer <- d1$K #
  d1$yield <- as.numeric(d1$rto_grano_kgha)
  d1$biomass_total <- as.numeric(d1$rto_biom_kgha)
  d1$trial_id <- "Caribbean"
  d1 <- d1[,c("trial_id","country", "adm1","adm2","adm3","longitude","latitude","crop", "rep","treatment","N_fertilizer", "P_fertilizer", "K_fertilizer","yield","biomass_total")]
  
  # processing 04. Rice Data - Pacific.tab
  f2 <- ff[basename(ff) == "04. Rice Data - Pacific.xlsx"]
  d2 <- data.frame(readxl::read_xlsx(f2))
  d2$country <- 'Nicaragua'
#  d2$region <- 'Latin America'
  d2$adm1 <- d2$Departamento 
  d2$adm2 <- d2$Municipio 
  d2$adm3 <- d2$Comunidad 
  d2$location <- d2$Localidad
  d2$longitude <- ifelse(d2$Comunidad == "Rio chiquito", -86.909579,
                         ifelse(d2$Comunidad == "El Ensayo", -87.170,
                                ifelse(d2$Comunidad == "El Tololar", -86.832, -85.764)))
  d2$latitude <- ifelse(d2$Comunidad == "Rio chiquito", 12.318,
                        ifelse(d2$Comunidad == "El Ensayo", 12.587,
                               ifelse(d2$Comunidad == "El Tololar", 12.486, 13.080)))
  d2$treatment <- as.character(d2$ttos)
  d2$crop <- 'rice'
  d2$N_fertilizer <- d1$N
  d2$P_fertilizer <- d1$P
  d2$K_fertilizer <- d1$K
  d2$yield <- as.numeric(d2$rto_grano_kgha)
  d2$biomass_total <- as.numeric(d2$rto_biom_kgha)
  d2$trial_id <- "Pacific"
  d2 <- d2[,c("trial_id","country", "adm1","adm2","adm3","longitude","latitude","crop", "rep","treatment","N_fertilizer", "P_fertilizer", "K_fertilizer","yield","biomass_total")]
  
  # processing 02. Soils Data.xlsx
  f3 <- ff[basename(ff) == "02. Soils Data.xlsx"]
  d3 <- data.frame(readxl::read_xlsx(f3))
  d3$adm1 <- d3$Departamento #
  d3$adm2 <- d3$Municipio#
  d3$adm2[grep("Kuka hill",d1$Municipio)] <- 'Kukrahill'
  d3$adm3 <- d3$Comunidad #
  d3$soil_pH <- as.numeric(d3$pH)
  d3$soil_SOC <- as.numeric(d3$MO)/1.724 # This is a coefficient to convert SOC into OM
  d3$soil_sand <- as.numeric(d3$Arena)
  d3$soil_clay <- as.numeric(d3$Arcilla)
  d3$soil_N <- as.numeric(d3$N)
  d3$soil_P_available <- as.numeric(d3$P)
  d3$soil_K <- (as.numeric(d3$K)*39)*10
  d3 <- d3[,c("adm1","adm2","adm3","soil_pH","soil_SOC","soil_sand","soil_clay","soil_N","soil_P_available","soil_K")]
  
  
  d4 <- merge(x = d1, y = d3, by = c("adm1", "adm2", "adm3"),all.x = TRUE)
  
  d5 <- merge(x = d2, y = d3, by = c("adm1", "adm2", "adm3"),all.x = TRUE)
  
  # compiling into a single final dataset
  f <- carobiner::bindr(d4,d5)
  f$dataset_id <- dataset_id
  f$on_farm <- TRUE
  f$is_survey <- FALSE

	f$rep <- as.integer(f$rep)
	carobiner::write_files(dset, f, path, dataset_id, group)
}

