

carob_script <- function(path) {
   
"Landraces of maize (Zea mays L.) are invaluable sources of genetic variability for improving agronomic traits, and they hold great promise in developing new maize varieties with enhanced resilience to stresses. Even though phenotypic characterization is an inexpensive approach for elucidating variation hidden in genetic resources, information on the genetic diversity patterns in large collections of landraces remains limited and this constitutes a major impediment for their optimal utilization in modern maize breeding programs. We investigated the extent of phenotypic diversity among 196 maize landraces, representing gene pools from Burkina Faso, Ghana and Togo, and 14 improved populations/varieties from the Maize Improvement Program of International Institute of Tropical Agriculture (IITA-MIP). The germplasm was assessed for 26 agronomic traits. Highly significant differences (P < 0.001) were observed among the accessions for all measured traits.
Cluster analysis separated the maize germplasm into five major groups, differentiated largely by phenology and overall phenotypic appeal, enabling identification of outstanding genotypes for further screening for stress tolerance. Wide genetic diversity was observed between Burkinabe and improved gene pools, suggesting that the original Sahelian gene pool might not have contributed much to modern cultivars. This gene pool offers opportunities for pre-breeding by providing novel alleles for enriching elite maize germplasm. Shannon-Weaver diversity index (H’) revealed high genetic variability among the landraces (H’ = 0.73) and a narrow genetic base in the improved populations and varieties (H’ = 0.46). These results provide new insights into the potential of tropical maize landraces for genetic improvement of maize.
	
"
   
   uri <-  "doi:10.25502/4PZ9-GQ83/D"
   group <- "maize_trials" 

   ff <- carobiner::get_data(uri, path, group)

   dset <- data.frame(
   	carobiner::read_metadata(uri, path, group, major=1, minor=3),
      publication= "doi:10.1080/15427528.2019.1674760",
      data_institutions = "IITA",
      carob_contributor="Cedric Ngakou",
      data_type="experiment",
      project=NA,
  	  carob_date="2023-10-03"
   )
   
   
   bn <- basename(ff)
   
   # read the dataset
   r <- read.csv(ff[bn=="Phenotypic characterization data.csv"])  
   
   d <- r[,c("ID", "COUNTRY", "ENTRY", "YEAR", "REP", "GENOTYPE", "POLLEN", "DYSK", "ASI", "PLHT", "EHT", "PASP", "EROT", "EASP", "YIELD", "HUSK")]#
   colnames(d) <- c("ID", "country", "variety_code", "planting_date", "rep", "variety", "dy_poll", "silking_days", "asi","plant_height", "e_ht", "p_asp", "e_rot", "e_asp", "yield", "husk")#,
   
   # add columns
   d$country[d$country=="BF"] <- "Burkina Faso"
   d$country[d$country=="TG"] <- "Togo"
   d$country[d$country=="GH"] <- "Ghana"
   d$country[d$country=="IM"] <- "Nigeria"
   d$crop <- "maize" 
   
   d$trial_id <- paste(d$ID,d$country,sep = "-")
   d$yield_part <- "grain"
   d$on_farm <- TRUE
   d$irrigated <- FALSE
   d$borer_trial <- FALSE
   d$striga_infected <- FALSE
   d$ID <- NULL
   d$striga_trial <- FALSE

   ### add long and lat coordinate
   d$longitude[d$country=="Burkina Faso"] <- -1.6880314
      d$latitude[d$country=="Burkina Faso"] <- 12.0753083
      
      d$longitude[d$country=="Ghana"] <- -1.0800271
      d$latitude[d$country=="Ghana"] <- 8.0300284
      
      d$longitude[d$country=="Togo"] <- 1.0199765
      d$latitude[d$country=="Togo"] <- 8.7800265
     
       d$longitude[d$country=="Nigeria"] <- 3.8972497
      d$latitude[d$country=="Nigeria"] <- 7.3777462
   #data type
   d$variety_code <- as.character(d$variety_code)
   d$planting_date <- as.character(d$planting_date)
   
   carobiner::write_files(dset, d, path=path)
   
}


