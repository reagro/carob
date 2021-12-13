# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:

    In 2010, the Africa-wide Rice Breeding Task Force was launched by AfricaRice involving National Agricultural Research System (NARS) from about 30 countries. The objectives of the network are to evaluate the stability of traits incorporated in breeding processes and to identify varieties best fit to growth conditions in target regions and to markets. The Task Force also accumulates data on performance of new elite lines, thereby facilitating varietal release procedures. Furthermore, by exposing breeders from NARS and farmers to these elite lines during the testing phase, dissemination will be facilitated. The activities conducted by the Task Force consists of a series of consecutive trials. Promising breeding lines developed by AfricaRice or by national and international partners, such as IRRI, CIAT and the NARS are nominated for evaluation in one or several rice cultivation environments: rainfed lowland, irrigated lowland, rainfed upland, high elevation and mangrove. All nominated lines should be fixed and accompanied by supporting data on traits incorporated during the breeding process and with information on yield performance. These characteristics are checked at AfricaRice before incorporation into the network. The first phase (MET, Multi-Environment Testing) consists of an initial evaluation of about 100 lines selected from the nominated lines. Each national partner evaluates these lines at sites in his/her country. Such sites may be at an experimental station under optimal management to evaluate yield potential, or may be ‘hot spots’ to check the performance of the nominations in a stressed growth environment. Trials are replicated three times and include at least a common and a local check. The second phase (PET,Participatory Evaluation Trial) serves to evaluate and confirm the performance of the selected lines. These lines are cultivated using the same experimental design with 3 replications. An important feature of PET is that farmer and other stakeholders such as miller and traders are invited to participate in varietal selection and their opinion on the performance of all entries (i.e. participatory varietal selection, PVS) collected. Based on the data collected, observations by the breeders and the opinion of stakeholder groups, NARS partners select up to 10 lines. Further, NARS evaluated these lines in at least three sites per country and during one or more growing seasons, depending on varietal release requirements. All stakeholders are again invited to get acquainted with the new lines and voice their opinion to help select lines for further advancement. Among the 10 lines, farmers are invited to select three lines and cultivate these in their own fields, together with a common check and their own variety.

"
  
  uri <- "doi:10.7910/DVN/OF7M9D"
  dataset_id <- agro::get_simple_URI(uri)
  group <- "variety_performance"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication="doi_10.7910_DVN_OF7M9D.ris",
    carob_contributor="Eduardo Garcia Bendito",
    experiment_type="variety_performance",
    has_weather=FALSE,
    has_management=FALSE
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- js$data$latestVersion$license
  
  
  f <- ff[basename(ff) == "2016-RU-MET-Burkina Faso-Farakoba.xlsx"]
  
  d <- data.frame(readxl::read_excel(f))
  
  # Start formatting dataset
  d$country <- d$Country
  d$adm1 <- "Haut-Bassins"
  d$adm2 <- "Houet"
  d$adm3 <- "Bobo-Dioulasso"
  d$site <- d$Site
  d$trial_id <- paste0(dataset_id, '-', d$Site)
  d$latitude <- 11.082302
  d$longitude <- -4.339967
  d$start_date <- "2016"
  d$end_date <- "2016"
  d$season <- d$Season
  d$on_farm <- "no"
  d$is_survey <- "no"
  d$crop <- "rice"
  d$variety_code <- d$Genotype
  d$yield <- d$YIELD*1000
  
  # process file(s)
  d <- d[,c("country", "adm1", "adm2", "adm3", "site", "trial_id", "latitude", "longitude", "start_date", "end_date", "season", "on_farm", "is_survey", "crop", "variety_code", "yield")]
  d$dataset_id <- dataset_id
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path, dataset_id, group)
  TRUE
}
