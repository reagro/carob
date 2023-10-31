# R script for "carob"

# ## ISSUES 

#RH: this is 


# ....


carob_script <- function(path) {
  
  "Description:
Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory alternative cropping systems trials were conducted comparing to existing systems and to find out suitable and more profitable cropping systems, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
  "
  
  uri <- "hdl:11529/10547997"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "conservation_agriculture"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="Rabi (winter) crops-all nodes-Alternative cropping systems trial-Sunsari-Nepal",
    uri=uri,
    data_citation= "Gathala, Mahesh K. (CIMMYT) - ORCID: 0000-0001-8282-2953, Tiwari, Thakur P. (CIMMYT), Islam, Saiful (CIMMYT) - ORCID: 0000-0002-6482-5031, Shrestha, Renuka (Nepal Agricultural Research Council), Shrestha, H.K. (Nepal Agricultural Research Council), Manandhar, S. (Nepal Agricultural Research Council) - ORCID: 0000-0002-6353-3539, Shrestha, Shukra Raj (Nepal Agricultural Research Council).",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="on-farm experiment",
    carob_contributor="Fredy chimire",
    carob_date="2023-10-31"
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
 # dset$license <- "not specified" #carobiner::get_license(js)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "Maize-Rabi 2016-17-ACS-Bhokraha-Sunsari.xlsx"]
  
  
  # Select sheet with yields from the excel file 
  r <- carobiner::read.excel(f, sheet = "14 - Grain Harvest ",skip=4)
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d <- data.frame(date=r$"...1",trial_id = r$"...3",season=r$"...2",treatment=r$"...9", yield = r$`Grain yield (t/ha)`*1000,
                  biomass_total=r$`Biomass (t/ha)`*1000)
  
  # for first dataset
  d$dataset_id <- dataset_id

  
  ##### Location #####
  d$country <- "Nepal"
  d$adm2 <- "Sunsari" # district provided in the excel
  d$site <- "Bhokraha" # community provided in the excel
  d$crop <- "maize"
  d$N_fertilizer <- 114
  d$P_fertilizer <- 60/2.29
  d$K_fertilizer <- 40/1.2051
  d$harvest <- 151
  d$emergence <- 9
  d$harvest_date  <- as.Date("06/29/17", format = "%m/%d/%y")
  d$variety <-  "TX369"
  d$row_spacing <- 50
  d$fertilizer_type <- "Diammonium phosphate"
  
  
  d$latitude <- 26.5920003 # https://www.gps-coordinates.net/
  d$longitude <- 87.1025479
  d$yield_part <- "grain"
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
  
  
}
