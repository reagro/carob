# R script for "carob"

# ## ISSUES 

#RH: this is 


# ....


carob_script <- function(path) {
  
  "Description:
• APSIM is an internationally well-recognized cropping systems models for scientists. An SRFSI team conducted many on-station trials in partners’ research station, which managed by SRFSI NARES partners’ scientists for validation of APSIM. The models simulated biophysical performances on the systems on a daily basis in terms of crop yields, water use, soil dynamics, climate, the fate of nutrients, and residues management. • APSIM was regionally validated using also results from on-farm experiments; validated models were used to explore the profitability, sustainability, and risk reduction potential of technological options under different contemporary and future climate scenarios. Using actual, historical, and predicted sets of climate data, the model was used to explore technology effects, sustainability, and riskiness over longer time periods than possible for field results in a 4-year project. Researchers from the region developed expertise in the use of the APSIM model, and the modeling effort was supported by CSIRO scientists

"
  
  uri <- "hdl:11529/10548052"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "simulation"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="Gaydon, Don; Laing, Alison; Poulton, Perry; SRFSI team, 2018. 7.2-APSIM-BARI OFRD-Rangpur-on-station research trials-SRFSI Project-ACIAR-CIMMYT, https://hdl.handle.net/11529/10548052, CIMMYT Research Data & Software Repository Network, V2",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="on_station",
    carob_contributor="Fredy chimire",
    carob_date="2023-10-17"
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- "not specified" #carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "APSIM-IrrXN- rabi maize 2015-16-SRFSI-OFRD-Rangpur.xlsx"]
  
  # Select sheet with revised data from the excel file 
  r <- carobiner::read.excel(f, sheet = "Stat")
   
  #### about the data #####
  ## (TRUE/FALSE)
  
  d <- data.frame(plant_height = r$`Plant height`, yield = r$`Grain Yield`*1000)

  d$dataset_id <- dataset_id
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- r$Irrigation=="I1"
  ## the treatment code	

	d$treatment <- 
		ifelse(r$Irrigation == "I5",
             "CT Maize (6 irrigations) (irrig at V4, V8, V12, tasseling, milking, soft-dough)",
        ifelse(r$Irrigation == "I4",
             "ST Maize (6 irrigations) (irrig at V4, V8, V12, tasseling, milking, soft-dough)",
        ifelse(r$Irrigation == "I3",
             "ST Maize (4 irrigations) (irrig at V6, V10, tasseling, soft-dough)",
        ifelse(r$Irrigation == "I2",
              "ST Maize (2 irrigations) (irrig at V6, tasseling)",
              "ST Maize (0 irrigations)"
         )))
	)

   ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d$country <- "Bangladesh"
  
  #d$trial_id <- d$`HH-ID`
  d$latitude <- 25.72093 # Gps are available from General and Phenology sheet
  d$longitude <- 89.26442
  d$planting_date <- "2015-11-23"
  d$yield_part <- "grain"
  d$variety <-  "Kavery25K60"
  d$crop <-"maize"


print("many more variables need to be included")  
 
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}
