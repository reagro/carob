# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

These are the raw data of the paper: 'Mulch application as the overarching factor explaining increase in soil organic carbon stocks under conservation agriculture in two 8-year-old experiments in Zimbabwe.' authored by Armwell Shumba, Regis Chikowo, Christian Thierfelder, Marc Corbeels, Johan Six, Rémi Cardinael and submitted for publication in a peer-reviewed journal.]

"

  uri <- "doi:10.18167/DVN1/VPOCHN"
  group <- "conservation_agriculture"
  
  dataset_id <- carobiner::simple_uri(uri)
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  
  ## dataset level data 
  dset <- data.frame(
    carobiner::extract_metadata(js, uri, group),
    project = NA,
    #data_citation ="Shumba, Armwell; Chikowo, Regis; Thierfelder, Christian; Corbeels, Marc; Six, Johan; Cardinael, Rémi, 2023, Data for Mulch application as the overarching factor explaining increase in soil organic carbon stocks under conservation agriculture in two 8-year-old experiments in Zimbabwe, https://doi.org/10.18167/DVN1 /VPOCHN , CIRAD Dataverse, V2",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication = "doi:10.1016/j.agee.2022.108207",
    data_institutions = "CIRAD, University of Zimbabwe, CIMMYT, ETH ZURICH",
    # e.g. "on-farm experiment", "survey", "compilation"
    data_type = "experiment", 
    carob_contributor = "Hope Takudzwa Mazungunye",
    carob_date = "2024-02-15",
    revised_by = "Eduardo Garcia Bendito",
    revision_date = "2024-03-07"
  )
  
  f <- ff[basename(ff) == "Shumba_et_al_Raw_data_SOC_paper_vf.xlsx"]
  r <- carobiner::read.excel(f, sheet = "Seasonal_OC_inputs")
	
## process file(s)
  d <- data.frame(dataset_id = dataset_id, trial_id = NA, on_farm = FALSE, is_survey = FALSE, 
                  country = "Zimbabwe", site = ifelse(r$Site == "DTC", "Domboshava Training Centre", "University of Zimbabwe Farm"),
                  longitude = ifelse(r$Site == "DTC", 31.125, 31.013), latitude = ifelse(r$Site == "DTC", -17.588, -17.706),
                  treatment = r$Treatment, rep = as.integer(r$Rep), crop = tolower(r$Crop), crop_rotation = "maize", previous_crop = "maize")

  # Assign trial_id
  t <- 1
  for (id in 1:nrow(r)) {
    if (id == 1){
      d$trial_id[id] <- t
    } else {
      if (r$Rep[id] >= r$Rep[id-1]) {
        d$trial_id[id] <- t
      } else {
        t <- t + 1
        d$trial_id[id] <- t
      }
    }
  }
  d$trial_id <- as.character(d$trial_id)
  
  # Add planting and harvest dates from publication
  r$pyr <- gsub("\\/.*","",r$Year...2)
  r$planting_date <- as.character(paste0(r$pyr, "-11"))
  r$planting_date[r$pyr == "2019" & r$Site == "DTC"] <- "2019-11-21"
  r$planting_date[r$pyr == "2019" & r$Site == "UZF"] <- "2019-11-19"
  r$planting_date[r$pyr == "2020"] <- "2020-11-25"
  r$hyr <- as.character(as.integer(r$pyr) + 1)
  r$harvest_date <- as.character(paste0(r$hyr, "-04"))
  r$harvest_date[r$hyr == "2020" & r$Site == "DTC"] <- "2020-04-15"
  r$harvest_date[r$hyr == "2020" & r$Site == "UZF"] <- "2020-04-14"
  r$harvest_date[r$hyr == "2021" & r$Site == "DTC"] <- "2021-04-25"
  r$harvest_date[r$hyr == "2021" & r$Site == "UZF"] <- "2021-04-15"
  d$planting_date <- r$planting_date
  d$harvest_date <- r$harvest_date
  
  # Biomass variables
  d$dmy_roots <- r$Root_biomass_kg_ha1
  d$dmy_total <- r$Root_biomass_kg_ha1 + r$Total_AGB_kg_ha1
  
  # Yield variables
  d$yield_part <- "grain"
  d$residue_yield <- r$`Veg_Biomass (kg/ha)`
  d$yield[d$crop == "cowpea"] <- as.numeric(r$`Grain (kg/ha)`[r$Crop == "cowpea"] * 1.125) # Adding fresh weight
  d$yield[d$crop == "maize"] <- as.numeric(r$`Grain (kg/ha)`[r$Crop == "maize"] * 1.13) # Adding fresh weight
  d$yield <- as.numeric(d$yield)
  
  # Fertilizer variables
  d$fertilizer_type <- "NPK; AN"
  # 3 equal amoun splits according to publication
  d$N_fertilizer[d$crop == "maize"] <- 11.6 + (23.1 * 2)
  d$N_fertilizer[d$crop == "cowpea"] <- 11.6
  d$P_fertilizer <- 10.6
  d$K_fertilizer <- 9.6
  d$N_splits[d$crop == "maize"] <- 3L
  d$N_splits[d$crop == "cowpea"] <- 1L
  
  # Spacing
  d$plant_spacing <- 25
  d$row_spacing[d$crop == "maize"] <- 90
  d$plant_density[d$crop == "maize"] <- 44444
  d$row_spacing[d$crop == "cowpea"] <- 45
  d$plant_density[d$crop == "cowpea"] <- 88888
  
  # Land preparation
  d$land_prep_method <- "conventional tillage"
  d$land_prep_method[grep("NT", d$treatment)] <- "no-tillage"
  
  # Residue management
  d$previous_crop_residue <- NA
  d$previous_crop_residue[d$treatment %in% c("NTMR", "NTM")] <- 20 # Assuming that 2.5 t DM/h is equivalent to 20%... Not sure if this is acceptable.
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}
