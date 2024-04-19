
# R script for "carob"

## ISSUES
# intercrop datasets dd2 and dd3 need to the added to main dataset d


carob_script <- function(path) {
  
  "
Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT),
were conducted in several fields in each community. 
Likewise, farmer-participatory intercropping leafy vegetables in maize trials were conducted comparing to exiting practices and
to find out suitable and more profitable crop production practices,
prioritized to increase visibility and to avoid implementation and 
management problems that emerge when utilizing small plots with significant edge effects.
Most trials were replicated in several fields within each community and 
were farmer-managed with backstopping from project staff and NARES partners. 
Project partners and staff coordinated monitoring and data acquisition.
Where possible, collaborating farmers were selected by the community,
and the project worked with existing farmer groups, with groups of both men and women farmers.
"
  
  #### Identifiers
  uri <- "hdl:11529/10548002"
  group <- "intercrop"
  
  #### Download data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ##### dataset level metadata 
  dset <- data.frame(
    # change the major and minor versions if you see a warning
    carobiner::read_metadata(uri, path, group, major=2, minor=2),
    data_institutions = "CIMMYT",
    publication= NA,
    project=NA,
    data_type= "experiment",
    carob_contributor= "Mitchelle Njukuya",
    carob_date="2024-04-19"
  )
  
  ##### PROCESS data records
  
  proc <- function(f) {
    r2 <- carobiner::read.excel.hdr(f, sheet ="2 - Site information", skip=4, hdr=3)
    d2 <- data.frame(
      season=r2$Season, location=r2$Node, 
      trial_id=r2$Site.No.Unique.farmer.ID,
      treatment=r2$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
      soil_type=r2$Soil.texture.sand.silt.clay.etc)
    
    r3 <- carobiner::read.excel.hdr(f, sheet ="4- Stand counts & Phenology", skip=4, hdr=3)
    d3 <- data.frame(
      treatment=r3$Tmnt, trial_id=r3$Site.No, 
      variety=r3$Variety,
      row_spacing=r3$Row.spacing.cm,
      crop=tolower(r3$Crop),
      planting_date=as.character(r3$Date.of.seeding.dd.mm.yy),
      harvest_date=as.character(r3$Date.of.harvest.dd.mm.yy),
      flowering=r3$X50pct.first.flowering.DAS,
      anthesis=r3$X50pct.anthesis.DAS,
      maturity=r3$X80pct.physiological.maturity.DAS,
      harvest=r3$Harvesting.DAS)
    
    r4 <- carobiner::read.excel.hdr(f, sheet ="6 - Fertilizer amounts ", skip=4, hdr=3)
    
    d4 <- data.frame(
      treatment=r4$Tmnt, trial_id=r4$Site.No,
      N_fertilizer=r4$N.kg.ha, 
      P_fertilizer=r4$P2O5.kg.ha / 2.29, 
      gypsum=r4$Gypsum.Kg.ha,
      Zn_fertilizer=r4$ZnSO4.kg.ha,
      K_fertilizer=r4$K2O.kg.ha / 1.2051,
      S_fertilizer = NA)
    
    d4$fertilizer_type <- apply(r4[, grep("Application_Product.used", names(r4))], 1, 
                                \(i) paste(unique(i), collapse="; "))
    
    
    
    r5 <- carobiner::read.excel.hdr(f, sheet ="13 - Grain Harvest ", skip=4, hdr=3)	
   
    d5 <- data.frame(
      treatment=r5$Tmnt, trial_id=r5$Site.No, 
      yield=r5$Grain.yield.t.ha * 1000,
      residue_yield=r5$Straw.yield.t.ha * 1000,
      dmy_total=r5$Biomass.t.ha * 1000)
    
    ## merge all 
    dd0 <- merge(d2, d3, by=c("treatment", "trial_id"), all.x=TRUE)
    dd0 <- merge(dd0, d4, by=c("treatment", "trial_id"), all.x=TRUE)
    dd0 <- merge(dd0, d5, by=c("treatment", "trial_id"), all.x=TRUE) }
    
  f <- ff[basename(ff) == "Inter-cropping 2014-15-all nodes-Rangpur.xlsx"]
  d0 <- proc(f)
  
  
  
  proc_1 <- function(f1) {
    r6 <- carobiner::read.excel.hdr(f1, sheet ="2 - Site information", skip=5, hdr=4)
    d6 <- data.frame(
      season=r6$Season, location=r6$Node, 
      trial_id=r6$Site.No.Unique.farmer.ID,
      treatment=r6$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
      soil_type=r6$Soil.texture.sand.silt.clay.etc)
    
    r7 <- carobiner::read.excel.hdr(f1, sheet ="4- Stand counts & Phenology", skip=4, hdr=3)
    d7 <- data.frame(
      treatment=r7$Tmnt, trial_id=r7$Site.No, 
      variety=r7$Variety,
      row_spacing=r7$Row.spacing.cm,
      crop=tolower(r7$Crop),
      planting_date=as.character(r7$Date.of.seeding.dd.mm.yy),
      harvest_date=as.character(r7$Datw.of.harvest.dd.mm.yy),
      flowering=r7$X50pct.first.flowering.DAS,
      anthesis=r7$X50pct.anthesis.DAS,
      maturity=r7$X80pct.physiological.maturity.DAS,
      harvest=r7$Harvesting.DAS)
    
    r8 <- carobiner::read.excel.hdr(f1, sheet ="6 - Fertilizer amounts ", skip=4, hdr=3)
    
    d8 <- data.frame(
      treatment=r8$Tmnt, trial_id=r8$Site.No,
      N_fertilizer=r8$N.kg.ha, 
      P_fertilizer=r8$P2O5.kg.ha / 2.29,
      gypsum = NA,
      Zn_fertilizer=r8$Zn.kg.ha,
      K_fertilizer=r8$K.kg.ha,
      S_fertilizer=r8$S.kg.ha)
    
    d8$fertilizer_type <- apply(r8[, grep("Application_Product.used", names(r8))], 1, 
                                \(i) paste(unique(i), collapse="; "))
    
    
    
    r9 <- carobiner::read.excel.hdr(f1, sheet ="13 - Grain Harvest ", skip=4, hdr=3)	
    
    d9 <- data.frame(
      treatment=r9$Tmnt, trial_id=r9$Site.No, 
      yield=r9$Grain.yield.t.ha * 1000,
      residue_yield=r9$Straw.yield.t.ha * 1000,
      dmy_total=r9$Biomass.t.ha * 1000)
    
    ## merge all 
    dd1 <- merge(d6, d7, by=c("treatment", "trial_id"), all.x=TRUE)
    dd1 <- merge(dd1, d8, by=c("treatment", "trial_id"), all.x=TRUE)
    dd1 <- merge(dd1, d9, by=c("treatment", "trial_id"), all.x=TRUE) }
   
  
  f1 <- ff[basename(ff) == "Inter-cropping 2015-16-all nodes-Rangpur.xlsx"]
  d1 <- proc_1(f1)
  
  d <- rbind(d0, d1)
  
  #add columns
  
  d$country <- "Bangladesh"
  d$site <- "Rangpur"
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$inoculated <- FALSE
  d$yield_part <- "grain" 
  
  geo= data.frame(location=c("Durgapur","Borodarga","Kolkondo","Mohanpur","Lakhitari"), 
                  latitude=c(25.55461, 25.82308, 25.86245, 25.93235, 25.88404), 
                  longitude=c(89.29195, 89.25810, 89.20563, 88.69496, 89.25188))
  
  d <- merge(d, geo, by="location", all.x = TRUE)	
  
  rr0 <- carobiner::read.excel.hdr(f, sheet ="15-Intercrop", skip=4, hdr=3)
  dd2 <- data.frame(season = rr0$Season, 
                    intercrop = rr0$Type.of.trial,
                    location = rr0$Node,
                    treatment = rr0$Tmnt, 
                    crop = rr0$Crop.information.and.phenology.of.inter.crop_Name.of.Inter.Crop,
                    variety = rr0$Variety,
                    planting_date = rr0$Date.of.seeding.dd.mm.yy,
                    harvest_date_amaranth = rr0$Date.of.harvesting.of.amaranth,
                    harvest_date_spinach = rr0$Date.of.harvesting.of.spinach,
                    harvest_date_napa = rr0$Date.of.harvesting.of.Napa.shak,
                    yield = rr0$Ccalculaation_Intercrop.t.ha *1000)
  
  rr1 <- carobiner::read.excel.hdr(f1, sheet ="15-Intercrop", skip=4, hdr=3)
  dd3 <- data.frame(season = rr1$Season, 
                    intercrop = rr1$X.1,
                    location = rr1$X.6,
                    treatment = rr1$Tmnt, 
                    crop = rr1$Crop.information.and.phenology.of.inter.crop_Name.of.Inter.Crop,
                    variety = rr1$Variety,
                    planting_date = rr1$Date.of.seeding.dd.mm.yy,
                    harvest_date = rr1$Date.of.harvest.dd.mm.yy ,
                    yield = rr1$Intercrop.t.ha *1000)
  
  
    carobiner::write_files(path, dset, d)
}
