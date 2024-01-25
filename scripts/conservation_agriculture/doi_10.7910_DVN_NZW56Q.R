# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:

    [This dataset is a result of a study that was carried out in nine on-farm sites of Central and Southern Malawi to understand and compare the effects of different cropping systems (conservation agriculture (CA) and conventional) on soil physical and chemical parameters and long-term maize productivity. Six experiments were established in each target community. Each experiment had three treatments at one farm and was treated as a replicate, plot sizes were 0.1 ha per treatment. The treatments were as follows:
1. Conventional control plot consisting of the traditional ridge and furrow land preparation planted with continuous monocrop maize (CPM). The residues were managed using methods commonly practiced in each extension planning area; i.e., the residues were incorporated into the ridges. Continuous monocrop maize was planted on the ridges.
2. CA plot with continuous monocrop maize (CAM) planted into the previous years’ ridges (where they still existed) or directly into the plot without previous ridge formation. Crop residues from the previous years’ harvests were retained as a surface mulch. Maize seeds were planted as sole crops in no-till methods using a pointed stick (dibble stick).
3. CA plot with maize intercropped with a legume [cowpea or pigeon pea or groundnut. Both crops were planted with the dibble stick into the previous years’ ridges (where they still existed) or directly into the plot without further ridging. Crop residues were retained as surface mulch as in treatment 2.]

"
  
  uri <- "doi:10.7910/DVN/NZW56Q"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "conservation_agriculture"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="International Maize and Wheat Improvement Center (CIMMYT), 2020, Conservation Agriculture Mother Trials in Malawi, https://doi.org/10.7910/DVN/NZW56Q, Harvard Dataverse, V2, UNF:6:3aVA30+F7m2MeLgav1F6XQ== [fileUNF]",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_institutions = "CIMMYT,IFPRI",
    data_type="on-farm experiment", 
    carob_contributor="Mitchelle Njukuya",
    carob_date="2024-01-16"
  )
  
  ## download and read data 
  
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
  dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
  
  
  f <- ff[basename(ff) == "003_AR_MAL_CIMMYT_CAmother_onfarm_2019_Data.csv"]
  
  r <- read.csv(f)
  
  ## process file(s)
  d <- data.frame(trial_id=r$No, country=r$Country, 
					adm2=r$District, location=r$Village, 
					treatment=r$Treat, variety=r$Variety, crop=r$crop.grown, 
					plant_density=r$Plantpopulation, yield=r$Grain.yield, 
					harvest_date=as.character(r$Harvest.Year), 
					residue_yield=r$Biomassyield

				)
  
#  d <- carobiner::change_names(d,
# note error in district/village c("No","Country","District","Village","Treat","Variety","crop.grown","Plantpopulation","Grain.yield")
#    ,c("trial_id","country","location","adm1","treatment","variety","crop","plant_density","yield"))
 
  d$trial_id <- as.numeric(d$trial_id)
  
  #fixing crop names
  d$crop <- carobiner::replace_values(
			tolower(d$crop), c("pigeonpea", "groundnuts"),
                             c("pigeon pea", "groundnut"))
  
  #protocol had no information on cowpea and pigeon pea variety used 
  #but genotype information was provided by the author via email
  d$variety <- trimws(d$variety)
  d$variety[d$crop=="groundnut"] <- "CG7"
  d$variety[d$crop=="cowpea"] <- "Sudan"
  d$variety[d$crop=="pigeon pea"] <- "Mtawajuni"
  
  d$dataset_id <- dataset_id
  d$trial_id <- as.character(d$trial_id)
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  #fixing location name
  d$adm2 <- carobiner::replace_values(d$adm2, "Nkotakhota", "Nkhotakota")
  
  d$planting_date <- "2018"
  

  ##### Fertilizers #####
  # Protocol specified basal dressing with 23:21:0(N:P:K)
  # Top dressing was done with urea (46%N)
  # Protocol did not specify rate of fertilizer application or meaning of 100:100
  # Author provided information that application rate for urea was 100kg/ha
  

  d$fertilizer_type <- "urea"
  d$N_fertilizer <- "100"
  d$yield_part <- "seed"
  d$yield_part[d$crop=="maize"] <- "grain"

### this needs improvement. "location" should be used when possible, not "adm2" 
#  d1 <- unique(d[,c("country", "adm2", "location")])
  geo <- data.frame(country = "Malawi", 
                 adm2 = c("Balaka","Dowa", "Machinga", "Nkhotakota", "Salima", "Zomba"),
                 longitude = c(35.0532, 33.7781, 35.6026, 34.0329, 34.4524, 35.4575),
                 latitude = c(-15.0485, -13.5388, -14.9027, -12.8322, -13.7629, -15.4337))

  d <- merge(d, geo, by=c("country", "adm2"), all.x=TRUE)  

  
#  d <- d[,c("dataset_id","on_farm","is_experiment","trial_id","country","location","adm1",
#          "longitude","latitude","crop","variety","treatment","plant_density","planting_date",
#          "harvest_date","fertlizer_type","residue_yield","yield","yield_part")]  
    
    carobiner::write_files(dset, d, path=path)
}

