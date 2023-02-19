
"
	Description:
   The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel 
   sites across sub-Saharan Africa using the Land Degradation
   Surveilllance framework and inlcuded also multi-location diagnostic
   trials in selected sentiale sites to determine nutrient limitations
   and response to improved soil management practices (soil amendments)
"

carob_script <- function(path) {

uri <- "doi:10.25502/20180814/1355/HJ"
dataset_id <- carobiner::simple_uri(uri)
group <- "fertilizer"
## dataset level data 
dset <- data.frame(
  dataset_id = dataset_id,
  group=group,
  uri=uri,
  publication=NA,
  data_citation = "Huising, J. (2018). Africa Soil Information System - Phase 1, Nkhata Bay S2 [Data set].
  International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180814/1355/HJ",
  data_institutions = "IITA",
  carob_contributor="Cedric Ngakou",
  experiment_type="fertilizer",
  has_weather=FALSE,
  has_management=TRUE
)

## download and read data 

ff  <- carobiner::get_data(uri, path, group)
js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
dset$license <- carobiner::get_license(js)


f1 <- ff[basename(ff) == "Nkhata Bay_S2_Field.csv"] # get Field dataset
f2 <- ff[basename(ff) == "Nkhata Bay_S2_Plant.csv"] # get Plant dataset
f3 <- ff[basename(ff) == "Nkhata Bay_S2_plot.csv"]# get plot dataset

# read dataset
d1 <- read.csv(f1)
d2 <- read.csv(f2)
d3 <- read.csv(f3)
#d <- readxl::read_excel(f) |> as.data.frame()

# process file(s)
#d <- carobiner::change_names(d, from, to)
d3$dataset_id <- dataset_id
d1$dataset_id <- dataset_id

# process field data
d1$trial_id<- c(paste0(d1$dataset_id,"-",d1$ID))
d1$latitude<-d1$Flat
d1$longitude<-d1$Flong
d1$location<-d1$Village
d1$soil_type<-d1$Soil.texture.class
d1$OM_type<- d1$MType1
d1$previous_crop<-d1$PCrop1

# add column

d1$country<- "Malawi"
d1$crop<-"maize"
d1$OM_used=ifelse(d1$OM_type== "None","FALSE",
                  ifelse(d1$OM_type=="NA", "FALSE ", "TRUE" ))

d1<- d1[,c("dataset_id","trial_id","location","country",
           "latitude","longitude","crop","soil_type","previous_crop",
           "OM_type","OM_used")]


# process plot dataset
d3$rep<-d3$Rep

d3$treatment<-d3$TrtDesc

d3$yield<-(d3$TGrainYld_adj)*1000

d3$residue_yield<-(d3$AdjTStoverYld)*1000
d3$season<-d3$Season
d3$site<-d3$Site

d3$N_fertilizer<-ifelse(d3$TrtDesc=="Control",0,
                        ifelse(d3$TrtDesc=="PK",0,100))

d3$K_fertilizer<-ifelse(d3$TrtDesc=="Control",0,
                        ifelse(d3$TrtDesc=="NP",0,60))

d3$P_fertilizer<-ifelse(d3$TrtDesc=="Control",0,
                        ifelse(d3$TrtDesc=="NK",0,30))

d3$Zn_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",3,0)

d3$S_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",5,0)

d3=transform(d3,N_splits=ifelse(d3$N_fertilizer>0,3,0))

d3<-d3[,c("dataset_id","site","rep","treatment","season","yield","residue_yield","N_fertilizer",
          "K_fertilizer","P_fertilizer","Zn_fertilizer","S_fertilizer","N_splits")]

#merge all the data
d<-merge(d1,d3,by="dataset_id", all.x = TRUE)
# data type
d$season<-as.character(d$season)
d$OM_type<-as.character(d$OM_type)
d$OM_used<-as.logical(d$OM_used)
# crop terms normalization
p <- carobiner::fix_name(gsub("/", "; ", d$previous_crop), "lower")
p <- gsub("maize & s", "sweetpotato", p)
p <- gsub("potatoes", "potato", p)
p <- gsub("cowpeas", "cowpea", p)
p <- gsub("maize & cassava", "maize; cassava", p)
p <- gsub(" potato&cassava", "potato; cassava", p)
p <- gsub("nuts&cassava", "cassava; nuts", p)
p <- gsub(" maize&s", "maize" , p)
p <- gsub(" nuts", "nuts", p)
p <- gsub("s",NA , p)
p <- gsub("fallow", "no crop", p)


d$previous_crop <- p
# change date format

# fill whitespace in soil_type 
d<- replace(d,d=='',NA)
# all scripts must end like this

carobiner::write_files(dset, d, path, dataset_id, group)


}