


carob_script <- function(path) {
  
  "Description:

   Summary results and individual trial results from the International Late Yellow Hybrid - ILYH, 
   (Mid-altitude / Subtropical Three Way Crosses Yellow Hybrids, with High Concentrations of Provitamins A 
   (especially beta-carotene) - CHTSPROA) conducted in 2012.

"
  
  uri <- "hdl:11529/10667"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "maize_trials"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="Global Maize Program, 2019, International Late Yellow Hybrid - ILYH1231, https://hdl.handle.net/11529/10667, CIMMYT Research Data & Software Repository Network, V1",
    publication= NA,
    data_institutions = "CIMMYT,GMP,CGIAR",
    data_type="experiment", 
    carob_contributor="Mitchelle Njukuya",
    # date of first submission to carob
    carob_date="2023-01-30"
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "12CHTSPROA10-1.xls"]
  f1 <- ff[basename(ff) == "12CHTSPROA11-1.xls"]
  f2 <- ff[basename(ff) == "12CHTSPROA12-1.xls"]
  f3 <- ff[basename(ff) == "12CHTSPROA16-1.xls"]
  f4 <- ff[basename(ff) == "12CHTSPROA17-1.xls"]
  f5 <- ff[basename(ff) == "12CHTSPROA18-1.xls"]
  f6 <- ff[basename(ff) == "12CHTSPROA19-1.xls"]
  f7 <- ff[basename(ff) == "12CHTSPROA22-1.xls"]
  
  r <- readxl::read_excel(f) |> as.data.frame()
  r1 <- readxl::read_excel(f1) |> as.data.frame()
  r2 <- readxl::read_excel(f2) |> as.data.frame()
  r3 <- readxl::read_excel(f3) |> as.data.frame()
  r4 <- readxl::read_excel(f4) |> as.data.frame()
  r5 <- readxl::read_excel(f5) |> as.data.frame()
  r6 <- readxl::read_excel(f6) |> as.data.frame()
  r7 <- readxl::read_excel(f7) |> as.data.frame()
  
  ## process file(s)
  d <- r
  #removing rows from dataset
  d <- d[-c(25:50), ]
  ## use a subset
  d <- carobiner::change_names(d,c("Entry","Yield t/ha","Ear      Rot        %","Rt Ldg    %","Stlk   Ldg         %","% Moi-sture",
                                   "Plt. Hght (cm)","Pedigree","Ear Asp.      (1-5)","Plt. Asp.      (1-5)",
                                   "Text        (1-5)","Ear Hght (cm)"),
                               c("trial_id","yield","e_rot","rlper","slper","moist","pl_ht","treatment",
                                 "e_asp","p_asp","gtext","e_ht"))
  d$yield <- as.numeric(d$yield) 
  d$yield <- d$yield*1000
  
  d$dataset_id <- dataset_id
  d$on_farm <- TRUE
  d$striga_trial <- FALSE 
  d$striga_infected <- FALSE
  d$is_experiment <- TRUE
  d$borer_trial <- FALSE
  
    ##### Location #####
  # Geographical information was provided in a separate
  #excel file -> 12CHTSPROA-Locations
  
  d$country <- "Bolivia"
  d$location <- "San Pedro" 
  d$longitude <- 63.4667
  d$latitude <- 16.9 
  d$elevation <- 230
        
  d$crop <- "maize"
  
  #information on dates was provided in a separate excel file 12CHTSPROA-Locations
  d$planting_date <- as.character(as.Date("2013-01-17"))
  d$harvest_date  <- as.character(as.Date("2013-05-30"))
  d$yield_part <- "grain" 
  
  d<-d[,c("dataset_id","trial_id","is_experiment","on_farm","striga_trial","striga_infected","borer_trial","country","location","longitude","latitude","elevation",
          "treatment","crop","planting_date","harvest_date","pl_ht","e_ht","e_rot","rlper","slper","moist","e_asp","p_asp","gtext","yield","yield_part")]
  
  d1 <- r1
  #removing rows from data set
  d1 <- d1[-c(25:50), ]
  ## use a subset
  d1 <- carobiner::change_names(d1,c("Entry","Yield t/ha","Ear Rot      %","Rt Ldg       %","Stlk Ldg    %","%                            Moi-sture",
                                     "ASI","Pedigree","Ear Asp.                    (1-5)","Plt. Asp.            (1-5)",
                                     "Plt.                     Hght                (cm)","Ear                  Hght                  (cm)"),
                               c("trial_id","yield","e_rot","rlper","slper","moist","asi","treatment","e_asp","p_asp","pl_ht","e_ht"))
  
  d1$yield <- as.numeric(d1$yield) 
  d1$yield <- d1$yield*1000
  
  d1$dataset_id <- dataset_id
  d1$on_farm <- TRUE
  d1$striga_trial <- FALSE 
  d1$striga_infected <- FALSE
  d1$is_experiment <- TRUE
  d1$borer_trial <- FALSE
  
  ##### Location #####
  # Geographical information was provided in a separate
  #excel file -> 12CHTSPROA-Locations
  
  d1$country <- "Bolivia"
  d1$location <- "Algarrobal" 
  d1$longitude <- 63.65
  d1$latitude <- 21.45 
  d1$elevation <- 580
  
  d1$crop <- "maize"
  
  #information on dates was provided in a separate excel file 12CHTSPROA-Locations
  d1$planting_date <- as.character(as.Date("2013-01-05"))
  d1$harvest_date  <- as.character(as.Date("2013-06-14"))
  d1$yield_part <- "grain" 
  
  d1<-d1[,c("dataset_id","trial_id","is_experiment","on_farm","striga_trial","striga_infected","borer_trial","country","location","longitude","latitude","elevation",
          "treatment","crop","planting_date","harvest_date","asi","e_rot","rlper","slper","moist","e_asp","p_asp","pl_ht","e_ht","yield","yield_part")]
  
  d2 <- r2
  #removing rows from data set
  d2 <- d2[-c(25:50), ]
  ## use a subset
  d2 <- carobiner::change_names(d2,c("Entry","Yield t/ha","Ear      Rot      %","Rt Ldg    %","Stlk Ldg %","% Moi-sture","Plt. Hght    (cm)","Curv LS          (1-5)",
                                     "Pedigree","Ear Asp.      (1-5)","Plt. Asp.      (1-5)","Ear Hght    (cm)"),
                                c("trial_id","yield","e_rot","rlper","slper","moist","pl_ht","curv","treatment","e_asp","p_asp","e_ht"))
  
  d2$yield <- as.numeric(d2$yield) 
  d2$yield <- d2$yield*1000
  
  d2$dataset_id <- dataset_id
  d2$on_farm <- TRUE
  d2$striga_trial <- FALSE 
  d2$striga_infected <- FALSE
  d2$is_experiment <- TRUE
  d2$borer_trial <- FALSE
  
  ##### Location #####
  # Geographical information was provided in a separate
  #excel file -> 12CHTSPROA-Locations
  
  d2$country <- "Bolivia"
  d2$location <- "Muyupampa" 
  d2$longitude <- 63.75
  d2$latitude <- 19.8833 
  d2$elevation <- 1177
  
  d2$crop <- "maize"
  
  #information on dates was provided in a separate excel file 12CHTSPROA-Locations
  d2$planting_date <- as.character(as.Date("2013-01-15"))
  d2$harvest_date  <- as.character(as.Date("2013-06-20"))
  d2$yield_part <- "grain" 
  
  d2<-d2[,c("dataset_id","trial_id","is_experiment","on_farm","striga_trial","striga_infected","borer_trial","country","location","longitude","latitude","elevation",
            "treatment","crop","planting_date","harvest_date","pl_ht","e_ht","e_rot","rlper","slper","curv","moist","p_asp","e_asp","yield","yield_part")]
  
  d3 <- r3
  #removing rows from dataset
  d3 <- d3[-c(25:50), ]
  ## use a subset
  d3 <- carobiner::change_names(d3,c("Entry","Yield      gn wgt             t/ha","Ear              Rot                     %","Rt            Ldg            %","Stlk           Ldg            %",
                                     "%                Moi-sture","Plt.              Hght               (cm)",
                                     "ASI","Pedigree","Ear          Asp.            (1-5)","Plt.               Asp.           (1-5)",
                                     "Text             (1-5)","Ear           Hght           (cm)"),
                                c("trial_id","yield","e_rot","rlper","slper","moist","pl_ht","asi","treatment",
                                  "e_asp","p_asp","gtext","e_ht"))
  
  d3$yield <- as.numeric(d3$yield) 
  d3$yield <- d3$yield*1000
  
  d3$dataset_id <- dataset_id
  d3$on_farm <- TRUE
  d3$striga_trial <- FALSE 
  d3$striga_infected <- FALSE
  d3$is_experiment <- TRUE
  d3$borer_trial <- FALSE
  
  ##### Location #####
  # Geographical information was provided in a separate
  #excel file -> 12CHTSPROA-Locations
  
  d3$country <- "Ghana"
  d3$location <- "Kwadaso" 
  d3$longitude <- 1.65
  d3$latitude <- 6.7 
  d3$elevation <- 275
  
  d3$crop <- "maize"
  
  #information on dates was provided in a separate excel file 12CHTSPROA-Locations
  d3$planting_date <- as.character(as.Date("2012-10-15"))
  d3$harvest_date  <- as.character(as.Date("2013-01-30"))
  d3$yield_part <- "grain" 
  
  d3<-d3[,c("dataset_id","trial_id","is_experiment","on_farm","striga_trial","striga_infected","borer_trial","country","location","longitude","latitude","elevation",
            "treatment","crop","planting_date","harvest_date","pl_ht","e_ht","asi","e_rot","rlper","slper","e_asp","p_asp","gtext","moist","yield","yield_part")]
  
  d4 <- r4
  #removing rows from dataset
  d4 <- d4[-c(25:50), ]
  ## use a subset
  d4 <- carobiner::change_names(d4,c("Entry","Yield t/ha","Ear                  Rot                %","Rt                    Ldg                  %",
                                     "Stlk                     Ldg                   %",
                                     "%                           Moi-sture","Plt.                          Hght                       (cm)",
                                     "ASI","Pedigree","Ear                                 Asp.                          (1-5)",
                                     "Plt.                        Asp.                           (1-5)",
                                     "Ear                Hght                 (cm)","Text                       (1-5)"),
                                c("trial_id","yield","e_rot","rlper","slper","moist","pl_ht","asi","treatment","e_asp","p_asp","e_ht","gtext"))
  
  d4$yield <- as.numeric(d4$yield) 
  d4$yield <- d4$yield*1000
  
  d4$dataset_id <- dataset_id
  d4$on_farm <- TRUE
  d4$striga_trial <- FALSE 
  d4$striga_infected <- FALSE
  d4$is_experiment <- TRUE
  d4$borer_trial <- FALSE
  
  ##### Location #####
  # Geographical information was provided in a separate
  #excel file -> 12CHTSPROA-Locations
  
  d4$country <- "Ghana"
  d4$location <- "Kwadaso" 
  d4$longitude <- 1.65
  d4$latitude <- 6.7 
  d4$elevation <- 275
  
  d4$crop <- "maize"
  
  #information on dates was provided in a separate excel file 12CHTSPROA-Locations
  d4$planting_date <- as.character(as.Date("2012-10-15"))
  d4$harvest_date  <- as.character(as.Date("2013-01-30"))
  d4$yield_part <- "grain" 
  
  d4<-d4[,c("dataset_id","trial_id","is_experiment","on_farm","striga_trial","striga_infected","borer_trial","country","location","longitude","latitude","elevation",
            "treatment","crop","planting_date","harvest_date","pl_ht","e_ht","asi","e_rot","rlper","slper","p_asp","e_asp","gtext","moist","yield","yield_part")]
  
  d5 <- r5
  #removing rows from dataset
  d5 <- d5[-c(25:50), ]
  
  d5 <- carobiner::change_names(d5,c("Entry","Yield t/ha","Ear    Rot      %","Rt                  Ldg                 %",
                                     "Stlk      Ldg        %",
                                     "%              Moi-sture","Plt.      Hght        (cm)",
                                     "ASI","Pedigree","Plt.      Asp.           (1-5)","Ear    Asp.          (1-5)",
                                     "Text                  (1-5)","Ear       Hght        (cm)"),
                                c("trial_id","yield","e_rot","rlper","slper","moist","pl_ht","asi","treatment",
                                  "p_asp","e_asp","gtext","e_ht"))
  
  d5$yield <- as.numeric(d5$yield) 
  d5$yield <- d5$yield*1000
  
  d5$dataset_id <- dataset_id
  d5$on_farm <- TRUE
  d5$striga_trial <- FALSE 
  d5$striga_infected <- FALSE
  d5$is_experiment <- TRUE
  d5$borer_trial <- FALSE
  
  ##### Location #####
  # Geographical information was provided in a separate
  #excel file -> 12CHTSPROA-Locations
  
  d5$country <- "Ghana"
  d5$location <- "Kwadaso" 
  d5$longitude <- 1.65
  d5$latitude <- 6.7 
  d5$elevation <- 275
  
  d5$crop <- "maize"
  
  #information on dates was provided in a separate excel file 12CHTSPROA-Locations
  d5$planting_date <- as.character(as.Date("2012-10-15"))
  d5$harvest_date  <- as.character(as.Date("2013-01-30"))
  d5$yield_part <- "grain" 
  
  d5<-d5[,c("dataset_id","trial_id","is_experiment","on_farm","striga_trial","striga_infected","borer_trial","country","location","longitude","latitude","elevation",
            "treatment","crop","planting_date","harvest_date","pl_ht","e_ht","asi","e_rot","rlper","slper","p_asp","e_asp","gtext","moist","yield","yield_part")]
  d6 <- r6
  #removing rows from dataset
  d6 <- d6[-c(25:50), ]
  
  d6 <- carobiner::change_names(d6,c("Entry","Yield t/ha","% Moi-sture","Plt. Hght (cm)","Pedigree","Plt. Asp. (1-5)","Ear Asp. (1-5)","Ear Hght (cm)"),
                               c("trial_id","yield","moist","pl_ht","treatment","p_asp","e_asp","e_ht"))
  
  d6$yield <- as.numeric(d6$yield) 
  d6$yield <- d6$yield*1000
  
  d6$dataset_id <- dataset_id
  d6$on_farm <- TRUE
  d6$striga_trial <- FALSE 
  d6$striga_infected <- FALSE
  d6$is_experiment <- TRUE
  d6$borer_trial <- FALSE
  
  ##### Location #####
  # Geographical information was provided in a separate
  #excel file -> 12CHTSPROA-Locations
  
  d6$country <- "Myanmar"
  d6$location <- "Yezin" 
  d6$longitude <- 96
  d6$latitude <- 19
  d6$elevation <- NA
  
  d6$crop <- "maize"
  
  #information on dates was provided in a separate excel file 12CHTSPROA-Locations
  d6$planting_date <- as.character(as.Date("2012-10-19"))
  d6$harvest_date  <- as.character(as.Date("2013-02-10"))
  d6$yield_part <- "grain" 
  
  d6<-d6[,c("dataset_id","trial_id","is_experiment","on_farm","striga_trial","striga_infected","borer_trial","country","location","longitude","latitude","elevation",
            "treatment","crop","planting_date","harvest_date","pl_ht","e_ht","p_asp","e_asp","moist","yield","yield_part")]
  
  d7 <- r7
  #removing rows from data set
  d7 <- d7[-c(25:50), ]
  
  d7 <- carobiner::change_names(d7,c("Entry","Yield t/ha","Rust P.sor     (1-5)","MSV      (1-5)",
                                     "GLS       (1-5)","Ear Asp.            (1-5)","ASI",
                                     "Stem Bor.                (1-5)","Plt. Hght (cm)","Plt. Asp.                    (1-5)",
                                     "Pedigree"),
                                c("trial_id","yield","rust","streak","gls","e_asp","asi","borer","pl_ht","p_asp","treatment"))
  
  d7$yield <- as.numeric(d7$yield) 
  d7$yield <- d7$yield*1000
  
  d7$dataset_id <- dataset_id
  d7$on_farm <- TRUE
  d7$striga_trial <- FALSE 
  d7$striga_infected <- FALSE
  d7$is_experiment <- TRUE
  d7$borer_trial <- TRUE
  
  ##### Location #####
  # Geographical information was provided in a separate
  #excel file -> 12CHTSPROA-Locations
  
  d7$country <- "Uganda"
  d7$location <- "Nalweyo, Kibaale District" 
  d7$longitude <- 0.9667
  d7$latitude <- 30.9833 
  d7$elevation <- 1007
  
  d7$crop <- "maize"
  
  #information on dates was provided in a separate excel file 12CHTSPROA-Locations
  d7$planting_date <- as.character(as.Date("2012-10-19"))
  d7$harvest_date  <- as.character(as.Date("2013-02-05"))
  d7$yield_part <- "grain" 
  
  d7<-d7[,c("dataset_id","trial_id","is_experiment","on_farm","striga_trial","striga_infected","borer_trial","country","location","longitude","latitude","elevation",
            "treatment","crop","planting_date","harvest_date","pl_ht","asi","e_asp","p_asp","rust","streak","gls","borer","yield","yield_part")]
  
  
  ## merge all 
  dd <- merge(d1, d2, all = TRUE)
  dd <- merge(dd, d3, all = TRUE)
  dd <- merge(dd, d4, all = TRUE)
  dd <- merge(dd, d5, all = TRUE)
  dd <- merge(dd, d6, all = TRUE)
  dd <- merge(dd, d7, all = TRUE)
   
    # all scripts must end like this
    carobiner::write_files(dset, d, path=path)
}

