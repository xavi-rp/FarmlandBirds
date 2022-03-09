

###############################################
####        Downloading occurrences        ####
####           of Farmland Bird            ####
####             Index species             ####
###############################################

if(Sys.info()[4] == "D01RI1700308") {
  wd <- ""
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- ""
}else if(Sys.info()[4] == "L2100739RI") {
  wd <- "C:/Users/rotllxa/D5_FFGRCC_FarmlandBirds/"
  gbif_creds <- "C:/Users/rotllxa/"
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", "jeodpp-terminal-03")) {
  if(!dir.exists("/eos/jeodpp/home/users/rotllxa/FarmlandBirds/")) 
    dir.create("/eos/jeodpp/home/users/rotllxa/FarmlandBirds/")
  wd <- "/eos/jeodpp/home/users/rotllxa/FarmlandBirds/"
  gbif_creds <- "/home/rotllxa/Documents/"
}else{
  wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/"
  gbif_creds <- "/Users/xavi_rp/Dropbox/GBIF_credentials/"
}

setwd(wd)

library(tidyr)
library(data.table)
library(devtools)
library(ggplot2)
install_github("xavi-rp/PreSPickR", 
               ref = "v2", 
               INSTALL_opts = c("--no-multiarch"))  # https://github.com/rstudio/renv/issues/162
library(PreSPickR)



list.files()
fbi_sps <- read.csv("/home/rotllxa/Documents/FarmlandBirds/sp_list.csv", header = FALSE)
head(fbi_sps)
length(fbi_sps$V2)



## Checking number of occs in GBIF (EU-27) ####

sp_1_key <- as.data.frame(name_backbone(name='Iphiclides podalirius'))$speciesKey

countr <- c("BE", "EL", "LT", "PT", "BG", "ES", "LU", "RO", "CZ", "FR", "HU", "SI", "DK", "HR", "MT", "SK", "DE", "IT", "NL", "FI", "EE", "CY", "AT", "SE", "IE", "LV", "PL")
countr <- sort(countr)
length(countr)


num_eu_occs_df <- c()
count <- 1
for(sp in fbi_sps$V2){
  sp_key <- as.data.frame(name_backbone(name = sp))$usageKey
  num_eu_occs <- 0
  for(c in countr){
    num_occs <- occ_count(taxonKey = sp_key,
                          country = c,
                          from = 1990,
                          to = 2022)
    num_eu_occs <- num_eu_occs + num_occs
  }
  num_eu_occs_df <- rbind(num_eu_occs_df, data.frame(sp, sp_key, num_eu_occs))
  print(paste0(sp, " - sp ", count, ": ", num_eu_occs))
  count <- count + 1
}

as.data.table(num_eu_occs_df)
num_eu_occs_df[num_eu_occs_df$num_eu_occs == min(num_eu_occs_df$num_eu_occs), ]

write.csv(num_eu_occs_df, "Number_occs_sp_EU.csv", row.names = FALSE)
num_eu_occs_df <- fread("Number_occs_sp_EU.csv", header = TRUE)
num_eu_occs_df <- num_eu_occs_df[order(num_eu_occs_df$sp), ]

library(viridis)

pdf("num_occs_GBIF.pdf", width = 7, height = 9)
num_eu_occs_df %>%
  ggplot(aes(x = reorder(sp, desc(sp)), y = num_eu_occs)) + 
  geom_bar(stat = "identity", fill = viridis(39)) +
  ggtitle("Farmland Birds") +
  labs(x = "Species", y = "Number of Occurrences GBIF (1990-2022)") +
  #theme(plot.title = element_text(color="red", size=14, face="bold.italic")) +
  theme(plot.title = element_text(hjust = 0.3, size = 14, face = "bold")) +
  coord_flip()
dev.off()



#library(lattice)
#barchart(sp ~ num_eu_occs, data = num_eu_occs_df, 
#         main = "Farmland Birds",
#         xlab = "Number of Occurrences GBIF (1990-2022)",
#         ylab = "Species",
#         col = viridis(2))

num_eu_occs_df[order(num_eu_occs_df$num_eu_occs), ]




## Downloading the data ####

taxons <- fbi_sps$V2

t0 <- Sys.time()
GetBIF(credentials = paste0(gbif_creds, "/gbif_credentials.RData"),
       taxon_list = taxons,
       download_format = "SIMPLE_CSV",
       download_years = c(1990, 2021),
       download_coords = c(-13, 48, 35, 72), #order: xmin, xmax, ymin, ymax
       download_coords_accuracy = c(0, 250),
       rm_dupl = TRUE,
       cols2keep = c("species", "decimalLatitude", "decimalLongitude", #"elevation",
                     "gbifID",
                     "coordinateUncertaintyInMeters",
                     "countryCode", "year", 
                     #"institutionCode",	"collectionCode",
                     #"ownerInstitutionCode",
                     "datasetKey"),
       out_name = paste0("sp_records_", format(Sys.Date(), "%Y%m%d")))

Sys.time() - t0


## if GetBIF didn't manage to create/write out the data frame with presences:
taxon_dir <- getwd()
#taxons <- taxons$sp
data1 <- Prep_BIF(taxon_dir = paste0(taxon_dir, "/"),
                  taxons = taxons,
                  cols2keep = c("species", "decimalLatitude", "decimalLongitude", #"elevation",
                                "gbifID",
                                "coordinateUncertaintyInMeters",
                                "countryCode", "year", 
                                #"institutionCode",	"collectionCode",
                                #"ownerInstitutionCode",
                                "datasetKey"
                  ),
                  #cols2keep = "all",
                  rm_dupl = TRUE)

head(data1)
nrow(data1)
unique(data1$species)
if(length(unique(data1$species)) != length(unique(data1$sp2))) print("Check the error in 'sp2'!!!")

table(data1$species)

data_sp_year <- data1[, .SD, .SDcols = c("species", "year")] %>% group_by(species) %>% table
data_sp_year
apply(data_sp_year, 2, sum)  # 2017 is the year with more occurrences

data1_2018 <- data1[year == 2018, ]
data1_2018[, .SD, .SDcols = c("species", "countryCode")] %>% group_by(species) %>% table



## Saving the data as a csv
print(paste0("Saving GBIF data as ", "/sp_records_20220308", ".csv"))
write.csv(data1, file = paste0("sp_records_20220308", ".csv"),
          quote = FALSE, row.names = FALSE)


data <- fread(paste0("sp_records_20220308", ".csv"), header = TRUE)
data



## Falco tinnunculus
#load("download_info_Falco tinnunculus.RData", verbose = TRUE)
#rqst_02_meta
#
#falco <- fread("0175876-210914110416597.csv", header = TRUE, nrows = 220848)
#falco <- fread("0175876-210914110416597.csv", header = TRUE, quote = "")
##falco <- read.csv("0175876-210914110416597.csv", header = TRUE, sep = "\t")
#nrow(falco)
#as.data.table(falco)
#





## Citing information ####
# see https://www.gbif.org/citation-guidelines

load("download_info_Alauda arvensis.RData", verbose = TRUE)
citation_02

