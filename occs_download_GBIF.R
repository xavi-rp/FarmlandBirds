

###############################################
####      Downloading occurrences of       ####
####         Farmland Bird Index           ####
###############################################

if(Sys.info()[4] == "D01RI1700308") {
  wd <- ""
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- ""
}else if(Sys.info()[4] == "L2100739RI") {
  wd <- "C:/Users/rotllxa/D5_FFGRCC_FarmlandBirds/"
  gbif_creds <- "C:/Users/rotllxa/"
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", "jeodpp-terminal-03")) {
  if(!dir.exists("/eos/jeodpp/home/users/rotllxa/European_butterflies_SDMs_data/")) 
    dir.create("/eos/jeodpp/home/users/rotllxa/European_butterflies_SDMs_data/")
  wd <- "/eos/jeodpp/home/users/rotllxa/European_butterflies_SDMs_data/"
  gbif_creds <- "/home/rotllxa/Documents/"
}else{
  wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/European_butterflies_SDMs_data"
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
fbi_sps <- read.csv("sp_list.csv", header = FALSE)
head(fbi_sps)
length(fbi_sps$V2)


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
