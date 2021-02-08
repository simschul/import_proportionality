# 1. Paths ---------------------------------------------------------------------
path2data <- "./data"
#path2data <- "/home/simon/Documents/PhD_PROSET/data/EXIOBASE3/V3.6/IOT_2011_pxp"
path2plot <- "./plots"
path2temp_results <- "./temp_results"
path2results <- "./results"

# meta data
# EB3_metadata <- readRDS(file.path(path2data, "EB3_metadata.RData"))
# EB3_concordance <- readRDS(file.path(path2data, "EB3_concordance.RData"))
# EB3_midpoints <- readRDS(file.path(path2data, "EB3_midpoints.RData"))


#RhpcBLASctl::blas_set_num_threads(3)
# 2. Settings ---------------------------------------------------------------------
years <- 1995:2016
n_countries <- 49
n_products <- 200
n_dim <- n_countries * n_products
n_fdcat <- 7

#ids_fd <- EB3_metadata$fd[short %in% c("Households", "NPISH", "Government", "GFCF")]
# EB3_metadata$stressorsV3.6[grepl(combine_words_regexp(c("CO2")), 
#                                                  stressor_name, perl = TRUE)]
#ids_stressor <- c(1,70,71,405,415,416)
# EB3_metadata$colnames200[country_name == "Germany" & 
#                                         grepl("Motor vehicles", product200_name)]$id
#id_sector <- 1123

