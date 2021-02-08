#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2020-04-15 10:50:03
#' 
#' Content:
#'  

############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(data.table)
library(tidyverse)
library(my.utils)
library(pbmcapply)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
options("datatable.print.class" = TRUE)

get_last_modified_file <- function(path, full.names = FALSE) {
  path %>% 
    list.files(full.names = full.names, pattern = 'Trade_Split_MC_results') %>% 
    sort(., decreasing = TRUE) %>% 
    .[1]  
} 

path2results <- get_last_modified_file("results", TRUE)
EB3_metadata <- readRDS("./data/EB3_metadata.RData")

colnames_Y <- data.table("country" = rep(EB3_metadata$regions$country_name, each = 4) %>% 
                           as.factor, 
                         "fd_cat" = rep(EB3_metadata$fd$short[1:4], times = 49), 
                         "id" = 1:(4*49))
coef_var <- function(x) {
  cv <- sqrt(var(x)) / mean(x)
  return(ifelse(is.finite(cv), cv, 0))
}



midpoints_selected <-  c(5,9,23,32)
midpoints_labels <- data.table(YLabels_DE = c('CO2-Emissionen (Mt CO$_2$-eq).',
                                              'Landnutzung (1000 km^2).',
                                              'Material-Entnahme (Mt).',
                                              'Frischwasser-Entnahme (km3).'),
                               FootprintNames_DE    = c('Klima','Land','Material','Wasser'), 
                               YLabels_EN = c('Carbon emissions (Mt CO$_2$-eq).','Land use (1000 km2).','Material extraction (Mt).','Blue water use (km3).'), 
                               FootprintNames_EN = c('Carbon','Land','Material','Water'))

############################################################################## # 
##### load data #############################################################
############################################################################## # 
# 0. Exiobase footprints -------------------------------------------------------
product_fp_EB3 <- readRDS('results/product_fp2011_EB3.RData')
national_fp_EB3 <- readRDS('results/national_fp2011_EB3.RData')


# 1. National footprints -------------------------------------------------------
national_fp <- readRDS(file.path(path2results, "national_fp_raw.RData"))

national_fp_agg <- national_fp[, sum(value, na.rm = TRUE), 
                               by = .(fp_type, country, ID_run)] 

national_fp_agg[grepl('Global', fp_type), fp_type := 'Carbon']
national_fp_agg[grepl('Land', fp_type), fp_type := 'Land']
national_fp_agg[grepl('Water', fp_type), fp_type := 'Water']
national_fp_agg[grepl('Domestic', fp_type), fp_type := 'Material']
# convert to right units:
# from kg to Mt: /1E9
# from km2 to 1000km2: /1E3
# from kt to Mt: /1E3
# from Mm3 to km3: /1E3
conv_rate <- list(Carbon = 1E-9, 
                  Land = 1E-3,
                  Material = 1E-3,
                  Water = 1E-3)
for (i in names(conv_rate)) {
  national_fp_agg[fp_type == i, V1 := V1*conv_rate[[i]]]
  national_fp_EB3[fp_type == i, value := value*conv_rate[[i]]]
}

national_fp_agg[, "country_mean" := mean(V1), by = .(fp_type, country)]
national_fp_agg[, "country_median" := median(V1), by = .(fp_type, country)]
national_fp_agg[, "value_norm" := V1/country_mean]
national_fp_agg[is.nan(value_norm), value_norm := NA]



# merge with EB3 footprints
national_fp_agg <- merge(national_fp_agg, national_fp_EB3, by = c('fp_type', 'country'))
setnames(national_fp_agg, 'value', 'value_EB3')


saveRDS(national_fp_agg, file.path(path2results, "national_fp_agg.RData"))
#national_fp_agg <- readRDS(file.path(path2results, "national_fp_agg.RData"))



# 2. Product footprints -------------------------------------------------------
product_fp_CV <- readRDS(file.path(path2results, "product_fp_CV.RData"))
product_fp_mean <- readRDS(file.path(path2results, "product_fp_mean.RData"))
product_fp_median <- readRDS(file.path(path2results, "product_fp_median.RData"))
product_fp_2.5 <- readRDS(file.path(path2results, "product_fp_2.5.RData"))
product_fp_97.5 <- readRDS(file.path(path2results, "product_fp_97.5.RData"))

# merge with EB3 footprints
product_fp <- merge(product_fp_CV, product_fp_EB3, by = c('fp_type', 'id')) %>% 
  merge(product_fp_mean, by = c('fp_type', 'id')) %>% 
  merge(product_fp_median, by = c('fp_type', 'id')) %>% 
  merge(product_fp_2.5, by = c('fp_type', 'id')) %>% 
  merge(product_fp_97.5, by = c('fp_type', 'id')) 
  

# convert to right units:
# from kg to t: /1E3
# from km2 to 1000km2: 1
# from kt to Mt: /1
# from Mm3 to km3: /1
conv_rate_products <- list(Carbon = 1E-3, 
                  Land = 1,
                  Material = 1,
                  Water = 1)
for (i in names(conv_rate_products)) {
  product_fp[fp_type == i, fp_eb_total := fp_eb_total*conv_rate_products[[i]]]
  product_fp[fp_type == i, fp_eb_intensity := fp_eb_intensity*conv_rate_products[[i]]]
  product_fp[fp_type == i, mean := mean*conv_rate_products[[i]]]
  product_fp[fp_type == i, median := median*conv_rate_products[[i]]]
  product_fp[fp_type == i, percentile2.5 := percentile2.5*conv_rate_products[[i]]]
  product_fp[fp_type == i, percentile97.5 := percentile97.5*conv_rate_products[[i]]]
  
}

saveRDS(product_fp, file.path(path2results, "product_fp.RData"))
# product_fp <- readRDS(file.path(path2results, "product_fp.RData"))








# _d) only selected sectors, but all samples -------------------------------------------------------------
product_fp_split <- split(product_fp, f = product_fp$fp_type)
isectors <- lapply(product_fp_split, function(x) {
  x[rank < 6 | (fp_eb_total > median(fp_eb_total) & 
                  value > 0.75*max(value))]
})

product_fp_selected <- files %>% 
  lapply(., function(x) {
    mat <- readRDS(x)
    list <- create_named_list(names(product_fp_split))
    for (i in 1:4) {
      list[[i]] <- data.table(id = isectors[[i]]$id, 
                              value = mat[i,isectors[[i]]$id])
    }
    list <- rbindlist(list, idcol = 'fp_type')
    return(list)
  }) %>% 
  rbindlist(idcol = "ID_run") %>% 
  .[]

product_fp_selected[, "sector_mean" := mean(value), by = .(fp_type, id)]
product_fp_selected[, "sector_median" := median(value), by = .(fp_type, id)]
product_fp_selected[, "value_norm" := value/sector_mean]
product_fp_selected[is.nan(value_norm), value_norm := NA]

# merge with EB3 footprints
product_fp_selected <- merge(product_fp_selected, product_fp_EB3, by = c('fp_type', 'id'))

saveRDS(product_fp_selected, file.path(path2results, "product_fp_selected.RData"))





# THE END ---------------------------------------------------------------------









