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

# 1. National footprints -------------------------------------------------------
#temp <- readRDS('results/2020-09-25_23:06:40_Trade_Split_MC_results_ixi/national_fp1.RData')

national_fp <- list.files(path2results, pattern = "national_fp[0-9]", full.names = TRUE) %>% 
  lapply(., function(x) {
    x %>% 
      readRDS 
  })

national_fp <- lapply(national_fp, function(x){
  x %>% 
    as.data.table(keep.rownames = "fp_type") %>% 
    melt %>% 
    .[, variable := substring(variable, 2) %>% as.numeric] %>% 
    merge(., colnames_Y, by.x = "variable", by.y = "id") %>% 
    .[, variable := NULL]
}) %>% 
  rbindlist(idcol = "ID_run") %>% 
  .[]

saveRDS(national_fp, file.path(path2results, "national_fp_raw.RData"))



# 2. Product footprints -------------------------------------------------------
files <- list.files(path2results, pattern = "product_fp[0-9]", full.names = TRUE) 
length(files)

# _a) coef of var -------------------------------------------------------------
.calc_varcoef_fun <- function(i) {
  res <- files %>% 
    lapply(., function(x) {
      x %>% 
        readRDS %>% 
        .[i,]
    }) %>% 
    do.call(cbind, .) %>% 
    apply(., 1, coef_var)
  gc()
  return(res)
}

product_fp <- pbmclapply(1:4, .calc_varcoef_fun, mc.cores = 2)

product_fp <- lapply(product_fp, as.data.table) %>% 
  setNames(midpoints_labels$FootprintNames_EN) %>% 
  rbindlist(idcol = "fp_type") %>% 
  setnames("V1", "value") %>% 
  .[, "id" := rep(1:7987, times = length(midpoints_selected))]
product_fp[value == max(value)]

product_fp[, rank := frankv(value, order = -1), by = fp_type]

saveRDS(product_fp, file.path(path2results, "product_fp_CV.RData"))





# _a) MEAN -------------------------------------------------------------


.calc_mean_fun <- function(i) {
  res <- files %>% 
    lapply(., function(x) {
      x %>% 
        readRDS %>% 
        .[i,]
    }) %>% 
    do.call(cbind, .) %>% 
    apply(., 1, function(x) mean(x, na.rm = TRUE))
  gc()
  return(res)
}



product_fp_mean <- pbmclapply(1:4, .calc_mean_fun, mc.cores = 2)

product_fp_mean <- lapply(product_fp_mean, as.data.table) %>% 
  setNames(midpoints_labels$FootprintNames_EN) %>% 
  rbindlist(idcol = "fp_type") %>% 
  setnames("V1", "mean") %>% 
  .[, "id" := rep(1:7987, times = length(midpoints_selected))]

saveRDS(product_fp_mean, file.path(path2results, "product_fp_mean.RData"))


# _c) median -------------------------------------------------------------


.calc_median_fun <- function(i) {
  res <- files %>% 
    lapply(., function(x) {
      x %>% 
        readRDS %>% 
        .[i,]
    }) %>% 
    do.call(cbind, .) %>% 
    apply(., 1, function(x) median(x, na.rm = TRUE))
  gc()
  return(res)
}



product_fp_median <- pbmclapply(1:4, .calc_median_fun, mc.cores = 2)

product_fp_median <- lapply(product_fp_median, as.data.table) %>% 
  setNames(midpoints_labels$FootprintNames_EN) %>% 
  rbindlist(idcol = "fp_type") %>% 
  setnames("V1", "median") %>% 
  .[, "id" := rep(1:7987, times = length(midpoints_selected))]
saveRDS(product_fp_median, file.path(path2results, "product_fp_median.RData"))


# _d) 2.5 percentile -------------------------------------------------------------


.calc_percentile2.5_fun <- function(i) {
  res <- files %>% 
    lapply(., function(x) {
      x %>% 
        readRDS %>% 
        .[i,]
    }) %>% 
    do.call(cbind, .) %>% 
    apply(., 1, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
  gc()
  return(res)
}



product_fp_2.5 <- pbmclapply(1:4, .calc_percentile2.5_fun, mc.cores = 2)

product_fp_2.5 <- lapply(product_fp_2.5, as.data.table) %>% 
  setNames(midpoints_labels$FootprintNames_EN) %>% 
  rbindlist(idcol = "fp_type") %>% 
  setnames("V1", "percentile2.5") %>% 
  .[, "id" := rep(1:7987, times = length(midpoints_selected))]
saveRDS(product_fp_2.5, file.path(path2results, "product_fp_2.5.RData"))

# _e) 97.5 percentile -------------------------------------------------------------


.calc_percentile97.5_fun <- function(i) {
  res <- files %>% 
    lapply(., function(x) {
      x %>% 
        readRDS %>% 
        .[i,]
    }) %>% 
    do.call(cbind, .) %>% 
    apply(., 1, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
  gc()
  return(res)
}



product_fp_97.5 <- pbmclapply(1:4, .calc_percentile97.5_fun, mc.cores = 2)

product_fp_97.5 <- lapply(product_fp_97.5, as.data.table) %>% 
  setNames(midpoints_labels$FootprintNames_EN) %>% 
  rbindlist(idcol = "fp_type") %>% 
  setnames("V1", "percentile97.5") %>% 
  .[, "id" := rep(1:7987, times = length(midpoints_selected))]
saveRDS(product_fp_97.5, file.path(path2results, "product_fp_97.5.RData"))

#### boxplot stats ----------------------------------------------------
.calc_varcoef_fun <- function(i) {
  res <- files %>% 
    lapply(., function(x) {
      x %>% 
        readRDS %>% 
        .[i,]
    }) %>% 
    do.call(cbind, .) %>% 
    apply(., 1, boxplot.stats)
  gc()
  return(res)
}
product_fp_boxplot <- pbmclapply(1:4, .calc_varcoef_fun, mc.cores = 2)

ggplot(product_fp_boxplot[[1]][[1000]])
test <- do.call(rbind, product_fp_boxplot[[1]])

product_fp_boxplot <- lapply(product_fp_boxplot, as.data.table) %>% 
  setNames(midpoints_labels$FootprintNames_EN) %>% 
  rbindlist(idcol = "fp_type") %>% 
  setnames("V1", "value") %>% 
  .[, "id" := rep(1:7987, times = length(midpoints_selected))]
product_fp[value == max(value)]

product_fp[, rank := frankv(value, order = -1), by = fp_type]
product_fp[rank == 1]

# THE END ---------------------------------------------------------------------









