#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2020-12-14 11:36:16
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(data.table)
library(tidyverse)
library(Rfast)
library(my.utils)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
options("datatable.print.class" = TRUE)
theme_set(theme_bw())


path2data <- file.path('./data')
EB3_metadata <- readRDS(file.path(path2data,"EB3_metadata.RData"))

midpoints_selected =  c(5,9,23,32)
midpoints_labels <- data.table(YLabels_DE = c('CO2-Emissionen (Mt CO$_2$-eq).','Landnutzung (1000 km^2).','Material-Entnahme (Mt).','Frischwasser-Entnahme (km3).'),
                               FootprintNames_DE    = c('Klima','Land','Material','Wasser'), 
                               YLabels_EN = c('Carbon emissions (Mt CO$_2$-eq).','Land use (1000 km2).','Material extraction (Mt).','Blue water use (km3).'), 
                               FootprintNames_EN = c('Carbon','Land','Material','Water'))

############################################################################## # 
##### functions #################################################################
############################################################################## # 

aggregate_Y <- function(Y, groupings) {
  if (length(groupings) != ncol(Y)) stop('groupings need to have the same length as nrow(Y)')
  grouping_levels <- unique(groupings)
  n_groups <- length(grouping_levels)
  Ynew <- matrix(0, nrow = nrow(Y), ncol = n_groups)
  colnames(Ynew) <- grouping_levels
  for (i in 1:n_groups) {
    Ynew[,i] <- rowsums(Y[, groupings == grouping_levels[i]])
  }
  return(Ynew)
}


as.sparse.matrix <- function(mat, rownames = NULL, colnames = NULL, 
                             suffices = c('.row', '.col')) {
  
  ### WARNING: do not modify function here. Do it in my.utils package!!! 
  mat <- data.table::as.data.table(mat)
  
  if (!(is.null(rownames) | is.null(colnames))) {
    # check for duplicates
    dup_rows <- colnames(rownames) %in% colnames(colnames)
    dup_cols <- colnames(colnames) %in% colnames(rownames)
    colnames(rownames)[dup_rows] <- paste0(colnames(rownames)[dup_rows], suffices[1])
    colnames(colnames)[dup_cols] <- paste0(colnames(colnames)[dup_cols], suffices[2])
  }
  
  if (is.null(rownames)) {
    rownames <- data.table(row = 1:nrow(mat))
  }
  mat <- cbind(rownames, mat)
  mat <- data.table::melt(mat, id.vars = colnames(rownames), 
                          na.rm = TRUE,
                          variable.name = 'col')
  mat <- mat[value > 0]
  mat[, col := as.integer(substring(col, 2))]
  if (is.null(colnames)) {
  } else {
    mat <- merge(mat, cbind(colnames, col = (1:nrow(colnames))), 
                 by = 'col')
    mat[, col := NULL]
  }
  
  
  setcolorder(mat, c(colnames(rownames), colnames(colnames)))
  return(mat[])
}

############################################################################## # 
##### load data #############################################################
############################################################################## # 
data <- readRDS(file.path(path2data, "IOT_2011_ixi.RData"))

# Characterizisation factors
EB3_midpoints <- readRDS(file.path(path2data, "EB34_midpoints.RData")) %>% 
  .[["matrix"]] %>% 
  .[, midpoints_selected]


# 1. Calculate product and national footprints --------------------------------
# _a) calculations ------------------------------------------------------------
# Pre-calculate C * S (to only do it once + save RAM)
CS <- t(EB3_midpoints) %*% data$S
rm(EB3_midpoints)
data$S <- NULL
gc()

data$L <- calculate_L(calculate_A(data$Z, data$x))

# footprint by industry and fp type
product_fp_total <- CS %*% data$L %*% 
  diag(data$Y[,EB3_metadata$colnamesY$id_orig] %>% rowSums)  # fp = C * S * L * y^
dim(product_fp_total)

product_fp_intensity <- CS %*% data$L # fp = C * S * L 
dim(product_fp_intensity)

# national fp's
national_fp <- data$L %>%  
`%*%`(., (data$Y[,EB3_metadata$colnamesY$id_orig])) %>%
  `%*%`(CS, .) # fp = CSLY
dim(national_fp)


# _b) transform to data.table ---------------------------------------------------

product_fp_total <- product_fp_total %>%  
  as.data.table %>% 
  .[, fp_type := midpoints_labels$FootprintNames_EN] %>% 
  melt(id.vars = 'fp_type', variable.name = 'id', variable.factor = FALSE) %>% 
  .[, id := as.numeric(substring(id, 2))] %>% 
  .[]
setnames(product_fp_total, 'value', 'fp_eb_total')

product_fp_intensity <- product_fp_intensity %>%  
  as.data.table %>% 
  .[, fp_type := midpoints_labels$FootprintNames_EN] %>% 
  melt(id.vars = 'fp_type', variable.name = 'id', variable.factor = FALSE) %>% 
  .[, id := as.numeric(substring(id, 2))] %>% 
  .[]
setnames(product_fp_intensity, 'value', 'fp_eb_intensity')

product_fp <- merge(product_fp_total, product_fp_intensity, 
                    by = c('fp_type', 'id'))

national_fp <- national_fp %>% 
  as.data.table %>% 
  setnames(as.character(1:ncol(.))) %>% 
  .[, fp_type := midpoints_labels$FootprintNames_EN] %>% 
  melt(id.vars = 'fp_type', variable.name = 'id', variable.factor = FALSE) %>% 
  .[, id := as.numeric(id)] %>% 
  .[]

national_fp <- merge(national_fp, EB3_metadata$colnamesY, by = 'id') %>% 
  .[, list(value = sum(value, na.rm = TRUE)), by = .(fp_type, country)]


# save results
saveRDS(product_fp, 'results/product_fp2011_EB3.RData')
saveRDS(national_fp, 'results/national_fp2011_EB3.RData')



# 2. Calculate import shares  --------------------------------
# _a) national footprints ----------------------------------
Y <- aggregate_Y(data$Y[,EB3_metadata$colnamesY$id_orig], EB3_metadata$colnamesY$country)
x_mat <- data$L %*% Y

imp_shares_national <- lapply(1:nrow(CS), function(i) {
  fp <- diag(CS[i,]) %*% x_mat # CS^ * L * Y
  fp %>% 
    as.data.table %>% 
    .[, id := 1:nrow(data$L)] %>% 
    melt(id.vars = 'id') %>% 
    merge(EB3_metadata$regions[, c('country_name', 'country_code2')], 
          by.x = 'variable', by.y = 'country_name') %>% 
    .[, variable := NULL] %>%
    .[, list(value = sum(value)), by = .(id, country_code2)] %>%
    merge(EB3_metadata$colnames163i[, c('id', 'country_code2', 'industry163_code')], 
          by = 'id', suffixes = c('', '_industry')) %>% 
    .[, origin := 'domestic'] %>% 
    .[country_code2 != country_code2_industry, origin := 'imported'] %>% 
    .[, list(value = sum(value)), by = .(country_code2, origin)] %>% 
    dcast(country_code2 ~ origin, value.var = 'value') %>% 
    .[, import_share := imported / (imported + domestic)] %>% 
    .[]
}) %>% 
  setNames(midpoints_labels$FootprintNames_EN)

imp_shares_national <- rbindlist(imp_shares_national, idcol = 'fp_type')
saveRDS(imp_shares_national, 'results/imp_shares_national.RData')

# _b) product footprints -------------------------------------------------------

imp_shares_product <- lapply(1:nrow(CS), function(i) {  
  fp <- diag(CS[i,]) %*% data$L %*% diag(rowsums(Y)) # fp = CS^ * L * y^
  fp <- as.sparse.matrix(fp, rownames = EB3_metadata$colnames163i[, c('country_code2')], 
                         colnames = EB3_metadata$colnames163i[, c('id', 'country_code2', 'industry163_code')], 
                         suffices = c('_source', '_product'))
  
  fp[, origin := 'domestic']
  fp[country_code2_source != country_code2_product, origin := 'imported'] 
  fp <- fp[, list(value = sum(value)), by = .(id, country_code2_product,
                                              industry163_code, origin)] %>% 
    dcast(country_code2_product + industry163_code + id ~ origin, 
          value.var = 'value')
  fp[, import_share := imported / (imported + domestic)] 
  return(fp[])
}) %>% 
  setNames(midpoints_labels$FootprintNames_EN)


imp_shares_product <- rbindlist(imp_shares_product, idcol = 'fp_type')
setnames(imp_shares_product, 'country_code2_product', 'country_code2')
saveRDS(imp_shares_product, 'results/imp_shares_product.RData')





# THE END ---------------------------------------------------------------------


















