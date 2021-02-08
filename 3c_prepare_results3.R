#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2020-09-11 09:57:42
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(data.table)
library(tidyverse)
library(my.utils)
library(viridis)
library(plotly)
library(gridExtra)
library(Rfast)
library(ggrepel)
library(ggthemes)
library(grid)
library(scales)
library(ggpubr)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
options("datatable.print.class" = TRUE)
theme_set(theme_bw())
my_scale_fill <-scale_fill_colorblind()
my_cols <- (colorblind_pal()(8))
show_col(my_cols)

EB3_metadata <- readRDS("./data/EB3_metadata.RData")

get_last_modified_file <- function(path, full.names = FALSE) {
  path %>% 
    list.files(full.names = full.names, pattern = 'Trade_Split_MC_results') %>% 
    sort(., decreasing = TRUE) %>% 
    .[1]  
} 

weighted.var <- function (x, w = NULL, na.rm = FALSE){
  # from:   https://rdrr.io/github/hadley/bigvis/man/weighted.var.html
  if (na.rm) {
    na <- is.na(x) | is.na(w)
    x <- x[!na]
    w <- w[!na]
  }
  sum(w * (x - weighted.mean(x, w))^2)/(sum(w) - 1)
}

weighted.varcoef <- function (x, w = NULL, na.rm = FALSE) {
  sqrt(weighted.var(x = x, w = w, na.rm = na.rm)) / mean(x, na.rm = na.rm)
}

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

median.quartile <- function(x){
  out <- quantile(x, probs = c(0.25,0.5,0.75))
  names(out) <- c("ymin","y","ymax")
  return(out) 
}

percentile <- function(x) {
  out <- quantile(x, probs = c(0.025,0.975))
  names(out) <- c("ymin","ymax")
  return(out)
}

colnames_Y <- data.table("country" = rep(EB3_metadata$regions$country_name, each = 4) %>% 
                           as.factor, 
                         "fd_cat" = rep(EB3_metadata$fd$short[1:4], times = 49), 
                         "id" = 1:(4*49))


path2results <- get_last_modified_file("results", TRUE)

############################################################################## # 
##### load data #############################################################
############################################################################## # 

# _a) import shares ---------------------------------------------------------

imp_shares_national <- readRDS('results/imp_shares_national.RData')
imp_shares_product <- readRDS('results/imp_shares_product.RData')


# _b) national fp data ---------------------------------------------------------

# __i. data to plot sample distributions ---------------------------------------
national_fp_agg <- readRDS(file.path(path2results, "national_fp_agg.RData")) %>% 
  merge(EB3_metadata$regions[,c('country_name' ,'country_code2' )], 
        by.x = 'country', by.y = 'country_name')
national_fp_agg[, 'percentile2.5' := quantile(value_norm, 0.025), 
                by = .(fp_type, country)]
national_fp_agg[, 'percentile97.5' := quantile(value_norm, 0.975), 
                by = .(fp_type, country)]
national_fp_agg <- merge(national_fp_agg,imp_shares_national, 
                         by = c('fp_type', 'country_code2'))
national_fp_agg[, id_fp := paste0(fp_type, '_', country_code2)]

saveRDS(national_fp_agg, file.path(path2results, 'national_fp_distplot.RData'))


# __ii. aggregated data using summary statitstics ---------------------------------------

national_fp_agg2 <- national_fp_agg[, list(eb3_mean_norm = mean(value_EB3) / mean(country_mean), 
                                           percentile2.5 = quantile(value_norm, 0.025),
                                           percentile97.5 = quantile(value_norm, 0.975), 
                                           CV = coef_var(V1), 
                                           mean = mean(country_mean), 
                                           median = mean(country_median),
                                           fp_eb3 = mean(value_EB3)),
                                    by = .(country, fp_type, country_code2)]
national_fp_agg2 <- merge(national_fp_agg2,imp_shares_national, 
                          by = c('fp_type', 'country_code2'))
national_fp_agg2[, id_fp := paste0(fp_type, '_', country_code2)]

saveRDS(national_fp_agg2, file.path(path2results, 'national_fp_scatterplot.RData'))


# _c) product fp data ---------------------------------------------------------
# __i. data for scatterplots ---------------------------------------

product_fp <- readRDS(file.path(path2results, "product_fp.RData")) %>% 
  merge(., EB3_metadata$colnames163i[, c("id", "country_code2", "industry163_code", 
                                         'industry163_name')], 
        by = "id")%>% .[]
product_fp <- product_fp[fp_eb_total > 0 & value > 0 & fp_eb_intensity > 0]

product_fp[, rank := frankv(value, order = -1), by = fp_type]
# product_fp <- merge(product_fp, data.table(id = 1:7987, x = data$x), by = 'id')
product_fp[, sd := value * mean]
product_fp <- merge(product_fp,imp_shares_product, 
                    by = c('fp_type', 'id', 'country_code2', 'industry163_code'))

product_fp[, label := ""]
product_fp[rank < 6 | (fp_eb_total > median(fp_eb_total) & value > 0.75*max(value)), 
           label := paste(country_code2, industry163_code, sep = ': '), by = fp_type]

product_fp[, value_scaled := scale(value), by = fp_type]
product_fp[, rank_fp := frankv(fp_eb_total, order = 1), by = fp_type]


saveRDS(product_fp, file.path(path2results, 'product_fp_scatterplot.RData'))

# __ii. data to plot sample distributions for selected sectors ---------------------------------------

product_fp_selected <- readRDS(file.path(path2results, "product_fp_selected.RData")) %>% 
  merge(EB3_metadata$colnames163i[, c("id", "country_code2", "industry163_code")], 
        by = "id")
product_fp_selected[, fp_eb_norm := fp_eb_intensity / sector_mean]
product_fp_selected[, label := paste0(country_code2, ': ', industry163_code)]
product_fp_selected <- merge(product_fp_selected,imp_shares_product, 
                             by = c('fp_type', 'id', 'country_code2', 'industry163_code'))

saveRDS(product_fp_selected, file.path(path2results, 'product_fp_distplot.RData'))



# THE END ---------------------------------------------------------------------



















