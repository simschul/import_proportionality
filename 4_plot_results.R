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
# library(my.utils)
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
path2suppl <- tempdir()
  #'/home/simon/Documents/PhD_PROSET/tex/import_proportionality/supplementary'

############################################################################## # 
##### load data #############################################################
############################################################################## # 


# _a) national fp data ---------------------------------------------------------
national_fp_agg <- readRDS(file.path(path2results, 'national_fp_scatterplot.RData'))
national_fp_full <- readRDS(file.path(path2results, 'national_fp_distplot.RData'))


# _b) product fp data ---------------------------------------------------------
product_fp <- readRDS(file.path(path2results, 'product_fp_scatterplot.RData'))
product_fp_selected <- readRDS(file.path(path2results, 'product_fp_distplot.RData'))


############################################################################## # 
##### plots #############################################################
############################################################################## # 



# 1. national footprints --------------------------------------------------------------
# prepare plots

iregions <- list(Carbon = c('LUX', 'CHE',  'SVN'), 
                 Land = c('TWN', 'NLD', 'BEL'), 
                 Material = c('LUX', 'WWA', 'BEL'), 
                 Water = c('LUX', 'NLD', 'LTU'))

iregions_vec <- iregions %>% 
  as.data.table %>% 
  t() %>% 
  as.data.table(keep.rownames = TRUE) %>% 
  melt(id.vars = 'rn') %>% 
  .[, paste0(rn, '_', value)]


national_fp_agg[, CVs := scale(CV), by = fp_type]
national_fp_agg[, label_size := 1]
national_fp_agg[id_fp %in% iregions_vec, label_size := 1.2]

boxplot_data <- national_fp_agg[, list(boxplot.stats(CV)$stats), by = fp_type] %>% 
  .[, variable := rep(c("ymin", "lower", "middle", "upper", "ymax"), 4)] %>% 
  dcast(fp_type ~ variable, value.var = 'V1') %>% 
  cbind(national_fp_agg[, list(xmin = (min(fp_eb3)), 
                               xmax = (max(fp_eb3)), 
                               y2.5 = quantile(CV, 0.025), 
                               y97.5 = quantile(CV, 0.975)), by = fp_type][,2:5]) %>% 
  as.data.table
yrange <- 1/30
boxplot_data[, xlower := 10^ (log10(xmin) - yrange * (log10(xmax) - log10(xmin)))]
boxplot_data[, xupper := xmin]

# _a) scatterplot --------------------------------------------------------------
p1 <- ggplot(national_fp_agg, aes(x = fp_eb3, y = CV)) +
  geom_linerange(data = boxplot_data, 
                 aes(ymax = ymax, ymin = ymin, x = xlower), inherit.aes = FALSE) +
  geom_crossbar(data = boxplot_data, 
                aes(ymax = upper, ymin = lower, y = middle, x = xlower), 
                fill = 'white', inherit.aes = FALSE)  +
  geom_point(aes(col = import_share)) +
  geom_text_repel(data = national_fp_agg[id_fp %in% iregions_vec], 
                  aes(label = country_code2,col = import_share), 
                  size = 3, seed = 3) +
  geom_text_repel(data = national_fp_agg[!(id_fp %in% iregions_vec)], 
                  aes(label = country_code2,col = import_share), 
                  size = 2, seed = 3) +
  #scale_size(limits = c(NA, 1), breaks = unique(national_fp_agg$label_size)) +
  scale_x_log10() +
  scale_color_viridis(direction = -1, end = 1, option = 'viridis', 
                      breaks = seq(0, 1, .25), labels = percent, 
                      limits = c(0,1)) +
  facet_wrap(~fp_type, scales = 'free', ncol = 1) + 
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white', color = 'white'), 
        strip.text = element_text(size = rel(1), hjust = 0 ), 
        legend.position = 'bottom') + 
  guides(size = FALSE, col = guide_colorbar(barwidth = unit(100, 'mm'))) + 
  ylab('CV') + 
  labs(col = '%FP sourced \n from imports\n ') +
  xlab('Footprint size [Carbon: Mt CO2-eq | \nLand: 1000 km^2 | Material: Mt | Water: km^3]')  
p1


# _b) boxplots ----------------------------------------------------------------

ids <- as.data.table(iregions, keep.rownames = TRUE) %>% 
  t %>% 
  as.data.table(keep.rownames = 'fp_type') %>% 
  melt(id.vars = 'fp_type') %>% 
  .[, paste0(fp_type, '_', value)]

p2 <- ggplot(national_fp_full[id_fp %in% ids], 
             aes(y = value_norm, x = country_code2, 
                 col = import_share, fill = import_share)) + 
  geom_violin(aes(y = value_norm), 
              trim = FALSE, alpha = 0.2) + 
  stat_summary(fun.data =  percentile, geom = 'errorbar', width = .1) +
  stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2, fill = 'white') +
  stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2, alpha=0.4) +
  geom_hline(yintercept = 1, linetype = 'solid', col = my_cols[1]) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9)) + 
  xlab("") + ylab('') +
  facet_wrap(~fp_type, scales = "free", ncol = 1) +
  scale_fill_viridis(option = 'viridis', direction = -1, 
                     breaks = seq(0, 1, .25), labels = percent, 
                     limits = c(0,1)) +
  scale_color_viridis(option = 'viridis', direction = -1, 
                      breaks = seq(0, 1, .25), labels = percent, 
                      limits = c(0,1)) +
  theme(strip.background = element_rect(fill = 'white', color = 'white'), 
        strip.text = element_text(size = rel(1), hjust = 0 ))   + 
  ylab('Normalized footprints, 4897 simulations') + 
  geom_point(data = national_fp_agg[id_fp %in% ids], 
             aes(y = eb3_mean_norm, x = country_code2), 
             col = my_cols[7], size = 1.8) +
  geom_linerange(data = national_fp_agg[id_fp %in% ids],
                 aes(ymin = eb3_mean_norm, ymax = 1, x = country_code2), 
                 col = my_cols[7], inherit.aes = FALSE, size = 0.5) + 
  guides(fill = guide_colorbar(barheight = unit(100, 'mm'),
                               barwidth = unit(3, 'mm'),
                               title ='%FP sourced \n from imports\n '), 
         col = FALSE) +
  scale_y_continuous(position = "right")

p2


# _c) combine both into one plot ----------------------------------------------

ggarrange(p1, p2, ncol = 2, widths = c(3,1), common.legend = TRUE, 
          legend = 'right', legend.grob = get_legend(p2), 
          labels = 'AUTO', hjust = c(-0.5, 0.7))


ggsave(filename = "./figures/figure3.png", 
       width = 170, height = 200, units = 'mm', 
       dpi = 600)





# 2. product footprints --------------------------------------------------------
# prepare data
ylim <- 10

boxplot_data <- product_fp[, list(boxplot.stats(value)$stats), by = fp_type] %>% 
  .[, variable := rep(c("ymin", "lower", "middle", "upper", "ymax"), 4)] %>% 
  dcast(fp_type ~ variable, value.var = 'V1') %>% 
  cbind(product_fp[rank_fp > ylim, list(xmin = (min(fp_eb_total)), 
                                        xmax = (max(fp_eb_total)), 
                                        y2.5 = quantile(value, 0.025), 
                                        y97.5 = quantile(value, 0.975)), by = fp_type][,2:5])
yrange <- 1/30
boxplot_data[, xlower := 10^ (log10(xmin) - yrange * (log10(xmax) - log10(xmin)))]
boxplot_data[, xupper := xmin]


# kick out products with a very small fp

# _a) scatterplot --------------------------------------------------------------
p1 <- ggplot(product_fp[rank_fp > ylim], aes(x = fp_eb_total, y = value)) +
  geom_linerange(data = boxplot_data, 
                 aes(ymax = ymax, ymin = ymin, x = xlower), inherit.aes = FALSE) +
  geom_crossbar(data = boxplot_data, 
                aes(ymax = upper, ymin = lower, y = middle, x = xlower), 
                fill = 'white', inherit.aes = FALSE, width = 1)  +
  geom_point(aes(col = import_share, alpha = value_scaled)) + #, alpha = 0.2
  geom_text_repel(aes(label = label, col = import_share ), size = 3) +
  scale_x_log10() +
  scale_color_viridis(direction = -1, end = 1, option = 'viridis', 
                      breaks = seq(0, 1, .25), labels = percent, 
                      limits = c(0,1)) +
  facet_wrap(~fp_type, scales = 'free', ncol = 2) + 
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white', color = 'white'), 
        strip.text = element_text(size = rel(1), hjust = 0 ), 
        legend.position = 'bottom') + 
  guides(size = FALSE, alpha = FALSE, col = guide_colorbar(barwidth = unit(100, 'mm'))) + 
  
  labs(col = '%FP sourced \n from imports\n ') +
  ylab('CV') + 
  xlab('Footprint size [Carbon: t CO2-eq | \nLand: km^2 | Material: kg | Water: Mm^3]') 

p1
# p1 + 
#   #theme(legend.position = 'right') + 
#   #guides(size = FALSE, alpha = FALSE, col = guide_colorbar(barheight = unit(100, 'mm'))) +
#   facet_wrap(~fp_type, scales = 'free', ncol = 2)  



ggsave(filename = "./figures/figure4.png", 
       width = 170, height = 170, units = 'mm',dpi = 600)



# _b) boxplot --------------------------------------------------------------



ipoint.size <- 1.8
iline.size <- 0.5


product_fp_selected[, label := gsub(' ', '\n', label)]
product_fp[, id_fp := paste0(fp_type, id)]
product_fp_selected[, id_fp := paste0(fp_type, id)]

p2 <- ggplot(product_fp_selected[id_fp %in% product_fp[label != '']$id_fp], 
             aes(y = value_norm, x = label)) + #, col = import_share, fill = import_share 
  geom_jitter(shape=16, size = 0.3,
              position=position_jitter(0.15), alpha = 0.1, col = "grey20") +
  # stat_summary(aes(col = log(fp_eb_total)),
  #              fun.data =  median.quartile, geom = 'crossbar', width = .2, fill = 'white') +
  geom_violin(aes(y = value_norm), 
              trim = TRUE, alpha = 0.2, col = my_cols[2], fill = my_cols[2], 
              size = 0.5) + #, draw_quantiles = c(0.025,0.25,0.5,0.75,0.975) 
  stat_summary(fun.data = percentile, geom = 'errorbar', 
               width = .2, col = my_cols[3]) +
  stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2, 
               alpha=0.4, col = my_cols[3]) +
  geom_hline(yintercept = 1, linetype = 'solid', col = my_cols[1]) + 
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9)) + 
  scale_y_log10(position = "left") +
  geom_point(aes(y = fp_eb_norm),
             col = my_cols[7], size = ipoint.size, 
             shape = 19) +
  geom_linerange(aes(ymin = fp_eb_norm, ymax = 1),
                 col = my_cols[7], size = iline.size) +
  xlab("") + ylab("Normalized footprints, 4897 simulations") +
  facet_wrap(~fp_type, scales = "free", ncol = 2) +
  scale_fill_viridis(option = 'viridis', direction = -1, 
                     breaks = seq(0, 1, .25), labels = percent, 
                     limits = c(0,1)) +
  scale_color_viridis(option = 'viridis', direction = -1, 
                      breaks = seq(0, 1, .25), labels = percent, 
                      limits = c(0,1)) +
  #theme(legend.position = 'none') +
  guides(col =FALSE) +
  guides(fill = guide_colorbar(title.position = "top", barwidth = 5)) +
  theme(strip.background = element_rect(fill = 'white', color = 'white'), 
        strip.text = element_text(size = rel(1), hjust = 0 ), 
        legend.position = 'bottom') +
  # coord_flip() +
  labs(fill = "log abs FP")

p2 

ggsave(p2, filename = "./figures/figure5.png", 
       width = 170, height = 170, units = 'mm', dpi = 600)


# _c) combine both into one plot ----------------------------------------------




ggarrange(p1, p2, ncol = 2, widths = c(3,2), common.legend = TRUE, 
          legend = 'bottom', legend.grob = get_legend(p1), 
          labels = 'AUTO')

ggsave(filename = "./figures/product_fp_complete.png", 
       width = 170, height = 200, units = 'mm', dpi = 600)

# 3) save data behind the plots in nice format  ----------------------------------------------

# test <- layer_data(p1,3) %>% as.data.table
# ggplot_build(p1)$plot$data

national_fp_agg[, domestic := NULL]
national_fp_agg[, imported := NULL]
national_fp_agg[, id_fp := NULL]
national_fp_agg[, eb3_mean_norm := NULL]
national_fp_agg[, rank := frankv(CV, order = -1), by = fp_type]
national_fp_agg[, percentile2.5 := percentile2.5 * mean]
national_fp_agg[, percentile97.5 := percentile97.5 * mean]

setnames(national_fp_agg, 
         c('country_code2', 'fp_eb3'), 
         c('country_code', 'fp_impprop'))
setcolorder(national_fp_agg, c('fp_type', 'country_code', 'country', 'CV', 'mean', 
                               'median', 'percentile2.5', 'percentile97.5', 
                               'fp_impprop', 'import_share', 'rank'))
national_fp_list <- split(national_fp_agg, by = 'fp_type')
national_fp_list$Carbon[, unit := 'Mt CO2-eq']
national_fp_list$Land[, unit := '1000 km2']
national_fp_list$Material[, unit := 'Mt']
national_fp_list$Water[, unit := 'km3']


national_fp_list$variable_description <- data.table(
  variables = names(national_fp_list$Carbon), 
  description = c('Type of environmental footprint', 
                  '3-letter country codes according to ISO 3166-1 alpha-3 (except EXIOBASE RoW-regions)', 
                  'Country/Region name', 
                  'Coefficient of Variation', 
                  'Sample mean', 
                  'Sample median', 
                  '2.5th percentile of sample',
                  '97.5th percentile of sample', 
                  'Footprint size calculated with the default version of EXIOBASE V3.4', 
                  'The share of the footprint sourced from imports', 
                  'Rank by CV', 
                  'Unit of mean, median, percentiles, fp_improp'))

rio::export(national_fp_list, file.path(path2suppl,'national_footprints.xlsx'), 
            headerStyle = openxlsx::createStyle(textDecoration = "Bold"))


product_fp[, domestic := NULL]
product_fp[, imported := NULL]
product_fp[, value_scaled := NULL]
product_fp[, rank_fp := NULL]
product_fp[, label := NULL]
setnames(product_fp, 
         c('country_code2', 'industry163_code', 'value', 'fp_eb_total', 
           'fp_eb_intensity', 'industry163_name'),
         c('country_code', 'industry_code', 'CV', 'fp_impprop_total', 
           'fp_impprop_intensity', 'industry') 
)
setcolorder(product_fp, c('fp_type', 'id', 'country_code', 'industry_code', 'industry', 
                          'CV', 'mean', 'median', 'sd', 'percentile2.5', 'percentile97.5', 
                          'fp_impprop_total','fp_impprop_intensity', 'import_share', 'rank'))

product_fp_list <- split(product_fp, by = 'fp_type')
product_fp_list$Carbon[, unit := 't CO2-eq']
product_fp_list$Land[, unit := 'km2']
product_fp_list$Material[, unit := 'kg']
product_fp_list$Water[, unit := 'Mm3']

product_fp_list$variable_description <- data.table(
  variables = names(product_fp_list$Carbon), 
  description = c('Type of environmental footprint',
                  'ID of sector-country combination. Corresponds to the appearance in EXIOBASE.', 
                  '3-letter country codes according to ISO 3166-1 alpha-3 (except EXIOBASE RoW-regions)', 
                  'Industry code according to EXIOBASE industry classification',
                  'Industry name', 
                  'Coefficient of Variation', 
                  'Sample mean', 
                  'Sample median',
                  'Sample standard deviation',
                  '2.5th percentile of sample',
                  '97.5th percentile of sample', 
                  'Consumption-based product footprint calculated with the default version of EXIOBASE V3.4', 
                  'Footprint intensity calculated with the default version of EXIOBASE V3.4', 
                  'The share of the footprint sourced from imports', 
                  'Rank by CV', 
                  'Unit of mean, median, percentiles, and footprints'))

rio::export(product_fp_list, file.path(path2suppl,'product_footprints.xlsx'))




