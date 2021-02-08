#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2019-10-31 16:55:52
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 
sessionInfo()

library(data.table)
library(tidyverse)
library(parallel)
library(tictoc)
library(Rfast)

############################################################################## # 
##### settings #################################################################
############################################################################## # 

getwd()
setwd("./trade_split")
getwd()

source("./settings.R")
source("./functions.R")
#path2data <- '/home/simon/Documents/PhD_PROSET/code/R/Import_shares/data'

midpoints_selected =  c(5,9,23,32)

path2current_results <- file.path(path2results,
                                  paste0(Sys.time() %>% format(format = "%Y-%m-%d_%H:%M:%S"), 
                                         "_Trade_Split_MC_results_ixi"))


dir.create(path2current_results)
file.copy(from = list.files(".", pattern = ".R$", 
                            full.names = TRUE), 
          to = path2current_results)

n_countries <- 49
n_products <- 163
n_dim <- n_countries * n_products

# Functions --------------------------------------------------------------------


randomize_flows2 <- function(x,y) {
  new_flow_mat <- matrix(0, nrow = length(y), 
                         ncol = length(x), 
                         dimnames = list(names(y), 
                                         names(x)))
  #y = supply, x = use
  yleft <- y 
  j <- 1
  for (i in sample(1:length(x))) {
    #cat(i, "")
    # go randomly through all industries
    xleft <- x[i]
    while (xleft > 0 & j <= length(y)) {
      # as long as industry i still requires supply
      if (yleft[j] <= xleft) {
        # supply of country j is smaller (or equal) than use of industry i
        new_flow_mat[j,i] <- yleft[j]
        xleft <- xleft - yleft[j]
        j <- j + 1 # go to next country
      } else {
        # supply of country j is larger than use of industry i
        new_flow_mat[j,i] <- xleft
        yleft[j] <- yleft[j] - xleft
        xleft <- 0
      }
    }
  }
  return(new_flow_mat)
}

############################################################################## # 
##### load data #############################################################
############################################################################## # 
data <- readRDS(file.path(path2data, "IOT_2011_ixi.RData"))

# Z matrix as array
Z <- data$Z %>% as.matrix %>% 
  array(., dim = c(n_products,n_countries,n_products,n_countries))
# Y matrix as array
Y <- data$Y %>% 
  array(., dim = c(n_products,n_countries,n_fdcat,n_countries)) %>% 
  .[,,1:4,]
data$Z <- data$Y <- NULL

# Characterizisation factors
EB3_midpoints <- readRDS(file.path(path2data, "EB34_midpoints.RData")) %>% 
  .[["matrix"]] %>% 
  .[, midpoints_selected]

# Pre-calculate C * S (to only do it once + save RAM)
CS <- t(EB3_midpoints) %*% data$S
rm(EB3_midpoints)
data$S <- NULL
gc()



# 1. declare function ----------------------------------------------------------

.trade_split_fun <- function(i, Z, Y) {
  for (ic in 1:n_countries) {
    for (ip in 1:n_products) {
      s <- rowSums(Z[ip,-ic,,ic])  + rowSums(Y[ip,-ic,,ic]) 
      if (sum(s) > 0) {
        u <- c(colSums(Z[ip,-ic,,ic]), colSums(Y[ip,-ic,,ic])) 
        T_new <- randomize_flows2(u, s)
        Z[ip,-ic,,ic] <- T_new[,1:n_products]
        Y[ip,-ic,,ic] <- T_new[,(n_products+1):ncol(T_new)]
      } 
    }
  }
 
  Z <- matrix(Z, nrow = n_dim, ncol = n_dim) # reshape Z to matrix
  A <- calculate_A(Z, data$x)# A = Z*(xhat^-1)
  rm(Z)
  gc()
  L <- calculate_L(A)  # L = (I-A)^-1
  rm(A)
  gc()
  product_fp <- CS %*% (L) # fp = C * S * L
  rm(L)
  gc() 
  # national fp's
  national_fp <- product_fp %*% matrix(Y, nrow = n_dim, ncol = 4*n_countries) # fp = CSLY
  
  #tic("4. Save product_fp")
  # save results
  saveRDS(product_fp, file.path(path2current_results, 
                                paste0("product_fp", i, ".RData")))
  #toc()
  #tic("5. Save national_fp")
  saveRDS(national_fp, file.path(path2current_results, 
                                 paste0("national_fp", i, ".RData")))
  rm(national_fp, product_fp)
  
  gc()
  #toc()
  return(NULL)
}

# 2. run function -----------------------------------------------------------------
n.cores <- 12  #10 works, 12 definitly too much
N <- 5000 #n.cores*500
RNGkind("L'Ecuyer-CMRG") # choose RNG suitable for parallel computing (see: https://www.r-bloggers.com/%F0%9F%8C%B1-setting-a-seed-in-r-when-using-parallel-simulation/)

# _a) mclapply -----
  results <- mclapply(X = 1:N,
                      FUN = .trade_split_fun,
                      Z = Z, Y = Y,
                      mc.cores = n.cores,
                      mc.set.seed = TRUE)


# 3. save results --------------------------------------------------------------
#saveRDS(results, file.path(path2current_results, "results.RData"))

# THE END ---------------------------------------------------------------------






