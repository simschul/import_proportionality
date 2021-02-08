library(tidyverse)
library(data.table)
library(easyalluvial)
library(ggalluvial)
library(my.utils)
library(gridExtra)
library(cowplot)
library(ape)
library(tikzDevice)
library(RColorBrewer)
library(ggthemes)


as.sparse.matrix <- function(mat) {
  mat <- data.table::as.data.table(mat)
  colnames(mat) <- paste0(1:ncol(mat))
  #cat(is.data.table(mat))
  mat <- mat[, "row" := 1:.N]
  # mat <- cbind(mat, row = 1:nrow(mat))
  mat <- data.table::melt(mat, id.vars = "row", na.rm = TRUE,
                          variable.name = "col") %>%
    .[, col := as.integer(col)] %>%
    .[]
  return(mat)
}

extrafont::loadfonts()
theme_set(theme_minimal() + #base_size = 9
              theme(legend.position = "none", 
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.text.y=element_blank(), 
                    plot.title = element_text(hjust = 0.5, size = 12),
                    text=element_text(family="DejaVu Serif", size = 12)))

my_scale_fill <-scale_fill_colorblind()
# scale_fill_discrete(palette = "Set2"))
# scale_fill_brewer(type = "qual", palette = "Set2"))
label_size <- 3

randomize_flows2 <- function(x,y, case=NULL) {
  new_flow_mat <- matrix(0, nrow = length(y), 
                         ncol = length(x), 
                         dimnames = list(names(y), 
                                         names(x)))
  #y = supply, x = use
  yleft <- y 
  j <- 1
  order <- 0L
  
  if (is.null(case)) seq <- sample(1:length(x))
  else if (case == 1) seq <- c(3, sample((1:length(x))[-3]))
  else if (case == 2) seq <- c(2, sample((1:length(x))[-2]))
  else if (case == 3) seq <- c(6, sample((1:length(x))[-6]))
  
  for (i in seq) {
    #  cat(i, ",")
    order[length(order)+1] <- i
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
  return(list(Tmat = new_flow_mat, order = order[-1]))
}

s <- c(20,40,30) 
u <- c(10,25,20,10,15,10)
names(s) <- paste0('r', 1:3)
names(u) <- c(paste0('p',1:4), paste0('fd',1:2))



# 1. Random import matrices ----------------------------------------------------

set.seed(1)
# Create 3 random import matrices
N <- 3
p <- vector("list", length = N)
for (i in 1:N) {
  data <- randomize_flows2(u,s, case = i) 
  order <- data$order
  data <-   as.sparse.matrix(data$Tmat)
  setnames(data, c('supply', 'use', 'flow'))
  data[, label:=ifelse(data$use %in% 1:4, 
                       paste0('p', data$use), paste0("fd", data$use - 4))]
  data[, use:=as.character(use)]
  for(j in 1:6) data[use==as.character(j), use:=paste0(which(order==j), '_', use)][]
  data[, supply := factor(supply, levels=c(1,2,3))]
  data
  p[[i]] <-   ggplot(data,
                     aes(y = flow, axis1 = supply, axis2 = use, label=label)) +
    geom_alluvium(aes(fill = supply), width = 1/12) +
    geom_stratum(width = 2/12, fill = "white", color = "black") +
    scale_x_discrete(limits = c("s", "u"), expand = c(.05, .05))+
    geom_text(stat = "stratum", label.strata = TRUE, size = label_size) +
    geom_label(aes(x = c(rep(1, 3), rep(NA, 15)), 
                   y= c(cumsum(rev(s)) - (0.5*rev(s)),rep(NA, 15)), 
                   label = c(paste0("r", 4:2), rep(NA, 15))),
               label.size = NA, size = label_size, 
               family = 'Latin Modern Sans Serif') +
   # scale_fill_brewer(type = "qual", palette = "Set1") +
    labs(x = NULL, y = NULL) + 
    ggtitle(mixedFontLabel(LETTERS[i], ': Case ', i, bold = 1, sep='')) +
    my_scale_fill
  
    # ggtitle(expression(paste0(bold(LETTERS[i]), ': Case ', i)))
}
p[[1]]

# grid.arrange(grobs =p, nrow=1)
# plot_grid(plotlist=p, nrow=1)

#ggsave("alluvial_plot.png")



# 2. Proportionality Assumption ------------------------------------------------
prop_flows <- function(s,u) {
  props <- s / sum(s)
  return(matrix(props,ncol=1) %*% matrix(u,nrow=1))
}

data <- prop_flows(u=u,s=s) 
data <-   my.utils::as.sparse.matrix(data)
setnames(data, c('supply', 'use', 'flow'))
data[, label:=ifelse(data$use %in% 1:4, 
                     paste0('p', data$use), paste0("fd", data$use - 4))]
data[, use:=as.character(use)]
data[, supply := factor(supply, levels=c(1,2,3))]

p_prop <- ggplot(data,
       aes(y = flow, axis1 = supply, axis2 = use, label=label)) +
  geom_alluvium(aes(fill = supply), width = 1/12) +
  geom_stratum(width = 1.5/12, fill = "white", color = "black") +
  scale_x_discrete(limits = c("s", "u"), expand = c(.05, .05))+
  geom_text(stat = "stratum", label.strata = TRUE, size = label_size) +
  geom_label(aes(x = c(rep(1, 3), rep(NA, 15)), 
                 y= c(cumsum(rev(s)) - (0.5*rev(s)),rep(NA, 15)), 
                 label = c(paste0("r", 4:2), rep(NA, 15))),
             label.size = NA, size = label_size, 
             family = 'Latin Modern Sans Serif') +
  labs(x = NULL, y = NULL) + 
  ggtitle(mixedFontLabel('D', ': Proportionality Assumption', 
                         bold = 1, sep=''))+
  my_scale_fill


# 3. random split method -------------------------------------------------------
data <- r2dtable(1, r = s, c = u)[[1]]
data <-   my.utils::as.sparse.matrix(data)
setnames(data, c('supply', 'use', 'flow'))
data[, label:=ifelse(data$use %in% 1:4, 
                     paste0('p', data$use), paste0("fd", data$use - 4))]
data[, use:=as.character(use)]
data[, supply := factor(supply, levels=c(1,2,3))]

p_rand <- ggplot(data,
       aes(y = flow, axis1 = supply, axis2 = use, label=label)) +
  geom_alluvium(aes(fill = supply), width = 1/12) +
  geom_stratum(width = 1.5/12, fill = "white", color = "black") +
  scale_x_discrete(limits = c("s", "u"), expand = c(.05, .05))+
  geom_text(stat = "stratum", label.strata = TRUE, size = label_size) +
  geom_label(aes(x = c(rep(1, 3), rep(NA, 15)), 
                 y= c(cumsum(rev(s)) - (0.5*rev(s)),rep(NA, 15)), 
                 label = c(paste0("r", 4:2), rep(NA, 15))),
              label.size = NA, size = label_size, 
             family = 'Latin Modern Sans Serif') +
  labs(x = NULL, y = NULL) + 
  ggtitle(mixedFontLabel('E', ': Random Split', 
                         bold = 1, sep='')) +
  my_scale_fill


# options(tikzDocumentDeclaration = "\\documentclass{bmcart}")
# options(tikzDocumentDeclaration = "\\documentclass[10pt]{article}")


(final_plot <- grid.arrange(arrangeGrob(p[[1]], p[[2]],p[[3]], ncol = 3), 
                            arrangeGrob(p_prop, p_rand, ncol=2), nrow=2))

ggsave("figures/figure2.png",final_plot,  width = 170, height = 150,
       units = 'mm', dpi = 600)



# tikz(file = "plots/plot_test.tex", width = 5, height = 5, standAlone = FALSE)
# 
# (final_plot <- grid.arrange(arrangeGrob(p[[1]], p[[2]],p[[3]], ncol = 3), 
#             arrangeGrob(p_prop, p_rand, ncol=2), nrow=2))
#  
#  dev.off()







