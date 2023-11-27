# ============================================================================
# Author: CWM
# Date: 6/25/20
# Purpose: Session 2 code notes, created automatically with knitr::purl(*.Rmd)
# ============================================================================


## ----setup, include=FALSE----
# knitr::opts_chunk$set(echo = T)
# knitr::opts_chunk$set(size = 'scriptsize')


## ----read-in----------------
# aqs_data <- read.csv('aqs_data.csv')
aqs_data <- read.csv('https://raw.githubusercontent.com/cmilando/BU_Rworkshop/main/aqs_data.csv')



## ----eval=F-----------------
## library(haven) # part of tidyverse
## raw <- read_sas("../SCC db/brfss2012.sas7bdat")


## ----eval=F-----------------
## library(readxl) # part of tidyverse
## read_excel("my_excel_file.xlsx")


## ----eval=F-----------------
## library(rgdal)  # for shapefiles and spatial data
## library(raster) # for the library of projections
## PUMA_USA <- readOGR(dsn = 'ipums_puma_2010.shp')
## PUMA_MA <- subset_sp(PUMA_USA, STATEFIP == 25)


## ---------------------------
# ======================================================
# Author: CWM
# Date: 6/24/2020
# Purpose: Scripts to use for R coding session
# ======================================================


## ---- message=F, eval = F----
## library(tidyverse)
##
## # read in file
## aqs_data <- read.csv('aqs_data.csv')
##
## # --------------------------------------------- (... to 80)
## # Section 1: find max
##
## ## ...
## <code>


## ---------------------------
aqs_data_wide <- read.csv('https://raw.githubusercontent.com/cmilando/BU_Rworkshop/main/wide_fmt.csv')


## ---------------------------
# need a new function that handles missing values
get_max <- function(x) max(x, na.rm = T)

# starting at column 2 because 1st column is Date
get_max(aqs_data_wide[, 2])

# for each column 2 to the end
for (j in 2:ncol(aqs_data_wide)) {
  print(get_max(aqs_data_wide[, j]))
}


## ---------------------------
apply(X = aqs_data_wide[, -1],  # the matrix
      MARGIN = 2,               # (2) means columns, (1) means rows
      FUN = get_max)            # function to "apply" to each row or col



## ---------------------------
my_list <- vector('list', length = 10)
site_IDs <- unique(aqs_data$Site.ID)
for (i in 1:length(site_IDs)) {
  my_list[[i]] <- subset(aqs_data, Site.ID == site_IDs[i],
                         select = Daily.Mean.PM2.5.Concentration)
}

lapply(my_list, get_max)


## ---------------------------
library(parallel)
detectCores()


## ---------------------------
get_max <- function(x) max(x, na.rm = T)

## Loop 1
set.seed(1) # if you randomize, use this for reproducability
bs_rows <- sample(x = 1:nrow(aqs_data_wide), size = 500, replace = T)
head(apply(X = aqs_data_wide[bs_rows, -1], 2, get_max))

## Loop 2
set.seed(2)
bs_rows <- sample(x = 1:nrow(aqs_data_wide), size = 500, replace = T)
head(apply(X = aqs_data_wide[bs_rows, -1], 2, get_max))



## ---------------------------
get_bs_max <- function(loop_i) {
  set.seed(loop_i)
  bs_rows <- sample(x = 1:nrow(aqs_data_wide), size = 500, replace = T)
  bs_max <- apply(X = aqs_data_wide[bs_rows, -1], 2, get_max)
  return(bs_max)
}
head(get_bs_max(2)) # matches previous slide!



## ---- message=F-------------
library(foreach)
library(doParallel)
seeds <- 1:5

# this tells R the number of cores
registerDoParallel(detectCores())

# this uses rbind, function that combines all outputs together
foreach (i = seeds, .combine=rbind) %dopar% {
  get_bs_max(i)
}

# close the cluster when done
stopImplicitCluster()


## ---------------------------
head(aqs_data[, 1:6])


## ---- message = F-----------
library(tidyverse)
aqs_data %>%
  group_by(Site.ID, POC) %>%  # similar to looping over matrix columns
  summarize(site_max = max(Daily.Mean.PM2.5.Concentration))


## ---- echo=F----------------
t(aqs_data[1, ])


## ---------------------------
wide_fmt <- aqs_data %>%
  pivot_wider(id_cols = c(Site.ID, Date, POC),
              names_from = c(Site.ID, POC),
              values_from = Daily.Mean.PM2.5.Concentration)


## ---- eval = F--------------
##   aqs_data %>%
##     group_by(Site.ID, POC, ...) # other constraints
##     summarize(max = max(val),
##               min = min(val), ...) # other summary stats


## ---- fig.height = 3--------
# libary(ggplot) is not necessary, part of tidyverse

# add a shorter conc column
aqs_data$PM25 <- aqs_data$Daily.Mean.PM2.5.Concentration

ggplot(data = aqs_data) +
  geom_boxplot(mapping = aes(x = PM25), color = 'blue')


## ---- eval = F--------------
## ggplot(data = aqs_data) +


## ---- eval = F--------------
## geom_boxplot(mapping = aes(x = PM25), color = 'blue')


## ---- fig.height = 3.5------
ggplot(data = aqs_data) +
  geom_boxplot(aes(x = PM25, y = Site.Name), color = 'blue')


## ---- fig.height = 3.5------
ggplot(data = aqs_data) +
  geom_boxplot(aes(x = PM25, y = Site.Name, fill = COUNTY),
               color = 'blue')


## ---- fig.height=3----------
ggplot(data = aqs_data) +
  geom_boxplot(aes(x = PM25, y = Site.Name, fill = COUNTY),
               color = 'black') +
  theme(panel.background = element_rect(fill = 'white')) +
  coord_cartesian(xlim = c(2, 15))


## ----echo = F---------------
library(RColorBrewer)


## ---- fig.height=3----------
ggplot(data = aqs_data) +
  geom_boxplot(aes(x = PM25, y = Site.Name, fill = COUNTY),
               color = 'black') +
  theme(panel.background = element_rect(fill = 'white')) +
  coord_cartesian(xlim = c(2, 15)) +
  scale_fill_brewer(palette = 'Paired')


## ---- echo = F, message = F----
library(tools)


## ---- message=F, eval = F----
## ggplot(data = aqs_data) +
##   geom_boxplot(aes(x = PM25, y = toTitleCase(tolower(Site.Name)),
##                    fill = COUNTY), color = 'black') +
##   ylab(NULL) +
##   xlab(expression(PM[2.5]~(mu*g/m^3))) + #modify x axis title
##   theme_classic(base_size = 12, base_family = 'serif') +
##   theme(axis.text.y = element_text(hjust = 0)) +
##   coord_cartesian(xlim = c(2, 15)) +
##   scale_fill_brewer(palette = 'Paired')


## ---- echo = F, fig.height=4----
ggplot(data = aqs_data) +
  geom_boxplot(aes(x = PM25, y = toTitleCase(tolower(Site.Name)),
                   fill = COUNTY), color = 'black') +
  ylab(NULL) +
  xlab(expression(PM[2.5]~(mu*g/m^3))) + #modify x axis title
  theme_classic(base_size = 14, base_family = 'serif') +
  theme(axis.text.y = element_text(hjust = 0)) +
  coord_cartesian(xlim = c(2, 15)) +
  scale_fill_brewer(palette = 'Paired')


## ---- fig.height=3.5--------
ggplot(data = subset(aqs_data, COUNTY == 'Essex')) +
  geom_boxplot(aes(x = factor(POC), y = PM25, fill = factor(POC)),
               color = 'black') +
  theme_bw() +
  facet_grid(~ Site.Name) # rows ~ column factors


## ---- echo = F, message = F----
library(gridExtra)


## ---- fig.height = 4--------
p1 <- ggplot(iris) + geom_boxplot(aes(x = Species, y = Sepal.Length))
p2 <- ggplot(iris) + stat_density(aes(x = Petal.Length, fill = Species))

grid.arrange(p1, p2, nrow = 1, widths = c(1, 1.5))


## ---- eval = F--------------
## p1 <- ggplot(iris) + geom_boxplot(aes(x = Species, y = Sepal.Length))
## p2 <- ggplot(iris) + stat_density(aes(x = Petal.Length, fill = Species))
##
## final <- grid.arrange(p1, p2, nrow = 1, widths = c(1, 1.5))
##
## ggsave(filename = "final.png", plot = final, width = 8, height = 3,
##        units = "in", dpi = 300)
##

