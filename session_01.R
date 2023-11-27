# ============================================================================
# Author: CWM
# Date: 6/25/20
# Purpose: Session 1 code notes, created automatically with knitr::purl(*.Rmd)
# ============================================================================

## ----setup, include=FALSE----
knitr::opts_chunk$set(echo = T)
knitr::opts_chunk$set(size = 'scriptsize')


## ----read-in----------------
# aqs_data <- read.csv('aqs_data.csv')
aqs_data <- read.csv('https://raw.githubusercontent.com/cmilando/BU_Rworkshop/main/aqs_data.csv')

## ----dim--------------------
dim(aqs_data)


## ----view, eval=F-----------
## View(aqs_data)


## ---------------------------
aqs_data[1, 2]


## ---------------------------
aqs_data[ c(1, 2), c(1, 2)]


## ---------------------------
typeof(2.0)


## ---------------------------
object.size(2.0)


## ---------------------------
object.size(TRUE)


## ---------------------------
object.size("2")


## ---------------------------
x <- c(4, 5)
x[1]


## ---- eval = F--------------
## aqs_data[ c(1, 2), c(1, 2)]


## ---- eval=F----------------
## head(aqs_data)


## ----message=F--------------
library(tidyverse)

aqs_data %>% glimpse()


## ---------------------------
animal_type <- c('pig', 'pig', 'chicken', 'horse')
farm_animals <- rep(animal_type, times = 100) # repeat 100x
animals_as_factor <- factor(farm_animals)    # convert to factor
#object.size(farm)                # 3416 bytes
#object.size(farm_as_factor)      # 2232 bytes
head(as.integer(animals_as_factor))  # internally alphabetical


## ---------------------------
# animals <- c('pig', 'pig', 'chicken', 'horse')
animal_int_score <- c(10, 10, 2, 8)
farm_int <- rep(animal_int_score, 100) + rnorm(n = 400)
coef(lm(farm_int ~ farm_animals))        # as character
coef(lm(farm_int ~ animals_as_factor))   # as factor, same as above
coef(lm(farm_int ~ as.integer(animals_as_factor))) # different


## ---------------------------
typeof(aqs_data)


## ---------------------------
sites <- unique(aqs_data$Site.Name) # gets unique sites
sites[1]

# STEP 1. Get data from a single station
site_1 <- subset(aqs_data, Site.Name == sites[1])


## ---- eval = F--------------
## 1 == 1    # is the left side 'is equal to' the right side? T
## 1 <= 2    # is the left side 'less than or equal to' the right side? T
## 1 != "1"  # is the left side 'not equal to' the right side


## ---------------------------
# STEP 2: Find max
max_site_1 <- max(site_1$Daily.Mean.PM2.5.Concentration)


## ---------------------------
# STEP 3: print max
paste('Max value for ', sites[2], 'was', max_site_1, 'ug/m3')


## ---------------------------
# STEP 1. Get data from a single station
site_2 <- subset(aqs_data, Site.Name == sites[2])
# STEP 2: Find max
max_site_2 <- max(site_2$Daily.Mean.PM2.5.Concentration)
# STEP 3: print max
paste('Max value for', sites[2], 'was', max_site_1, 'ug/m3')


## ---------------------------
a <- 1
b <- 2
add <- function(arg1, arg2) {
  arg1 + arg2 # could also do return(arg1 + arg2)
}
add(a, b) # you call a function using the ()


## ---------------------------
a <- 1
add <- function(arg1, arg2) {
  a <- 3
  return(arg1 + arg2)
}
add(a, 2) # function is called
a         # unchanged


## ---- eval = F--------------
## get_site_max <- function(  ) {
##   # STEP 1. Get data from a single station
##   site_x <- subset(aqs_data, Site.Name == sites[  ])
##   # STEP 2: Find max
##   max_site_x <- max(site_x$Daily.Mean.PM2.5.Concentration)
##   # STEP 3: print max
##   paste('Max value for', sites[  ], 'was', max_site_x, 'ug/m3')
## }


## ---- error=T---------------
get_site_max <- function(i) {
  # STEP 1. Get data from a single station
  site_x <- subset(aqs_data, Site.Name == sites[i])
  # STEP 2: Find max
  max_site_x <- max(site_x$Daily.Mean.PM2.5.Concentration)
  # STEP 3: print max
  paste('Max value for', sites[i], 'was', max_site_x, 'ug/m3')
}
get_site_max(i = 1)
max_site_i # doesnt exist in global environment


## ---------------------------
length(unique(aqs_data$Site.Name))


## ---------------------------
for(i in 1:3) { # could be 1:11, just wanted to minimize printing
  print(get_site_max(i = i))
}


## ---- eval = F--------------
## for(i in 1:11) {
##   print(get_site_max(i = i))
## }


## ---------------------------
get_site_max_date <- function(i) {
  # STEP 1. Get data from a single station
  site_x <- subset(aqs_data, Site.Name == sites[i])
  # STEP 2: Find max
  max_site_x <- max(site_x$Daily.Mean.PM2.5.Concentration)
  # >>>: Which date was this
  row_of_max <- which(site_x$Daily.Mean.PM2.5.Concentration == max_site_x)
  date_of_max <- site_x$Date[row_of_max]
  # STEP 3: print date of max
  paste('Date of max value for', sites[i], 'was', date_of_max)
}
get_site_max_date(i = 1)


## ---------------------------
typeof(aqs_data$Date[1]) # 05/27/2016


## ---------------------------
test_date   <- '05/27/2016'
strptime(x = test_date, format = '%m/%d/%Y' , tz = 'America/New_York')


## ---------------------------
test_date   <- '05/27/2016'
formatted_date <- strptime(x = test_date,
                          format = '%m/%d/%Y',
                          tz = 'America/New_York')
strftime(formatted_date, format = '%A')


## ---------------------------
aqs_data$Date_formatted <- strptime(x = aqs_data$Date,
                          format = '%m/%d/%Y',
                          tz = 'America/New_York')
aqs_data$day_of_week <- strftime(aqs_data$Date_formatted, format = '%A')
head(aqs_data$day_of_week)


## ---------------------------
get_site_max_date <- function(i) {
  # STEP 1. Get data from a single station
  site_x <- subset(aqs_data, Site.Name == sites[i])
  # STEP 2: Find max
  max_site_x <- max(site_x$Daily.Mean.PM2.5.Concentration)
  # >>>: Which date was this
  row_of_max <- which(site_x$Daily.Mean.PM2.5.Concentration == max_site_x)
  weekday_of_max <- site_x$day_of_week[row_of_max]
  # STEP 3: print date of max
  paste('Weekday value for', sites[i], 'was', weekday_of_max)
}
get_site_max_date(i = 1)


## ---------------------------
worst_weekdays <- vector(mode = "list", length = 11)

length(worst_weekdays)
worst_weekdays[[1]]    # extra bracket for indexing into lists
worst_weekdays %>% glimpse()


## ---------------------------
get_site_p95_date <- function(i) {
  # STEP 1. Get data from a single station
  site_x <- subset(aqs_data, Site.Name == sites[i])
  # STEP 2: Find top 95% value
  p95_site_x <- quantile(site_x$Daily.Mean.PM2.5.Concentration, 0.95)
  # >>>: Which date was at or above the 95th percentile
  rows_p95 <- which(site_x$Daily.Mean.PM2.5.Concentration >= p95_site_x)
  weekdays_of_max <- site_x$day_of_week[rows_p95]
  # STEP 3: return weekdays of at or above the 95th percentile
  return(weekdays_of_max)
}
get_site_p95_date(1)


## ---------------------------
worst_weekdays <- vector(mode = "list", length = 11)

for(i in 1:11) {
  worst_weekdays[[i]] <- get_site_p95_date(i)
}

worst_weekdays %>% glimpse()


## ---- eval=F----------------
## # one by one?
## worst_weekdays_vec <- c(worst_weekdays[[1]], worst_weekdays[[2]], ...)


## ---------------------------
# all together!
worst_weekdays_vec <- do.call(what = c, args = worst_weekdays)


## ---------------------------
table(worst_weekdays_vec)


## ---------------------------
barplot.default(table(worst_weekdays_vec))

