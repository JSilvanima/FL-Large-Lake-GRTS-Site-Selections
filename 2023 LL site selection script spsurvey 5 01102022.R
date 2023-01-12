# File: 2023 LL site selection script.R
# Purpose: Florida Lakes Survey Design
# Original Programmer for SPLUS: Tony Olsen 
# Modifications by Jay Silvanima, Stephanie Sunderman-Barnes, and Chris Sedlacek, 
# Date: 12/29/2023

# Code updated 12/29/2023 so it may be run in spsurvey 5 by Jay Silvanima. 
# This is the first set of unequal probability site selections performed using spsurvey version 5.4.0.  

#  Code developed using R version 4.1.2 and spsurvey version 5.4.0. 

##Set directory. This is where the outputs will be saved. 
#   Alter to desired location. Use getwd() to determine the directory for
#     your r project.

getwd()

# Load spsurvey
library(spsurvey)
library(dplyr)

# Create two simple features objects from shapefiles.  
# 1) Polygon features representing the 2023 target population of large lakes (Cycle17_LargeLakes_coverage_2023).  
# 2) Polygon features representing the Zones (Watershed_Monitoring_Section_(WMS)_Cycle_3_Reporting_Units).  
# Change all projections to Florida Albers HARN(CRS code 3087).  

dsgn_ll <- st_read(dsn=".",layer="Cycle17_LargeLakes_coverage_2023")
wms_c3_reporting_units <- st_read(dsn=".",layer="Watershed_Monitoring_Section_(WMS)_Cycle_3_Reporting_Units")
wms_c3_reporting_units <- st_transform(wms_c3_reporting_units, crs = 3087)
wms_c3_reporting_units

# For large lakes target population: Convert all column names to lowercase. Inspect data.  

names(dsgn_ll)<-tolower(names(dsgn_ll))
names(dsgn_ll)
head(dsgn_ll)
tail(dsgn_ll)

# Calculate Lake polygon areas per c3_zone for all zones.  

lakearea<-tapply(dsgn_ll$hectares,list(dsgn_ll$repunit), sum)
lakearea[is.na(lakearea)] <- 0
round(addmargins(lakearea),1)

# Plot the reporting unit polygons and 2023 target population of large lakes polygons.  
jpeg('2023_LL_Population.jpg', units = 'in', width = 7, height = 7, res = 300)
plot(st_geometry(wms_c3_reporting_units), border='darkgray', main= '2023 Target Population of Large Lakes')
plot(st_geometry(dsgn_ll), border = 'blue', col = 'blue', add = TRUE)
legend(150000, 300000, legend=c('Zones','Lakes'), col=c('darkgray','blue'),lty=c(1,1))
dev.off()

# Create a factor column to use as a stratum variable and check the components in 
# the stratum and c3_zone columns.

dsgn_ll$stratum<-factor(as.character(dsgn_ll$repunit))
levels(dsgn_ll$stratum)
head(dsgn_ll)

# Create lake area categories for each Region to use
# process is to do calculation and then select cutpoints 
# manually based on that information

dsgn_ll$area_cat <- rep(NA,nrow(dsgn_ll))
dsgn_ll$area_cat <- dsgn_ll$hectares
abbr <- c("Zone 1","Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6")
names(abbr) <- levels(dsgn_ll$stratum)


### Unequal probability of selection within each strata based on lake area.  
#  Within each stratum lakes are ordered from smallest to largest area.  
#  Then the cumulative sum of lake area is computed, split into five equal 
#  area parts, and the lake identified at the boundary of the parts.  
#  The area of those five lakes is used to identify the lake area categories
#  for the unequal probability sampling.  For example using large lakes, 
#  the Suwannee region has 89 large lakes with a total lake area of 7632.871
#  ha.  The five cumulative area breaks are 48 ha, 222 ha, 467 ha, 805 ha, 
#  and 1630+ ha. The area categories for the Suwannee are [10,49], (49,215],
#  (215,467], (467,805], and (805,1.63e+03]. The number of lakes in each area
#  category is 68, 14, 4, 2, and 1. The total lake area in each area category
#  is 1484.778 ha, 1545.794 ha, 1406.269 ha, 1534.205 ha, and 1633.170 ha, 
#  respectively.

# The below loop creates five lake categories based on size per reporting 
#  unit.   

for(i in levels(dsgn_ll$stratum) ) {
  tst <- dsgn_ll$stratum == i
  itmp <- order(dsgn_ll$hectares[tst])
  tmp <- cumsum(sort(dsgn_ll$hectares[tst]))
  n <- length(itmp)
  if(i != "Zone 6")   # Special case for Lake Okeechobee
  {ctmp <- cut(tmp, 
               breaks=c(seq(10, tmp[n], length=6)),
               include.lowest = TRUE)
  }
  else
  {ctmp <- cut(tmp, 
               breaks=c(seq(10, tmp[n-1], length=5), tmp[n]),
               include.lowest = TRUE)
  }
  icut <- cumsum(table(ctmp))
  acut <- unique(ceiling(dsgn_ll$hectares[tst][itmp][icut]))
  dsgn_ll$area_cat[tst] <- 
    paste(abbr[i],(cut(dsgn_ll$hectares[tst], breaks=c(10,acut),
                       include.lowest=TRUE)), sep="_")
}
dsgn_ll$area_cat <- factor(dsgn_ll$area_cat)


area_cat <-levels(dsgn_ll$area_cat)
area_cat <-as.data.frame(area_cat)

write.csv(area_cat, "area_cat.csv")

# Create lake area data frame, rename columns to 'Zones', 'Number of Lakes, and 'hectares' and remove row names.  
# Write out data frame to .csv file.  

dsgn_ll %>%
  group_by(repunit) %>%
  summarize(lake_number = n_distinct(lakecode))

Zones <-c("Zone 1","Zone 2","Zone 3", "Zone 4", "Zone 5", "Zone 6", "Sum")
Lake_Number <-c("239","92","733","501","125","9","1699")
Lakeareas <- data.frame(Zones,Lake_Number, round(addmargins(lakearea),1))
names(Lakeareas) <-c("Zones", "Number of Lakes", "hectares") 
row.names(Lakeareas)<- NULL
Lakeareas
write.csv(Lakeareas, "2023 Large Lake Framesize.csv", row.names = FALSE)

# Create lake size category data frame, rename columns to 'area_cat' and 'Number of Lakes' and remove row names.  
# Write out data frame to .csv file. 

area_cat_summary <- data.frame(table(dsgn_ll$area_cat))
names(area_cat_summary) <- c('area_cat','Number_Lakes')
area_cat_summary
write.csv(area_cat_summary, "2023 Large Lake Size Categories.csv", row.names = FALSE)

### Survey Design: 
#    A Generalized Random Tessellation Stratified (GRTS) survey design for 
#     an areal lake resource was used.  The GRTS design includes reverse 
#     hierarchical ordering of the selected sites.

#  Create the stratification to be used.

LL_base <-c("Zone 1"=15,"Zone 2"=15,"Zone 3"=15, "Zone 4"=15, "Zone 5"=15, "Zone 6"=15)
LL_over <-c("Zone 1"=135,"Zone 2"=135,"Zone 3"=135, "Zone 4"=135, "Zone 5"=135, "Zone 6"=135)
LL_select <-c("Zone 1"="unequal","Zone 2"="unequal","Zone 3"="unequal", "Zone 4"="unequal", "Zone 5"="unequal", "Zone 6"="unequal")

LL_catyn <-list("Zone 1" = c('Zone 1_(1.02e+03,1.43e+03]' = 3,
                          'Zone 1_(1.43e+03,3.27e+03]' = 3,
                          'Zone 1_(144,1.02e+03]' = 3,
                          'Zone 1_(45,144]' = 3,
                          'Zone 1_[10,45]' = 3),
                        "Zone 2" = c('Zone 2_(215,467]' = 3,
                          'Zone 2_(467,805]' = 3,
                          'Zone 2_(49,215]' = 3,
                          'Zone 2_(805,1.59e+03]' = 3,
                          'Zone 2_[10,49]' = 3), 
                        "Zone 3" = c('Zone 3_(1.78e+03,3.26e+03]' = 3,
                          'Zone 3_(182,1.78e+03]' = 3,
                          'Zone 3_(3.26e+03,7.44e+03]' = 3,
                          'Zone 3_(7.44e+03,1.76e+04]' = 3,
                          'Zone 3_[10,182]' = 3),
                        "Zone 4" = c('Zone 4_(1.32e+03,1.86e+03]' = 3,
                          'Zone 4_(175,429]' = 3,
                          'Zone 4_(429,1.32e+03]' = 3,
                          'Zone 4_(55,175]' = 3,
                          'Zone 4_[10,55]' = 3), 
                        "Zone 5" = c('Zone 5_(1.89e+03,3.97e+03]' = 3,
                          'Zone 5_(3.97e+03,9.66e+03]' = 3,
                          'Zone 5_(730,1.89e+03]' = 3,
                          'Zone 5_(9.66e+03,1.25e+04]' = 3,
                          'Zone 5_[10,730]' = 3),
                        "Zone 6" = c('Zone 6_(150,163]' = 3,
                          'Zone 6_(163,228]' = 3,
                          'Zone 6_(228,1.29e+05]' = 3,
                          'Zone 6_(59,150]' = 3,
                          'Zone 6_[10,59]' = 3))


# Run random sample once to get random seed and put result into set.seed.  
# Reason is so that exactly the same sites can be reproduced if rerun.
# Don't change set.seed unless you want a different set of sites.

sample(1000000,1)

set.seed(598771) 

# Create variable to keep track of how long spsurvey takes to run grts function.

dsgntime <- proc.time()  # keep track of how long spsurvey takes


### Create the GRTS survey design__  
#    Stratification:  
#     Stratify by zones/basins created for statewide coverage.  
#     Expected sample size: 15 sites within six of the state’s zones.  
#     Oversample: 9x sample sites for each zone.  
#     Site Use: The base design has 15 sites for each of the six zones in the 
#       stratum.  Sites are listed in SiteID order and must be used in that 
#       order.  All sites that occur prior to the last site used must have been
#       evaluated for use and then either sampled or the reason documented as 
#       to why that site was not used.

# Print the initial six lines of the survey design.  
# Print dsgntime to view run time for grts function in minutes.

sites<- grts(dsgn_ll, 
             stratum_var="stratum", 
             n_base = LL_base, 
             n_over = LL_over, 
             seltype = LL_select, 
             caty_var = "area_cat",
             caty_n = LL_catyn,
             wgt_units = "ha",
             pt_density =  1000,
             DesignID="FLLL23001")

head(sites)
dsgntime <- (proc.time() - dsgntime)/60
dsgntime

# Print the initial six lines of the survey design
head(sites)

# Make sure the columns are matching the previous submissions
# Still need to check on old submission to determine column names
#   for matching.

sites<-sp_rbind(sites)

table(sites$stratum)

## Plotting code supplied by Stephanie Sunderman-Barnes 8/16/2022
wms_c3_reporting_units <- st_read(dsn=".",layer="Watershed_Monitoring_Section_(WMS)_Cycle_3_Reporting_Units")
wms_c3_reporting_units <- st_transform(wms_c3_reporting_units, crs = 3087)
sites_base <-subset(sites, (sites$siteuse == 'Base'))
sites_over <-subset(sites, (sites$siteuse == 'Over'))
plot(st_geometry(wms_c3_reporting_units))
plot(st_geometry(dsgn_ll), col = 'blue', add = TRUE)
plot(st_geometry(sites_over), pch = 1, col = 'red', add = TRUE)
plot(st_geometry(sites_base), pch = 21, bg = 'red', add = TRUE)

# To check the selection summary and export out an excel and shapefile run the following
sp_summary(sites)
sf::st_write(sites,"2023_LL_sites.shp", append = FALSE)
write.csv(sites, "2023_LL_site_selections.csv", row.names = FALSE)


