# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# global.R
# METCAP Shiny App
#
# Authors: Olivier Celhay - olivier.celhay@gmail.com, Sheetal Silal - Sheetal.Silal@uct.ac.za
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# empty workspace
rm(list = ls())

# load packages
library(DT)
library(knitr)
library(leaflet)
library(RColorBrewer)
library(scales)
library(shiny)
library(shinyBS)
library(rgeos)
library(shinycssloaders)
library(tidyverse)

# load data
load("./www/data/APLMA_Data_latest.RData")

# minimum elimination package
# this should be manually updated if the package change
country_codes$`Minimum Scenario` <- ifelse(is.na(country_codes$`Minimum Scenario`), 0, country_codes$`Minimum Scenario`)
country_codes$color <- 'grey'
country_codes$color[country_codes$`Minimum Scenario`==0] <- brewer.pal(9, 'Set1')[1]
country_codes$color[country_codes$`Minimum Scenario`==28] <- brewer.pal(9, 'Set1')[2]
country_codes$color[country_codes$`Minimum Scenario`==29] <- brewer.pal(9, 'Set1')[3]
country_codes$color[country_codes$`Minimum Scenario`==37] <- brewer.pal(9, 'Set1')[4]
country_codes$color[country_codes$`Minimum Scenario`==40] <- brewer.pal(9, 'Set1')[5]
country_codes$color[country_codes$`Minimum Scenario`==61] <- brewer.pal(9, 'Set1')[6]
country_codes$color[country_codes$`Minimum Scenario`==65] <- brewer.pal(9, 'Set1')[7]
country_codes$color[country_codes$`Minimum Scenario`==66] <- brewer.pal(9, 'Set1')[8]
country_codes$color[country_codes$`Minimum Scenario`==67] <- brewer.pal(9, 'Set1')[9]
country_codes$color[country_codes$`Minimum Scenario`==68] <- brewer.pal(3, 'Set2')[1]
country_codes$color[country_codes$`Minimum Scenario`==77] <- brewer.pal(3, 'Set2')[2]
country_codes$color[country_codes$`Minimum Scenario`==79] <- brewer.pal(3, 'Set2')[3]

country_codes$scenario_min_elim <- country_codes$scenario_expanded
country_codes$scenario_min_elim[is.na(country_codes$scenario_min_elim)] <- 'Predicted elimination achieved by 2017'

# list of 22 Asia-Pacific countries and subsets
list_countries <- list("Afghanistan"="Afghanistan", 
                       "Bangladesh"="Bangladesh", 
                       "Bhutan"="Bhutan", 
                       "Cambodia"="Cambodia", 
                       "DPR Korea"="DPR Korea", 
                       
                       "India"="India",
                       "Indonesia"="Indonesia", 
                       "Lao PDR"="Lao PDR",
                       "Malaysia"="Malaysia", 
                       "Myanmar"="Myanmar",
                       
                       "Nepal"="Nepal", 
                       "Pakistan"="Pakistan",
                       "Papua New Guinea"="Papua New Guinea", 
                       "People’s Republic of China"="People’s Republic of China",
                       "Philippines"="Philippines", 
                       
                       "Republic of Korea"="Republic of Korea", 
                       "Solomon Islands"="Solomon Islands", 
                       "Sri Lanka"="Sri Lanka",
                       "Thailand"="Thailand", 
                       "Timor-Leste"="Timor-Leste", 
                       
                       "Vanuatu"="Vanuatu", 
                       "Viet Nam"="Viet Nam")

vector_countries <- names(list_countries)

countries_short <- c("af", "bd", "bt", "kh", "kr", 
                     "in", "id", "la", "my", "mm",
                     "np", "pk", "pg", "cn", "ph", 
                     "kp", "sb", "lk", "th", "tl",
                     "vu", "vn")

flags <- c(
  "flags/af.png",
  "flags/bd.png",
  "flags/bt.png",
  "flags/kh.png",
  "flags/kp.png",
  
  "flags/in.png",
  "flags/id.png",
  "flags/la.png",
  "flags/my.png",
  "flags/mm.png",
  
  "flags/np.png",
  "flags/pk.png",
  "flags/pg.png",
  "flags/cn.png",
  "flags/ph.png",
  
  "flags/kr.png",
  "flags/sb.png",
  "flags/lk.png",
  "flags/th.png",
  "flags/tl.png",
  
  "flags/vu.png",
  "flags/vn.png"
)


list_WP <- list_countries[c(4, 14, 8, 9, 13, 15, 16, 17, 21, 22)]
vector_WP <- names(list_WP)
flags_WP <- flags[c(4, 14, 8, 9, 13, 15, 16, 17, 21, 22)]

list_SEA <- list_countries[c(2, 3, 5, 6, 7, 10, 11, 18, 19, 20)]
vector_SEA <- names(list_SEA)
flags_SEA <- flags[c(2, 3, 5, 6, 7, 10, 11, 18, 19, 20)]

list_EMR <- list_countries[c(6, 12)]
vector_EMR <- names(list_EMR)
flags_EMR <- flags[c(6, 12)]

list_GMS <- list_countries[c(4, 8, 10, 19, 22)]
vector_GMS <- names(list_GMS)
flags_GMS <- flags[c(4, 8, 10, 19, 22)]

# list of all predictors (prevalence are not used)
list_prediction <- list(
  `Key Indicators`=c(
    'Reported Incidence Pf + Pv + Mix Pf/Pv' = 'Reported Incidence Pf + Pv + Mix Pf/Pv',
    'Estimated Incidence Pf + Pv + Mix Pf/Pv' = 'Estimated Incidence Pf + Pv + Mix Pf/Pv',
    'Reported fatalities' = 'Reported Fatalities'),
  `Other Indicators`=c(
    'Reported Pf + Mix Pf/Pv Cases'='Reported Pf + Mix Pf/Pv Cases',
    'Reported Pv Cases'='Reported Pv Cases',
    'Estimated Incidence Pf + Mix Pf/Pv'='Estimated Incidence Pf + Mix Pf/Pv',
    'Estimated Incidence Pv'='Estimated Incidence Pv',
    'Severe Incidence  Pf + Pv + Mix Pf/Pv'='Severe Incidence  Pf + Pv + Mix Pf/Pv'
    )
)

# associate a color to each scenario in the Model Stage 2/Scenario Comparison
scenario_colors <- c('grey',  brewer.pal(8, 'OrRd')[c(4, 6, 8)], 'green4', brewer.pal(5, 'Pastel1'))
names(scenario_colors) <- scenario_names
scenario_colors_mda <- c('grey',  brewer.pal(3, 'OrRd'), 'green4', brewer.pal(5, 'Pastel2'))
names(scenario_colors_mda) <- scenario_names


# contextual help
popup_reverse <- HTML('<b>Reverse scenario 3:</b><ul><li>Discontinue IRS activities in 2017</li><li>Discontinue LLIN activities in 2017</li><li>Reduce treatment rates by 50% until 2030</li></ul><b>Reverse scenario 2:</b><ul><li>Discontinue IRS activities in 2017</li><li>Discontinue LLIN activities in 2017</li></ul><b>Reverse scenario 1:</b><ul><li>Discontinue IRS activities in 2017</li></ul>')
popup_continue <- HTML('<b>Business as Usual</b><ul><li>Continue existing vector control interventions (IRS and LLIN distribution) at 2014 coverage rates and maintain treatment rates until 2030.</li></ul>')
popup_accelerate <- HTML('<b>Universal Coverage:</b><ul><li>Increase in treatment rates to 80% by 2025</li><li>Switching from quinine to injectable artesunate for treatment of severe malaria in 2017</li></ul><hr><b>IRS:</b><ul><li><b>Universal Coverage</b> plus</li><li>Double IRS coverage from 2017</li></ul><hr><b>Effective Usage:</b><ul><li><b>Universal Coverage</b> plus</li><li>Increase effectiveness of LLINs from 15% to 30%</li><li>Increase surveillance</li></ul>')
popup_innovate <- HTML('<b>Single dose new Pv treatment:</b><ul><li><b>Effective Usage</b> plus</li><li>Replace Primaquine with a single dose drug such as Tafenoquine</li></ul><b>New LLINs:</b><ul><li><b>Single dose new Pv treatment</b> plus </li><li>New LLINs with double life of existing nets</li></ul><b>New Pf drug:</b><ul><li><b>New LLINs</b> plus</li><li>Replace ACT with new candidate for Pf treatment</li></ul>')
